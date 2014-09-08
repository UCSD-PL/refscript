{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ImpredicativeTypes        #-}

{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DoAndIfThenElse           #-}


module Language.Nano.Typecheck.Sub (convert, isSubtype, safeExtends, Related (..)) where

import           Control.Applicative                ((<$>))

import           Data.Generics
import qualified Data.HashSet                       as S
import           Data.List                          (sort)
import           Data.Maybe                         (maybeToList, catMaybes, fromMaybe)
import           Control.Monad.State
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc 
import qualified Language.Fixpoint.Types            as F

import           Language.ECMAScript3.PrettyPrint

import           Language.Nano.Annots
import           Language.Nano.Types
import           Language.Nano.Misc                 (setSnd4)
import           Language.Nano.Names
import           Language.Nano.Locations
import           Language.Nano.Environment
import           Language.Nano.Typecheck.Environment
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Errors
import qualified Data.HashMap.Strict                as M

-- import           Debug.Trace                      (trace)

type PPR r = (PP r, F.Reftable r)

instance PP a => PP (S.HashSet a) where
  pp = pp . S.toList 


--------------------------------------------------------------------------------
isSubtype :: (PPR r, Functor g, EnvLike () g) 
          => g r -> RType r -> RType r -> Bool
--------------------------------------------------------------------------------
isSubtype γ t1 t2 =
  case convert (srcPos dummySpan) γ t1 t2 of
    Right CNo       -> True
    Right (CUp _ _) -> True
    _               -> False


-- | `convert`
--------------------------------------------------------------------------------
convert :: (PPR r, Functor g, EnvLike () g) 
        => SourceSpan -> g r -> RType r -> RType r -> Either Error (Cast r)
--------------------------------------------------------------------------------
convert l γ t1 t2  
  =  do c <- convert' l γ' τ1 τ2
        case c of 
          CDNo   -> return $ CNo
          CDUp   -> return $ CUp (rType t1) (rType t2)
          CDDn   -> return $ CDn (rType t1) (rType t2)
          CDDead -> return $ CDead (errorDeadCast l t1 t2) (rType t1)
  where
    rType = ofType . toType
    τ1    = toType t1
    τ2    = toType t2
    γ'    = fmap (\_ -> ()) γ


--------------------------------------------------------------------------------
convert' :: (Functor g, EnvLike () g)
         => SourceSpan -> g () -> Type -> Type -> Either Error CastDirection
--------------------------------------------------------------------------------
convert' _ _ t1 t2 | toType t1 == toType t2     = Right CDNo

convert' _ _ t1 t2 | not (isTop t1) && isTop t2 = Right CDUp

-- UNDEFINED
-- convert' _ _ t1 t2 | isUndef t1                 = Right CDUp

convert' l γ t1 t2 | any isUnion [t1,t2]        = convertUnion   l γ t1 t2

convert' l γ t1 t2 | all isTObj  [t1,t2]        = convertObj     l γ t1 t2

convert' l γ t1 t2 | all isTFun  [t1, t2]       = convertFun     l γ t1 t2

convert' l γ (TClass  c1) (TClass  c2)          = convertTClass  l γ c1 c2

convert' l γ (TModule m1) (TModule m2)          = convertTModule l γ m1 m2

convert' l γ t1 t2                              = convertSimple  l γ t1 t2


-- | `convertObj`

--------------------------------------------------------------------------------
convertObj :: (Functor g, EnvLike () g)
           => SourceSpan -> g () -> Type -> Type -> Either Error CastDirection
--------------------------------------------------------------------------------
convertObj l γ t1@(TCons e1s μ1 r1) t2@(TCons e2s μ2 r2)

  --
  --                       μ <: μ
  --  ∀i . [comb(μ,μi)]fi:ti <: [comb(μ,μi)]fi:ti
  -- ------------------------------------------------
  --        [μ]{ [μi]fi:ti } <: [μ]{ [μi]fi:ti }
  --
  | sameBinders && mutabilitySub && deepSub = Right $ CDUp
  | sameBinders && mutabilitySub            = Left  $ errorSubtype l t1 t2
  | sameBinders                             = Left  $ errorIncompMutTy l t1 t2 
  -- 
  -- NO: { f1:t1,..,fn:tn } <: { f1:t1,..,fn:tn,..,fm:tm }
  --
  | not nullDif                             = Left $ errorMissingFields l t1 t2 df21
  -- 
  --  [μ]{ f1:t1,..,fn:tn } <: [μ]{ f1:t1,..,fn:tn }
  -- ------------------------------------------------------------
  --  [μ]{ f1:t1,..,fn:tn,..,fm:tm } <: [μ]{ f1:t1,..,fn:tn }
  --
  | otherwise                               = convertObj l γ c1 c2

    where

      sameBinders   = s1l == s2l
      mutabilitySub = isSubtype γ μ1 μ2

      deepSub       = deeps l γ μ1 μ2 b1s b2s
      nullDif       = S.null df21

      c1            = TCons e1s' μ1 r1 
      c2            = TCons e2s  μ2 r2

      e1s'          = [ e1 | e1 <- e1s
                           , F.symbol e1 `M.member` m1  
                           , F.symbol e1 `M.member` m2 ]
        
      -- All the bound elements that correspond to each binder 
      -- Map : symbol -> [ elements ]
      (m1,m2)       = mapPair toMap (e1s, e2s)
      -- Filter out constructors
      toMap         = foldr mi M.empty . filter nonConstrElt
      mi e          = M.insertWith (++) (F.symbol e) [e]
      -- Binders for each element
      (s1s,s2s)     = mapPair (S.fromList . M.keys) (m1,m2)
      (s1l,s2l)     = mapPair (sort     . M.keys) (m1,m2)
      -- Join elements on common binder
      (b1s,b2s)     = unzip [ (e1,e2) | s  <- S.toList in12 
                                      , e1 <- maybeToList $ M.lookup s m1 
                                      , e2 <- maybeToList $ M.lookup s m2 ]
      -- Difference and intersection of keys
      df21          = s2s `S.difference` s1s
      in12          = s1s `S.intersection` s2s
 
convertObj l γ t1@(TApp (TRef x1) (m1:t1s) _) t2@(TApp (TRef x2) (m2:t2s) _)

  -- * Incompatible mutabilities
  | not (isSubtype γ m1 m2) 
  = Left $ errorIncompMutTy l t1 t2
  
  -- * Both immutable, same name
  --
  --    Co-variant subtyping
  --
  | x1 == x2 && isImmutable m2 
  = do z <- zipWithM (convert l γ) t1s t2s
       if       all (noCast . castDirection) z then Right $ CDNo
       else if  all (dnCast . castDirection) z then Right $ CDDn
       else if  all (upCast . castDirection) z then Right $ CDUp
       else                                         Left  $ errorSubtype l t1 t2

  -- * Non-immutable, same name
  --
  --    We are treating the upcast case as a downcast (co/contra-variance)
  --
  --    FIXME: this is a bit dodgy !!!
  --
  | x1 == x2 
  = do z <- zipWithM (convert l γ) t1s t2s
       if       all (noCast . castDirection) z then Right $ CDNo
       else if  all (dnCast . castDirection) z then Right $ CDDn
       else if  all (upCast . castDirection) z then Right $ CDDn
       else                                         Left  $ errorSubtype l t1 t2


  -- * Compatible mutabilities, differenet names:
  | otherwise       
  = case weaken γ x1 x2 t1s of
      -- Adjusting the child class to the parent
      Just (_, t1s') -> 
          if all (uncurry $ isSubtype γ) $ zip t1s' t2s
            then Right CDUp 
            else Left  $ errorSubtype l t1 t2
      
      -- Structural subtyping
      Nothing       -> 
          case (flattenType γ t1, flattenType γ t2) of 
            (Just ft1, Just ft2) -> convertObj l γ ft1 ft2
            (Nothing , Nothing ) -> Left $ errorUnresolvedTypes l t1 t2
            (Nothing , _       ) -> Left $ errorUnresolvedType l t1 
            (_       , Nothing ) -> Left $ errorUnresolvedType l t2
  where
                          
convertObj l γ t1@(TApp (TRef _) _ _) t2 
  = case flattenType γ t1 of 
      Just ft1 -> convertObj l γ ft1 t2
      Nothing  -> Left $ errorUnresolvedType l t1

convertObj l γ t1 t2@(TApp (TRef _) _ _)
  = case flattenType γ t2 of 
      Just ft2 -> convertObj l γ t1 ft2
      Nothing  -> Left $ errorUnresolvedType l t2

convertObj l _ t1 t2 =  Left $ unimplemented l "convertObj" $ ppshow t1 ++ " -- " ++ ppshow t2


-- | Deep subtyping for object members

deeps l γ μ1 μ2 e1s e2s = and $ zipWith (deep l γ μ1 μ2) e1s e2s

-- | Treat the parts of `e1s` and `e2s` that correspond to the same binder 
--   as intersection types.
--
-- |  ∃ i s.t. si <: t
-- | ----------------------
-- |  s1 /\ ... /\ sn <: t
--
deep l γ μ1 μ2 e1s e2s = or $ map (\e1 -> deep1 l γ μ1 μ2 e1 e2s) e1s

--
-- |  s <: t1  ...  s <: tn
-- | ------------------------
-- |   s <: t1 /\ ... /\ tn
--
deep1 l γ μ1 μ2 e es = and $ map (subElt l γ μ1 μ2 e) es


-- subElt :: PPR r => SourceSpan -> Mutability -> Mutability -> TypeMember (RType r) -> TypeMember (RType r) -> TCM r Bool

-- | Call Signatures 
--    
--       ts<:ts          t<:t 
--  ---------------------------------------
--    { (ts)=>t } <: { (ts)=>t } 
--
subElt _ γ _ _ (CallSig t1) (CallSig t2)
  = isSubtype γ t1 t2

-- | Field signatures
--
--  { μ f: t } <: { μ f: t' }
--
-- NO :   { mutable  f: PosInt  } <: { immutable f: int }
-- NO :   { mutable  f: PosInt  } <: { mutable   f: int }
-- NO :   { readonly f: PosInt  } <: { readonly  f: int }
--
subElt _ γ μ1 μ2 (FieldSig _ μf1 t1) (FieldSig _ μf2 t2)
  | isSubtype γ m1 m2 =
      if isSubtype γ m2 t_immut then
        -- 
        --               t <: t'
        -- ------------------------------------------
        --  { immut f: t } <: { immut f: t' }
        --
        isSubtype γ t1 t2
      else 
        --  
        --  μ,μ =/= Immutable
        --  t<:t  t<:t
        -- ----------------------------------
        --  { μ f: t } <: { μ f: t }
        --
        and [ isSubtype γ t1 t2, isSubtype γ t2 t1 ]

  | otherwise = False 
  where
    t_immut = t_immutable -- t_Immutable $ get_common_ts γ
    -- FIXME
    m1      = combMut μ1 μf1
    m2      = combMut μ2 μf2

-- | Methods
-- 
subElt _ γ _ _ (MethSig _ _ t1) (MethSig _ _ t2) =
  isSubtype γ t1 t2
  

-- | Constructor signatures
--
-- { new (ts)=>() } <: { new (ts)=>() } 
--
subElt _ γ _ _ (ConsSig t1) (ConsSig t2)
  = isSubtype γ t1 t2 

-- | Index signatures
--
--    τ = τ  t<:t  t<:t
--  -----------------------------------
--    { [x:τ]: t } <: { [x:τ]: t }
--
subElt _ γ _ _ (IndexSig _ b1 t1) (IndexSig _ b2 t2)
  | b1 == b2 
  = and [ isSubtype γ t1 t2, isSubtype γ t2 t2 ]

-- | Static fields
--
subElt _ γ μ1 μ2 (StatSig _ μf1 t1) (StatSig _ μf2 t2)
  | isSubtype γ m1 m2 && isSubtype γ m2 t_immut 
  = isSubtype γ t1 t2
  | isSubtype γ m1 m2                        
  = and [ isSubtype γ t2 t1, isSubtype γ t1 t2 ]
  | otherwise                                   
  = False 
  where
    t_immut = t_immutable -- t_Immutable $ get_common_ts γ
    -- FIXME 
    m1      = combMut μ1 μf1
    m2      = combMut μ2 μf2

-- | otherwise fail
subElt _ _ _ _ _ _ = False



-- | `convertFun`
--------------------------------------------------------------------------------
convertFun :: (Functor g, EnvLike () g)
           => SourceSpan -> g () -> Type -> Type -> Either Error CastDirection
--------------------------------------------------------------------------------
convertFun l γ t1@(TFun s1 b1s o1 _) t2@(TFun s2 b2s o2 _) 
  | length b1s == length b2s
  = do  cs <- zipWithM (convert' l γ) (s1' : map b_type b2s) (s2' : map b_type b1s)
        co <- convert' l γ o1 o2
        if all noCast cs && noCast co then 
          Right CDNo
        else if all dnCast cs && upCast co then 
          Right CDUp
        else 
          Left $ errorFuncSubtype l t1 t2
  | otherwise 
  = Left $ errorFuncSubtype l t1 t2
  where
    s1' = fromMaybe tTop s1 
    s2' = fromMaybe tTop s2

convertFun l γ t1@(TAnd _) t2@(TAnd t2s) = 
  if and $ isSubtype γ t1 <$> t2s then Right CDUp
                                  else Left  $ errorFuncSubtype l t1 t2

convertFun l γ t1@(TAnd t1s) t2 = 
  let f t1 = isSubtype γ t1 t2 in 
  if or $ f <$> t1s then Right CDUp
                    else Left  $ errorFuncSubtype l t1 t2
 
convertFun l _ t1 t2 = Left $ unsupportedConvFun l t1 t2


-- | `convertTClass`
--------------------------------------------------------------------------------
convertTClass :: (Functor g, EnvLike () g)
              => SourceSpan -> g () -> RelName -> RelName -> Either Error CastDirection
--------------------------------------------------------------------------------
convertTClass l _ c1 c2 | c1 == c2                  = Right CDNo  
                        | otherwise                 = Left  $ errorTClassSubtype l c1 c2

-- | `convertTModule`
--------------------------------------------------------------------------------
convertTModule :: (Functor g, EnvLike () g)
              => SourceSpan -> g () -> RelPath -> RelPath -> Either Error CastDirection
--------------------------------------------------------------------------------
convertTModule l γ c1 c2 = 
  case (absolutePathInEnv γ c1, absolutePathInEnv γ c2) of
    (Just _, Just _) -> Right CDNo
    _                -> Left $ errorTModule l c1 c2



-- | `convertSimple`
--------------------------------------------------------------------------------
convertSimple :: (Functor g, EnvLike () g)
              => SourceSpan -> g r -> Type -> Type -> Either Error CastDirection
--------------------------------------------------------------------------------
convertSimple _ _ t1 t2
  | t1 == t2      = Right CDNo
  -- TOGGLE dead-code
  | otherwise = Right CDDead 
--  | otherwise     = Left  $ errorSubtype l t1 t2


-- | `convertUnion`
--------------------------------------------------------------------------------
convertUnion :: (Functor g, EnvLike () g)
             => SourceSpan -> g () -> Type -> Type -> Either Error CastDirection
--------------------------------------------------------------------------------
convertUnion _ γ t1 t2  
  | upcast    = Right CDUp 
  | deadcast  = Right CDDead
  | otherwise = Right CDDn
  where 
    upcast                = all (\t1 -> any (isSubtype γ t1) t2s) t1s
    deadcast              = all (\t1 -> not $ any (isSubtype γ t1) t2s) t1s
    (t1s, t2s)            = sanityCheck $ mapPair bkUnion (t1, t2)
    sanityCheck ([ ],[ ]) = errorstar "unionParts', called on too small input"
    sanityCheck ([_],[ ]) = errorstar "unionParts', called on too small input"
    sanityCheck ([ ],[_]) = errorstar "unionParts', called on too small input"
    sanityCheck ([_],[_]) = errorstar "unionParts', called on too small input"
    sanityCheck p         = p


--------------------------------------------------------------------------------
safeExtends :: (Data r, PPR r) => SourceSpan -> TCEnv r -> IfaceDef r -> Maybe Error
--------------------------------------------------------------------------------
safeExtends _ _ (ID _ _ _ Nothing _)         = Nothing
safeExtends l γ (ID _ c _ (Just (p, ts)) es) = 
    case catMaybes $ uncurry validElt <$> compairablePairs of 
       [] -> Nothing
       es -> Just $ errorClassExtends l c p (F.symbol <$> es)
  where
    compairablePairs  = [ (ee, filter (sameBinder ee) ps) | ee <- es ]
    -- parent            = resolveRelNameInEnv γ p
    ps                = case  resolveRelNameInEnv γ p of
                          Just par  -> concat $ maybeToList (flatten False γ (par,ts))
                          Nothing -> []
    validElt c ps     | null ps                = Nothing
                      | any (compatElt γ c) ps = Nothing
                      | otherwise              = Just c

compatElt γ (CallSig t1      ) (CallSig t2)       = isSubtype γ t1 t2 
compatElt _ (ConsSig _       ) (ConsSig _ )       = True
compatElt γ (IndexSig _ _ t1 ) (IndexSig _ _ t2)  = isSubtype γ t1 t2 && isSubtype γ t2 t1 
compatElt γ (FieldSig _ _ t1 ) (FieldSig _ _ t2)  = isSubtype γ t1 t2 
compatElt γ (MethSig _ m1 t1 ) (MethSig _ m2 t2)  = isSubtype (fmap (const ()) γ) m1 m2 && isSubtype γ (clearThis t1) (clearThis t2)
  where
  -- get rid of the 'this' part before doing the check
    clearThis = mkAnd . map mkFun . concat . maybeToList . fmap (map (`setSnd4` Nothing)) . bkFuns
compatElt γ (StatSig _ m1 t1 ) (StatSig _ m2 t2)  = isSubtype (fmap (const ()) γ) m1 m2 && isSubtype γ t1 t2 
compatElt _ _                  _                  = False 



-- | Related types ( ~~ ) 

class Related t where
  related :: (Functor g, EnvLike () g, PPR r) => g r -> t r -> t r -> Bool

instance Related RType where
  related γ t t' = isSubtype γ t t' || isSubtype γ t' t
  
instance Related TypeMember where
  related γ (CallSig t1)      (CallSig t2)      = related γ t1 t2
  related γ (ConsSig t1)      (ConsSig t2)      = related γ t1 t2
  related γ (IndexSig _ _ t1) (IndexSig _ _ t2) = related γ t1 t2
  related γ (StatSig _ _ t1)  (StatSig _ _ t2)  = related γ t1 t2
  related γ (FieldSig _ _ t1) (FieldSig _ _ t2) = related γ t1 t2
  -- Mutability should have been checked earlier
  related γ (MethSig  _ _ t1) (MethSig  _ _ t2) = related γ t1 t2
  related _ _                       _           = False 
 
