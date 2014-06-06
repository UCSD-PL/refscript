{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DoAndIfThenElse           #-}


module Language.Nano.Typecheck.Sub (convert, isSubtype, safeExtends) where

import           Language.ECMAScript3.PrettyPrint
import           Control.Applicative                ((<$>), (<*>))
import           Data.Function                      (on)
import qualified Data.HashSet                       as S
import           Data.Maybe                         (fromJust, catMaybes)
import           Data.List                          (elem, groupBy, sort, nub, (\\))
import           Control.Monad.State
import           Control.Monad.Error                hiding (Error)
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc 
import qualified Language.Fixpoint.Types            as F

import           Language.Nano.Env
import           Language.Nano.Types
import           Language.Nano.Misc
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Lookup
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Unify
import           Language.Nano.Errors
import           Data.Monoid                  
import qualified Data.HashMap.Strict                as M
import           Language.ECMAScript3.Syntax

-- import           Debug.Trace                      (trace)
import qualified System.Console.CmdArgs.Verbosity   as V



type PPR r = (PP r, F.Reftable r)
   
type TDR r = TDefEnv (RType r)


instance PP a => PP (S.HashSet a) where
  pp = pp . S.toList 


isSubtype :: (PPR r) => SourceSpan -> TDR r -> RType r -> RType r -> Bool
isSubtype l δ t1 t2 =
  case convert l δ t1 t2 of
    Right CNo       -> True
    Right (CUp _ _) -> True
    _               -> False


-- | @convert@ returns:
-- * An equivalent version of @t1@ that has the same sort as the first input type
-- * An equivalent version of @t2@ that has the same sort as the second input type
-- * A subtyping direction between @t1@ and @t2@
--  
-- Padding the input types gives them the same sort, i.e. makes them compatible. 
--------------------------------------------------------------------------------
convert :: (PPR r) => SourceSpan -> TDR r -> RType r -> RType r -> Either Error (Cast r)
--------------------------------------------------------------------------------

convert l _ t1 t2 | toType t1 == toType t2 
                                       = Right $ CNo

convert _ _ t1 t2 | not (isTop t1) && 
                  isTop t2             = Right $ CUp t1 t2

convert l δ t1 t2 | any isUnion [t1,t2]  = convertUnion l δ t1 t2

convert l δ t1 t2 | all isTObj  [t1,t2]  = convertObj l δ t1 t2

convert l δ t1 t2 | all isTFun  [t1, t2] = convertFun l δ t1 t2

convert l δ t1 t2                        = convertSimple l δ t1 t2 


-- | `convertObj`

--------------------------------------------------------------------------------
convertObj :: (PPR r) => SourceSpan -> TDR r -> RType r -> RType r -> Either Error (Cast r)
--------------------------------------------------------------------------------
convertObj l δ t1@(TCons e1s μ1 r1) t2@(TCons e2s μ2 r2)
  --
  --                       μ <: μ
  --  ∀i . [comb(μ,μi)]fi:ti <: [comb(μ,μi)]fi:ti
  -- ------------------------------------------------
  --        [μ]{ [μi]fi:ti } <: [μ]{ [μi]fi:ti }
  --
  | s1l == s2l 

  = if isSubtype l δ (ofType μ1) (ofType μ2) then 
      if deeps l δ μ1 μ2 b1s b2s then  
        Right $ CNo
      else 
        Left $ errorSimpleSubtype l t1 t2
    else 
      Left $ errorIncompMutTy l t1 t2      
  -- 
  -- NO: { f1:t1,..,fn:tn } <: { f1:t1,..,fn:tn,..,fm:tm }
  --
  | not (S.null df21) 
  = Left $ errorMissFlds l t1 t2 df21
  -- 
  --  [μ]{ f1:t1,..,fn:tn } <: [μ]{ f1:t1,..,fn:tn }
  -- ------------------------------------------------------------
  --  [μ]{ f1:t1,..,fn:tn,..,fm:tm } <: [μ]{ f1:t1,..,fn:tn }
  --
  | otherwise
  = case convertObj l δ (TCons e1s μ1 r1) (TCons e2s μ2 r2) of
      Left l  -> Left  $ l
      Right _ -> Right $ CUp t1 t2

    where
       -- All the bound elements that correspond to each binder 
       -- Map : symbol -> [ elements ]
       (m1,m2)    = mapPair toMap (e1s, e2s)
       toMap      = foldr mi M.empty . filter (\x -> nonStaticElt x && nonConstrElt x)
       mi e       = M.insertWith (++) (F.symbol e) [e]

       -- Binders for each element
       (s1s,s2s)  = mapPair (S.fromList . M.keys) (m1,m2)
       (s1l,s2l)  = mapPair (sort     . M.keys) (m1,m2)
       
       cs         = S.toList $ S.intersection s1s s2s

       -- Join elements on common binder
       -- cmnBinds   = idxMap (\s -> (lkp s m1, lkp s m2)) cs where lkp s = fromJust . M.lookup s
       -- idxMap f   = map $ \x -> (x, f x)
       
       b1s        = map (`lkp` m1) cs 
       b2s        = map (`lkp` m2) cs 

       lkp s      = fromJust . M.lookup s

       -- Difference and intersection of keys
       df21       = s2s `S.difference` s1s
       in12       = s1s `S.intersection` s2s

       e1s       = concat [ fromJust $ M.lookup s m1 | s <- S.toList in12 ]

       group1     = [ (F.symbol $ safeHead "convertObj-1" g1s, g1s) | g1s <- groupBy sameBinder e1s ]
       group2     = [ (F.symbol $ safeHead "convertObj-2" g2s, g2s) | g2s <- groupBy sameBinder e2s ]


-- FIXME !!!!!!!!
-- Do nominal subtyping and fall back to structural if the former fails.
convertObj l δ t1@(TApp (TRef x1 s1) t1s r1) t2@(TApp (TRef x2 s2) t2s r2)
  | (x1,s1) == (x2,s2)
  = do -- FIXME: Using covariance here !!!
       let bs = zipWith (isSubtype l δ) t1s t2s
       if and bs then Right $ CNo
                 else Left  $ errorSimpleSubtype l t1 t2
  | otherwise
  = case weaken δ (findSymOrDie x1 δ,t1s) x2 of
      Just (_, t1s) -> Right $ CUp (TApp (TRef x2 s1) t1s r1) t2
      Nothing        -> convertObj l δ (flattenType δ t1) (flattenType δ t2)
                          
convertObj l δ t1@(TApp (TRef _ _) _ _) t2 
  = convertObj l δ (flattenType δ t1) t2

convertObj l δ t1 t2@(TApp (TRef _ _) _ _)
  = convertObj l δ t1 (flattenType δ t2) 

convertObj _ _ _ _ =  error "BUG: Case not supported in convertObj"


-- | Deep subtyping for object members

deeps l δ μ1 μ2 e1s e2s = and $ zipWith (deep l δ μ1 μ2) e1s e2s

-- | Treat the parts of `e1s` and `e2s` that correspond to the same binder 
--   as intersection types.
--
-- |  ∃ i s.t. si <: t
-- | ----------------------
-- |  s1 /\ ... /\ sn <: t
--
deep l δ μ1 μ2 e1s e2s = or $ map (\e1 -> deep1 l δ μ1 μ2 e1 e2s) e1s

--
-- |  s <: t1  ...  s <: tn
-- | ------------------------
-- |   s <: t1 /\ ... /\ tn
--
deep1 l δ μ1 μ2 e es = and $ map (subElt l δ μ1 μ2 e) es


-- subElt :: PPR r => SourceSpan -> Mutability -> Mutability -> TElt (RType r) -> TElt (RType r) -> TCM r Bool

-- | Call Signatures 
--    
--    ts<:ts  t<:t 
--  ---------------------------------------
--    { (ts)=>t } <: { (ts)=>t } 
--
subElt l δ _ _ (CallSig t1) (CallSig t2)
  = isSubtype l δ t1 t2

-- | Field signatures
--
--  { μ f[τ]: t } <: { μ f[τ]: t }
--
-- NO :   { mutable f: PosInt  } <: { immutable f: int }
--
-- NO :   { mutable f: PosInt  } <: { mutable f: int }
--
-- NO :   { readonly f: PosInt  } <: { readonly f: int }
--
subElt l δ μ1 μ2 f1@(FieldSig _ _ μf1 τ1 t1) f2@(FieldSig _ _ μf2 τ2 t2)
  = let m1 = combMut μ1 μf1 in
    let m2 = combMut μ2 μf2 in
    if isSubtype l δ (ofType m1) (ofType m2) then 
      if isImmutable m2 then
        -- 
        --  t<:t  τ<:τ
        -- ------------------------------------------
        --  { immut f[τ]: t } <: { immut f[τ]: t }
        --
        and [ isSubtypeOpt l δ τ2 τ1, isSubtype l δ t1 t2 ]
      else 
        --  
        --  μ,μ =/= Immutable
        --  t<:t  t<:t  τ<:τ  τ<:τ
        -- ----------------------------------
        --  { μ f[τ]: t } <: { μ f[τ]: t }
        --
        and [ isSubtypeOpt l δ τ1 τ2, isSubtype l δ t1 t2,
              isSubtypeOpt l δ τ2 τ1, isSubtype l δ t2 t1 ]
    else
      False 
      -- Left $ errorIncompMutElt l f1 f2
  

-- | Constructor signatures
--
-- { new (ts)=>() } <: { new (ts)=>() } 
--
subElt l δ _ _ (ConsSig t1) (ConsSig t2)
  = isSubtype l δ t1 t2 

-- | Index signatures
--
--    τ = τ  t<:t  t<:t
--  -----------------------------------
--    { [x:τ]: t } <: { [x:τ]: t }
--
subElt l δ _ _ (IndexSig _ b1 t1) (IndexSig _ b2 t2)
  | b1 == b2 
  = and [ isSubtype l δ t1 t2, isSubtype l δ t2 t2 ]

-- | otherwise fail
subElt _ _ _ _ _ _ = False


isSubtypeOpt l δ (Just t1) (Just t2) = isSubtype l δ t2 t1
isSubtypeOpt l δ Nothing   (Just t2) = isSubtype l δ tTop t2
isSubtypeOpt l δ (Just t1) Nothing   = isSubtype l δ t1 tTop
isSubtypeOpt _ _ _         _         = True


-- | `convertFun`
--------------------------------------------------------------------------------
convertFun :: PPR r => SourceSpan -> TDR r -> RType r -> RType r -> Either Error (Cast r)
--------------------------------------------------------------------------------
convertFun l δ t1@(TFun b1s o1 _) t2@(TFun b2s o2 _) =
    if length b1s /= length b2s then 
      do  cs <- zipWithM (convert l δ) (b_type <$> b2s) (b_type <$> b1s)
          co <- convert l δ o1 o2
          if all noCast cs && noCast co then 
            Right $ CNo
          else if all dnCast cs && upCast co then 
            Right $ CUp t1 t2
          else 
            Left $ errorFuncSubtype l t1 t2
    else 
      Left $ errorFuncSubtype l t1 t2


convertFun _ _ _ _ = error "convertFun: no other cases supported"


-- | `convertSimple`
--------------------------------------------------------------------------------
convertSimple :: PPR r => SourceSpan -> TDR r -> RType r -> RType r -> Either Error (Cast r)
--------------------------------------------------------------------------------
convertSimple l _ t1 t2
  | t1 == t2      = Right $ CNo
  -- TOGGLE dead-code
--   | otherwise = return $ CDead t2
  | otherwise     = Left  $ errorSimpleSubtype l t1 t2


-- | `convertUnion`
--------------------------------------------------------------------------------
convertUnion :: PPR  r => SourceSpan -> TDR r -> RType r -> RType r -> Either Error (Cast r)
--------------------------------------------------------------------------------
convertUnion l _ t1 t2 = parts   $ unionParts t1 t2
  where 
    parts (_,[],[])  = Right $ CNo
    parts (_,[],_ )  = Right $ CUp t1 t2
    parts (_,_ ,[])  = Right $ CDn t1 t2
    parts (_, _ ,_)  = Left  $ errorUnionSubtype l t1 t2


--------------------------------------------------------------------------------
safeExtends :: PPR r => SourceSpan -> TDR r -> TDef (RType r) -> [Error]
--------------------------------------------------------------------------------
safeExtends l δ (TD _ c _ (Just (p, ts)) es) =
    [ errorClassExtends l c p δ ee pe | pe <- flatten δ (findSymOrDie p δ,ts)
                                      , ee <- es, sameBinder pe ee 
                                      , let t1 = eltType ee
                                      , let t2 = eltType pe
                                      , not (isSubtype l δ t1 t2) ]
safeExtends _ _ (TD _ _ _ Nothing _)  = []

