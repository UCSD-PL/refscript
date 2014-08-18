{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Language.Nano.Typecheck.Typecheck (verifyFile, typeCheck {-, testFile-}) where 

import           Control.Applicative                ((<$>))
import           Control.Monad                

import qualified Data.HashMap.Strict                as M 
import           Data.Maybe                         (catMaybes, fromMaybe, listToMaybe, fromJust, maybeToList, isJust)
import           Data.List                          (find)
import           Data.Function                      (on)
import           Data.Generics                   

import           Text.PrettyPrint.HughesPJ          (text, ($+$))

import           Language.Nano.CmdLine              (getOpts)
import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Annots
import           Language.Nano.Env
import qualified Language.Nano.Misc                 as NM
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Parse 
import           Language.Nano.Typecheck.Sub
import           Language.Nano.Typecheck.TCMonad
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Lookup
import           Language.Nano.Liquid.Alias
import           Language.Nano.SSA.SSA

import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Misc             as FM 
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax.Annotations
import           Debug.Trace                        hiding (traceShow)

import qualified System.Console.CmdArgs.Verbosity as V


--------------------------------------------------------------------------------
-- | Top-level Verifier 

--------------------------------------------------------------------------------
verifyFile :: [FilePath] -> IO (UAnnSol a, F.FixResult Error)
--------------------------------------------------------------------------------
verifyFile fs = parse fs $ ssa $ tc

parse fs next = parseNanoFromFiles fs >>= next

ssa   next p  = ssaTransform p >>= either (lerror . single) (next . expandAliases)

tc    p       = typeCheck p    >>= either unsafe safe


--------------------------------------------------------------------------------
-- testFile fs = parseNanoFromFiles fs 
--           >>= ssaTransform  
--           >>= either (print . pp) (\p -> typeCheck (expandAliases p)
--           >>= either (print . vcat . (pp <$>)) 
--                      (\(Nano {code = Src ss}) -> 
--                            {- print (pp p') >> -} print "Casts:" >> print (pp $ getCasts ss)))
--------------------------------------------------------------------------------

lerror        = return . (NoAnn,) . F.Unsafe

unsafe errs = do putStrLn "\n\n\nErrors Found!\n\n" 
                 forM_ errs (putStrLn . ppshow) 
                 return $ (NoAnn, F.Unsafe errs)

safe (Nano {code = Src fs})
  = do nfc       <- noFailCasts <$> getOpts
       return     $ (NoAnn, failCasts nfc fs)


-- | Cast manipulation

failCasts True  _  = F.Safe
failCasts False fs = applyNonNull F.Safe F.Unsafe $ concatMap castErrors $ getCasts fs 

-------------------------------------------------------------------------------
getCasts         :: (Data r, Typeable r) => [Statement (AnnType r)] -> [AnnType r]
-------------------------------------------------------------------------------
getCasts stmts   = everything (++) ([] `mkQ` f) stmts
  where 
    f            :: Expression (AnnType r) -> [(AnnType r)]
    f (Cast a _) = [a]
    f _          = [] 

-------------------------------------------------------------------------------
castErrors :: PPR r => AnnType r -> [Error] 
-------------------------------------------------------------------------------
castErrors (Ann l facts) = downErrors
  where 
    downErrors           = [errorDownCast l t1 t2 | TCast _ (CDn t1 t2) <- facts]


-------------------------------------------------------------------------------
typeCheck :: (Data r, PPR r) => NanoSSAR r -> IO (Either [Error] (NanoTypeR r))
-------------------------------------------------------------------------------
typeCheck pgm = do 
  v <- V.getVerbosity
  let r = execute v pgm $ tcNano pgm 
  return $ snd <$> r


-------------------------------------------------------------------------------
-- | TypeCheck Nano Program
-------------------------------------------------------------------------------
tcNano :: (Data r, PPRSF r) => NanoSSAR r -> TCM r (AnnInfo r, NanoTypeR r)
-------------------------------------------------------------------------------
tcNano p@(Nano {code = Src fs})
  = do  setDef         $ defs p
        checkInterfaces p
        (fs', _)      <- tcStmts γ fs
        m             <- getAnns 
        θ             <- getSubst
        let p1         = p {code = (patchAnn m . apply θ) <$> Src fs'}
        d             <- getDef
        return         $ (m, p1 { defs  = d })     -- Update TDefEnv before exiting
    where
       γ       = initEnv p


-- FIXME: check for mutability parameter
checkInterfaces p = 
    forM_ (concatMap (safeExtends l (defs p)) is) tcError
  where 
    l  = srcPos dummySpan
    is = [ d |d@(TD False _ _ _ _) <- tDefToList $ defs p ]

patchAnn m (Ann l fs) = Ann l $ sortNub $ eo ++ fo ++ ti ++ fs 
  where
    ti                = [f | f@(TypInst _ _    ) <- M.lookupDefault [] l m]
    fo                = [f | f@(Overload _ _   ) <- M.lookupDefault [] l m]
    eo                = [f | f@(EltOverload _ _) <- M.lookupDefault [] l m]

initEnv pgm      = TCE (envUnion (specs pgm) (externs pgm)) (specs pgm)
                       emptyContext


-------------------------------------------------------------------------------
-- | Typecheck Environment ----------------------------------------------------
-------------------------------------------------------------------------------

--   We define this alias as the "output" type for typechecking any entity
--   that can create or affect binders (e.g. @VarDecl@ or @Statement@)
--   @Nothing@ means if we definitely hit a "return" 
--   @Just γ'@ means environment extended with statement binders

type TCEnvO r = Maybe (TCEnv r)


data TCEnv r  = TCE { 
    tce_env  :: Env (RType r)               -- ^ This starts off the same
                                            --   as tce_spec, but grows with
                                            --   typechecking
  , tce_spec :: Env (RType r)               -- ^ Program specs. Includes:
                                            --    * functions 
                                            --    * variable annotations
                                            --    * ambient variable declarations
                                            --    * class types (after being computed)
  , tce_ctx  :: !IContext 
  }


-- Q: Who needs this?
-- A: Unification is not local any more. For example an empty array may be
--    created and its type can be instantiated only after a use of it which can 
--    be arbitrarily far from creation point. At that point we need to update
--    the current environment by applying the substitution.
--
instance PPR r => Substitutable r (TCEnv r) where 
  apply θ γ = γ { tce_env = apply θ (tce_env γ) }

instance PPR r => PP (TCEnv r) where
  pp = ppTCEnv

ppTCEnv (TCE env spc ctx) 
  =   text "******************** Environment ************************"
  $+$ pp env
  $+$ text "******************** Specifications *********************"
  $+$ pp spc 
  $+$ text "******************** Call Context ***********************"
  $+$ pp ctx


tcEnvPushSite i γ            = γ { tce_ctx = pushContext i    $ tce_ctx γ }

tcEnvAdds                   :: (IsLocated a, F.Reftable r) => [(Id a, RType r)] -> TCEnv r -> TCEnv r
tcEnvAdds     x γ            = γ { tce_env = envAdds x $ tce_env γ }

tcEnvAddReturn x t γ         = γ { tce_env = envAddReturn x t $ tce_env γ }
-- tcEnvMem x                   = envMem x      . tce_env 
tcEnvFindTy x                = envFindTy x   . tce_env
tcEnvFindReturn              = envFindReturn . tce_env

tcEnvFindSpec x              = envFindTy x   . tce_spec 
tcEnvFindSpecOrTy x γ        = msum [tcEnvFindSpec x γ, tcEnvFindTy x γ]

tcEnvFindTyOrDie l x         = fromMaybe ugh . tcEnvFindTy x  where ugh = die $ errorUnboundId (ann l) x


-------------------------------------------------------------------------------
-- | Shorthand aliases 
-------------------------------------------------------------------------------
type PPR r = (PP r, F.Reftable r)
type PPRSF r = (PPR r, Substitutable r (Fact r), Free (Fact r)) 


-------------------------------------------------------------------------------
-- | TypeCheck Function 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
tcFun :: PPR r =>
  TCEnv r -> Statement (AnnSSA r) -> TCM r (Statement (AnnSSA r), Maybe (TCEnv r))
-------------------------------------------------------------------------------
tcFun γ (FunctionStmt l f xs body)
  = case tcEnvFindTy f γ of
      Nothing   -> die $ errorMissingSpec (srcPos l) f
      Just ft   -> do ts    <- tcFunTys l f xs ft
                      body' <- foldM (tcFun1 γ l f xs) body ts
                      return $ (FunctionStmt l f xs body', Just γ) 
tcFun _  s = die $ bug (srcPos s) $ "Calling tcFun not on FunctionStatement"

-------------------------------------------------------------------------------
tcFun1 :: (PPR r, IsLocated l, CallSite t) 
       => TCEnv r -> (AnnSSA r) -> l -> [Id (AnnSSA r)] -> [Statement (AnnSSA r)] 
       -> (t, ([TVar], [RType r], RType r)) -> TCM r [Statement (AnnSSA r)]
-------------------------------------------------------------------------------
tcFun1 γ l f xs body (i, (αs,ts,t)) = tcFunBody γ' l body t
  where 
    γ'                              = envAddFun f i αs xs ts t γ 

-- FIXME: Check for mutability (the second part in the triplet)
--        If this argument is "immutable" We will have to check
--        statements/expressions that operate on "this" and make sure that they
--        do not mutate it.
--
--        For the moment it just does a regular function check
--        
tcMeth1 γ l f xs body (i, _,ft) = tcFun1 γ l f xs body (i, ft)


tcFunBody γ l body t = tcStmts γ body >>= go
  where go (_, Just _) | t /= tVoid
                       = tcError $ errorMissingReturn (srcPos l)
        go (b, _     ) | otherwise                        
                       = return b

-------------------------------------------------------------------------------
envAddFun :: (F.Reftable r, IsLocated c, IsLocated a, CallSite b) =>
  a -> b -> [TVar] -> [Id c] -> [RType r] -> RType r -> TCEnv r -> TCEnv r
-------------------------------------------------------------------------------
envAddFun f i αs xs ts t = tcEnvAdds tyBinds 
                         . tcEnvAdds (varBinds xs ts) 
                         . tcEnvAddReturn f t
                         . tcEnvPushSite i 
  where  
    tyBinds                = [(tVarId α, tVar α) | α <- αs]
    varBinds               = zip
    
-- -------------------------------------------------------------------------------
-- validInst :: (PP a, Free a) => TCEnv r -> (SourceSpan, a) -> Maybe Error
-- -------------------------------------------------------------------------------
-- validInst γ (l, ts)
--   = case [β | β <- HS.toList $ free $ ts, not $ tVarId β `tcEnvMem` γ] of
--       [] -> Nothing
--       βs -> Just $ errorFreeTyVar l βs

-- | Strings ahead: HACK Alert
tVarId (TV a l) = Id l $ "TVAR$$" ++ F.symbolString a   


---------------------------------------------------------------------------------------
tcClassElt :: PPR r 
          => TCEnv r -> Id (AnnSSA r) -> ClassElt (AnnSSA r) -> TCM r (ClassElt (AnnSSA r))
---------------------------------------------------------------------------------------
--
-- FIXME: 1. Check for void return type for constructor
--        2. Use tcMeth1 instead of tcFun1
--
tcClassElt γ _ (Constructor l xs body) 
  = case [ c | ConsAnn c  <- ann_fact l ] of
      [ConsSig ft]  -> do t        <- tcFunTys l i xs ft
                          body'    <- foldM (tcFun1 γ l i xs) body t
                          return    $ Constructor l xs body'
      _             -> tcError $ unsupportedNonSingleConsTy $ srcPos l
  where i   = Id l "constructor"

-- Using a function call to "init" to keep track of overloading on field initialization
--
-- FIXME: should the rest of the fields be in scope ???
--
tcClassElt γ cid (MemberVarDecl l static (VarDecl l1 x eo))
  = case anns of 
      []  ->  tcError       $ errorClassEltAnnot (srcPos l1) cid x
      fs  ->  case eo of
                Just e     -> do ([e'],_)  <- tcNormalCall γ l1 "field init" [e] $ ft fs
                                 return     $ (MemberVarDecl l static (VarDecl l1 x $ Just e'))
                Nothing    -> return        $ (MemberVarDecl l static (VarDecl l1 x Nothing))
  where
    anns | static    = [ s | StatAnn  s <- ann_fact l1 ]
         | otherwise = [ f | FieldAnn f <- ann_fact l1 ]
    ft flds = mkAnd $ catMaybes $ mkInitFldTy <$> flds

--
--  Currently we allow a single type annotation that can be an overloaded
--  function though. Proceed as follows:
--    1. Get all overloads
--    2. Check the body for each one of them
--
--  FIXME: check for mutability (purity)
--
tcClassElt γ cid (MemberMethDecl l static i xs body) 
  = case anns of 
      [mt]  -> do mts    <- tcMethTys l i mt
                  body'  <- foldM (tcMeth1 γ l i xs) body mts
                  return  $ MemberMethDecl l static i xs body'
      _    -> tcError     $ errorClassEltAnnot (srcPos l) cid i
  where
    anns | static    = [ (m, t) | StatAnn (StatSig _ m t)  <- ann_fact l ]
         | otherwise = [ (m, t) | MethAnn (MethSig _ m t)  <- ann_fact l ]


--------------------------------------------------------------------------------
tcSeq :: (TCEnv r -> a -> TCM r (b, TCEnvO r)) -> TCEnv r -> [a] -> TCM r ([b], TCEnvO r)
--------------------------------------------------------------------------------
tcSeq f             = go []
  where
    go acc γ []     = return (reverse acc, Just γ)
    go acc γ (x:xs) = do (y, γo) <- f γ x
                         case γo of
                           Nothing -> return (reverse (y:acc), Nothing) 
                           Just γ' -> go (y:acc) γ' xs


--------------------------------------------------------------------------------
tcStmts :: PPRSF r => 
  TCEnv r -> [Statement (AnnSSA r)] -> TCM r ([Statement (AnnSSA r)], TCEnvO r)
--------------------------------------------------------------------------------
tcStmts γ stmts 
  = do γ' <- addStatementFunBinds γ stmts
       tcSeq tcStmt γ' stmts

addStatementFunBinds γ stmts 
  = do fts   <- forM fns $ \f -> (f,) <$> getSpecOrDie f
       return $ tcEnvAdds fts γ
    where
       fs  = concatMap getFunctionStatements stmts
       fns = [f | FunctionStmt _ f _ _ <- fs]

-------------------------------------------------------------------------------
tcStmt  :: PPRSF r =>
  TCEnv r -> Statement (AnnSSA r) -> TCM r (Statement (AnnSSA r), TCEnvO r)
-------------------------------------------------------------------------------
-- skip
tcStmt γ s@(EmptyStmt _) 
  = return (s, Just γ)

-- x = e
tcStmt γ (ExprStmt l1 (AssignExpr l2 OpAssign (LVar lx x) e))
  = do (e', g) <- tcAsgn l1 γ (Id lx x) e
       return   (ExprStmt l1 (AssignExpr l2 OpAssign (LVar lx x) e'), g)

-- e1.f = e2
tcStmt γ (ExprStmt l (AssignExpr l2 OpAssign (LDot l1 e1 f) e2))
  = do z             <- tcNormalCall γ l BISetProp [e1,e2] $ setPropTy (F.symbol f) l $ tce_env γ
       case z of 
         ([e1',e2'], _)
                     -> return (ExprStmt l $ AssignExpr l2 OpAssign (LDot l1 e1' f) e2', Just γ)
         (e,t)       -> error $ "BUG: tcStmt - e.f = e : " ++ ppshow e ++ "\n" ++ ppshow t

-- e
tcStmt γ (ExprStmt l e)   
  = do (e', _) <- tcExpr γ e 
       return (ExprStmt l e', Just γ) 

-- s1;s2;...;sn
tcStmt γ (BlockStmt l stmts) 
  = do (stmts', g) <- tcStmts γ stmts
       return (BlockStmt l stmts', g)

-- if b { s1 }
tcStmt γ (IfSingleStmt l b s)
  = tcStmt γ (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
tcStmt γ (IfStmt l e s1 s2)
  = do z <- tcNormalCall γ l BITruthy [e] $ builtinOpTy l BITruthy (tce_env γ)
       case z of 
         ([e'], _) -> do (s1', γ1) <- tcStmt γ s1
                         (s2', γ2) <- tcStmt γ s2
                         γ3        <- envJoin l γ γ1 γ2
                         return       (IfStmt l e' s1' s2', γ3)
         _          -> error "BUG: tcStmt - If then else"

-- while c { b } 
tcStmt γ (WhileStmt l c b) 
  = do (c', t)      <- tcExpr γ c
       unifyTypeM (srcPos l) t tBool
       (b', γl)     <- tcStmt γ' b

       γout         <- envLoopJoin l γ γl
       return       (WhileStmt l c' b', γout)  
    where 
       xts'          = [(mkNextId x, tcEnvFindTyOrDie l x γ) | x <- phiVarsAnnot l]
       γ'            = tcEnvAdds xts' γ

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt γ (VarDeclStmt l ds)
  = do (ds', z) <- tcSeq tcVarDecl γ ds
       return      (VarDeclStmt l ds', z)

-- return e 
tcStmt γ (ReturnStmt l eo) 
  = do  (es',_) <- tcNormalCall γ l "return" (maybeToList eo) $ returnTy (tcEnvFindReturn γ) (isJust eo)
        return   $ (ReturnStmt l $ listToMaybe es', Nothing)

-- throw e 
tcStmt γ (ThrowStmt l e) 
  = (,Nothing) . ThrowStmt l . fst <$> tcExpr γ e


tcStmt γ s@(FunctionStmt _ _ _ _)
  = tcFun γ s

-- class A<S...> [extends B<T...>] [implements I,J,...] { ... }
--
-- 1. Compute / get the class type 
-- 2. Add the type vars in the environment
-- 3. Compute type for "this" and add that to the env as well
--    - This type uses the classes type variables as type parameters.
--    - For the moment this type does not have a refinement. Maybe use
--      invariants to add some.
-- 4. Typecheck the class elements in this extended environment.
tcStmt γ c@(ClassStmt l i e is ce) 
  = do  TD _ _ αs _ _ <- classFromStmt c                              -- (1)
        let γ'        = tcEnvAdds (tyBinds αs) γ                      -- (2)
        let thisT     = TApp (TRef $ F.symbol i) (tVars αs) fTop      -- (3)
        ce'          <- tcWithThis thisT $ mapM (tcClassElt γ' i) ce  -- (4)
        return          (ClassStmt l i e is ce', Just γ)
  where
    tVars   αs    = [ tVar   α | α <- αs ] 
    tyBinds αs    = [(tVarId α, tVar α) | α <- αs]

-- OTHER (Not handled)
tcStmt _ s 
  = convertError "tcStmt" s


-- Get the id of a class from its name using:
--  * the top-level class annotation
--  * the annotations of the class fields
-- NOTE: we are not checking if the class has a parent or not.
---------------------------------------------------------------------------------------
classFromStmt :: PPR r => Statement (AnnSSA r) -> TCM r (TDef r)
---------------------------------------------------------------------------------------
classFromStmt (ClassStmt l id _ _ cs) 
  = do  δ <- getDef
        case findSym sym δ of
          Just d  -> return d   -- if already computed
          Nothing -> do let elts      = addConstr δ $ concatMap (classEltType tClass) cs
                        let freshD    = TD True (fmap ann id) vs p elts
                        mapM_ tcError $ safeExtends (srcPos l) δ freshD
                        setDef        $ addSym sym freshD δ
                        return        $ freshD
  where
    sym      = F.symbol id
    (vs, p)  = classAnnot l
    tvs v    = TVar v fTop
    tClass   = TApp (TRef $ F.symbol id) (tvs <$> vs) fTop
    addConstr δ es = 
      case [ t | ConsSig t <- es ] of
        -- Constructor is defined in class
        [_] -> es
        -- Get constructor from parent class
        [ ] -> parConstr δ : es 
        _   -> error "Cannot have more than one constructors."
        
    parConstr δ = 
      case p of 
        Just (i, ts) -> let TD _ _ vs' _ es' = findSymOrDie i δ in
                        fromJust $ find isConstr $ apply (fromList $ zip vs' ts) es'
        -- No parent class around - infer a default type
        Nothing      -> ConsSig $ TFun [] tVoid fTop

    tVoid :: PPR r => RType r
    tVoid = TApp TVoid [] fTop

classFromStmt _ = errorstar "classId should only be called with ClassStmt"


---------------------------------------------------------------------------------------
findClass :: (PPR r, PP s, F.Symbolic s, IsLocated s) 
          => s -> TCM r (Maybe (TDef r))
---------------------------------------------------------------------------------------
findClass s = envFindTy (F.symbol s) <$> getClasses 
          >>= maybe (return Nothing) ((Just <$>) . classFromStmt) 

---------------------------------------------------------------------------------------
classAnnot :: Annot (Fact t) a -> ([TVar], Maybe (Id SourceSpan, [RType t]))
---------------------------------------------------------------------------------------
classAnnot l = safeHead "classAnnot" [ t | ClassAnn t <- ann_fact l ]


---------------------------------------------------------------------------------------
classEltType :: PPR r => RType r -> ClassElt (AnnSSA r) -> [TElt r]
---------------------------------------------------------------------------------------
classEltType _ (Constructor l _ _ ) = [ c | ConsAnn c   <- ann_fact l ]

classEltType _ (MemberVarDecl _ static (VarDecl l _ _)) 
    | static    = [ s | StatAnn  s@(StatSig _ _ _)  <- ann_fact l ]
    | otherwise = [ f | FieldAnn f@(FieldSig _ _ _) <- ann_fact l ]

classEltType t (MemberMethDecl l static _ _ _ )
    | static    = [ s                  | StatAnn s@(StatSig _ _ _)  <- ann_fact l ]
    | otherwise = [ setThisBinding m t | MethAnn m@(MethSig _ _ _)  <- ann_fact l ]


-- Variable declarations should have the type annotations available locally
---------------------------------------------------------------------------------------
tcVarDecl ::  PPR r 
          => TCEnv r -> VarDecl (AnnSSA r) -> TCM r (VarDecl (AnnSSA r), TCEnvO r)
---------------------------------------------------------------------------------------
tcVarDecl γ v@(VarDecl l x (Just e))
  = do ann         <- listToMaybe <$> scrapeVarDecl v
       (e', t)     <- tcExprT l γ e ann
       return       $ (VarDecl l x (Just e'), Just $ tcEnvAdds [(x, t)] γ)

tcVarDecl γ v@(VarDecl l x Nothing) 
  = do ann <- scrapeVarDecl v
       case ann of
         [t] -> return (v, Just $ tcEnvAdds [(x, t)] γ)
         _   -> tcError $ errorVarDeclAnnot (srcPos l) x

-------------------------------------------------------------------------------
tcAsgn :: PPR r 
       => AnnSSA r -> TCEnv r -> Id (AnnSSA r) -> ExprSSAR r -> TCM r (ExprSSAR r, TCEnvO r)
-------------------------------------------------------------------------------
tcAsgn l γ x e
  = do (e' , t) <- tcExprT l γ e rhsT
       return      (e', Just $ tcEnvAdds [(x, t)] γ)
    where
    -- Every variable has a single raw type, whether it has been annotated with
    -- it at declaration or through initialization.
       rhsT      = tcEnvFindSpecOrTy x γ

-- | There are two versions for `tcExprT`. If `init` is True then this is the 
-- variable initialization phase, otherwise any other assignment.
-------------------------------------------------------------------------------
tcExprT :: PPR r 
        => AnnSSA r -> TCEnv r -> ExprSSAR r -> Maybe (RType r) 
        -> TCM r (ExprSSAR r, RType r)
-------------------------------------------------------------------------------
tcExprT l γ e to 
  = do (e', t)    <- tcExpr γ e
       case to of
         Nothing -> return (e', t)
         Just ta -> do θ <- unifyTypeM (srcPos l) t ta
                       (,ta) <$> castM (tce_ctx γ) e' (apply θ t) ta

-------------------------------------------------------------------------------
tcExpr :: PPR r => TCEnv r -> ExprSSAR r -> TCM r (ExprSSAR r, RType r)
-------------------------------------------------------------------------------
tcExpr _ e@(IntLit _ _)
  = return (e, tInt)

tcExpr _ e@(BoolLit _ _)
  = return (e, tBool)

tcExpr _ e@(StringLit _ _)
  = return (e, tString)

tcExpr _ e@(NullLit _)
  = return (e, tNull)

tcExpr _ e@(ThisRef _)
  = (e,) <$> tcPeekThis

tcExpr γ e@(VarRef l x)
  = case tcEnvFindTy x γ of
      Just t  -> return (e,t)           -- Global or local reference
      Nothing -> do z <- findClass x 
                    case z of
                      Just (TD _ s _ _ _) -> return (e, TApp (TTyOf $ F.symbol s) [] fTop) -- Static reference
                      Nothing             -> tcError $ errorUnboundId (ann l) x
 
tcExpr γ e@(CondExpr _ _ _ _)
  = tcCall γ e 

tcExpr γ e@(PrefixExpr _ _ _)
  = tcCall γ e 

tcExpr γ e@(InfixExpr _ _ _ _)
  = tcCall γ e 

tcExpr γ e@(CallExpr _ _ _)
  = tcCall γ e 

tcExpr γ e@(ArrayLit _ _)
  = tcCall γ e 
 
tcExpr γ e@(ObjectLit _ _) 
  = tcCall γ e

-- FIXME: compound casts ???
--
-- | <T>e
--
tcExpr γ ex@(Cast l@(Ann loc fs) e)
  = do  (e', t) <- tcExpr γ  e
        δ       <- getDef
        case [ ct | UserCast ct <- fs ] of
          -- Stuff from before
          [  ] -> case e' of
                    Cast (Ann _ fs') e'' -> return (Cast (Ann loc (fs ++ fs')) e'', t)
                    _                    -> return (Cast l e', t)
          -- User cast
          [t1] -> return $ (Cast (mp (rplc δ t) l) e', t1)
          _    -> tcError $ unimplemented (srcPos l) "Compound casts" $ ppshow ex
  where
    mp f (Ann l fs)         = Ann l $ f <$> fs
    rplc :: PPR r => TDefEnv r -> RType r -> Fact r -> Fact r
    rplc δ t0 (UserCast t1) | on (==) toType t0 t1  = TCast (tce_ctx γ) $ CNo
                            | isSubtype δ t0 t1     = TCast (tce_ctx γ) $ CUp t0 t1
                            | isSubtype δ t1 t0     = TCast (tce_ctx γ) $ CDn t0 t1
                            | otherwise             = TCast (tce_ctx γ) $ CDead t1
    rplc _ _  a                                     = a
    
 
-- | e.f
tcExpr γ e@(DotRef _ _ _) 
  = tcCall γ e
 
-- | e1[e2]
tcExpr γ e@(BracketRef _ _ _) 
  = tcCall γ e

-- | e1[e2] = e3
tcExpr γ e@(AssignExpr _ OpAssign (LBracket _ _ _) _)
  = tcCall γ e

-- | new C(e, ...)
tcExpr γ e@(NewExpr _ _ _) 
  = tcCall γ e

-- | super
tcExpr _ e@(SuperRef l) = (e,) <$> (getSuperM l =<< tcPeekThis)

-- | function(xs) { }
tcExpr γ (FuncExpr l fo xs body)
  = case anns of 
      [ft] -> do  ts    <- tcFunTys l f xs ft
                  body' <- foldM (tcFun1 γ l f xs) body ts
                  return $ (FuncExpr l fo xs body', ft)
      _    -> tcError    $ errorNonSingleFuncAnn $ srcPos l
  where
    anns    = [ t | FuncAnn t <- ann_fact l ]
    f       = maybe (F.symbol "<anonymous>") F.symbol fo

tcExpr _ e 
  = convertError "tcExpr" e


---------------------------------------------------------------------------------------
tcCall :: PPR r => TCEnv r -> ExprSSAR r -> TCM r (ExprSSAR r, RType r)
---------------------------------------------------------------------------------------

-- | `o e`
tcCall γ (PrefixExpr l o e)        
  = do z                      <- tcNormalCall γ l o [e] (prefixOpTy o $ tce_env γ) 
       case z of
         ([e'], t)            -> return (PrefixExpr l o e', t)
         _                    -> tcError $ impossible (srcPos l) "tcCall PrefixExpr"

-- | `e1 o e2`
tcCall γ (InfixExpr l o@OpInstanceof e1 e2) 
  = do (e2',t)                <- tcExpr γ e2
       case t of
         TApp (TTyOf x) _ _   -> do ([e1',_], t) <- tcNormalCall γ l o
                                                      [e1, StringLit l2 (tracePP "instanceof" $ F.symbolString x)] 
                                                      (infixOpTy o $ tce_env γ)
                                    return        $ (InfixExpr l o e1' e2', t) 
         _                    -> tcError          $ unimplemented (srcPos l) "tcCall-instanceof" $ ppshow e2
  where
    l2 = getAnnotation e2

-- | e ? e1 : e2
tcCall γ (CondExpr l e e1 e2)
  = do z                      <- tcNormalCall γ l BICondExpr [e,e1,e2] (builtinOpTy l BICondExpr $ tce_env γ)
       case z of
         ([e',e1',e2'], t)    -> return (CondExpr l e' e1' e2', t)
         _                    -> tcError $ impossible (srcPos l) "tcCall CondExpr"

tcCall γ (InfixExpr l o e1 e2)        
  = do z                      <- tcNormalCall γ l o [e1, e2] (infixOpTy o $ tce_env γ) 
       case z of
         ([e1', e2'], t)      -> return (InfixExpr l o e1' e2', t)
         _                    -> tcError $ impossible (srcPos l) "tcCall InfixExpr"


-- | `e1[e2]`
tcCall γ (BracketRef l e1 e2)
  = do z                      <- tcNormalCall γ l BIBracketRef [e1, e2] $ builtinOpTy l BIBracketRef $ tce_env γ 
       case z of
         ([e1', e2'], t)      -> return (BracketRef l e1' e2', t)
         _                    -> tcError $ impossible (srcPos l) "tcCall BracketRef"
   
-- | `e1[e2] = e3`
tcCall γ (AssignExpr l OpAssign (LBracket l1 e1 e2) e3)
  = do z                      <- tcNormalCall γ l BIBracketAssign [e1,e2,e3] $ builtinOpTy l BIBracketAssign $ tce_env γ
       case z of
         ([e1', e2', e3'], t) -> return (AssignExpr l OpAssign (LBracket l1 e1' e2') e3', t)
         _                    -> tcError $ impossible (srcPos l) "tcCall AssignExpr"

-- | `[e1,...,en]`
tcCall γ (ArrayLit l es)
  = do (es', t)               <- tcNormalCall γ l BIArrayLit es $ arrayLitTy l (length es) $ tce_env γ
       return                  $ (ArrayLit l es', t)

-- | `{ f1:t1,...,fn:tn }`
tcCall γ (ObjectLit l bs) 
  = do (es', t)               <- tcNormalCall γ l "ObjectLit" es $ objLitTy l ps $ tce_env γ 
       return                  $ (ObjectLit l (zip ps es'), t)
  where
    (ps,es) = unzip bs

-- | `new e(e1,...,en)`
tcCall γ (NewExpr l (VarRef lv i) es) 
  = do tc                     <- getConstr (srcPos l) γ i
       when                      (not $ isTFun tc) 
                               $ tcError $ errorConstNonFunc (srcPos l) i
       (es', t)               <- tcNormalCall γ l "constructor" es tc
       return                  $ (NewExpr l (VarRef lv i) es', t)

-- | e.f 
tcCall γ ef@(DotRef l e f)
  = do  z              <- runFailM $ tcExpr γ e
        case z of
          Right (_, t) -> 
            do  δ      <- getDef 
                case getElt δ f t of 
                  [FieldSig _ _ ft] -> do ([e'], t') <- tcNormalCall γ l ef [e] $ mkTy ft
                                          return      $ (DotRef l e' f, t')
                  _                 -> tcError $ errorExtractNonFld (srcPos l) f e 
          Left err     -> tcError err
  where
    mkTy t   = mkFun ([α], [B (F.symbol "this") tα], t) 
    α        = TV (F.symbol "α" ) (srcPos l)
    tα       = TVar α fTop

         
-- | `super(e1,...,en)`
--   TSC will already have checked that `super` is only called whithin the constructor.
tcCall γ (CallExpr l e@(SuperRef _)  es) 
  = do elts                   <- t_elts <$> (getSuperDefM l =<< tcPeekThis)
       go                        [ t | ConsSig t <- elts ]
  where
       go [t]                  = 
          do (es', t')        <- tcNormalCall γ l "constructor" es t
             return            $ (CallExpr l e es', t')
       go _                    = error "tcError $ errorConsSigMissing (srcPos l) e"
   
-- | `e(es)`
tcCall γ (CallExpr l em@(DotRef _ e f) es)
  = do z              <- runFailM $ tcExpr γ e
       case z of 
         Right (_, t) -> do δ             <- getDef 
                            (em', es', t) <- tcCallDotRef γ (getElt δ f t) l em es
                            return         $ (CallExpr l em' es', t)
         Left err     -> tcError err

-- | `e(es)`
tcCall γ (CallExpr l e es)
  = do (e', ft0)              <- tcExpr γ e
       (es', t)               <- tcNormalCall γ l e es ft0
       return                  $ (CallExpr l e' es', t)

tcCall _ e = tcError $ unimplemented (srcPos e) "tcCall" e


tcCallDotRef γ elts l em@(DotRef l1 e f) es 
    -- Static call
    | all isStaticSig elts
    = do  (e' , _ )   <- tcExpr γ e       -- TC `e` separately
          (es', t')   <- tcNormalCall γ l em es {-$ tracePP ("statsig " ++ ppshow (srcPos l)) -} $ ft isStaticSig
          return       $ (DotRef l1 e' f, es', t')

    -- Virtual method call
    | all isMethodSig elts 
    = do  (e':es', t) <- tcNormalCall γ l em (e:es) {-$ tracePP ("methSig " ++ ppshow (srcPos l)) -} $ ft isMethodSig
          return       $ (DotRef l1 e' f, es', t)

    -- Normal function call
    | all isFieldSig elts
    = do  (e' , _ )   <- tcExpr γ e       -- TC `e` separately
          (es', t')   <- tcNormalCall γ l em es {-$ tracePP ("fieldSig " ++ ppshow (srcPos l)) -} $ ft isFieldSig
          return       $ (DotRef l1 e' f, es', t')

    | otherwise
    = tcError $ unsupportedDotRef (srcPos l) em
  where
    ft f = mkAnd $ catMaybes $ mkEltFunTy <$> filter f elts

tcCallDotRef _ _ _ _ _ = error "tcCallDotRef-unsupported"



-- | `getConstr` first checks whether input @s@ is a class, in which case it
-- tries to retrieve a constructor binding, using a default one if that fails.
-- Otherwise, it tries to retrieve an object with the same name from the
-- environment that has a constructor property.
--
-- FIXME: Constructor lookup is done by string - we have a special term for that
----------------------------------------------------------------------------------
getConstr :: (PPR r, IsLocated a) 
          => SourceSpan -> TCEnv r -> Id a -> TCM r (RType r)
----------------------------------------------------------------------------------
getConstr l γ s = 
  do  c         <- findClass s 
      case c of 
        Just t  -> do p <- getPropTDefM False l "__constructor__" t (tVar <$> t_args t)
                      case p of 
                        Just (TFun bs _ r) -> return $ abs (t_args t) $ TFun bs (retT t) r
                        Just _             -> error  $ "Unsupported constructor type"
                        Nothing            -> return $ abs (t_args t) $ TFun [] (retT t) fTop
        Nothing -> case tcEnvFindTy s γ of
                     Just t  -> do p <- getPropM False l "__constructor__" t
                                   case p of 
                                     Just t  -> return t
                                     Nothing -> error "tcError $ errorConsSigMissing (srcPos l) s"
                     Nothing -> tcError $ errorClassMissing (srcPos l) s
  where
    -- Constructor's return type is void - instead return the class type
    -- FIXME: type parameters in returned type: inferred ... or provided !!! 
    retT t   = TApp (TRef $ F.symbol s) (tVar <$> t_args t) fTop
    abs [] t = t
    abs vs t = foldr TAll t vs


-- | Signature resolution

tcNormalCall γ l fn es ft0 
  = do (es', ts)      <- unzip <$> mapM (tcExpr γ) es
       z              <- {- tracePP ("resolved overload for " ++ ppshow fn) <$> -} 
                         resolveOverload γ l fn es' ts ft0
       case z of 
         Just (θ, ft) -> do addAnn (srcPos l) $ Overload (tce_ctx γ) ft
                            addSubst l θ
                            (es'', ot, _ ) <- tcCallCase γ l fn es' ts ft
                            return          $ (es'', ot)
         Nothing      -> tcError $ errorCallNotSup (srcPos l) fn es ts 


-- When resolving an overload there are two prossible cases:
--   * There is only a single signature available: then return just this
--     signature regardless of subtyping constraints
--   * There are more than one signature available: return all that pass the
--     subtype check (this is what tcCallCaseTry does).
resolveOverload γ l fn es ts ft 
  = do δ        <- getDef
       let sigs  = catMaybes (bkFun <$> getCallable δ ft)
       case [ mkFun (vs, τs,τ) | (vs, τs, τ) <- sigs, length τs == length es ] of
         [t]    -> do θ     <- getSubst 
                      return $ Just (θ,t)
         fts    -> do θs    <- mapM (\t -> tcCallCaseTry γ l fn ts t) fts
                      return $ listToMaybe [ (θ, apply θ t) | (t, Just θ) <- zip fts θs ]


-- | A successful pairing of formal to actual parameters will return `Just θ`,
-- where θ is the corresponding substitution. If the types are not
-- acceptable this will return `Nothing`.
-- In this case successful means:
--  * Unifying without errors.
--  * Passing the subtyping test.
--
-- The monad state is completely reversed after this function returns, thanks to
-- `runMaybeM`. We don't need to reverse the action of `instantiate`.
----------------------------------------------------------------------------------
tcCallCaseTry :: (PPR r, PP a) 
              => TCEnv r -> Annot b SourceSpan -> a -> [RType r] -> RType r 
              -> TCM r (Maybe (RSubst r))
----------------------------------------------------------------------------------
tcCallCaseTry γ l fn ts ft = runMaybeM $ 
  do (_,ibs,_) <- instantiate l (tce_ctx γ) fn ft
     let its    = b_type <$> ibs
     θ'        <- unifyTypesM (ann l) "tcCallCaseTryAux" ts its
     zipWithM_    (subtypeM (ann l)) (apply θ' ts) (apply θ' its)
     return     $ θ'


tcCallCase γ l fn es ts ft 
  = do let ξ            = tce_ctx γ
       -- Generate fresh type parameters
       (_,ibs,ot)      <- {- tracePP ("inst " ++ ppshow ft) <$> -} instantiate l ξ fn ft
       let its          = b_type <$> ibs
       θ               <- {- tracePP "unif" <$> -} unifyTypesM (srcPos l) "tcCall" ts its
       let (ts',its')   = mapPair (apply θ) (ts, its)
       es'             <- NM.zipWith3M (castM ξ) es ts' its'
       return             (es', apply θ ot, θ)

instantiate l ξ fn ft 
  = do t'              <- freshTyArgs (srcPos l) ξ αs t 
       maybe err return $ bkFun t'
    where
       (αs, t)          = bkAll ft
       err = tcError    $ errorNonFunction (ann l) fn ft

--              
-- tcPropRead getter γ l e fld 
--   = do (e', te)         <- tcExpr γ e
--        (δ, ε)           <- (,) <$> getDef <*> getExts
--        case getter l ε δ fld te of
--          Nothing        -> tcError $  errorPropRead (srcPos l) e fld
--          Just (te', tf) -> tcWithThis te $ (, tf) <$> castM l (tce_ctx γ) e' te te'
-- 

----------------------------------------------------------------------------------
envJoin :: PPR r => (AnnSSA r) -> TCEnv r -> TCEnvO r -> TCEnvO r -> TCM r (TCEnvO r)
----------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l γ (Just γ1) (Just γ2) = 
  do let xs = phiVarsAnnot l
     ts    <- mapM (getPhiType l γ1 γ2) xs
     θ     <- getSubst
     return $ Just $ tcEnvAdds (zip xs ts) (apply θ γ)


----------------------------------------------------------------------------------
envLoopJoin :: PPR r => (AnnSSA r) -> TCEnv r -> TCEnvO r -> TCM r (TCEnvO r)
----------------------------------------------------------------------------------
envLoopJoin _ γ Nothing   = return $ Just γ
envLoopJoin l γ (Just γl) = 
  do let xs = phiVarsAnnot l 
     ts    <- mapM (getLoopNextPhiType l γ γl) xs
     θ     <- getSubst
     return $ Just $ tcEnvAdds (zip xs ts) (apply θ γ)
 
----------------------------------------------------------------------------------
getPhiType :: PPR r 
           => (AnnSSA r) -> TCEnv r -> TCEnv r -> Id SourceSpan-> TCM r (RType r)
----------------------------------------------------------------------------------
getPhiType l γ1 γ2 x =
  case (tcEnvFindTy x γ1, tcEnvFindTy x γ2) of
    -- These two should really be the same type... cause we don't allow strong
    -- updates on the raw type, even on local vars (that are SSAed)
    (Just t1, Just t2) -> 
      do  when (t1 /= t2) (tcError $ errorEnvJoin (ann l) x t1 t2)
          return t1
    (_      , _      ) -> if forceCheck x γ1 && forceCheck x γ2 
                            then tcError $ bug loc "Oh no, the HashMap GREMLIN is back..."
                            else tcError $ bugUnboundPhiVar loc x
                          where loc = srcPos $ ann l

----------------------------------------------------------------------------------
getLoopNextPhiType :: PPR r 
           => (AnnSSA r) -> TCEnv r -> TCEnv r -> Id SourceSpan-> TCM r (RType r)
----------------------------------------------------------------------------------
getLoopNextPhiType l γ γl x =
  case (tcEnvFindTy x γ, tcEnvFindTy (mkNextId x) γl) of 
    (Just t1, Just t2) -> 
      do  when (t1 /= t2) (tcError $ errorEnvJoin (ann l) x t1 t2)
          return t1
    (_      , _      ) -> 
      tcError $ bugUnboundPhiVar loc x where loc = srcPos $ ann l


forceCheck x γ = elem x $ fst <$> envToList (tce_env γ)


-- | scrapeVarDecl: Scrape a variable declaration for annotations
----------------------------------------------------------------------------------
scrapeVarDecl :: VarDecl (AnnSSA r) -> TCM r [RType r]
----------------------------------------------------------------------------------
scrapeVarDecl (VarDecl l _ _) = 
  mapM (sanity $ srcPos l) $ [ t | VarAnn                 t  <- ann_fact l ] 
                          ++ [ t | FieldAnn (FieldSig _ _ t) <- ann_fact l ]

sanity l t@(TApp (TRef i) ts _) 
  = do  δ       <- getDef 
        case findSym i δ of
          Just (TD _ _ αs _ _) | length αs == length ts -> return  $ t 
          Just (TD _ n αs _ _) | otherwise              -> tcError $ errorTypeArgsNum l n (length αs) (length ts)
          Nothing                                       -> error   $ "BUG: Id: " ++ ppshow i 
                                                                  ++ " was not found in env at " 
                                                                  ++ ppshow (srcPos l) 
sanity _ t = return t


