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

module Language.Nano.Typecheck.Typecheck (verifyFile, typeCheck) where 

import           Control.Applicative                ((<$>))
import           Control.Monad                

import qualified Data.HashMap.Strict                as M 
import           Data.Maybe                         (catMaybes, fromMaybe, listToMaybe, fromJust, maybeToList, isJust)
import           Data.List                          (find)
import           Data.Default
import           Data.Data 
import           Data.Char                          (isUpper)
import qualified Data.Foldable                      as FO
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
import           Language.Nano.Typecheck.Resolve
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



type PPR   r = (PP r, F.Reftable r, Data r)
type PPRSF r = (PPR r, Substitutable r (Fact r), Free (Fact r)) 



--------------------------------------------------------------------------------
-- | Top-level Verifier 

--------------------------------------------------------------------------------
verifyFile :: [FilePath] -> IO (UAnnSol a, F.FixResult Error)
--------------------------------------------------------------------------------
verifyFile fs = parse fs $ ssa $ tc

parse fs next = parseNanoFromFiles fs >>= next

-- testSSA fs
--   = do p <- parseNanoFromFiles fs 
--        s <- ssaTransform p
--        case s of
--          Left l -> print l
--          Right s -> print $ ppshow s

ssa next p  
  = do  r <- ssaTransform $ tracePP "PARSED FILE" p 
        case r of 
          Left  l -> lerror [l] 
          Right x -> next $ expandAliases x

tc p
  = do  r <- typeCheck p    
        case r of
          Left  l -> unsafe l
          Right x -> safe x

lerror        = return . (NoAnn,) . F.Unsafe

unsafe errs = do putStrLn "\n\n\nErrors Found!\n\n" 
                 forM_ errs (putStrLn . ppshow) 
                 return $ (NoAnn, F.Unsafe errs)

safe (Nano {code = Src fs})
  = do  nfc       <- noFailCasts <$> getOpts
        return     $ (NoAnn, failCasts nfc fs)
    where
        failCasts True  _  = F.Safe
        failCasts False fs = applyNonNull F.Safe F.Unsafe 
                           $ concatMap castErrors 
                           $ casts fs 

        casts             :: Data r => [Statement (AnnType r)] -> [AnnType r]
        casts stmts        = everything (++) ([] `mkQ` f) stmts
          where 
            f             :: Expression (AnnType r) -> [(AnnType r)]
            f (Cast a _)   = [a]
            f _            = [] 

-------------------------------------------------------------------------------
castErrors :: PPR r => AnnType r -> [Error] 
-------------------------------------------------------------------------------
castErrors (Ann l facts) = downErrors
  where 
    downErrors           = [errorDownCast l t1 t2 | TCast _ (CDn t1 t2) <- facts]


-------------------------------------------------------------------------------
typeCheck :: PPR r => NanoSSAR r -> IO (Either [Error] (NanoTypeR r))
-------------------------------------------------------------------------------
typeCheck pgm = do 
  v <- V.getVerbosity
  let r = execute v pgm $ tcNano pgm 
  return $ snd <$> r


-------------------------------------------------------------------------------
-- | TypeCheck Nano Program
-------------------------------------------------------------------------------
tcNano :: PPRSF r => NanoSSAR r -> TCM r (AnnInfo r, NanoTypeR r)
-------------------------------------------------------------------------------
tcNano p@(Nano {code = Src fs})
  = do  -- setDef         $ defs p
        -- CHECK TO BE DONE ON THE SPOT
        -- checkInterfaces p
        (fs', _)       <- tcStmts γ fs
        m              <- getAnns 
        θ              <- getSubst
        let p1          = p {code = (patchAnn m . apply θ) <$> Src fs'}
        -- d              <- getDef
        -- return          $ (m, p1 { defs  = d })     -- Update IfaceEnv before exiting
        return          $ (m, p1)
    where
        γ               = initGlobalEnv p


-- FIXME: check for mutability parameter
-- checkInterfaces p = 
--     forM_ (concatMap (safeExtends l (defs p)) is) tcError
--   where 
--     l  = srcPos dummySpan
--     is = [ d | (_, d@(ID False _ _ _ _)) <- envToList $ defs p ]
-- 
patchAnn m (Ann l fs)   = Ann l $ sortNub $ eo ++ fo ++ ti ++ fs 
  where
    ti                  = [f | f@(TypInst _ _    ) <- M.lookupDefault [] l m]
    fo                  = [f | f@(Overload _ _   ) <- M.lookupDefault [] l m]
    eo                  = [f | f@(EltOverload _ _) <- M.lookupDefault [] l m]

-------------------------------------------------------------------------------
-- | Initialize environment
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
initGlobalEnv  :: PPR r => NanoSSAR r -> TCEnv r
-------------------------------------------------------------------------------
initGlobalEnv (Nano { code = Src ss }) = initModuleEnv Nothing Nothing ss

-- -------------------------------------------------------------------------------
-- initEnv      :: Data r
--              => Maybe (TCEnv r) -> Maybe F.Symbol -> [Statement (AnnSSA r)] -> TCEnv r
-- -------------------------------------------------------------------------------
-- initEnv γ n s = TCE env iface mod ctx nspace parent
--   where
--     env       = populateFuncs s
--     iface     = maybe envEmpty tce_iface γ    -- TODO: add names in scope
--     mod       = maybe envEmpty tce_mod γ      -- TODO: add modules in scope
--     ctx       = emptyContext
--     nspace    = maybe [] tce_nspace γ ++ maybeToList n
--     parent    = γ


initFuncEnv γ f i αs xs ts t s = TCE env iface mod ctx nspace parent
  where
    tyBinds   = [(tVarId α, tVar α) | α <- αs]
    varBinds  = zip (fmap ann <$> xs) ts
    env       = envAddReturn f t $ envAdds (tyBinds ++ varBinds) $ populateFuncs s
    iface     = envEmpty 
    mod       = envEmpty
    ctx       = pushContext i (tce_ctx γ) 
    nspace    = tce_nspace γ
    parent    = Just γ


---------------------------------------------------------------------------------------
initClassEnv  :: PPR r => TCEnv r -> IfaceDef r -> TCEnv r
---------------------------------------------------------------------------------------
initClassEnv γ (ID _ n vs h es) = TCE env iface mod ctx nspace parent
  where
    env         = envAdds [(F.symbol "this", tThis)] envEmpty
    iface       = envEmpty 
    mod         = envEmpty
    ctx         = emptyContext 
    nspace      = tce_nspace γ
    parent      = Just γ
    tThis       = TApp (TRef x) ts fTop
    x           = QN [] (F.symbol n)
    ts          = tVar <$> vs


---------------------------------------------------------------------------------------
initModuleEnv :: PPR r => TCEnvO r -> Maybe F.Symbol -> [Statement (AnnSSA r)] -> TCEnv r
---------------------------------------------------------------------------------------
initModuleEnv γo n s = TCE env iface mod ctx nspace parent 
  where
    iface     = populateTypes s
    env       = envEmpty
    mod       = populateModules s
    ctx       = emptyContext
    nspace    = maybe [] tce_nspace γo ++ maybeToList n
    parent    = γo



---------------------------------------------------------------------------------------
populateModules :: Data r => [Statement (AnnSSA r)] -> Env (TCEnv r)
---------------------------------------------------------------------------------------
populateModules s = envEmpty

---------------------------------------------------------------------------------------
populateFuncs :: Data r => [Statement (AnnSSA r)] -> Env (RType r)
---------------------------------------------------------------------------------------
populateFuncs s = envFromList [ (n,t) | (n,ls) <- funcDecls, VarAnn t <- ls ]
  where
    funcDecls = hoistFuncDecls $ fmap ann_fact <$> s


---------------------------------------------------------------------------------------
populateTypes :: PPR r => [Statement (AnnSSA r)] -> IfaceEnv r
---------------------------------------------------------------------------------------
populateTypes         = foldl resolveAndAdd envEmpty . hoistTypes
  where
    resolveAndAdd γ s = envAdds (resolveType s) γ

-- FIXME (?): Does not take into account classes with missing annotations.
--            Ts -> rsc translation should add annotations everywhere.
-- TODO: Use safeExtends to check inheritance
---------------------------------------------------------------------------------------
resolveType :: PPR r => Statement (AnnSSA r) -> [(F.Symbol, IfaceDef r)]
---------------------------------------------------------------------------------------
resolveType  (ClassStmt l id _ _ cs)
  = case [ t | ClassAnn t <- ann_fact l ] of
      [(vs, h)] -> [(s, ID True (fmap ann id) vs h (resolveMembers (tc vs) cs))]
      _         -> []
  where
    s         = F.symbol id
    x         = QN [] s
    tc vs     = TApp (TRef x) ((`TVar` fTop) <$> vs) fTop

-- TODO 
resolveType (IfaceStmt l)
  = case [ t | IfaceAnn t <- ann_fact l ] of
    _ -> []

resolveType _ = [] 


-- ---------------------------------------------------------------------------------------
-- resolveMembers :: (Data r, PPR r) => TCEnv r -> SourceSpan -> RType r -> F.Symbol -> 
--                   Heritage r -> [ClassElt (AnnSSA r)] -> TCM r [TypeMember r]
-- ---------------------------------------------------------------------------------------
-- resolveMembers γ l t s h cs = 
--     patchConstructor γ l s h es 
--   where 
--     es = concatMap (resolveMemberType t) cs

resolveMembers t cs = concatMap (resolveMemberType t) cs


---------------------------------------------------------------------------------------
resolveMemberType :: PPR r => RType r -> ClassElt (AnnSSA r) -> [TypeMember r]
---------------------------------------------------------------------------------------
resolveMemberType _ (Constructor l _ _ ) = [ c | ConsAnn c   <- ann_fact l ]

resolveMemberType _ (MemberVarDecl _ static (VarDecl l _ _)) 
    | static    = [ s | StatAnn  s@(StatSig _ _ _)  <- ann_fact l ]
    | otherwise = [ f | FieldAnn f@(FieldSig _ _ _) <- ann_fact l ]

resolveMemberType t (MemberMethDecl l static _ _ _ )
    | static    = [ s                  | StatAnn s@(StatSig _ _ _)  <- ann_fact l ]
    | otherwise = [ setThisBinding m t | MethAnn m@(MethSig _ _ _)  <- ann_fact l ]


-- ---------------------------------------------------------------------------------------
-- patchConstructor :: (PPR r, Data r) => TCEnv r -> SourceSpan -> F.Symbol -> Heritage r -> 
--                     [TypeMember r] -> TCM r [TypeMember r]
-- ---------------------------------------------------------------------------------------
-- patchConstructor γ l s heritage es 
--   | isJust (find isConstr es) = return $ es
--   | otherwise
--   = case heritage of 
--       Just (p,ts) ->  
--           case resolveIface γ p of
--             Just (ID _ _ vs' _ es') -> 
--                 case [ t | ConsSig t <- es'] of
--                   [t] -> return (ConsSig (tt vs' ts t) : es)
--                   _   -> tcError $ errorConstrMissing l p 
--             _ -> tcError $ errorParentClassMissing l s p 
--             -- No parent class around - infer a default type
--       Nothing       -> return (ConsSig (TFun [] tVoid fTop) : es)
--   where
--     tVoid :: PPR r => RType r
--     tVoid = TApp TVoid [] fTop
--     tt vs ts = apply (fromList $ zip vs ts)








-- -- Q: Who needs this?
-- -- A: Unification is not local any more. For example an empty array may be
-- --    created and its type can be instantiated only after a use of it which can 
-- --    be arbitrarily far from creation point. At that point we need to update
-- --    the current environment by applying the substitution.
-- --
-- instance PPR r => Substitutable r (TCEnv r) where 
--   apply θ γ = γ { tce_env = apply θ (tce_env γ) }
-- 
-- instance PPR r => PP (TCEnv r) where
--   pp = ppTCEnv


-- FIXME !!!
ppTCEnv (TCE env ifaces modules ctx _ _ )
  =   text "******************** Environment ************************"
  $+$ pp env
--   $+$ text "******************** Interfaces *************************"
--   $+$ pp ifaces
--   $+$ text "******************** Modules ****************************"
--   $+$ text "TODO" -- pp modules
--   $+$ text "******************** Call Context ***********************"
--   $+$ pp ctx


tcEnvAdds     x γ     = γ { tce_env = envAdds x $ tce_env γ }

tcEnvFindTy x γ       = case envFindTy x (tce_env γ) of 
                          Just t   -> Just t
                          Nothing  -> 
                            case tce_parent γ of 
                              Just γ' -> tcEnvFindTy x γ'
                              Nothing -> Nothing

tcEnvFindReturn       = envFindReturn . tce_env

tcEnvFindTyOrDie l x  = fromMaybe ugh . tcEnvFindTy x  where ugh = die $ errorUnboundId (ann l) x




-------------------------------------------------------------------------------
-- | TypeCheck Function 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
tcFun :: PPR r => TCEnv r -> Statement (AnnSSA r) -> TCM r (Statement (AnnSSA r), TCEnvO r)
-------------------------------------------------------------------------------
tcFun γ (FunctionStmt l f xs body)
  = case tcEnvFindTy f γ of
      Nothing       -> die $ errorMissingSpec (srcPos l) f
      Just ft       -> do ts    <- tcFunTys l f xs ft
                          body' <- foldM (tcFun1 γ l f xs) body ts
                          return $ (FunctionStmt l f xs body', Just γ) 

tcFun _  s = die $ bug (srcPos s) $ "Calling tcFun not on FunctionStatement"


tcFun1 γ l f xs body (i, (αs,ts,t)) 
  = tcFunBody (initFuncEnv γ f i αs xs ts t body) l body t


tcMethSingleSig γ l f xs body (i, _,ft) 
  = tcFun1 γ l f xs body (i, ft)


-- FIXME: Check for mutability (the second part in the triplet)
--        If this argument is "immutable" We will have to check
--        statements/expressions that operate on "this" and make sure that they
--        do not mutate it.
--
--        For the moment it just does a regular function check
--        

tcFunBody γ l body t 
  = do  z                          <- tcStmts γ body
        case z of 
          (_, Just _) | t /= tVoid -> tcError $ errorMissingReturn (srcPos l)
          (b, _     ) | otherwise  -> return b


-- | Strings ahead: HACK Alert
tVarId (TV a l) = Id l $ "TVAR$$" ++ F.symbolString a   



---------------------------------------------------------------------------------------
tcClassElt :: PPR r => TCEnv r -> Id (AnnSSA r) -> ClassElt (AnnSSA r) -> TCM r (ClassElt (AnnSSA r))
---------------------------------------------------------------------------------------
--
-- FIXME: 1. Check for void return type for constructor
--        2. Use tcMethSingleSig instead of tcFun1
--
tcClassElt γ _ (Constructor l xs body) 
  = case [ c | ConsAnn c  <- ann_fact l ] of
      [ConsSig ft]  -> do t        <- tcFunTys l i xs ft
                          body'    <- foldM (tcFun1 γ l i xs) body t
                          return    $ Constructor l xs body'
      _             -> tcError $ unsupportedNonSingleConsTy $ srcPos l
  where i   = Id l "constructor"

tcClassElt γ cid (MemberVarDecl l static (VarDecl l1 x eo))
  = case anns of 
      []  ->  tcError       $ errorClassEltAnnot (srcPos l1) cid x
      fs  ->  case eo of
                Just e     -> do ([e'],_)  <- tcNormalCall γ l1 "field init" [e] $ ft fs
                              -- Using a function call to "init" to keep track of 
                              -- overloading on field initialization
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
                  body'  <- foldM (tcMethSingleSig γ l i xs) body mts
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
tcStmts = undefined -- tcSeq tcStmt 

-- THIS IS DONE ALREADY
-- addStatementFunBinds γ stmts 
--   = do fts   <- forM fns $ \f -> (f,) <$> getSpecOrDie f
--        return $ tcEnvAdds fts γ
--     where
--        fs  = concatMap getFunctionStatements stmts
--        fns = [f | FunctionStmt _ f _ _ <- fs]


-- TODO: Add case for module and change Functions etc...
-------------------------------------------------------------------------------
tcStmt  :: PPRSF r =>
  TCEnv r -> Statement (AnnSSA r) -> TCM r (Statement (AnnSSA r), TCEnvO r)
-------------------------------------------------------------------------------
-- skip
tcStmt γ s@(EmptyStmt _) 
  = return (s, Just γ)

-- declare function foo(...): T; 
-- this definitions will be hoisted
tcStmt γ s@(FunctionDecl _ _ _) 
  = return (s, Just γ)

-- interface Foo;
-- this definitions will be hoisted
tcStmt γ s@(IfaceStmt _) 
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
         (e,t)       -> error $ "BUG: tcStmt - e.f = e : " -- ++ ppshow e ++ "\n" ++ ppshow t

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
       unifyTypeM (srcPos l) γ t tBool
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
  = do  ID _ _ αs _ _ <- undefined -- resolveType c                              -- (1)
        let γ'        = tcEnvAdds (tyBinds αs) γ                      -- (2)
        let thisT     = TApp (TRef (QN [] (F.symbol i))) (tVars αs) fTop      -- (3)
        ce'          <- tcWithThis thisT $ mapM (tcClassElt γ' i) ce  -- (4)
        return          (ClassStmt l i e is ce', Just γ)
  where
    tVars   αs    = [ tVar   α | α <- αs ] 
    tyBinds αs    = [(tVarId α, tVar α) | α <- αs]

-- OTHER (Not handled)
tcStmt _ s 
  = convertError "tcStmt" s



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
       rhsT      = tcEnvFindTy x γ
       -- rhsT      = tcEnvFindSpecOrTy x γ

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
         Just ta -> do θ <- unifyTypeM (srcPos l) γ t ta
                       (,ta) <$> castM γ e' (apply θ t) ta

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
      Just t  -> return (e, t)
      Nothing -> tcError $ errorUnboundId (ann l) x
 
tcExpr γ e@(CondExpr _ _ _ _)
  = tcCall γ e 

tcExpr γ e@(PrefixExpr _ _ _)
  = tcCall γ e 

tcExpr γ e@(InfixExpr _ _ _ _)
  = tcCall γ e 

-- | f(e)
tcExpr γ e@(CallExpr _ _ _)
  = tcCall γ e 

-- | [e1,..,en]
tcExpr γ e@(ArrayLit _ _)
  = tcCall γ e 
 
-- | { f: e }
tcExpr γ e@(ObjectLit _ _) 
  = tcCall γ e

-- | < t > e
tcExpr γ ex@(Cast l@(Ann loc fs) e)
  = do  (e', t)             <- tcExpr γ  e
        case userCasts of
          -- Stuff from before
          [  ]              -> case e' of
                                 Cast (Ann _ fs') e'' -> return (Cast (Ann loc (fs ++ fs')) e'', t)
                                 _                    -> return (Cast l e', t)
          -- User cast
          [t1]              -> return  $ (Cast (Ann loc $ replace t <$> fs) e', t1)
          _                 -> tcError $ bugMultipleCasts loc ex
  where
    userCasts                = [ ct | UserCast ct <- fs ]
    replace t0 (UserCast t1) | on (==) toType t0 t1  = TCast (tce_ctx γ) $ CNo
                             | isSubtype γ t0 t1     = TCast (tce_ctx γ) $ CUp t0 t1
                             | isSubtype γ t1 t0     = TCast (tce_ctx γ) $ CDn t0 t1
                             | otherwise             = TCast (tce_ctx γ) $ CDead t1
    replace _  a                                     = a
    
 
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

-- | function (x,..) {  }
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
         TClass (QN p x)      -> do ([e1',_], t) <- tcNormalCall γ l o
                                                      [e1, StringLit l2 (F.symbolString x)] 
                                                      -- FIXME: add qualified name
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
  = do (es', t)               <- tcNormalCall γ l "ObjectLit" es $ undefined -- FIXME: objLitTy l ps 
       return                  $ (ObjectLit l (zip ps es'), t)
  where
    (ps,es) = unzip bs

-- | `new e(e1,...,en)`
tcCall γ (NewExpr l e es) 
  = do (e',t)                     <- tcExpr γ e
       case getConstructor γ t of 
         Just ct -> do (es', t)   <- tcNormalCall γ l "constructor" es ct
                       return      $ (NewExpr l e' es', t)
         _       -> tcError $ errorConstrMissing (srcPos l) t

-- | e.f 
tcCall γ ef@(DotRef l e f)
  = do z              <- runFailM $ tcExpr γ e
       case z of
         Right (_, t) -> case getProp γ (F.symbol f) t of
                           Just (s, t) -> do ([e'], t') <- tcNormalCall γ l ef [e] $ mkTy s t
                                             return      $ (DotRef l e' f, t')
                           _           -> tcError $ errorMissingFld (srcPos l) f t
         Left err     -> tcError err
  where
    mkTy s t = mkFun ([], [B (F.symbol "this") s], t) 

         
-- | `super(e1,...,en)`
--   TSC will already have checked that `super` is only called whithin the constructor.
tcCall γ (CallExpr l e@(SuperRef _)  es) 
  =  do  elts         <- t_elts <$> (getSuperDefM l =<< tcPeekThis)
         case [ t | ConsSig t <- elts ] of
           [t]        -> do (es', t')        <- tcNormalCall γ l "constructor" es t
                            return            $ (CallExpr l e es', t')
           _          -> error "tcError $ errorConsSigMissing (srcPos l) e"
   
-- | `e(es)`
tcCall γ (CallExpr l em@(DotRef _ e f) es)
  = do z              <- runFailM $ tcExpr γ e
       case z of 
         Right (_, t) -> do case getElt γ f t of
                              Just tf -> do (em', es', t) <- tcCallDotRef γ tf l em es
                                            return         $ (CallExpr l em' es', t)
                              Nothing -> tcError $ errorMissingFld (srcPos l) f t
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
resolveOverload γ l fn es ts ft = undefined
--   = do -- δ        <- getDef
--        let sigs  = catMaybes (bkFun <$> getCallable γ ft)
--        case [ mkFun (vs, τs,τ) | (vs, τs, τ) <- sigs, length τs == length es ] of
--          [t]    -> do θ     <- getSubst 
--                       return $ Just (θ,t)
--          fts    -> do θs    <- mapM (\t -> tcCallCaseTry γ l fn ts t) fts
--                       return $ listToMaybe [ (θ, apply θ t) | (t, Just θ) <- zip fts θs ]


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
tcCallCaseTry γ l fn ts ft = undefined
-- runMaybeM $ 
--   do (_,ibs,_) <- instantiate l (tce_ctx γ) fn ft
--      let its    = b_type <$> ibs
--      θ'        <- unifyTypesM (ann l) "tcCallCaseTryAux" ts its
--      zipWithM_    (subtypeM (ann l)) (apply θ' ts) (apply θ' its)
--      return     $ θ'


tcCallCase γ l fn es ts ft = undefined 
--   = do let ξ            = tce_ctx γ
--        -- Generate fresh type parameters
--        (_,ibs,ot)      <- {- tracePP ("inst " ++ ppshow ft) <$> -} instantiate l ξ fn ft
--        let its          = b_type <$> ibs
--        θ               <- {- tracePP "unif" <$> -} unifyTypesM (srcPos l) "tcCall" ts its
--        let (ts',its')   = mapPair (apply θ) (ts, its)
--        es'             <- NM.zipWith3M (castM ξ) es ts' its'
--        return             (es', apply θ ot, θ)

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
-- envJoin l γ (Just γ1) (Just γ2) = 
--   do let xs = phiVarsAnnot l
--      ts    <- mapM (getPhiType l γ1 γ2) xs
--      θ     <- getSubst
--      return $ Just $ tcEnvAdds (zip xs ts) (apply θ γ)


----------------------------------------------------------------------------------
envLoopJoin :: PPR r => (AnnSSA r) -> TCEnv r -> TCEnvO r -> TCM r (TCEnvO r)
----------------------------------------------------------------------------------
envLoopJoin _ γ Nothing   = return $ Just γ
-- envLoopJoin l γ (Just γl) = 
--   do let xs = phiVarsAnnot l 
--      ts    <- mapM (getLoopNextPhiType l γ γl) xs
--      θ     <- getSubst
--      return $ Just $ tcEnvAdds (zip xs ts) (apply θ γ)
 
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
  -- mapM (sanity $ srcPos l) $ [ t | VarAnn                 t  <- ann_fact l ] 
  --                         ++ [ t | FieldAnn (FieldSig _ _ t) <- ann_fact l ]
  return                   $ [ t | VarAnn                 t  <- ann_fact l ] 
                          ++ [ t | FieldAnn (FieldSig _ _ t) <- ann_fact l ]

-- sanity l t@(TApp (TRef i) ts _) 
--   = do  δ       <- getDef 
--         case findSym i δ of
--           Just (ID _ _ αs _ _) | length αs == length ts -> return  $ t 
--           Just (ID _ n αs _ _) | otherwise              -> tcError $ errorTypeArgsNum l n (length αs) (length ts)
--           Nothing                                       -> error   $ "BUG: Id: " ++ ppshow i 
--                                                                   ++ " was not found in env at " 
--                                                                   ++ ppshow (srcPos l) 
-- sanity _ t = return t

