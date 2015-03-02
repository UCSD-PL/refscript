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

import           Control.Applicative                ((<$>), (<*>))
import           Control.Monad                
import           Control.Exception                  (throw)
import           Control.Arrow                      ((***))

import qualified Data.IntMap.Strict                 as I
import qualified Data.Map.Strict                    as M
import           Data.Maybe                         (catMaybes, maybeToList, fromMaybe)
import           Data.List                          (nub, find, sortBy)
import           Data.Monoid                        (mappend)
import           Data.Function                      (on)
import           Data.Generics                   
import qualified Data.Traversable                   as T              

import           Language.Nano.Annots
import           Language.Nano.CmdLine              (noFailCasts, Config)
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Program
import           Language.Nano.Types
import           Language.Nano.Env
import           Language.Nano.Misc                 (convertError, zipWith3M, dup)
import           Language.Nano.SystemUtils
import           Language.Nano.Typecheck.Unify
import           Language.Nano.Typecheck.Environment
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Typecheck.Parse 
import           Language.Nano.Typecheck.TCMonad
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Lookup
import           Language.Nano.Liquid.Types
import           Language.Nano.SSA.SSA
import           Language.Nano.Visitor

import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Misc             as FM 
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax.Annotations

-- import           Debug.Trace                        hiding (traceShow)

import qualified System.Console.CmdArgs.Verbosity as V


type PPRSF r = (PPR r, Substitutable r (Fact r), Free (Fact r)) 

--------------------------------------------------------------------------------
-- | Top-level Verifier 

--------------------------------------------------------------------------------
verifyFile :: Config -> [FilePath] -> IO (UAnnSol a, F.FixResult Error)
--------------------------------------------------------------------------------
verifyFile cfg fs
  = parseNanoFromFiles fs >>= \case 
      Left  l -> return (NoAnn, l)
      Right x -> ssaTransform x >>= \case
                   Left  l -> return (NoAnn, l)
                   Right y -> typeCheck cfg y >>= \case
                                Left  l -> unsafe l
                                Right z -> return $ safe cfg z

unsafe errs = do putStrLn "\n\n\nErrors Found!\n\n" 
                 return $ (NoAnn, errs)

safe cfg (Nano {code = Src fs}) = (NoAnn, failCasts (noFailCasts cfg) fs)
    where
        failCasts True  _   = F.Safe
        failCasts False fs  = applyNonNull F.Safe F.Unsafe 
                            $ concatMap castErrors 
                            $ casts fs 

        casts              :: Data r => [Statement (AnnType r)] -> [AnnType r]
        casts stmts         = everything (++) ([] `mkQ` f) stmts
          where 
            f              :: Expression (AnnType r) -> [(AnnType r)]
            f (Cast_ a _)   = [a]
            f _             = [] 

-------------------------------------------------------------------------------
castErrors :: PPRSF r => AnnType r -> [Error] 
-------------------------------------------------------------------------------
castErrors (Ann _ l facts) = downErrors
  where 
    downErrors             = [errorDownCast l t1 t2 | TCast _ (CDn t1 t2) <- facts]


-------------------------------------------------------------------------------
typeCheck :: PPRSF r 
          => Config 
          -> NanoSSAR r 
          -> IO (Either (F.FixResult Error) (NanoTypeR r))
-------------------------------------------------------------------------------
typeCheck cfg pgm = do 
  v <- V.getVerbosity
  let r = execute cfg v pgm $ tcNano pgm 
  return $ r


-------------------------------------------------------------------------------
-- | TypeCheck Nano Program
-------------------------------------------------------------------------------
tcNano :: PPRSF r => NanoSSAR r -> TCM r (NanoTypeR r)
-------------------------------------------------------------------------------
tcNano p@(Nano {code = Src fs})
  = do  _       <- checkTypes γ 
        (fs',_) <- tcStmts γ fs
        fs''    <- patch fs'
        ast_cnt <- getAstCount
        return   $ p { code   = Src fs'' 
                     , max_id = ast_cnt }
  where
    γ = initGlobalEnv p


-- | Patch annotation on the AST
--
-------------------------------------------------------------------------------
patch :: PPRSF r => [Statement (AnnSSA r)] -> TCM r [Statement (AnnSSA r)]
-------------------------------------------------------------------------------
patch fs = 
  do  (m,θ)                   <- (,) <$> getAnns <*> getSubst
      --   
      -- patch code with annotations gathered in `m`
      --
      return                   $ (pa m <$>) <$> apply θ <$> fs
  where
    pa m     (Ann i l fs)      = Ann i l $ nub $ fs ++ filter accepted (I.findWithDefault [] i m)
    accepted (TypInst _ _ _  ) = True
    accepted (Overload _ _   ) = True
    accepted (EltOverload _ _) = True
    accepted (PhiVarTy _     ) = True
    accepted (PhiVarTC _     ) = True
    accepted (PhiVar _       ) = True
    accepted _                 = False

-------------------------------------------------------------------------------
-- | Initialize environment
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
initGlobalEnv :: PPRSF r => NanoSSAR r -> TCEnv r
-------------------------------------------------------------------------------
initGlobalEnv pgm@(Nano { code = Src ss }) = TCE nms mod cha ctx pth Nothing
  where
    reshuffle1 = \(_,_,c,d,e) -> (d,c,e)
    reshuffle2 = \(_,c,d,e)   -> (d,c,e)
    nms        = envUnion
                 (envAdds extras    $ envMap reshuffle1 $ mkVarEnv visibleNs)
                 (envMap reshuffle2 $ envUnionList $ maybeToList 
                                    $ m_variables <$> qenvFindTy pth mod)
    visibleNs  = visibleVars ss
    extras     = [(Id (srcPos dummySpan) "undefined", undefInfo)]
    undefInfo  = (TApp TUndef [] fTop, ReadOnly, Initialized)
    cha        = pCHA pgm 
    mod        = pModules pgm 
    ctx        = emptyContext
    pth        = mkAbsPath []


initFuncEnv γ f i αs thisTO xs ts t args s = TCE nms mod cha ctx pth parent
  where
    tyBinds   = [(tVarId α, (tVar α, ReadOnly, Initialized)) | α <- αs]
    varBinds  = zip (fmap ann <$> xs) $ (,WriteLocal, Initialized) <$> ts
    nms       = envAddReturn f (t, ReadOnly, Initialized)
              $ envAdds (thisBind ++ tyBinds ++ varBinds ++ args) 
              $ envMap (\(_,_,c,d,e) -> (d,c,e)) 
              $ mkVarEnv $ visibleVars s
    mod       = tce_mod γ
    cha       = tce_cha γ
    ctx       = pushContext i (tce_ctx γ) 
    pth       = tce_path γ
    parent    = Just γ
    -- FIXME: this shouldn't be a special binding ... Include in xs
    thisBind  = (Id (srcPos dummySpan) "this",) . 
                (, ReadOnly, Initialized)   <$> 
                maybeToList thisTO


---------------------------------------------------------------------------------------
initModuleEnv :: (PPRSF r, F.Symbolic n, PP n) => TCEnv r -> n -> [Statement (AnnSSA r)] -> TCEnv r
---------------------------------------------------------------------------------------
initModuleEnv γ n s = TCE nms mod cha ctx pth parent
  where
    reshuffle1 = \(_,_,c,d,e) -> (d,c,e)
    reshuffle2 = \(_,c,d,e)   -> (d,c,e)
    nms        = (envMap reshuffle1 $ mkVarEnv $ visibleVars s) `envUnion`
                 (envMap reshuffle2 $ envUnionList $ maybeToList 
                                    $ m_variables <$> qenvFindTy pth mod)
    mod        = tce_mod γ
    ctx        = emptyContext
    cha        = tce_cha γ
    pth        = extendAbsPath (tce_path γ) n
    parent     = Just γ


-------------------------------------------------------------------------------
-- | Environment wrappers
-------------------------------------------------------------------------------

tcEnvAdds     xs γ      = γ { tce_names = envAdds xs $ tce_names γ }

tcEnvAdd      x t γ     = γ { tce_names = envAdd x t $ tce_names γ }

tcEnvFindTy            :: (PPRSF r, F.Symbolic x, IsLocated x) => x -> TCEnv r -> Maybe (RType r)
tcEnvFindTy x γ         = fst3 <$> tcEnvFindTyWithAgsn x γ 

tcEnvFindTyWithAgsn    :: (PPRSF r, F.Symbolic x) => x -> TCEnv r -> Maybe (RType r, Assignability, Initialization)
tcEnvFindTyWithAgsn x γ = case envFindTy x $ tce_names γ of 
                            Just t -> Just $ adjustInit t
                            Nothing     ->
                              case tce_parent γ of 
                                Just γ' -> tcEnvFindTyWithAgsn x γ'
                                Nothing -> Nothing
  where
    adjustInit s@(_, _, Initialized) = s
    adjustInit (t, a, _ ) = (orUndef t, a, Uninitialized)



-- 
-- This is a variant of the above that doesn't add the ' + undefined' for
-- non-initialized variables.
-- 
tcEnvFindTyForAsgn    :: (PPRSF r, F.Symbolic x) => x -> TCEnv r -> Maybe (RType r, Assignability, Initialization)
tcEnvFindTyForAsgn x γ = case envFindTy x $ tce_names γ of 
                           Just t -> Just $ t
                           Nothing     -> 
                             case tce_parent γ of 
                               Just γ' -> tcEnvFindTyWithAgsn x γ'
                               Nothing -> Nothing

safeTcEnvFindTy l γ x   = case tcEnvFindTy x γ of
                            Just t  -> return t
                            Nothing -> die $ bugEnvFindTy (srcPos l) x 

tcEnvFindReturn         = fst3 . envFindReturn . tce_names

tcEnvFindTypeDefM l γ x 
  = case resolveTypeInEnv γ x of 
      Just t  -> return t
      Nothing -> die $ bugClassDefNotFound (srcPos l) x


-------------------------------------------------------------------------------
-- | TypeCheck Function 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
tcFun :: PPRSF r 
      => TCEnv r 
      -> Statement (AnnSSA r) 
      -> TCM r (Statement (AnnSSA r), Maybe (TCEnv r))
-------------------------------------------------------------------------------
tcFun γ (FunctionStmt l f xs body)
  = case tcEnvFindTy f γ of
      Just ft       -> do ts    <- tcFunTys l f xs ft
                          body' <- foldM (tcFun1 γ l f xs) body ts
                          return $ (FunctionStmt l f xs body', Just γ) 
      Nothing       -> die $ errorMissingSpec (srcPos l) f

tcFun _  s = die $ bug (srcPos s) $ "Calling tcFun not on FunctionStatement"

-------------------------------------------------------------------------------
tcFun1 :: (PPRSF r, IsLocated l, CallSite t) 
       => TCEnv r 
       -> AnnSSA r 
       -> l 
       -> [Id (AnnSSA r)] 
       -> [Statement (AnnSSA r)] 
       -> (t, ([TVar], Maybe (RType r), [RType r], RType r)) 
       -> TCM r [Statement (AnnSSA r)]
-------------------------------------------------------------------------------
tcFun1 g l f xs body fty 
  = do  body'           <- addReturnStmt l t body
        tcFunBody g1 l body' t
  where
    g1 					         = initFuncEnv g f i vs s xs ts t arg body
    (i, (vs,s,ts,t))     = fty
    arg                  = [(argId $ srcPos l, (aTy, ReadOnly, Initialized))]
    aTy                  = argTy l ts $ tce_names g


addReturnStmt l t body | isTVoid t 
                       = (body ++) . single <$> (`ReturnStmt` Nothing) 
                                            <$> freshenAnn l
                       | otherwise 
                       = return body
       
tcFunBody γ l body t 
  = do  z                          <- tcStmts γ body
        case z of 
          (_, Just _) | t /= tVoid -> tcError $ errorMissingReturn (srcPos l)
          (b, _     ) | otherwise  -> return b


-- | Strings ahead: HACK Alert
tVarId (TV a l) = Id l $ "TVAR$$" ++ F.symbolString a   


---------------------------------------------------------------------------------------
tcClassElt :: PPRSF r
           => TCEnv r 
           -> IfaceDef r 
           -> ClassElt (AnnSSA r) 
           -> TCM r (ClassElt (AnnSSA r))
---------------------------------------------------------------------------------------
tcClassElt γ d@(ID nm _ vs _ _ ) (Constructor l xs body) 
  = do  cTy      <- mkCtorTy
        its      <- tcFunTys l ctor xs cTy
        body'    <- foldM (tcFun1 γ'' l ctor xs) body its
        return    $ Constructor l xs body'
  where 
    ctor          = builtinOpId BICtor
    ctorExit      = builtinOpId BICtorExit
    super         = builtinOpId BISuper

    γ'            | Just t <- extractParent'' γ d 
                  = tcEnvAdd super (t,ReadOnly,Initialized) γ
                  | otherwise
                  = γ

    γ''           = tcEnvAdd ctorExit (mkCtorExitTy,ReadOnly,Initialized) γ'

    -- XXX        : * Keep the right order of fields
    --              * Make the return object immutable to avoid contra-variance
    --                checks at the return from the constructor.
    --              * Exclude __proto__ field 
    mkCtorExitTy  = mkFun (vs,Nothing,bs,tVoid)
      where 
        bs        | Just (TCons _ ms _) <- expandType γ (TRef nm (tVar <$> vs) fTop)
                  = sortBy c_sym [ B s t | ((_,InstanceMember),(FieldSig s _ _ t)) <- M.toList ms 
                                         , F.symbol s /= F.symbol "__proto__" ]
                  | otherwise
                  = []
    
    c_sym         = on compare b_sym

    -- FIXME      : Do case of mutliple overloads 
    mkCtorTy      | [ConsAnn (ConsSig t)] <- ann_fact l,
                    Just t'               <- fixRet $ mkAll vs t
                  = return t'
                  | otherwise 
                  = die $ unsupportedNonSingleConsTy (srcPos l)
                              
    -- FIXME      : Do case of mutliple overloads 
    fixRet t      | Just (vs,Nothing,bs,_)  <- bkFun t
                  = Just $ mkFun (vs, Nothing , bs, tVoid)
                  | otherwise 
                  = Nothing

-- | Static field
--
tcClassElt γ dfn (MemberVarDecl l True x (Just e))
  | Just (FieldSig _ _ _ t) <- spec
  = do  (FI _ [e'],_) <- tcNormalCall γ l "field init" (FI Nothing [(e, Just t)]) 
                      $ mkInitFldTy t
        return        $ MemberVarDecl l True x $ Just e'
  | otherwise
  = tcError $ errorClassEltAnnot (srcPos l) (t_name dfn) x
  where
    spec    = M.lookup (F.symbol x, StaticMember) (t_elts dfn)

tcClassElt _ _ (MemberVarDecl l True x Nothing)
  = tcError $ unsupportedStaticNoInit (srcPos l) x

-- | Instance variable
--
tcClassElt _ _ m@(MemberVarDecl _ False _ Nothing) 
  = return m
tcClassElt _ _ (MemberVarDecl l False x _) 
  = die $ bugClassInstVarInit (srcPos l) x

-- | Static method
--
tcClassElt γ dfn (MemberMethDef l True x xs body) 
  | Just (MethSig _ t) <- spec
  = do  its      <- tcFunTys l x xs t
        body'    <- foldM (tcFun1 γ l x xs) body its
        return    $ MemberMethDef l True x xs body'
  | otherwise
  = tcError       $ errorClassEltAnnot (srcPos l) (t_name dfn) x
  where 
    spec          = M.lookup (F.symbol x, StaticMember) (t_elts dfn)

-- | Instance method
--
tcClassElt γ dfn (MemberMethDef l False x xs bd) 
  | Just (MethSig _ t) <- spec
  = do  its      <- tcFunTys l x xs t
        bd'      <- foldM (tcFun1 γ l x xs) bd $ addSelfB <$> its
        return    $ MemberMethDef l False x xs bd'
  | otherwise
  = tcError       $ errorClassEltAnnot (srcPos l) (t_name dfn) x
  where
    spec               = M.lookup (F.symbol x, InstanceMember) (t_elts dfn)
    (QP AK_ _ ss)      = tce_path γ 
    addSelfB (i,(vs,so,xs,y)) = (i,(vs,mkSelf so,xs,y))
    mkSelf (Just (TSelf m)) 
                       = Just $ mkThis m (t_args dfn)
    mkSelf (Just t)    = Just t
    mkSelf Nothing     = Just $ mkThis t_readOnly (t_args dfn) 
    rn                 = QN AK_ (srcPos l) ss (F.symbol $ t_name dfn)
    mkThis m (_:αs)    = TRef rn (m : map tVar αs) fTop
    mkThis _ _         = throw $ bug (srcPos l) "Typecheck.Typecheck.tcClassElt MemberMethDef" 


tcClassElt _ _ m@(MemberMethDecl _ _ _ _ ) = return m


--------------------------------------------------------------------------------
tcSeq :: (TCEnv r -> a -> TCM r (a, TCEnvO r)) -> TCEnv r -> [a] -> TCM r ([a], TCEnvO r)
--------------------------------------------------------------------------------
tcSeq f             = go []
  where
    go acc γ []     = return (reverse acc, Just γ)
    go acc γ (x:xs) = do (y, γo) <- f γ x
                         case γo of
                           Nothing -> return (reverse acc ++ y:xs, Nothing) 
                           Just γ' -> go (y:acc) γ' xs

--------------------------------------------------------------------------------
tcStmts :: PPRSF r => 
  TCEnv r -> [Statement (AnnSSA r)] -> TCM r ([Statement (AnnSSA r)], TCEnvO r)
--------------------------------------------------------------------------------
tcStmts = tcSeq tcStmt 


-------------------------------------------------------------------------------
tcStmt  :: PPRSF r =>
  TCEnv r -> Statement (AnnSSA r) -> TCM r (Statement (AnnSSA r), TCEnvO r)
-------------------------------------------------------------------------------
-- skip
tcStmt γ s@(EmptyStmt _) 
  = return (s, Just γ)

-- declare function foo(...): T; 
-- this definitions will be hoisted
tcStmt γ s@(FuncAmbDecl _ _ _) 
  = return (s, Just γ)

tcStmt γ s@(FuncOverload _ _ _) 
  = return (s, Just γ)

-- interface Foo;
-- this definitions will be hoisted
tcStmt γ s@(IfaceStmt _ _) 
  = return (s, Just γ)

-- x = e
tcStmt γ (ExprStmt l1 (AssignExpr l2 OpAssign (LVar lx x) e))
  = do (e', g) <- tcAsgn l1 γ (Id lx x) e
       return   (ExprStmt l1 (AssignExpr l2 OpAssign (LVar lx x) e'), g)

-- e1.f = e2
tcStmt γ e@(ExprStmt l (AssignExpr l2 OpAssign r@(LDot l1 e1 f) e2))
  = do  z               <- runFailM ( tcExpr γ e1l Nothing )
        case z of 
          Right (_,te1) -> tcSetProp $ fmap snd3 $ getProp γ FieldAccess Nothing f te1
          Left _        -> tcSetProp $ Nothing
  where
    e1l = fmap (\a -> a { ann_fact = BypassUnique : ann_fact a }) e1
    tcSetProp rhsCtx = 
      do  opTy          <- setPropTy l (F.symbol f) <$> safeTcEnvFindTy l γ (builtinOpId BISetProp)
          ([e1',e2'],_) <- tcNormalCallWCtx γ l BISetProp [(e1l, Nothing), (e2, rhsCtx)] opTy
          return         $ (ExprStmt l $ AssignExpr l2 OpAssign (LDot l1 e1' f) e2', Just γ)

-- e
tcStmt γ (ExprStmt l e)
  = do (e', to) <- tcExprW γ e
       return (ExprStmt l e', const γ <$> to)
           
-- s1;s2;...;sn
tcStmt γ (BlockStmt l stmts) 
  = do (stmts', g) <- tcStmts γ stmts
       return (BlockStmt l stmts', g)

-- if b { s1 }
tcStmt γ (IfSingleStmt l b s)
  = tcStmt γ (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
tcStmt γ (IfStmt l e s1 s2)
  = do opTy         <- safeTcEnvFindTy l γ (builtinOpId BITruthy)
       ([e'], z)    <- tcNormalCallW γ l BITruthy [e] opTy
       case z of 
         Just _  -> do  (s1', γ1) <- tcStmt γ s1
                        (s2', γ2) <- tcStmt γ s2
                        z         <- envJoin l γ γ1 γ2
                        case z of 
                          Just (γ3',s1s,s2s) -> 
                            do  l1 <- freshenAnn l 
                                l2 <- freshenAnn l 
                                return $ (IfStmt l e' (postfixStmt l1 s1s s1') 
                                                      (postfixStmt l2 s2s s2')
                                         , Just γ3')
                          Nothing -> 
                            return (IfStmt l e' s1' s2', Nothing)
         _       -> return (IfStmt l e' s1 s2, Nothing)

-- while c { b } 
tcStmt γ (WhileStmt l c b) 
  = do (c', to)  <- tcExprW γ c
       case to of
         Nothing -> return (WhileStmt l c' b, Nothing)
         Just t  -> do unifyTypeM (srcPos l) γ t tBool
                       pTys         <- mapM (safeTcEnvFindTy l γ) phis
                       (b', γl)     <- tcStmt (tcEnvAdds (zip xs ((,WriteLocal,Initialized) <$> pTys)) γ) b
                       γout         <- envLoopJoin l γ γl
                       return        $ (WhileStmt l c' b', γout)  
    where 
       xs            = [ mkNextId x | x <- phis ]
       phis          = [ x | x <- phiVarsAnnot l ]

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt γ (VarDeclStmt l ds)
  = do (ds', z) <- tcSeq tcVarDecl γ ds
       return      (VarDeclStmt l ds', z)

-- return e 
tcStmt γ (ReturnStmt l eo) 
  = tcRetW γ l eo

-- throw e 
tcStmt γ (ThrowStmt l e) 
  = (,Nothing) . ThrowStmt l . fst <$> tcExprW γ e


tcStmt γ s@(FunctionStmt _ _ _ _)
  = tcFun γ s

-- | class A<S...> [extends B<T...>] [implements I,J,...] { ... }
tcStmt γ (ClassStmt l x e is ce) 
  = do  dfn      <- tcEnvFindTypeDefM l γ rn
        ms       <- mapM (tcClassElt γ dfn) ce
        return    $ (ClassStmt l x e is ms, Just γ)
  where
    rn            = QN AK_ (srcPos l) ss (F.symbol x)
    QP AK_ _ ss   = tce_path γ 

-- | module M { ... } 
tcStmt γ (ModuleStmt l n body) 
  = (ModuleStmt l n *** return (Just γ)) <$>  tcStmts (initModuleEnv γ n body) body

-- | enum M { ... } 
tcStmt γ (EnumStmt l n body) 
  = return (EnumStmt l n body, Just γ)

-- OTHER (Not handled)
tcStmt _ s 
  = convertError "tcStmt" s


---------------------------------------------------------------------------------------
tcVarDecl ::  PPRSF r => TCEnv r -> VarDecl (AnnSSA r) -> TCM r (VarDecl (AnnSSA r), TCEnvO r)
---------------------------------------------------------------------------------------
tcVarDecl γ v@(VarDecl l x (Just e))
  = case scrapeVarDecl v of
      [ ]     -> do (e', to)   <- tcExprW γ e
                    return      $ (VarDecl l x (Just e'), tcEnvAddo γ x $ (,WriteLocal,Initialized) <$> to)
      [(_,t)] -> f WriteGlobal <$> tcCast γ l e t
      _       -> tcError        $ errorVarDeclAnnot (srcPos l) x
  where
    f a = (VarDecl l x . Just *** Just . (`tcEnvAdds` γ) . single . (x,) . (,a, Initialized)) 

tcVarDecl γ v@(VarDecl l x Nothing)
  = case scrapeVarDecl v of
      [ ]                   -> tcVarDecl γ $ VarDecl l x $ Just $ VarRef l $ Id l "undefined"
      [(AmbVarDeclKind, t)] -> return      $ (v, Just $ tcEnvAdds [(x, (t,WriteGlobal, Initialized))] γ)
      [(_, t)]              -> return      $ (v, Just $ tcEnvAdds [(x, (t,WriteGlobal, Uninitialized))] γ)
      _                     -> tcError     $ errorVarDeclAnnot (srcPos l) x

-------------------------------------------------------------------------------
tcAsgn :: PPRSF r 
       => AnnSSA r -> TCEnv r -> Id (AnnSSA r) -> ExprSSAR r -> TCM r (ExprSSAR r, TCEnvO r)
-------------------------------------------------------------------------------
tcAsgn l γ x e
  = do (e' , to)    <- tcExprTW l γ e rhsT
       return       $ (e', tcEnvAddo γ x $ (,asgn,init) <$> to)
    where
       (rhsT, asgn, init) = case tcEnvFindTyForAsgn x γ of
                              Just (t,a,_) -> (Just t, a, Initialized)
                              Nothing      -> (Nothing, WriteLocal, Initialized)

tcEnvAddo _ _ Nothing  = Nothing
tcEnvAddo γ x (Just t) = Just $ tcEnvAdds [(x, t)] γ 

  
--------------------------------------------------------------------------------
tcExprTW :: PPRSF r => AnnSSA r -> TCEnv r -> ExprSSAR r 
                    -> Maybe (RType r) -> TCM r (ExprSSAR r, Maybe (RType r))
tcExprW  :: PPRSF r => TCEnv r -> ExprSSAR r -> TCM r (ExprSSAR r, Maybe (RType r))
tcExprWD :: PPRSF r => TCEnv r -> ExprSSAR r 
                    -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------
tcExprTW _ γ e Nothing    
  = tcExprW γ e
tcExprTW l γ e (Just t)   
  = (tcWrap $ tcExprT l γ e t) >>= tcEW γ e

tcExprW γ e        
  = (tcWrap $ tcExpr γ e Nothing) >>= tcEW γ e 

tcExprWD γ e t            
  = (tcWrap $ tcExpr γ e t) >>= tcEW γ e >>= \case 
      (e, Just t) -> return (e, t)
      (e, _     ) -> return (e, tNull)

 
tcNormalCallW γ l o es t 
  = (tcWrap $ tcNormalCall γ l o (FI Nothing ((,Nothing) <$> es)) t) >>= \case
      Right (FI _ es', t') -> return (es', Just t')
      Left err -> (,Nothing) <$> mapM (deadcastM (tce_ctx γ) err) es
                                

tcNormalCallWCtx γ l o es t 
  = (tcWrap $ tcNormalCall γ l o (FI Nothing es) t) >>= \case
      Right (FI _ es', t') -> return (es', Just t')
      Left err -> (,Nothing) <$> mapM (deadcastM (tce_ctx γ) err) (fst <$> es)

tcRetW γ l (Just e@(VarRef lv x))
  | Just t <- tcEnvFindTy x γ, needsCall t 
  = tcRetW (newEnv t) l (Just (CallExpr l (VarRef l fn) [e'])) >>= \case 
      -- e' won't be cast
      (ReturnStmt _ (Just (CallExpr _ _ [e''])),eo) -> return (ReturnStmt l (Just e''),eo)
      _ -> die $ errorUqMutSubtyping (srcPos l) e t retTy
  where 
    retTy    = tcEnvFindReturn γ 
    newEnv t = tcEnvAdd fn (finalizeTy t,ReadOnly,Initialized) γ
    fn       = Id l "__finalize__"
    e'       = fmap (\a -> a { ann_fact = BypassUnique : ann_fact a }) e
    needsCall (TRef x (m:ts) r) = isUniqueMutable m 
    needsCall _                 = False

tcRetW γ l eo = tcRetW' γ l eo

tcRetW' γ l (Just e)
  = (tcWrap $ tcNormalCall γ l "return" (FI Nothing [(e, Just retTy)]) (returnTy retTy True)) >>= \case
       Right (FI _ es', _) -> (,Nothing) . ReturnStmt l . Just <$> return (head es')
       Left err            -> (,Nothing) . ReturnStmt l . Just <$> deadcastM (tce_ctx γ) err e
  where
    retTy = tcEnvFindReturn γ 

tcRetW' γ l Nothing
  = do (_, _) <- tcNormalCall γ l "return" (FI Nothing []) $ returnTy (tcEnvFindReturn γ) False
       return  $ (ReturnStmt l Nothing, Nothing)
  
tcEW :: PPRSF r => TCEnv r -> ExprSSAR r -> Either Error ((ExprSSAR r), b) -> TCM r ((ExprSSAR r), Maybe b)
tcEW _ _ (Right (e', t')) = return $  (e', Just t')
tcEW γ e (Left err)       = (, Nothing) <$> deadcastM (tce_ctx γ) err e 

-------------------------------------------------------------------------------
tcExprT :: PPRSF r => AnnSSA r -> TCEnv r -> ExprSSAR r -> RType r -> TCM r (ExprSSAR r, RType r)
-------------------------------------------------------------------------------
tcExprT l γ e t 
  = do (FI _ [e'], _) <- tcNormalCall γ l "tcExprT" (FI Nothing [(e, Nothing)]) 
                       $ TFun Nothing [B (F.symbol "x") t] tVoid fTop
       return (e', t)

-------------------------------------------------------------------------------
tcExpr :: PPRSF r => TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
-------------------------------------------------------------------------------
tcExpr _ e@(IntLit _ _) _
  = return (e, tInt)

tcExpr _ e@(HexLit _ _) _
  = return (e, tBV32)

tcExpr _ e@(BoolLit _ _) _
  = return (e, tBool)

tcExpr _ e@(StringLit _ _) _
  = return (e, tString)

tcExpr _ e@(NullLit _) _
  = return (e, tNull)

tcExpr γ e@(ThisRef l) _
  = case tcEnvFindTy (F.symbol "this") γ of 
      Just t  -> return (e, t) 
      Nothing -> tcError $ errorUnboundId (ann l) "this"

tcExpr γ e@(VarRef l x) _
  | Just t <- to, BypassUnique `elem` ann_fact l 
  = return (e,t) 
  | Just t <- to, isCastId x  -- Avoid TC added casts
  = return (e,t)  
  | Just t <- to, disallowed t       
  = tcError $ errorAssignsFields (ann l) x t
  | Just t <- to
  = return (e,t)
  | otherwise
  = tcError $ errorUnboundId (ann l) x
  where 
    to = tcEnvFindTy x γ
    disallowed (TRef _ (m:_) _)      = isUniqueMutable m
    disallowed _                     = False
 
-- | e ? e1 : e2
--   
--   Conditional expresion is transformed to a call to a function with
--   signature: 
--
--     forall C . (c: C, _t: T, x: T, y: T) => T
--
--   The arguments are:
--    
--    * The condition expression
--    * A phony expression with: 
--      - The contextual type `t` if there exists one
--      - Top, otherwise
--    * The first conditional expression
--    * The second conditional expression
--
tcExpr γ (CondExpr l e e1 e2) to
  = do  opTy                      <- mkTy to <$> safeTcEnvFindTy l γ (builtinOpId BICondExpr)
        (sv,v)                    <- dup F.symbol (VarRef l) <$> freshCastId l
        let γ'                     = tcEnvAdd sv (tt, WriteLocal, Initialized) γ
        (FI _ [e',_,e1',e2'], t') <- tcNormalCall γ' l BICondExpr (args v) opTy
        return                     $ (CondExpr l e' e1' e2', t')
  where
    args v   = FI Nothing [(e,Nothing), (v, Nothing),(e1,to),(e2,to)]
    tt       = fromMaybe tTop to
    mkTy Nothing (TAll cv (TAll tv (TFun Nothing [B c_ tc, B t_ _   , B x_ xt, B y_ yt] o r))) = 
                  TAll cv (TAll tv (TFun Nothing [B c_ tc, B t_ tTop, B x_ xt, B y_ yt] o r))
    mkTy _ t = t

tcExpr γ e@(PrefixExpr _ _ _) _
  = tcCall γ e 

tcExpr γ e@(InfixExpr _ _ _ _) _
  = tcCall γ e 

-- | f(e)
tcExpr γ e@(CallExpr _ _ _) _
  = tcCall γ e 

-- | [e1,..,en]
tcExpr γ e@(ArrayLit _ _) _
  = tcCall γ e 
 
-- | { f: e }
tcExpr γ e@(ObjectLit _ _) _
  = tcCall γ e

-- | < t > e
tcExpr γ ex@(Cast l e) _
  = case [ ct | UserCast ct <- ann_fact l ] of
      [t] -> mapFst (Cast l) <$> tcCast γ l e t
      _   -> die $  bugNoCasts (srcPos l) ex

-- | Subtyping induced cast
--
--   If the existing cast has been added by another context (i.e. e' == Cast_  ...) 
--   then typecheck the contents of the cast and return the type of the internal 
--   expression.
--
--   If it's from the same context then the internal expression has been
--   typechecked, so just return the inferred type.
--
tcExpr γ (Cast_ l e) to
  = case [ t_ | TCast ξ t_ <- ann_fact l, tce_ctx γ == ξ ] of
      [ ] -> do (e', t)    <- tcExpr γ e to
                case e' of 
                  Cast_ {} -> die $ bugNestedCasts (srcPos l) e 
                  _        -> (,t) . (`Cast_` e') <$> freshenAnn l
      [c] -> return (Cast_ l e, castType c) 
      _   -> die $ bugNestedCasts (srcPos l) e
 
-- | e.f
tcExpr γ e@(DotRef _ _ _) _
  = tcCall γ e
 
-- -- | e1["s"]
-- tcExpr γ (BracketRef l1 e1 (StringLit l2 s)) to
--   = tcExpr γ (DotRef l1 e1 (Id l2 s)) to
-- 
-- -- | e1[1]
-- tcExpr γ (BracketRef l1 e1 (IntLit l2 i)) to
--   = tcExpr γ (DotRef l1 e1 (Id l2 $ show i)) to
-- 
-- | e1[e2]
tcExpr γ e@(BracketRef _ _ _) _
  = tcCall γ e

-- | e1[e2] = e3
tcExpr γ e@(AssignExpr _ OpAssign (LBracket _ _ _) _) _
  = tcCall γ e

-- | new C(e, ...)
tcExpr γ e@(NewExpr _ _ _) _ 
  = tcCall γ e

-- | super
tcExpr γ e@(SuperRef l) _
  = case tcEnvFindTy (builtinOpId BISuper) γ of
      Just t  -> return (e,t)
      Nothing -> tcError $ errorSuper (ann l)

-- | function (x,..) {  }
tcExpr γ (FuncExpr l fo xs body) tCtxO
  | Just ft   <- funTy 
  =  do ts    <- tcFunTys l f xs ft
        body' <- foldM (tcFun1 γ l f xs) body ts
        return $ (FuncExpr l fo xs body', ft)
  | otherwise        
  = tcError $ errorNoFuncAnn $ srcPos l
  where
    funTy | [ft]    <- [ t | FuncAnn t <- ann_fact l ] = Just ft
          | Just ft <- tCtxO                           = Just ft
          | otherwise                                  = Nothing
    f = maybe (F.symbol "<anonymous>") F.symbol fo

tcExpr _ e _ 
  = convertError "tcExpr" e


-- | @tcCast@ emulating a simplified version of a function call
---------------------------------------------------------------------------------------
tcCast :: PPRSF r => TCEnv r -> AnnSSA r -> ExprSSAR r -> RType r -> TCM r (ExprSSAR r, RType r)
---------------------------------------------------------------------------------------
tcCast γ l e tc 
  = do  opTy                <- safeTcEnvFindTy l γ (builtinOpId BICastExpr)
        cid                 <- freshCastId l
        let γ'               = tcEnvAdd (F.symbol cid) (tc, WriteLocal, Initialized) γ
        (FI _ [_, e'], t')  <- tcNormalCall γ' l "user-cast" 
                               (FI Nothing [(VarRef l cid, Nothing),(e, Just tc)]) opTy
        return               $ (e',t')


---------------------------------------------------------------------------------------
tcCall :: PPRSF r => TCEnv r -> ExprSSAR r -> TCM r (ExprSSAR r, RType r)
---------------------------------------------------------------------------------------

-- | `o e`
tcCall γ (PrefixExpr l o e)        
  = do opTy                   <- safeTcEnvFindTy l γ (prefixOpId o)
       z                      <- tcNormalCall γ l o (FI Nothing [(e, Nothing)]) opTy
       case z of
         (FI _ [e'], t)       -> return (PrefixExpr l o e', t)
         _                    -> tcError $ impossible (srcPos l) "tcCall PrefixExpr"

-- | `e1 o e2`
tcCall γ (InfixExpr l o@OpInstanceof e1 e2) 
  = do (e2',t)                <- tcExpr γ e2 Nothing
       case t of
         TClass (QN AK_ _ _ x)  -> 
              do  opTy              <- safeTcEnvFindTy l γ (infixOpId o)
                  (FI _ [e1',_], t) <- 
                      let args = FI Nothing ((,Nothing) <$> [e1, StringLit l2 (F.symbolString x)]) in
                      tcNormalCall γ l o args opTy
                      -- FIXME: add qualified name
                  return (InfixExpr l o e1' e2', t) 
         _  -> tcError $ unimplemented (srcPos l) "tcCall-instanceof" $ ppshow e2
  where
    l2 = getAnnotation e2

tcCall γ (InfixExpr l o e1 e2)        
  = do opTy                   <- safeTcEnvFindTy l γ (infixOpId o)
       z                      <- tcNormalCall γ l o (FI Nothing [(e1, Nothing), (e2, Nothing)]) opTy
       case z of
         (FI _ [e1', e2'], t) -> return (InfixExpr l o e1' e2', t)
         _                    -> tcError $ impossible (srcPos l) "tcCall InfixExpr"

-- | `e1[e2]`
--
--   Special case for Enumeration, and object literals with numeric or 
--   string arguments. 
--
tcCall γ e@(BracketRef l e1 e2)
  = runFailM (tcExpr γ e1 Nothing) >>= \case
      -- Enumeration
      Right (_, TEnum _) -> tcError $ unimplemented (srcPos l) msg e
      -- Default
      _ -> safeTcEnvFindTy l γ (builtinOpId BIBracketRef) >>= call
  where 
    msg     = "Support for dynamic access of enumerations"
    call ty = tcNormalCall γ l BIBracketRef (args e1 e2) ty >>= \case
          (FI _ [e1', e2'], t) -> return (BracketRef l e1' e2', t)
          _ -> tcError $ impossible (srcPos l) "tcCall BracketRef"
    args e1 e2 = FI Nothing [(e1, Nothing), (e2, Nothing)]
   
-- | `e1[e2] = e3`
tcCall γ (AssignExpr l OpAssign (LBracket l1 e1 e2) e3)
  = do opTy <- safeTcEnvFindTy l γ (builtinOpId BIBracketAssign)
       z <- tcNormalCall γ l BIBracketAssign (FI Nothing ((,Nothing) <$> [e1,e2,e3])) opTy
       case z of
         (FI _ [e1', e2', e3'], t) -> return (AssignExpr l OpAssign (LBracket l1 e1' e2') e3', t)
         _ -> tcError $ impossible (srcPos l) "tcCall AssignExpr"

-- | `[e1,...,en]`
tcCall γ (ArrayLit l es)
  = do opTy <- arrayLitTy l (length es) <$> safeTcEnvFindTy l γ (builtinOpId BIArrayLit)
       (FI _ es', t) <- tcNormalCall γ l BIArrayLit (FI Nothing ((,Nothing) <$> es)) opTy
       return $ (ArrayLit l es', t)

-- | `{ f1:t1,...,fn:tn }`
tcCall γ (ObjectLit l bs) 
  = do (FI _ es', t) <- tcNormalCall γ l "ObjectLit" args (objLitTy l ps)
       return $ (ObjectLit l (zip ps es'), t)
  where
    args    = FI Nothing ((,Nothing) <$> es)
    (ps,es) = unzip bs

-- | `new e(e1,...,en)`
tcCall γ (NewExpr l e es) 
  = do (e',t) <- tcExpr γ e Nothing
       case extractCtor γ t of 
         Just ct -> 
            do (FI _ es', t) <- tcNormalCall γ l "new" (FI Nothing ((,Nothing) <$> es)) ct
               return $ (NewExpr l e' es', t)
         _ -> tcError $ errorConstrMissing (srcPos l) t

-- | e.f 
-- 
tcCall γ ef@(DotRef l e f)
  = runFailM (tcExpr γ e Nothing) >>= \case 
      Right (_, te) -> 
          case getProp γ FieldAccess Nothing f te of
            -- 
            -- Special-casing Array.length
            -- 
            Just (TRef (QN _ _ [] s) _ _, _,_) 
              | F.symbol "Array" == s && F.symbol "length" == F.symbol f -> 
                  do (CallExpr l' (DotRef _ e' _) _, t') <- tcCall γ $ CallExpr l (DotRef l e $ Id l "_get_length_") []
                     return (DotRef l' e' f, t')
            -- 
            -- Proceed with field access
            --
            Just (te', t, _) ->
                do (FI _ [e'],τ) <- tcNormalCall γ l ef args $ mkTy te' t
                   return         $ (DotRef l e' f, τ)

            Nothing -> tcError $ errorMissingFld (srcPos l) f te

      Left err -> tcError err
  where
    args            = FI Nothing [(e,Nothing)]
    mkTy s t        = mkFun ([],Nothing,[B (F.symbol "this") s],t) 
         
-- | `super(e1,...,en)`
--
--   XXX: there shouldn't be any `super(..)` calls after SSA ... 
--
tcCall _ (CallExpr _ (SuperRef _)  _) 
  = error "BUG: super(..) calls should have been eliminated" 

-- | `e.m(es)`
--
--  FIXME: cast @e@ to the subtype for which @f@ is an existing field.
--
tcCall γ ex@(CallExpr l em@(DotRef l1 e f) es)
  = runFailM (tcExpr γ e Nothing) >>= go
  where
         -- Variadic call error
    go z 
--       | Right (_, t) <- z, isVariadicCall f, [] <- es
--       = tcError $ errorVariadicNoArgs (srcPos l) em

      -- Variadic call
      | Right (_, t) <- z, isVariadicCall f, v:vs <- es
      = do (e', _)                 <- tcExpr γ e Nothing
           (FI (Just v') vs', t')  <- tcNormalCall γ l em (argsThis v vs) t
           return                   $ (CallExpr l (DotRef l1 e' f) (v':vs'), t')

      -- Accessing and calling a function field
      | Right (_, t) <- z, Just (o,ft,_) <- getProp γ FieldAccess Nothing f t, isTFun ft
      = do e'                      <- castM γ e t o
           (FI _ es',t')           <- tcNormalCall γ l ex (args es) ft
           return                   $ (CallExpr l (DotRef l1 e' f) es', t')

      -- Invoking a method 
      | Right (_, t) <- z, Just (o,ft,_) <- getProp γ MethodAccess Nothing f t, isTFun ft
      = do e'                      <- castM γ e t o
           (FI (Just e'') es', t') <- tcNormalCall γ l  ex (argsThis e' es) ft
           return                   $ (CallExpr l (DotRef l1 e'' f) es', t')

      | Right (_,_) <- z
      = tcError $ errorCallNotFound (srcPos l) e f

      | Left err <- z
      = tcError err

    isVariadicCall f = F.symbol f == F.symbol "call"
    args vs          = FI Nothing            ((,Nothing) <$> vs)
    argsThis v vs    = FI (Just (v,Nothing)) ((,Nothing) <$> vs)

--     fixThis' thisT tFun 
--       | Just ts <- bkFuns tFun = mkAnd $ mkFun . fixThis thisT <$> ts
--       | otherwise              = tFun
-- 
--     fixThis thisT (vs,Nothing,ts,t) = (vs,Just thisT,ts,t)
--     fixThis _     t                 = t


-- | `e(es)`
tcCall γ (CallExpr l e es)
  = do (e', ft0)              <- tcExpr γ e Nothing
       (FI _ es', t)          <- tcNormalCall γ l e (FI Nothing ((,Nothing) <$> es)) ft0
       return                  $ (CallExpr l e' es', t)

tcCall _ e = tcError $ unimplemented (srcPos e) "tcCall" e

------------------------------------------------------------------------------------------
-- | @tcNormalCall@ resolves overloads and returns cast-wrapped versions of the arguments.
------------------------------------------------------------------------------------------
tcNormalCall :: (PPRSF r, PP a) 
             => TCEnv r 
             -> AnnSSA r 
             -> a 
             -> FuncInputs (ExprSSAR r, Maybe (RType r)) 
             -> RType r 
             -> TCM r (FuncInputs (ExprSSAR r), RType r) 
------------------------------------------------------------------------------------------
tcNormalCall γ l fn etos ft0 
  -- = do ets            <- ltracePP l (ppshow fn ++ "  " ++ ppshow etos) <$> T.mapM (uncurry $ tcExprWD γ) etos
  = do ets            <- T.mapM (uncurry $ tcExprWD γ) etos
       z              <- resolveOverload γ l fn ets ft0
       case z of 
         Just (θ, ft) -> do addAnn (ann_id l) $ Overload (tce_ctx γ) ft
                            addSubst l θ
                            tcWrap (tcCallCase γ l fn ets ft) >>= \case 
                              Right ets -> return ets
                              Left  err -> (,tNull) <$> T.mapM (deadcastM (tce_ctx γ) err . fst) ets 
         Nothing      -> tcError $ uncurry (errorCallNotSup (srcPos l) fn ft0) $ toLists ets
                         -- do tcWrap $ tcError $ uncurry (errorCallNotSup (srcPos l) fn ft0) $ toLists ets
                         --    return (fst <$> ets, tNull)
  where
    toList (FI to ts)  = maybeToList to ++ ts
    toLists f          = (toList $ fst <$> f, toList $ snd <$> f)


-- | `resolveOverload γ l fn es ts ft`
--
--   When resolving an overload there are two prossible cases:
--
--   * There is only a single signature available: then return just this
--     signature regardless of subtyping constraints
--
--   * There are more than one signature available: return all that pass the
--     subtype check (this is what tcCallCaseTry does).
--
------------------------------------------------------------------------------------------
resolveOverload :: (PPRSF r, PP a) 
                => TCEnv r 
                -> AnnSSA r 
                -> a 
                -> FuncInputs (ExprSSAR r, RType r) 
                -> RType r 
                -> TCM r (Maybe (RSubst r, RType r)) 
------------------------------------------------------------------------------------------
-- | A function signature is compatible if:
--
--   * The supplied parameters have the same number of 'normal' arguments, i.e.
--     not including the 'self' argument.
--
--   * If the function requires a 'self' argument, the parameters provide one.
--
resolveOverload γ l fn ets@(FI _ es) ft 
  = case [ mkFun (vs,s,bs,τ) | (vs,s,bs,τ) <- catMaybes $ bkFun <$> extractCall γ ft
                             , length bs == length es
                             , and $ zipWith (\b e -> matchTypes (b_type b) (snd e)) bs es ] of
      [t]    -> Just . (,t) <$> getSubst 
      fts    -> tcCallCaseTry γ l fn (snd <$> ets) fts
  where
    -- selfMatch (Just _) Nothing = False
    -- selfMatch _        _        = True 
    matchTypes (TFun _ as _ _) (TFun _ bs _ _) 
      | length as == length bs = and $ zipWith (on matchTypes b_type) as bs
      | otherwise              = False 
    matchTypes _  _            = True


----------------------------------------------------------------------------------
-- | A successful pairing of formal to actual parameters will return `Just θ`,
--   where @θ@ is the corresponding substitution. If the types are not acceptable 
--   this will return `Nothing`. In this case successful means:
--
--    * Unifying without errors.
--
--    * Passing the subtyping test.
--
--   The monad state is completely reversed after this function returns, thanks 
--   to `runMaybeM`. We don't need to reverse the action of `instantiateFTy`.
----------------------------------------------------------------------------------
tcCallCaseTry :: (PPRSF r, PP a) 
              => TCEnv r 
              -> AnnSSA r 
              -> a 
              -> FuncInputs (RType r) 
              -> [RType r]
              -> TCM r (Maybe (RSubst r, RType r))
----------------------------------------------------------------------------------
tcCallCaseTry _ _ _ _ [] = return Nothing
tcCallCaseTry γ l fn ts (ft:fts)
  = do  z <- runMaybeM $ 
               do  (_,its1,_)      <- instantiateFTy l (tce_ctx γ) fn ft
                   ts1             <- idxMapFI (instantiateTy l $ tce_ctx γ) ts
                   let (ts2, its2)  = balance ts1 its1
                   θ'              <- unifyTypesM (srcPos l) γ ts2 its2
                   zipWithM_          (subtypeM (srcPos l) γ) (apply θ' $ toList ts2) 
                                                              (apply θ' $ toList its2)
                   return           $ θ'
        case z of 
          Just θ  -> return $ Just (θ, ft)
          Nothing -> tcCallCaseTry γ l fn ts fts
  where
    toList (FI to ts)      = maybeToList to ++ ts

idxMapFI f (FI Nothing ts)  = FI     Nothing        <$> mapM (uncurry f) (zip [1..] ts)
idxMapFI f (FI (Just t) ts) = FI <$> Just <$> f 0 t <*> mapM (uncurry f) (zip [2..] ts)

-- `balance supplied_parameters function_sig` should preserve the structure of
-- the supplied_parameters.
balance (FI (Just to) ts) (FI Nothing fs)  = (FI (Just to) ts, FI (Just to) fs)
balance (FI Nothing ts)   (FI (Just _) fs) = (FI Nothing ts, FI Nothing fs)
balance ts                fs               = (ts, fs) 


----------------------------------------------------------------------------------
tcCallCase :: (PP a, PPRSF r) 
           => TCEnv r 
           -> AnnSSA r 
           -> a 
           -> FuncInputs (ExprSSAR r, RType r) 
           -> RType r 
           -> TCM r (FuncInputs (ExprSSAR r), RType r)
----------------------------------------------------------------------------------
tcCallCase γ l fn ets ft  
  = do let ξ            = tce_ctx γ
       (_,its1,ot)     <- instantiateFTy l ξ fn ft
       ts1             <- idxMapFI (instantiateTy l $ tce_ctx γ) ts
       let (ts2, its2)  = balance ts1 its1
       θ               <- unifyTypesM (srcPos l) γ ts2 its2
       let (ts3,its3)   = mapPair (apply θ) (ts2, its2)
       es'             <- app (castM γ) es ts3 its3
       return           $ (es', apply θ ot)
  where
    (es, ts)            = (fst <$> ets, snd <$> ets)
    app f (FI (Just a) as) (FI (Just b) bs) (FI (Just c) cs) = FI <$> (Just <$> f a b c) 
                                                                  <*> zipWith3M f as bs cs
    app f (FI _        as) (FI _        bs) (FI _        cs) = FI <$> return Nothing     
                                                                  <*> zipWith3M f as bs cs

----------------------------------------------------------------------------------
instantiateTy :: PPRSF r => AnnSSA r -> IContext -> Int -> RType r -> TCM r (RType r)
----------------------------------------------------------------------------------
instantiateTy l ξ i = uncurry (freshTyArgs l i ξ) . bkAll 

----------------------------------------------------------------------------------
instantiateFTy :: (PPRSF r, PP a) 
               => AnnSSA r 
               -> IContext 
               -> a 
               -> RType r 
               -> TCM r ([TVar], FuncInputs (RType r), RType r)
----------------------------------------------------------------------------------
instantiateFTy l ξ fn ft 
  = do t'              <- freshTyArgs l 0 ξ αs t 
       maybe err return $ bkFunNoBinds t'
    where
       (αs, t)          = bkAll ft
       err = tcError    $ errorNonFunction (srcPos l) fn ft

----------------------------------------------------------------------------------
-- envJoin :: PPRSF r => AnnSSA r -> TCEnv r -> TCEnvO r -> TCEnvO r -> TCM r (TCEnvO r)
----------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return $ fmap (,[],[]) x 
envJoin _ _ x Nothing           = return $ fmap (,[],[]) x 
envJoin l γ (Just γ1) (Just γ2) = 
  do  xts     <- catMaybes <$> mapM phiType xs
      r       <- foldM (envJoinStep l γ1 γ2 next) (γ,[],[]) xts
      return   $ Just r
  where
    phiType x = fmap (x,) <$> getPhiType l γ1 γ2 x
    xs        = concat [ xs | PhiVar xs  <- ann_fact l ] -- The PHI vars as reported by SSA - no casted vars 
    next      = concat [ vs | PhiPost vs <- ann_fact l ] -- These need to be added to each branch 

envJoinStep l γ1 γ2 next (γ, st01, st02) xt =
  case xt of
    (v, ta@(t, WriteLocal, _)) -> 
      case find ((v ==) . snd3) next of
        Just (_,va, vb) -> do 
          st1     <- mkVdStmt l γ1 va vb t          -- FIRST BRANCH
          st2     <- mkVdStmt l γ2 va vb t          -- SECOND BRANCH
          θ       <- getSubst
          addAnn (ann_id l) $ PhiVarTC vb
          return   $ (tcEnvAdd vb ta $ γ { tce_names = apply θ (tce_names γ) }, st1:st01, st2:st02)
        Nothing -> error $ "Couldn't find " ++ ppshow v ++ " in " ++ ppshow next
    (v, ta) -> 
          do  addAnn (ann_id l) $ PhiVarTC v
              return   $ (tcEnvAdd v ta $ γ { tce_names = tce_names γ }, [], [])

mkVdStmt l γ va vb t1 =
  do  t0           <- safeTcEnvFindTy l γ va
      rhs          <- VarRef <$> freshenAnn l <*> return va
      θ            <- unifyTypeM (srcPos l) γ t0 t1
      setSubst θ
      let (t0',t1') = apply θ (t0,t1)
      c1s          <- castM γ rhs t0' t1'
      vd1          <- mkVds vb c1s
      VarDeclStmt <$> freshenAnn l <*> return [vd1]
  where
    mkVds v e = VarDecl <$> freshenAnn l <*> return v <*> return (Just e)


----------------------------------------------------------------------------------
envLoopJoin :: PPRSF r => AnnSSA r -> TCEnv r -> TCEnvO r -> TCM r (TCEnvO r)
----------------------------------------------------------------------------------
envLoopJoin _ γ Nothing   = return $ Just γ
envLoopJoin l γ (Just γl) = 
  do  ts      <- mapM (getLoopNextPhiType l γ γl) xs
      let xts  = [ (x,t) | (x, Just t) <- zip xs ts ]
      mapM_      (addAnn (ann_id l) . PhiVarTy) (mapSnd (toType . fst3) <$> xts)
      γ'      <- (`substNames` γ) <$> getSubst
      return   $ Just $ tcEnvAdds xts γ'
  where 
      xs             = phiVarsAnnot l 
      substNames θ γ = γ { tce_names = apply θ (tce_names γ) }

-- 
-- Using @tcEnvFindTyForAsgn@ here as the initialization status is 
-- recorded in the initialization part of the output.
--
----------------------------------------------------------------------------------
getPhiType :: PPRSF r => AnnSSA r -> TCEnv r -> TCEnv r -> Var r 
           -> TCM r (Maybe (RType r, Assignability, Initialization))
----------------------------------------------------------------------------------
getPhiType l γ1 γ2 x =
  case (tcEnvFindTyForAsgn x γ1, tcEnvFindTyForAsgn x γ2) of
    (Just (t1,a1,i1), Just (t2,_,i2)) -> do θ     <- getSubst 
                                            t     <- unifyPhiTypes l γ1 x t1 t2 θ
                                            return $ Just (t, a1, i1 `mappend` i2)
    (_              , _             ) -> return Nothing 
    -- bindings that are not in both environments are discarded

----------------------------------------------------------------------------------
getLoopNextPhiType :: PPRSF r => AnnSSA r -> TCEnv r -> TCEnv r -> Var r 
                   -> TCM r (Maybe (RType r, Assignability, Initialization))
----------------------------------------------------------------------------------
getLoopNextPhiType l γ γl x =
  case (tcEnvFindTyForAsgn x γ, tcEnvFindTyForAsgn (mkNextId x) γl) of
    (Just (t1,a1,i1), Just (t2,_,i2)) -> 
        do  θ <- getSubst
            t <- unifyPhiTypes l γ x t1 t2 θ
            return $ Just (t, a1, i1 `mappend` i2)
    _ -> return Nothing
    -- bindings that are not in both environments are discarded

-- | `unifyPhiTypes` 
--
--   * Special casing the situation where one the types in undefined.
----------------------------------------------------------------------------------
unifyPhiTypes :: PPRSF r => AnnSSA r -> TCEnv r -> Var r 
                       -> RType r -> RType r -> RSubst r -> TCM r (RType r)
----------------------------------------------------------------------------------
unifyPhiTypes l γ x t1 t2 θ = 
  case unifys (srcPos l) γ θ [t1] [t2] of  
    Left  _  -> tcError $ errorEnvJoinUnif (srcPos l) x t1 t2
    Right θ' | isTUndef t1 -> setSubst θ' >> return (apply θ' $ orUndef t2)
             | isTUndef t2 -> setSubst θ' >> return (apply θ' $ orUndef t1)
             | isTNull  t1 -> setSubst θ' >> return (apply θ' $ orNull  t2)
             | isTNull  t2 -> setSubst θ' >> return (apply θ' $ orNull  t1)
             -- | isSubtype γ (apply θ' t2) (apply θ' t1) -> setSubst θ' >> return (apply θ' t1)
             | on (==) (apply θ') t1 t2   -> setSubst θ' >> return (apply θ' t1)
             | on (==) (apply θ') t1' t2' -> setSubst θ' 
                                          >> return (apply θ' $ fillNullOrUndef t1 t2 t1)
             | otherwise   -> tcError $ errorEnvJoin (srcPos l) x (toType t1) (toType t2)
  where
    -- t12                     = mkUnion [t1,t2]
    (t1', t2')              = mapPair (mkUnion . clear . bkUnion) (t1, t2)
    fillNullOrUndef t1 t2 t | any isMaybeNull  [t1,t2] = fillUndef t1 t2 $ orNull t
                            | otherwise                = t
    fillUndef t1 t2 t       | any isMaybeUndef [t1,t2] = orUndef t
                            | otherwise                = t
    isMaybeUndef            = any isUndef . bkUnion
    isMaybeNull             = any isNull  . bkUnion
    clear ts                = [ t | t <- ts, not (isNull t) && not (isUndef t) ]


-------------------------------------------------------------------------------------
postfixStmt :: a -> [Statement a] -> Statement a -> Statement a 
-------------------------------------------------------------------------------------
postfixStmt _ [] s = s 
postfixStmt l ss s = BlockStmt l $ expandBlock $ [s] ++ ss

-------------------------------------------------------------------------------------
expandBlock :: [Statement t] -> [Statement t]
-------------------------------------------------------------------------------------
expandBlock = concatMap f
  where
    f (BlockStmt _ ss) = ss
    f s                = [s ]

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
