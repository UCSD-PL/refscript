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
import           Control.Arrow                      ((***), first)

import qualified Data.HashMap.Strict                as M 
import           Data.Maybe                         (catMaybes, listToMaybe)
import           Data.Function                      (on)
import           Data.Generics                   

import           Language.Nano.Annots
import           Language.Nano.CmdLine              (getOpts, noFailCasts)
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Program
import           Language.Nano.Types
import           Language.Nano.Env
import           Language.Nano.Misc                 (convertError, zipWith3M)
import           Language.Nano.SystemUtils
import           Language.Nano.Typecheck.Unify
import           Language.Nano.Typecheck.Environment
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

-- import           Debug.Trace                        hiding (traceShow)

import qualified System.Console.CmdArgs.Verbosity as V


type PPR  r = (PP r, F.Reftable r, Data r)
type PPRSF r = (PPR r, Substitutable r (Fact r), Free (Fact r)) 


--------------------------------------------------------------------------------
-- | Top-level Verifier 

--------------------------------------------------------------------------------
verifyFile :: [FilePath] -> IO (UAnnSol a, F.FixResult Error)
--------------------------------------------------------------------------------
verifyFile fs = parse fs $ ssa $ tc

parse fs next 
  = do  r <- parseNanoFromFiles fs 
        case r of 
          Left  l -> return (NoAnn, l) 
          Right x -> next x

ssa next p  
  = do  r <- ssaTransform p
        case r of 
          Left  l -> lerror $ [l] 
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
  return $ r


-------------------------------------------------------------------------------
-- | TypeCheck Nano Program
-------------------------------------------------------------------------------
tcNano :: PPR r => NanoSSAR r -> TCM r (NanoTypeR r)
-------------------------------------------------------------------------------
tcNano p@(Nano {code = Src fs})
  = do  (fs',_)   <- tcStmts γ fs
        fs''      <- patch fs'
        return     $ p { code = Src fs'' }
    where
        γ          = initGlobalEnv p


-- | Patch annotation on the AST
--
-------------------------------------------------------------------------------
patch :: PPR r => [Statement (AnnSSA r)] -> TCM r [Statement (AnnSSA r)]
-------------------------------------------------------------------------------
patch fs = 
  do (m,θ)           <- (,) <$> getAnns <*> getSubst
     return           $ (pa m <$>) <$> apply θ <$> fs
  where
    pa m (Ann l fs)            = Ann l $ sortNub $ fs ++ filter accepted (M.lookupDefault [] l m)
    accepted (TypInst _ _    ) = True
    accepted (Overload _ _   ) = True
    accepted (EltOverload _ _) = True
    accepted _                 = False


-------------------------------------------------------------------------------
-- | Initialize environment
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
initGlobalEnv  :: PPR r => NanoSSAR r -> TCEnv r
-------------------------------------------------------------------------------
initGlobalEnv (Nano { code = Src ss }) = TCE nms mod ctx pth Nothing
  where
    nms       = envFromList $ visibleNames ss
    mod       = scrapeModules ss 
    ctx       = emptyContext
    pth       = AP $ QPath (srcPos dummySpan) []


initFuncEnv γ f i αs xs ts t args s = TCE nms mod ctx pth parent
  where
    tyBinds   = [(tVarId α, (tVar α, ReadOnly)) | α <- αs] ++
                [(tVarId α, (tVar α, ImportDecl)) | α <- αs]
    varBinds  = zip (fmap ann <$> xs) $ zip ts (repeat WriteLocal)
    nms       = envAddReturn f (t, ReadOnly) 
              $ envAdds (tyBinds ++ varBinds ++ args ++ visibleNames s) 
              $ envEmpty
    mod       = tce_mod γ
    ctx       = pushContext i (tce_ctx γ) 
    pth       = tce_path γ
    parent    = Just γ


---------------------------------------------------------------------------------------
initModuleEnv :: (PPR r, F.Symbolic n, PP n) => TCEnv r -> n -> [Statement (AnnSSA r)] -> TCEnv r
---------------------------------------------------------------------------------------
initModuleEnv γ n s = TCE nms mod ctx pth parent 
  where
    nms       = envFromList $ visibleNames s
    mod       = tce_mod γ
    ctx       = emptyContext
    pth       = extendAbsPath (tce_path γ) n
    parent    = Just γ


-------------------------------------------------------------------------------
-- | Environment wrappers
-------------------------------------------------------------------------------

tcEnvAdds     xs γ    = γ { tce_names = envAdds xs $ tce_names γ }

tcEnvAdd      x t γ   = γ { tce_names = envAdd x t $ tce_names γ }

-- tcEnvFindTy :: (F.Symbolic x) => x -> TCEnv r -> Maybe (RType r)
tcEnvFindTy x γ       = fst <$> tcEnvFindTyWithAgsn x γ 


-- tcEnvFindTyWithAgsn :: (F.Symbolic x) => x -> TCEnv r -> Maybe (RType r, Assignability)
tcEnvFindTyWithAgsn x γ = case -- trace ("looking for " ++ ppshow x ++ " in " ++ ppshow (envKeys (tce_names γ))) i $
                                  envFindTy x $ tce_names γ of 
                            Just t -> Just t
                            Nothing     -> 
                              case tce_parent γ of 
                                Just γ' -> tcEnvFindTyWithAgsn x γ'
                                Nothing -> Nothing


safeTcEnvFindTy l γ x = case tcEnvFindTy x γ of
                          Just t  -> return t
                          Nothing -> tcError $ bugEnvFindTy (srcPos l) x 

tcEnvFindReturn       = fst . envFindReturn . tce_names

tcEnvFindTypeDefM l γ x 
  = case resolveRelNameInEnv γ x of 
      Just t  -> return t
      Nothing -> tcError $ bugClassDefNotFound (srcPos l) x



-------------------------------------------------------------------------------
-- | TypeCheck Function 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
tcFun :: PPR r =>
  TCEnv r -> Statement (AnnSSA r) -> TCM r (Statement (AnnSSA r), Maybe (TCEnv r))
-------------------------------------------------------------------------------
tcFun γ (FunctionStmt l f xs body)
  = case tcEnvFindTy f γ of
      Just ft       -> do ts    <- tcFunTys l f xs ft
                          body' <- foldM (tcFun1 γ l f xs) body ts
                          return $ (FunctionStmt l f xs body', Just γ) 
      Nothing       -> die $ errorMissingSpec (srcPos l) f

tcFun _  s = die $ bug (srcPos s) $ "Calling tcFun not on FunctionStatement"

-------------------------------------------------------------------------------
tcFun1 :: (PPR r, IsLocated l, CallSite t) 
       => TCEnv r -> (AnnSSA r) -> l -> [Id (AnnSSA r)] -> [Statement (AnnSSA r)] 
       -> (t, ([TVar], [RType r], RType r)) -> TCM r [Statement (AnnSSA r)]
-------------------------------------------------------------------------------
tcFun1 γ l f xs body fty = tcFunBody γ' l body t
  where
    γ' 					         = initFuncEnv γ f i αs xs ts t arg body
    (i, (αs,ts,t))       = fty
    arg                  = [(argId $ srcPos l, (aTy, ReadOnly))]
    aTy                  = argTy l ts $ tce_names γ 

tcMethSingleSig γ l f xs body (i, _,ft) 
  = tcFun1 γ l f xs body (i, ft)


-- FIXME: Check for mutability (the second part in the triplet)
--        If this argument is "immutable" We will have to check
--        statements/expressions that operate on "this" and make
--        sure that they do not mutate it.
--
--        For the moment it just does a regular function check
--        
-- tcMeth1 γ l f xs body (i, _,ft) = tcFun1 γ l f xs body (i, ft)

tcFunBody γ l body t 
  = do  z                          <- tcStmts γ body
        case z of 
          (_, Just _) | t /= tVoid -> tcError $ errorMissingReturn (srcPos l)
          (b, _     ) | otherwise  -> return b


-- | Strings ahead: HACK Alert
tVarId (TV a l) = Id l $ "TVAR$$" ++ F.symbolString a   


---------------------------------------------------------------------------------------
tcClassElt :: PPR r 
          => TCEnv r -> Id (AnnSSA r) -> ClassElt (AnnSSA r) -> TCM r (ClassElt (AnnSSA r))
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
                Just e     -> do ([e'],_)  <- tcNormalCall γ l1 "field init" [(e, Nothing)] $ ft fs
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
tcStmts :: PPR r => 
  TCEnv r -> [Statement (AnnSSA r)] -> TCM r ([Statement (AnnSSA r)], TCEnvO r)
--------------------------------------------------------------------------------
tcStmts = tcSeq tcStmt 


-------------------------------------------------------------------------------
tcStmt  :: PPR r =>
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
  = do z               <- runFailM $ tcExpr γ e1 Nothing
       case z of 
         Right (_,te)  -> case getProp γ (F.symbol f) te of
                            Just (_, tf) -> tcSetProp $ Just tf
                            Nothing      -> tcSetProp $ Nothing
         Left _        -> tcSetProp $ Nothing
  where
    tcSetProp rhsCtx = 
      do  opTy          <- setPropTy l (F.symbol f) <$> safeTcEnvFindTy l γ (builtinOpId BISetProp)
          ([e1',e2'],_) <- tcNormalCall γ l BISetProp [(e1, Nothing), (e2, rhsCtx)] opTy
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
         Just _  -> do (s1', γ1) <- tcStmt γ s1
                       (s2', γ2) <- tcStmt γ s2
                       γ3        <- envJoin l γ γ1 γ2
                       return       (IfStmt l e' s1' s2', γ3)
         _       -> return (IfStmt l e' s1 s2, Nothing)

-- while c { b } 
tcStmt γ (WhileStmt l c b) 
  = do (c', to)  <- tcExprW γ c
       case to of
         Nothing -> return (WhileStmt l c' b, Nothing)
         Just t  -> do unifyTypeM (srcPos l) γ t tBool
                       pTys         <- mapM (safeTcEnvFindTy l γ) phis
                       (b', γl)     <- tcStmt (tcEnvAdds (zip xs (zip pTys (repeat WriteLocal))) γ) b
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
--
tcStmt γ (ClassStmt l x e is ce) 
  = do  dfn      <- tcEnvFindTypeDefM l γ rn
        case safeExtends (srcPos l) γ dfn of 
          Nothing -> (, Just γ) . ClassStmt l x e is <$> mapM (tcClassElt (newEnv $ t_args dfn) x) ce
          Just e  -> tcError e
  where
    tVars   αs    = [ tVar   α | α <- αs ] 
    tyBinds αs    = [(tVarId α, (tVar α, WriteGlobal)) | α <- αs]
    rn            = RN $ QName (srcPos l) [] (F.symbol x)
    mkThis αs     = TApp (TRef rn) (tVars αs) fTop
    newEnv αs     = tcEnvAdd (F.symbol "this") (mkThis αs, WriteGlobal) $ tcEnvAdds (tyBinds αs) γ

-- | module M { ... } 
--
tcStmt γ (ModuleStmt l n body) 
  = (ModuleStmt l n *** return (Just γ)) <$>  tcStmts (initModuleEnv γ n body) body

-- OTHER (Not handled)
tcStmt _ s 
  = convertError "tcStmt" s



-- Variable declarations should have the type annotations available locally
---------------------------------------------------------------------------------------
tcVarDecl ::  PPR r 
          => TCEnv r -> VarDecl (AnnSSA r) -> TCM r (VarDecl (AnnSSA r), TCEnvO r)
---------------------------------------------------------------------------------------
tcVarDecl γ v@(VarDecl l x (Just e)) =
  do  (e', to)   <- tcExprTW l γ e ann
      return      $ (VarDecl l x (Just e'), tcEnvAddo γ x $ (,asgn ann) <$> to)
  where
    ann           = listToMaybe $ scrapeVarDecl v
    asgn (Just _) = WriteGlobal
    asgn _        = WriteLocal

tcVarDecl γ v@(VarDecl l x Nothing) = 
  case scrapeVarDecl v of
    [t]          -> return (v, Just $ tcEnvAdds [(x, (t, WriteGlobal))] γ)
    _            -> tcError $ errorVarDeclAnnot (srcPos l) x

-------------------------------------------------------------------------------
tcAsgn :: PPR r 
       => AnnSSA r -> TCEnv r -> Id (AnnSSA r) -> ExprSSAR r -> TCM r (ExprSSAR r, TCEnvO r)
-------------------------------------------------------------------------------
tcAsgn l γ x e
  = do (e' , to)    <- tcExprTW l γ e rhsT
       return       $ (e', tcEnvAddo γ x $ (,asgn) <$> to)
    where
       (rhsT, asgn) = case tcEnvFindTyWithAgsn x γ of
                        Just (t, a) -> (Just t, a)
                        Nothing     -> (Nothing, WriteLocal)

tcEnvAddo _ _ Nothing  = Nothing
tcEnvAddo γ x (Just t) = Just $ tcEnvAdds [(x, t)] γ 

  
----------------------------------------------------------------------------------------------------------------
tcExprTW :: PPR r => AnnSSA r -> TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, Maybe (RType r))
tcExprW :: PPR r => TCEnv r -> ExprSSAR r -> TCM r (ExprSSAR r, Maybe (RType r))
----------------------------------------------------------------------------------------------------------------
tcExprTW l γ e to        = (tcWrap $ tcExprT l γ e to)   >>= tcEW γ e
tcExprW γ e              = (tcWrap $ tcExpr γ e Nothing) >>= tcEW γ e 
tcNormalCallW γ l o es t = (tcWrap $ tcNormalCall γ l o ((,Nothing) <$> es) t) >>= \case
                             Right (es', t') -> return (es', Just t')
                             Left err        -> do es' <- mapM (deadcastM (tce_ctx γ) err) es
                                                   return (es', Nothing) 


tcRetW γ l (Just e)
  = (tcWrap $ tcNormalCall γ l "return" [(e, Nothing)] (returnTy (tcEnvFindReturn γ) True)) >>= \case
       Right (es', _) -> return  (ReturnStmt l (Just $ head es'), Nothing)  
       Left err       -> (\e' -> (ReturnStmt l (Just e'), Nothing)) <$> deadcastM (tce_ctx γ) err e

tcRetW γ l Nothing
  = do ([], _) <- tcNormalCall γ l "return" [] $ returnTy (tcEnvFindReturn γ) False
       return $ (ReturnStmt l Nothing, Nothing)
  
tcEW :: PPR r => TCEnv r -> ExprSSAR r -> Either Error ((ExprSSAR r), b) -> TCM r ((ExprSSAR r), Maybe b)
tcEW _ _ (Right (e', t')) = return $  (e', Just t')
tcEW γ e (Left err)       = (, Nothing) <$> deadcastM (tce_ctx γ) err e 

-------------------------------------------------------------------------------
tcExprT :: PPR r 
        => AnnSSA r -> TCEnv r -> ExprSSAR r -> Maybe (RType r) 
        -> TCM r (ExprSSAR r, RType r)
-------------------------------------------------------------------------------
tcExprT l γ e to 
  = do (e', t)   <- tcExpr γ e Nothing
       case to of
         Nothing -> return (e', t)
         Just ta -> do θ <- unifyTypeM (srcPos l) γ t ta
                       (,ta) <$> castM γ e' (apply θ t) ta

-------------------------------------------------------------------------------
tcExpr :: PPR r => TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
-------------------------------------------------------------------------------
tcExpr _ e@(IntLit _ _) _
  = return (e, tInt)

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
  = case tcEnvFindTy x γ of
      Just t  -> return (e, t)
      Nothing -> tcError $ errorUnboundId (ann l) x
 
tcExpr γ e@(CondExpr _ _ _ _) _
  = tcCall γ e 

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
tcExpr γ ex@(Cast l@(Ann loc fs) e) _
  = do  (e', t)             <- tcExpr γ e Nothing
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
                             | otherwise             = TCast (tce_ctx γ) $ CDead (errorDeadCast (srcPos l) t0 t1) t1
    replace _  a                                     = a
    
 
-- | e.f
tcExpr γ e@(DotRef _ _ _) _
  = tcCall γ e
 
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
  = case tcEnvFindTy (F.symbol "this") γ of
      Just t   -> case extractParent γ t  of 
                    Just tp -> return (e, tp)
                    Nothing -> tcError $ errorSuper (ann l)
      Nothing  -> tcError $ errorSuper (ann l)

-- | function (x,..) {  }
tcExpr γ (FuncExpr l fo xs body) tCtxO
  = case tCtxO of
      Just tCtx -> tcFuncExpr tCtx
      Nothing   -> case anns of 
                     [ft] -> tcFuncExpr ft
                     _    -> tcError $ errorNoFuncAnn $ srcPos l
  where
    tcFuncExpr t = do ts    <- tcFunTys l f xs t
                      body' <- foldM (tcFun1 γ l f xs) body ts
                      return $ (FuncExpr l fo xs body', t)
    anns         = [ t | FuncAnn t <- ann_fact l ]
    f            = maybe (F.symbol "<anonymous>") F.symbol fo

tcExpr _ e _ 
  = convertError "tcExpr" e


---------------------------------------------------------------------------------------
tcCall :: PPR r => TCEnv r -> ExprSSAR r -> TCM r (ExprSSAR r, RType r)
---------------------------------------------------------------------------------------

-- | `o e`
tcCall γ (PrefixExpr l o e)        
  = do opTy                   <- safeTcEnvFindTy l γ (prefixOpId o)
       z                      <- tcNormalCall γ l o [(e, Nothing)] opTy
       case z of
         ([e'], t)            -> return (PrefixExpr l o e', t)
         _                    -> tcError $ impossible (srcPos l) "tcCall PrefixExpr"

-- | `e1 o e2`
tcCall γ (InfixExpr l o@OpInstanceof e1 e2) 
  = do (e2',t)                <- tcExpr γ e2 Nothing
       case t of
         TClass (RN (QName _ _ x)) -> 
              do  opTy         <- safeTcEnvFindTy l γ (infixOpId o)
                  ([e1',_], t) <- tcNormalCall γ l o ((,Nothing) <$> [e1, StringLit l2 (F.symbolString x)]) opTy
                  -- FIXME: add qualified name
                  return (InfixExpr l o e1' e2', t) 
         _  -> tcError $ unimplemented (srcPos l) "tcCall-instanceof" $ ppshow e2
  where
    l2 = getAnnotation e2

-- | e ? e1 : e2
tcCall γ (CondExpr l e e1 e2)
  = do opTy                   <- safeTcEnvFindTy l γ (builtinOpId BICondExpr)
       z                      <- tcNormalCall γ l BICondExpr ((,Nothing) <$> [e,e1,e2]) opTy
       case z of
         ([e',e1',e2'], t)    -> return (CondExpr l e' e1' e2', t)
         _                    -> tcError $ impossible (srcPos l) "tcCall CondExpr"

tcCall γ (InfixExpr l o e1 e2)        
  = do opTy                   <- safeTcEnvFindTy l γ (infixOpId o)
       z                      <- tcNormalCall γ l o [(e1, Nothing), (e2, Nothing)] opTy
       case z of
         ([e1', e2'], t)      -> return (InfixExpr l o e1' e2', t)
         _                    -> tcError $ impossible (srcPos l) "tcCall InfixExpr"


-- | `e1[e2]`
tcCall γ (BracketRef l e1 e2)
  = do opTy                   <- safeTcEnvFindTy l γ (builtinOpId BIBracketRef)
       z                      <- tcNormalCall γ l BIBracketRef [(e1, Nothing), (e2, Nothing)] opTy
       case z of
         ([e1', e2'], t)      -> return (BracketRef l e1' e2', t)
         _                    -> tcError $ impossible (srcPos l) "tcCall BracketRef"
   
-- | `e1[e2] = e3`
tcCall γ (AssignExpr l OpAssign (LBracket l1 e1 e2) e3)
  = do opTy                   <- safeTcEnvFindTy l γ (builtinOpId BIBracketAssign)
       z                      <- tcNormalCall γ l BIBracketAssign ((,Nothing) <$> [e1,e2,e3]) opTy
       case z of
         ([e1', e2', e3'], t) -> return (AssignExpr l OpAssign (LBracket l1 e1' e2') e3', t)
         _                    -> tcError $ impossible (srcPos l) "tcCall AssignExpr"

-- | `[e1,...,en]`
tcCall γ (ArrayLit l es)
  = do opTy                   <- arrayLitTy l (length es) <$> safeTcEnvFindTy l γ (builtinOpId BIArrayLit)
       (es', t)               <- tcNormalCall γ l BIArrayLit ((,Nothing) <$> es) opTy
       return                  $ (ArrayLit l es', t)

-- | `{ f1:t1,...,fn:tn }`
tcCall γ (ObjectLit l bs) 
  = do (es', t)               <- tcNormalCall γ l "ObjectLit" ((,Nothing) <$> es) $ objLitTy l ps 
       return                  $ (ObjectLit l (zip ps es'), t)
  where
    (ps,es) = unzip bs

-- | `new e(e1,...,en)`
tcCall γ (NewExpr l e es) 
  = do (e',t)                 <- tcExpr γ e Nothing
       case extractCtor γ t of 
         Just ct -> 
            do (es', t)       <- tcNormalCall γ l "constructor" ((,Nothing) <$> es) ct
               return          $ (NewExpr l e' es', t)
         _       -> tcError    $ errorConstrMissing (srcPos l) t

-- | e.f 
--   
--   Accessing field @f@ of an expression @e@ with type @te@, causes an implicit
--   coercion of @e@ to a type @te'@ (this type is a subtype of @te@ and
--   accounts for the case where @te@ is a union type where not all parts of the 
--   union can be accessed successfully at offset @f@. This is captured by the
--   call to `mkTy te' t`, that produces an accessor function with type:
--
--      getProp_f :: (this: te') => t
--
--   The coercion occurs when calling `getProp_f` with @e@ as argument.
--
tcCall γ ef@(DotRef l e f)
  = do z                      <- runFailM $ tcExpr γ e Nothing
       case z of
         Right (_, te)        -> 
            -- case tracePP ("Getting prop " ++ ppshow f ++ " from " ++ 
            --             ppshow e ++ " :: " ++ ppshow te) $ getProp γ (F.symbol f) te of
            case getProp γ (F.symbol f) te of
              Just (te', t)   ->
                  do ([e'],τ) <- tcNormalCall γ l ef [(e, Nothing)] $ mkTy te' t
                     return    $ (DotRef l e' f, τ)
              Nothing         -> tcError $ errorMissingFld (srcPos l) f te
         Left err     -> tcError err
  where
    mkTy s t                   = mkFun ([], [B (F.symbol "this") s], t) 

         
-- | `super(e1,...,en)`
tcCall γ (CallExpr l e@(SuperRef _)  es) 
  = case tcEnvFindTy (F.symbol "this") γ of 
      Just t -> 
          case extractParent γ t of 
            Just (TApp (TRef x) _ _) -> 
                case extractCtor γ (TClass x) of
                  Just ct -> first (CallExpr l e) <$> tcNormalCall γ l "constructor" ((,Nothing) <$> es) ct
                  _       -> tcError $ errorUnboundId (ann l) "super"
            _ -> tcError $ errorUnboundId (ann l) "super"
      Nothing -> tcError $ errorUnboundId (ann l) "this"

   
-- | `e.f(es)`
tcCall γ (CallExpr l em@(DotRef l1 e f) es)
  = do z              <- runFailM $ tcExpr γ e Nothing
       case z of 
         Right (_, TModule r) -> 
             case {- tracePP (ppshow (srcPos l) ++ "\n" ++
                           ppshow e ++ " :: " ++ ppshow r ++ "\n" ++
                           "Looking for name " ++ ppshow f ++ " in module " ++ ppshow r ++ 
                           " in abspath " ++ ppshow (tce_path γ)) $ -}
                    resolveRelPathInEnv γ r of
               Just m -> case envFindTy f $ m_variables m of
                           Just ft -> do (e' , _ ) <- tcExpr γ e Nothing
                                         (es', t') <- tcNormalCall γ l em ((,Nothing) <$> es) $ thd3 ft
                                         return (CallExpr l (DotRef l1 e' f) es', t')
                           Nothing -> tcError $ errorModuleExport (srcPos l) r f 
               Nothing -> tcError $ bugMissingModule (srcPos l) r 
         Right (_, t) -> do (em', es', t) <- tcCallDotRef γ (getElt γ f t) l em es
                            return         $ (CallExpr l em' es', t)
         Left err     -> tcError err

-- | `e(es)`
tcCall γ (CallExpr l e es)
  = do (e', ft0)              <- tcExpr γ e Nothing
       (es', t)               <- tcNormalCall γ l e ((,Nothing) <$> es) ft0
       return                  $ (CallExpr l e' es', t)

tcCall _ e = tcError $ unimplemented (srcPos e) "tcCall" e


tcCallDotRef γ elts l em@(DotRef l1 e f) es 
    -- Static call
    | all isStaticSig elts
    = do  (e' , _ )   <- tcExpr γ e Nothing
          (es', t')   <- tcNormalCall γ l em ((,Nothing) <$> es) $ ft isStaticSig
          return       $ (DotRef l1 e' f, es', t')

    -- Virtual method call
    | all isMethodSig elts 
    = do  (e':es', t) <- tcNormalCall γ l em ((,Nothing) <$> (e:es)) $ ft isMethodSig
          return       $ (DotRef l1 e' f, es', t)

    -- Normal function call
    | all isFieldSig elts
    = do  (e' , _ )   <- tcExpr γ e Nothing 
          (es', t')   <- tcNormalCall γ l em ((,Nothing) <$> es) $ ft isFieldSig
          return       $ (DotRef l1 e' f, es', t')

    | otherwise
    = tcError $ unsupportedDotRef (srcPos l) em
  where
    ft f = mkAnd $ catMaybes $ mkEltFunTy <$> filter f elts

tcCallDotRef _ _ _ _ _ = error "tcCallDotRef-unsupported"


------------------------------------------------------------------------------------------
-- | @tcNormalCall@ resolves overloads and returns cast-wrapped versions of the arguments.
------------------------------------------------------------------------------------------
tcNormalCall ::  (PPRSF r, PP a) 
             => TCEnv r -> AnnSSA r -> a -> 
                [(ExprSSAR r, Maybe (RType r))] -> 
                RType r -> TCM r ([ExprSSAR r], RType r) 
------------------------------------------------------------------------------------------
tcNormalCall γ l fn ets ft0 
  = do (es', ts)      <- unzip <$> mapM (\(e,t) -> tcExpr γ e t) ets
       z              <- resolveOverload γ l fn es' ts ft0
       case z of 
         Just (θ, ft) -> do addAnn (srcPos l) $ Overload (tce_ctx γ) ft
                            addSubst l θ
                            (es'', ot, _ ) <- tcCallCase γ l fn es' ts ft
                            return          $ (es'', ot)
         Nothing      -> tcError $ errorCallNotSup (srcPos l) fn (fst <$> ets) ts 


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
resolveOverload γ l fn es ts ft 
  = case [ mkFun (vs, τs,τ) | (vs, τs, τ) <- sigs, length τs == length es ] of
      [t]    -> Just . (,t) <$> getSubst 
      fts    -> do  θs    <- mapM (\t -> tcCallCaseTry γ l fn ts t) fts
                    return $ listToMaybe [ (θ, apply θ t) | (t, Just θ) <- zip fts θs ]
  where
    sigs  = catMaybes (bkFun <$> extractCall γ ft)


----------------------------------------------------------------------------------
-- | A successful pairing of formal to actual parameters will return `Just θ`,
--   where θ is the corresponding substitution. If the types are not
--   acceptable this will return `Nothing`.
--   In this case successful means:
--    * Unifying without errors.
--    * Passing the subtyping test.
--
--   The monad state is completely reversed after this function returns, thanks to
--   `runMaybeM`. We don't need to reverse the action of `instantiate`.
----------------------------------------------------------------------------------
tcCallCaseTry :: (PPR r, PP a) 
              => TCEnv r -> Annot b SourceSpan -> a -> [RType r] -> RType r 
              -> TCM r (Maybe (RSubst r))
----------------------------------------------------------------------------------
tcCallCaseTry γ l fn ts ft 
  = runMaybeM $ 
      do  (_,ibs,_) <- instantiate l (tce_ctx γ) fn ft
          let its    = b_type <$> ibs
          θ'        <- unifyTypesM (ann l) γ ts its
          zipWithM_    (subtypeM (ann l) γ) (apply θ' ts) (apply θ' its)
          return     $ θ'


tcCallCase γ l fn es ts ft  
  = do let ξ            = tce_ctx γ
       -- Generate fresh type parameters
       (_,ibs,ot)      <- instantiate l ξ fn ft
       let its          = b_type <$> ibs
       θ               <- unifyTypesM (srcPos l) γ ts its
       let (ts',its')   = mapPair (apply θ) (ts, its)
       es'             <- zipWith3M (castM γ) es ts' its'
       return             (es', apply θ ot, θ)

instantiate l ξ fn ft 
  = do t'              <- freshTyArgs (srcPos l) ξ αs t 
       maybe err return $ bkFun t'
    where
       (αs, t)          = bkAll ft
       err = tcError    $ errorNonFunction (ann l) fn ft

----------------------------------------------------------------------------------
envJoin :: PPR r => (AnnSSA r) -> TCEnv r -> TCEnvO r -> TCEnvO r -> TCM r (TCEnvO r)
----------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l γ (Just γ1) (Just γ2) = 
  do  tas   <- mapM (getPhiType l γ1 γ2) xs
      θ     <- getSubst
      return $ Just $ tcEnvAdds (zip xs tas) $ γ { tce_names = apply θ (tce_names γ) }
  where
      xs = phiVarsAnnot l


----------------------------------------------------------------------------------
envLoopJoin :: PPR r => (AnnSSA r) -> TCEnv r -> TCEnvO r -> TCM r (TCEnvO r)
----------------------------------------------------------------------------------
envLoopJoin _ γ Nothing   = return $ Just γ
envLoopJoin l γ (Just γl) = 
  do  ts    <- mapM (getLoopNextPhiType l γ γl) xs
      γ'    <- (`substNames` γ) <$> getSubst
      return $ Just $ tcEnvAdds (zip xs (zip ts (repeat WriteLocal))) $ γ'
  where 
      xs             = phiVarsAnnot l 
      substNames θ γ = γ { tce_names = apply θ (tce_names γ) }
 

----------------------------------------------------------------------------------
getPhiType :: PPR r => (AnnSSA r) -> TCEnv r -> TCEnv r -> Id SourceSpan -> TCM r (RType r, Assignability)
----------------------------------------------------------------------------------
getPhiType l γ1 γ2 x =
  case (tcEnvFindTyWithAgsn x γ1, tcEnvFindTyWithAgsn x γ2) of
    (Just (t1,a1), Just (t2,_ )) -> unif t1 a1 t2 
    (_      , _      )           |  forceCheck x γ1 && forceCheck x γ2 
                                 -> tcError $ bug loc "Oh no, the HashMap GREMLIN is back..."
                                 |  otherwise -> tcError $ bugUnboundPhiVar loc x
  where 
    loc = srcPos $ ann l
    unif t1 a1 t2  = 
      do  θ <- getSubst 
          case unifys (srcPos l) γ1 θ [t1] [t2] of
            Left  _ -> tcError $ errorEnvJoin (ann l) x t1 t2
            Right θ' | on (==) (toType . apply θ') t1 t2 -> setSubst θ' >> return (t1,a1)
                     | otherwise ->  tcError $ errorEnvJoin (ann l) x t1 t2


----------------------------------------------------------------------------------
getLoopNextPhiType :: PPR r => (AnnSSA r) -> TCEnv r -> TCEnv r -> Id SourceSpan-> TCM r (RType r)
----------------------------------------------------------------------------------
getLoopNextPhiType l γ γl x =
  case (tcEnvFindTy x γ, tcEnvFindTy (mkNextId x) γl) of
    (Just t1, Just t2) -> unif t1 t2 
    _                  -> tcError $ bugUnboundPhiVar loc x
  where 
    loc = srcPos $ ann l
    unif t1 t2  = 
      do  θ <- getSubst 
          case unifys (srcPos l) γ θ [t1] [t2] of
            Left  _ -> tcError $ errorEnvJoin (ann l) x t1 t2
            Right θ' | on (==) (toType . apply θ') t1 t2 -> setSubst θ' >> return t1
                     | otherwise ->  tcError $ errorEnvJoin (ann l) x t1 t2


forceCheck x γ = elem x $ fst <$> envToList (tce_names γ)


-- sanity l t@(TApp (TRef i) ts _) 
--   = do  δ       <- getDef 
--         case findSym i δ of
--           Just (ID _ _ αs _ _) | length αs == length ts -> return  $ t 
--           Just (ID _ n αs _ _) | otherwise              -> tcError $ errorTypeArgsNum l n (length αs) (length ts)
--           Nothing                                       -> error   $ "BUG: Id: " ++ ppshow i 
--                                                                   ++ " was not found in env at " 
--                                                                   ++ ppshow (srcPos l) 
-- sanity _ t = return t

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
