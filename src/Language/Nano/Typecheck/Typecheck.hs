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

module Language.Nano.Typecheck.Typecheck (verifyFile, typeCheck, testFile) where 

import           Control.Applicative                ((<$>), (<*>))
import           Control.Monad                

import qualified Data.HashSet                       as HS 
import qualified Data.HashMap.Strict                as M 
import           Data.Maybe                         (catMaybes, fromMaybe, listToMaybe, fromJust)
import           Data.List                          (find)
import           Data.Generics                   
import           Data.Monoid

import           Text.PrettyPrint.HughesPJ          (text, ($+$), vcat)

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
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
-- import           Debug.Trace                        hiding (traceShow)

import qualified System.Console.CmdArgs.Verbosity as V


--------------------------------------------------------------------------------
-- | Top-level Verifier 

--------------------------------------------------------------------------------
verifyFile :: FilePath -> IO (UAnnSol a, F.FixResult Error)
--------------------------------------------------------------------------------
verifyFile f = parse f $ ssa $ tc

parse f next  = parseNanoFromFile f >>= next
ssa   next p  = ssaTransform p >>= either (lerror . single) (next . expandAliases)
tc    p       = typeCheck p    >>= either unsafe safe


--------------------------------------------------------------------------------
testFile f = parseNanoFromFile f 
         >>= ssaTransform  
         >>= either (print . pp) (\p -> typeCheck (expandAliases p)
         >>= either (print . vcat . (pp <$>)) 
                    (\(Nano {code = Src ss}) -> 
                          {- print (pp p') >> -} print "Casts:" >> print (pp $ getCasts ss)))
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
  = do setDef     $ defs p
       checkInterfaces p
       (fs', γo) <- tcInScope γ $ tcStmts γ fs
       m         <- concatMaps <$> getAllAnns
       θ         <- getSubst
       let p1     = p {code = (patchAnn m . apply θ) <$> Src fs'}
       case γo of 
         Just _  -> do  d <- getDef
                        -- Update TDefEnv before exiting
                        let p2 = p1 { defs  = d }
                        return $ (m, p2)
         Nothing -> error "BUG:tcNano should end with an environment"
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
tcEnvMem x                   = envMem x      . tce_env 
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
-- | TypeCheck Scoped Block in Environment
-------------------------------------------------------------------------------

-- XXX: What could go wrong by removing this check?
tcInScope γ act = accumAnn annCheck act
  where
    annCheck m  = [] -- catMaybes $ validInst γ <$> M.toList m

-------------------------------------------------------------------------------
-- | TypeCheck Function 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
tcFun :: PPR r =>
  TCEnv r -> Statement (AnnSSA r) -> TCM r (Statement (AnnSSA r), Maybe (TCEnv r))
-------------------------------------------------------------------------------
tcFun γ (FunctionStmt l f xs body)
  = case tcEnvFindTy f γ of
      Nothing -> die $ errorMissingSpec (srcPos l) f
      Just ft -> do body' <- foldM (tcFun1 γ l f xs) body =<< tcFunTys l f xs ft
                    return   (FunctionStmt l f xs body', Just γ) 
tcFun _  s = die $ bug (srcPos s) $ "Calling tcFun not on FunctionStatement"

-------------------------------------------------------------------------------
tcFun1 :: (PPR r, IsLocated b, IsLocated l, IsLocated a, CallSite t) 
       => TCEnv r -> a -> l -> [Id b] -> [Statement (AnnSSA r)] -> 
            (t, ([TVar], [RType r], RType r)) -> TCM r [Statement (AnnSSA r)]
-------------------------------------------------------------------------------
tcFun1 γ l f xs body (i, (αs,ts,t)) = tcInScope γ' $ tcFunBody γ' l body t
  where 
    γ'                              = envAddFun f i αs xs ts t γ 

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
    
-------------------------------------------------------------------------------
validInst :: (PP a, Free a) => TCEnv r -> (SourceSpan, a) -> Maybe Error
-------------------------------------------------------------------------------
validInst γ (l, ts)
  = case [β | β <- HS.toList $ free $ ts, not $ tVarId β `tcEnvMem` γ] of
      [] -> Nothing
      βs -> Just $ errorFreeTyVar l βs

-- | Strings ahead: HACK Alert
tVarId (TV a l) = Id l $ "TVAR$$" ++ F.symbolString a   


---------------------------------------------------------------------------------------
tcClassElt :: PPR r 
          => TCEnv r -> Id (AnnSSA r) -> ClassElt (AnnSSA r) -> TCM r (ClassElt (AnnSSA r))
---------------------------------------------------------------------------------------
-- FIXME: check for void return type for constructor
tcClassElt γ cid (Constructor l xs body) 
  = case [ t | ConsAnn t  <- ann_fact l ] of
      [  ]  -> tcError    $ errorClEltAnMissing (srcPos l) cid "constructor"
      [ft]  -> Constructor l xs <$> (foldM (tcFun1 γ l id xs) body =<< tcFunTys l id xs ft)
      _     -> error      $ "[tcClassElt] Multiple-type constructor: " ++ ppshow l
  where id   = Id l "constructor"

tcClassElt γ cid (MemberVarDecl _ s v@(VarDecl l x _))
  = case [ t | FieldAnn (_,t)  <- ann_fact l ] of 
      [ ]  -> tcError    $ errorClEltAnMissing (srcPos l) cid x
      [_]  -> MemberVarDecl l s <$> (fst <$> tcVarDecl γ v)
      _    -> error      $ "tcClassEltType:multi-type variable: " ++ ppshow l

tcClassElt γ cid (MemberMethDecl l s i xs body) = 
  case [ t | MethAnn t  <- ann_fact l ] of 
    [  ]  -> tcError    $ errorClEltAnMissing (srcPos l) cid i
    [ft]  -> MemberMethDecl l s i xs <$> (foldM (tcFun1 γ l i xs) body =<< tcFunTys l i xs ft)
    _     -> error      $ "tcClassEltType:multi-type method: " ++ ppshow l


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
  = do z             <- tcCallMatch γ l BISetProp [e1,e2] $ setPropTy (F.symbol f) l1 $ tce_env γ 
       case z of 
         ([e1',e2'], TApp (TRef _ _) [t1,t2] _) 
                     -> do  e2'' <- castM le2 (tce_ctx γ) e2' t2 t1
                            return (exp e1' e2'', Just γ)
         (e,t)       -> error $ "BUG: tcStmt - e.f = e : " ++ ppshow e ++ "\n" ++ ppshow t
    where
       le2            = getAnnotation e2
       exp ε1 ε2      = ExprStmt l $ AssignExpr l2 OpAssign (LDot l1 ε1 f) ε2

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
  = do z <- tcCallMatch γ l BITruthy [e] $ builtinOpTy l BITruthy (tce_env γ)
       case z of 
         ([e'], _) -> do (s1', γ1) <- tcStmt γ s1
                         (s2', γ2) <- tcStmt γ s2
                         γ3        <- envJoin l γ γ1 γ2
                         return       (IfStmt l e' s1' s2', γ3)
         _          -> error "BUG: tcStmt - If then else"

-- while c { b } 
tcStmt γ (WhileStmt l c b) 
  = do (c', t)   <- tcExpr γ c
       unifyTypeM (srcPos l) t tBool
       (b', γl)  <- tcStmt γ' b

       γout      <- envLoopJoin l γ γl
       return       (WhileStmt l c' b', γout)  
    where 
       xts'       = [(mkNextId x, tcEnvFindTyOrDie l x γ) | x <- phiVarsAnnot l]
       γ'         = tcEnvAdds xts' γ

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt γ (VarDeclStmt l ds)
  = do (ds', z) <- tcSeq tcVarDecl γ ds
       return      (VarDeclStmt l ds', z)

-- return e 
tcStmt γ (ReturnStmt l eo) 
  = do  (eo', t)    <- case eo of 
                         Nothing -> return (Nothing, tVoid)
                         Just e  -> mapFst Just <$>  tcExpr γ e
        let rt       = tcEnvFindReturn γ 
        θ           <- unifyTypeM (srcPos l) t rt
        -- Apply the substitution
        let (rt',t') = mapPair (apply θ) (rt,t)
        -- Subtype the arguments against the formals and cast using subtyping result
        eo''         <- case eo' of
                         Nothing -> return Nothing
                         Just e' -> Just <$> castM l (tce_ctx γ) e' t' rt'
        return        $ (ReturnStmt l eo'', Nothing)

-- throw e 
tcStmt γ (ThrowStmt l e) 
  = (,Nothing) . ThrowStmt l . fst <$> tcExpr γ e


tcStmt γ s@(FunctionStmt _ _ _ _)
  = tcFun γ s

-- class A<S...> [extends B<T...>] [implements I,J,...] { ... }
tcStmt γ c@(ClassStmt l i e is ce) = do  
    -- * Compute / get the class type 
    TD _ _ αs _ _ <- classFromStmt c
    -- * Add the type vars in the environment
    let γ'        = tcEnvAdds (tyBinds αs) γ
    -- * Compute type for "this" and add that to the env as well
    --   - This type uses the classes type variables as type parameters.
    --   - For the moment this type does not have a refinement. Maybe use
    --     invariants to add some.
    let thisT     = TApp (TRef (F.symbol i) False) (tVars αs) fTop  
    -- * Typecheck the class elements in this extended environment.
    ce'          <- tcInScope γ' $ tcWithThis thisT $ mapM (tcClassElt γ' i) ce
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
classFromStmt (ClassStmt l id _ _ cs) =
  do  δ <- getDef
      case findSym sym δ of
        Just d  -> return d   -- if already computed
        Nothing -> do let elts      = addConstr δ $ classEltType <$> cs
                      let freshD    = TD True (fmap ann id) vs p elts
                      mapM_ tcError $ safeExtends (srcPos l) δ freshD
                      setDef        $ addSym sym freshD δ
                      return        $ freshD
  where
    sym      = F.symbol id
    (vs, p)  = classAnnot l

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
classEltType :: PPR r => ClassElt (AnnSSA r) -> TElt r
---------------------------------------------------------------------------------------
classEltType (Constructor l _ _ ) = ConsSig ann
  where 
    ann = safeHead "BUG:classEltType-1" [ κ | ConsAnn κ <- ann_fact l ]

-- FIXME: Mutability ???
classEltType (MemberVarDecl _ s (VarDecl l v _)) = 
    FieldSig (F.symbol v) s undefined Nothing t
  where 
    (m,t) = safeHead "BUG:classEltType-2" [ φ | FieldAnn φ <- ann_fact l ]

-- FIXME: add "this" as first parameter
classEltType (MemberMethDecl  l s f _ _ ) = 
    FieldSig (F.symbol f) s undefined Nothing t
  where 
    t = safeHead "BUG:classEltType-3" [ μ | MethAnn μ <- ann_fact l ]


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
                       (,ta) <$> castM l (tce_ctx γ) e' (apply θ t) ta

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
      Nothing -> findClass x >>= \case  -- Static reference
        Just (TD _ s _ _ _) -> return (e, TApp (TRef (F.symbol s) True) [] fTop)
        Nothing -> tcError $ errorUnboundId (ann l) x
 
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

tcExpr γ (Cast l@(Ann loc fs) e)
  = do (e', t) <- tcExpr γ e
       case e' of
         Cast (Ann _ fs') e'' -> return (Cast (Ann loc (fs ++ fs')) e'', t)
         _                    -> return (Cast l e', t)


-- | e.m(e1,e2,...)
--
-- FIXME: method calls 
--

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

tcExpr _ e 
  = convertError "tcExpr" e


---------------------------------------------------------------------------------------
tcCall :: PPR r => TCEnv r -> ExprSSAR r -> TCM r (ExprSSAR r, RType r)
---------------------------------------------------------------------------------------

-- | `o e`
tcCall γ (PrefixExpr l o e)        
  = do z                      <- tcCallMatch γ l o [e] (prefixOpTy o $ tce_env γ) 
       case z of
         ([e'], t)            -> return (PrefixExpr l o e', t)
         _                    -> error "IMPOSSIBLE:tcCall:PrefixExpr"

-- | `e1 o e2`
tcCall γ (InfixExpr l o e1 e2)        
  = do z                      <- tcCallMatch γ l o [e1, e2] (infixOpTy o $ tce_env γ) 
       case z of
         ([e1', e2'], t)      -> return (InfixExpr l o e1' e2', t)
         _                    -> error "IMPOSSIBLE:tcCall:InfixExpr"
         
-- | `super(e1,...,en)`
-- TSC will already have checked that `super` is only called whithin the constructor.
tcCall γ (CallExpr l e@(SuperRef _)  es) 
  = do elts                   <- t_elts <$> (getSuperDefM l =<< tcPeekThis)
       go                        [ t | ConsSig t <- elts ]
  where
       go [t]                  = 
          do (es', t')        <- tcCallMatch γ l "constructor" es t
             return            $ (CallExpr l e es', t')
       go _                    = tcError $ errorConsSigMissing (srcPos l) e   
   
-- | `e(es)`
tcCall γ (CallExpr l e es)
  = do (e', ft0)              <- tcExpr γ e
       (es', t)               <- tcCallMatch γ l e es ft0
       return                  $ (CallExpr l e' es', t)

-- | `e1[e2]`
tcCall γ (BracketRef l e1 e2)
  = do z                      <- tcCallMatch γ l BIBracketRef [e1, e2] $ builtinOpTy l BIBracketRef $ tce_env γ 
       case z of
         ([e1', e2'], t)      -> return (BracketRef l e1' e2', t)
         _                    -> error "BUG: tcCall BracketRef"
   
-- | `e1[e2] = e3`
tcCall γ ex@(AssignExpr l OpAssign (LBracket l1 e1 e2) e3)
  = do z                      <- tcCallMatch γ l BIBracketAssign [e1,e2,e3] $ builtinOpTy l BIBracketAssign $ tce_env γ
       case z of
         ([e1', e2', e3'], t) -> return (AssignExpr l OpAssign (LBracket l1 e1' e2') e3', t)
         _                    -> error "BUG: tcCall AssignExpr"

-- | `[e1,...,en]`
tcCall γ ex@(ArrayLit l es)
  = do (es', t)               <- tcCallMatch γ l BIArrayLit es $ arrayLitTy l (length es) $ tce_env γ
       return                  $ (ArrayLit l es', t)

-- | `{ f1:t1,...,fn:tn }`
--
-- FIXME: track `this`
--
tcCall γ ex@(ObjectLit l bs) 
  = do (es', t)               <- tcCallMatch γ l "ObjectLit" es $ objLitTy l ps 
       return                  $ (ObjectLit l (zip ps es'), t)
  where
    (ps,es) = unzip bs

-- | `new e(e1,...,en)`
tcCall γ (NewExpr l (VarRef lv i) es) 
  = do tc                     <- getConstr (srcPos l) γ i
       when                      (not $ isTFun tc) 
                               $ tcError $ errorConstNonFunc (srcPos l) i
       (es', t)               <- tcCallMatch γ l "constructor" es tc
       return                  $ (NewExpr l (VarRef lv i) es', t)

-- | e.f 
tcCall γ (DotRef l e f)
  = do z                  <- tcMethCall l γ e [] $ getPropTy (F.symbol f) l $ tce_env γ
       case z of 
         ([e'], t)        -> return (DotRef l e' f, t)
         _                -> error "BUG: tcStmt - If then else"


tcCall _ e
  = die $ bug (srcPos e) $ "tcCall: cannot handle: " ++ ppshow e


----------------------------------------------------------------------------------
tcMethCall :: PPR r => AnnSSA r -> TCEnv r -> ExprSSAR r -> 
                       [ExprSSAR r] -> RType r -> TCM r ([ExprSSAR r], RType r)
----------------------------------------------------------------------------------
tcMethCall l γ e es tm 
  = do z                   <- runFailM $ tcExpr γ e
       case z of 
         Right (e', t)     -> tcWithThis t $ tcCallMatch γ l "tcMethCall" (e:es) tm
         Left err          -> tcError err



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
    findClass s >>= \case 
      Just t  -> getPropTDefM False l "__constructor__" t (tVar <$> t_args t) >>= \case
                   Just (TFun bs _ r) -> return $ abs (t_args t) $ TFun bs (retT t) r
                   Just _             -> error  $ "Unsupported constructor type"
                   Nothing            -> return $ abs (t_args t) $ TFun [] (retT t) fTop
      Nothing -> case tcEnvFindTy s γ of
                   Just t  -> getPropM False l "__constructor__" t >>= \case
                                Just t  -> return t
                                Nothing -> tcError $ errorConsSigMissing (srcPos l) s
                   Nothing -> tcError $ errorClassMissing (srcPos l) s
  where
    -- Constructor's return type is void - instead return the class type
    -- FIXME: type parameters in returned type: inferred ... or provided !!! 
    retT t   = TApp (TRef (F.symbol s) False) (tVar <$> t_args t) fTop
    abs [] t = t
    abs vs t = foldr TAll t vs


-- | Signature resolution

tcCallMatch γ l fn es ft0 
  = do (es', ts)      <- unzip <$> mapM (tcExpr γ) es
       z              <- resolveOverload γ l fn es' ts ft0
       case z of 
         Just (θ, ft) -> do addAnn (srcPos l) $ Overload (tce_ctx γ) ft
                            addSubst l θ
                            (es'', ot, θ') <- tcCallCase γ l fn es' ts ft
                            return          $ (es'', ot)
         Nothing      -> tcError $ errorCallNotSup (srcPos l) fn es ts 


-- When resolving an overload there are two prossible cases:
--   * There is only a single signature available: then return just this
--     signature regardless of subtyping constraints
--   * There are more than one signature available: return all that pass the
--     subtype check (this is what tcCallCaseTry does).
resolveOverload γ l fn es ts ft 
  = do δ       <- getDef
       let sigs = catMaybes (bkFun <$> getCallable δ ft)
       case [ mkFun (vs, τs,τ) | (vs, τs, τ) <- sigs, length τs == length es ] of
         [t] -> getSubst >>= return  . Just . (,t)
         fts -> do θs    <- mapM (tcCallCaseTry γ l fn ts) fts
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
     let t1     = apply θ' ts
     let t2     = apply θ' its
     zipWithM_    (subtypeM (ann l)) t1 t2
     return     $ θ'


tcCallCase γ l fn es ts ft 
  = do let ξ            = tce_ctx γ
       -- Generate fresh type parameters
       (_,ibs,ot)      <- instantiate l ξ fn ft
       let its          = b_type <$> ibs
       θ               <- unifyTypesM (srcPos l) "tcCall" ts its
       let (ts',its')   = mapPair (apply θ) (ts, its)
       es'             <- NM.zipWith3M (castM l ξ) es ts' its'
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
  mapM (sanity $ srcPos l) $ [ t | VarAnn t <- ann_fact l ] ++ [ t | FieldAnn (_,t) <- ann_fact l ]

sanity l t@(TApp (TRef i _) ts _) = 
  do δ <- getDef 
     case findSym i δ of
       Just (TD _ _ αs _ _) 
         | length αs == length ts -> return t 
       Just (TD _ n αs _ _) 
         | otherwise -> tcError $ errorTypeArgsNum l n (length αs) (length ts)
       Nothing -> error $ "BUG: Id: " ++ ppshow i ++ " was not found in env at " ++ ppshow (srcPos l) 
sanity _ t = return t


