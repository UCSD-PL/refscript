{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}

module Language.Rsc.SSA.SSA (ssaTransform) where

import           Control.Monad
import           Data.Default
import           Data.Function                (on)
import qualified Data.HashSet                 as S
import qualified Data.IntMap.Strict           as IM
import           Data.List                    (sortBy)
import           Data.Maybe                   (catMaybes)
import qualified Data.Traversable             as T
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.EitherIO
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Misc
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.SSA.SSAMonad
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Language.Rsc.Visitor

-- import           Debug.Trace                        hiding (traceShow)

----------------------------------------------------------------------------------
ssaTransform
  :: PPR r => BareRsc r -> ClassHierarchy r -> EitherIO FError (SsaRsc r)
----------------------------------------------------------------------------------
ssaTransform p cha = EitherIO $ do
    startPhase Loud "SSA"
    return (execute p (ssaRsc cha p))


-- | `ssaRsc` Perfroms SSA transformation of the input program. The output
--   program is patched (annotated per AST) with information about:
--
--   * SSA-phi nodes
--   * Spec annotations (functions, global variable declarations)
--   * Type annotations (variable declarations (?), class elements)
--
----------------------------------------------------------------------------------
ssaRsc :: PPR r => ClassHierarchy r -> BareRsc r -> SSAM r (SsaRsc r)
----------------------------------------------------------------------------------
ssaRsc cha p@(Rsc { code = Src fs })
  = do  setMeas   $ S.fromList $ F.symbol <$> envIds (consts p)
        (_,fs1)  <- ssaStmts g fs
        ssaAnns  <- getAnns
        -- Replace the annotations to the respective nodes
        let fs2 = (patch ssaAnns <$>) <$> fs1
        -- Make sure NodeIds are unique!
        fs3      <- reassignIds fs2
        -- Update code and counter
        ast_cnt  <- getCounter
        return    $ p { code  = Src fs3
                      , maxId = ast_cnt }
    where
      g = initGlobSsaEnv fs cha
      patch ms    (FA i l fs) = FA i l (fs ++ IM.findWithDefault [] i ms)
      stepRecount (FA _ l fs) = tick >>= \n -> return (FA n l fs)
      reassignIds             = T.mapM $ T.mapM stepRecount


-------------------------------------------------------------------------------------
ssaStmts :: PPR r => SsaEnv r -> [Statement (AnnSSA r)]
                  -> SSAM r (Maybe (SsaEnv r), [Statement (AnnSSA r)])
-------------------------------------------------------------------------------------
ssaStmts g = fmap (mapSnd flattenBlock) . ssaSeqOpt ssaStmt g


-------------------------------------------------------------------------------------
ssaFun :: PPR r => SsaEnv r -> [Var r] -> [Statement (AnnSSA r)]
                -> SSAM r [Statement (AnnSSA r)]
-------------------------------------------------------------------------------------
ssaFun g xs body = snd <$> ssaStmts g' body
  where
    g' = initCallableSsaEnv g xs body


-------------------------------------------------------------------------------------
ssaSeqOpt :: (g -> a -> SSAM r (Maybe g, a)) -> g -> [a] -> SSAM r (Maybe g, [a])
-------------------------------------------------------------------------------------
ssaSeqOpt f g          = go . (Just g,)
  where
    go (Nothing, zs  ) = return (Nothing, zs)
    go (gOpt   , []  ) = return (gOpt, [])
    go (Just g , x:xs) = do (g1 , y)  <- f g x
                            (g2, ys)  <- go (g1, xs)
                            return       (g2, y:ys)

-----------------------------------------------------------------------------------
ssaStmt :: PPR r
  => SsaEnv r -> Statement (AnnSSA r) -> SSAM r (Maybe (SsaEnv r), Statement (AnnSSA r))
-----------------------------------------------------------------------------------
-- skip
ssaStmt g s@(EmptyStmt _)
  = return (Just g, s)

-- interface IA<V> extends IB<T> { ... }
ssaStmt g s@(InterfaceStmt _ _)
  = return (Just g, s)

-- x = e
ssaStmt g (ExprStmt l1 (AssignExpr l2 OpAssign (LVar l3 v) e)) = do
    let x            = Id l3 v
    (g', s, x', e') <- ssaAsgn g l2 x e
    return             (Just g', prefixStmt l1 s (ssaAsgnStmt l1 l2 x x' e'))

-- e
ssaStmt g (ExprStmt l e) = do
    (g', s, e') <- ssaExpr g e
    return (Just g', prefixStmt l s $ ExprStmt l e')

-- s1;s2;...;sn
ssaStmt g (BlockStmt l stmts) = do
    (b, stmts') <- ssaStmts g stmts
    return (b,  maybeBlock l $ flattenBlock stmts')

-- if b { s1 }
ssaStmt g (IfSingleStmt l b s)
  = ssaStmt g (IfStmt l b s (EmptyStmt l))

-- if (e1 || e2) { s1 } else { s2 }
ssaStmt g (IfStmt l (InfixExpr _ OpLOr e1 e2) s1 s2)
  = ssaExpandIfStmtInfixOr l e1 e2 s1 s2 >>= ssaStmt g

-- if (e1 && e2) { s1 } else { s2 }
ssaStmt g (IfStmt l (InfixExpr _ OpLAnd e1 e2) s1 s2)
  = ssaExpandIfStmtInfixAnd l e1 e2 s1 s2 >>= ssaStmt g

-- if b { s1 } else { s2 }
ssaStmt g (IfStmt l e s1 s2)
  = do  (ge, se, e')   <- ssaExpr g e
        (go1, s1')     <- ssaStmt ge s1
        (go2, s2')     <- ssaStmt ge s2
        (go, ss1, ss2) <- envJoin l g go1 go2
        let ifStmt      = IfStmt l e' (splice s1' ss1) (splice s2' ss2)
        let stmt'       = prefixStmt l se ifStmt
        return            (go, stmt')

--
--   while (i <- f(i) ; cond(i)) { <BODY> }
--
--   ===>
--
--   i = f(i); while (cond(i)) { <BODY>; i = f(i); }
--
--   XXX: The SSA version of the Φ-var that gets propagated is the
--        one at the end of the loop body.
--
ssaStmt g (WhileStmt l cnd body) = do
    (gc, sc, cnd')  <- ssaExpr g cnd
    -- Do not allow updates in the conditional
    unless (null sc) (ssaError $ errorUpdateInExpr (srcPos l) cnd)
    (gNopt, body')  <- ssaStmt gc body
    case gNopt of
      Just gN -> do
          let (_, δ, _) = envProgress (mSSA g) (mSSA gN)
          mapM_ (addAnn l . PhiLoop) (envValues δ)
          return (Just gN, WhileStmt l cnd' body')
      Nothing ->
          return (Just gc, WhileStmt l cnd' body')

ssaStmt _ (ForStmt _  NoInit _ _ _ )     =
    errorstar "unimplemented: ssaStmt-for-01"

ssaStmt g (ForStmt l v cOpt (Just (UnaryAssignExpr l1 o lv)) b) =
    ssaStmt g (ForStmt l v cOpt (Just $ AssignExpr l1 (op o) lv (IntLit l1 1)) b)
  where
    op PrefixInc   = OpAssignAdd
    op PrefixDec   = OpAssignSub
    op PostfixInc  = OpAssignAdd
    op PostfixDec  = OpAssignSub

ssaStmt g (ForStmt l (VarInit vds) cOpt (Just e@(AssignExpr l1 _ _ _)) b) = do
    e' <- expand e
    ssaForLoop g l vds cOpt (Just $ ExprStmt l1 e') b
  where
    expand (AssignExpr l1 o lv e) = AssignExpr l1 OpAssign lv <$> infOp o l1 lv e
    expand _ = errorstar "unimplemented: expand assignExpr"

ssaStmt g (ForStmt l (VarInit vds) cOpt (Just i) b) =
    ssaForLoop g l vds cOpt (Just $ ExprStmt (getAnnotation i) i) b

ssaStmt g (ForStmt l (VarInit vds) cOpt Nothing  b) =
    ssaForLoop g l vds cOpt Nothing b


ssaStmt g (ForStmt l (ExprInit ei) cOpt (Just e@(AssignExpr l1 _ _ _)) b) = do
    e' <- expand e
    ssaForLoopExpr g l ei cOpt (Just $ ExprStmt l1 e') b
  where
    expand (AssignExpr l1 o lv e) = AssignExpr l1 OpAssign lv <$> infOp o l1 lv e
    expand _ = errorstar "unimplemented: expand assignExpr"

ssaStmt g (ForStmt l (ExprInit e) cOpt (Just i) b) =
    ssaForLoopExpr g l e cOpt (Just $ ExprStmt (getAnnotation i) i) b

ssaStmt g (ForStmt l (ExprInit e) cOpt Nothing  b) =
    ssaForLoopExpr g l e cOpt Nothing b


-- | for (var k in obj) { <body> }
--
--      ==>
--
--   var _keys = builtin_BIForInKeys(obj);
--               // Array<Imm, { v: string | (keyIn(v,obj) && enumProp(v,obj)) }>
--
--   for (var _i = 0; _i < _keys.length; _i++) {
--     var k = _keys[_i];
--
--     <body>
--
--   }

ssaStmt g (ForInStmt l (ForInVar v) e b) =
    do  init_  <- initArr
        for_   <- forStmt
        ssaStmt g $ maybeBlock l [init_, for_]
  where
    fr          = fr_ l
    biForInKeys = return $ builtinId "BIForInKeys"

    initArr     = vStmt $ VarDecl <$> fr
                                  <*> keysArr
                                  <*> justM (CallExpr <$> fr
                                                      <*> (VarRef <$> fr <*> biForInKeys)
                                                      <**> [e])
    initIdx     = VarDecl <$> fr
                          <*> keysIdx
                          <*> (Just      <$> (IntLit  <$> fr <**> 0))
    condition   = Just    <$> (InfixExpr <$> fr
                                         <*> return OpLT
                                         <*> (VarRef  <$> fr <*> keysIdx)
                                         <*> (DotRef  <$> fr
                                                      <*> (VarRef <$> fr <*> keysArr)
                                                      <*> (Id     <$> fr <**> "length")
                                             ))
    increment   = Just <$> (UnaryAssignExpr
                                      <$> fr
                                      <*> return PostfixInc
                                      <*> (LVar    <$> fr
                                                   <*> (unId <$> keysIdx)))
    accessKeys  = vStmt $ VarDecl <$> fr
                                  <*> return v
                                  <*> justM (BracketRef <$> fr
                                                        <*> (VarRef <$> fr <*> keysArr)
                                                        <*> (VarRef <$> fr <*> keysIdx))
    forStmt     = ForStmt <$> fr
                          <*> (VarInit . single <$> initIdx)
                          <*> condition
                          <*> increment
                          <*> (maybeBlock <$> fr
                                         <*> ( (:[b]) <$> accessKeys))

    vStmt v     = VarDeclStmt <$> fr <*> (single <$> v)

    keysArr     = return $ mkKeysId    v
    keysIdx     = return $ mkKeysIdxId v

    mkId        = Id (FA def def def)
    builtinId s = mkId ("builtin_" ++ s)



-- var x1 [ = e1 ]; ... ; var xn [= en];
ssaStmt g (VarDeclStmt l [vd]) = do
    (g', ss, vd') <- ssaVarDecl g vd
    return         $ (Just g', maybeBlock l (ss ++ [VarDeclStmt l [vd']]))

    -- return           (Just g', mkStmt $ foldr crunch ([], []) stvds')
  where
--     crunch ([], d) (ds, ss') = (d:ds, ss')
--     crunch (ss, d) (ds, ss') = ([]  , mkStmts l ss (d:ds) ss')
--     mkStmts l ss ds ss'      = ss ++ VarDeclStmt l ds : ss'
--     mkStmt (ds', [] )        = VarDeclStmt l ds'
--     mkStmt (ds', ss')        = maybeBlock l (mkStmts l [] ds' ss')

ssaStmt _ v@(VarDeclStmt l _) =
  ssaError $ unimplSSAMulVarDecl l v

-- return;
ssaStmt _ s@(ReturnStmt _ Nothing) =
  return (Nothing, s)

-- return e
ssaStmt g (ReturnStmt l (Just e)) = do
    (_, s, e') <- ssaExpr g e
    return (Nothing,  prefixStmt l s $ ReturnStmt l (Just e'))

-- throw e
ssaStmt g (ThrowStmt l e) = do
    (_, s, e') <- ssaExpr g e
    return (Nothing, prefixStmt l s $ ThrowStmt l e')

-- function f(...){ s }
ssaStmt g (FunctionStmt l f xs (Just bd)) = do
    bd'   <- ssaFun g' xs bd
    return   (Just g, FunctionStmt l f xs (Just bd'))
  where
    g'  = initCallableSsaEnv g xs bd

ssaStmt g s@(FunctionStmt _ _ _ Nothing)
  = return (Just g, s)

-- -- switch (e) { ... }
-- ssaStmt g (SwitchStmt l e xs) = do
--     id <- updSsaEnv g (an e) (Id (an e) "__switchVar")
--     let go (l, e, s) = IfStmt (an s) (InfixExpr l OpStrictEq (VarRef l id) e) s
--     mapSnd (maybeBlock l) <$> ssaStmts g
--       [ VarDeclStmt (an e) [VarDecl (an e) id (Just e)], foldr go z sss ]
--   where
--     an                   = getAnnotation
--     sss                  = [ (l, e, maybeBlock l $ remBr ss) | CaseClause l e ss <- xs ]
--     z                    = headWithDefault (EmptyStmt l) [maybeBlock l $ remBr ss | CaseDefault l ss <- xs]
--
--     remBr                = filter (not . isBr) . flattenBlock
--     isBr (BreakStmt _ _) = True
--     isBr _               = False
--     headWithDefault a [] = a
--     headWithDefault _ xs = head xs

-- class A extends B implements I,J,... { ... }
ssaStmt g c@(ClassStmt l n bd) = do
    bd'   <- mapM (ssaClassElt g' c) bd
    return   (Just g, ClassStmt l n bd')
  where
    g' = initClassSsaEnv l g n

-- module M { ... }
ssaStmt g (ModuleStmt l n bd) = do
    (_, bd') <- ssaStmts g' bd
    return      (Just g, ModuleStmt l n bd')
  where
    g'  = initModuleSsaEnv l g n bd

-- enum { ... }
ssaStmt g (EnumStmt l n es) = do
    (g', exps') <- ssaPureExprs g exps
    return   (Just g', EnumStmt l n $ zipWith exprToElt es exps')
  where
    exprOfElt (EnumElt _ _ e) = e
    exps = map exprOfElt es
    exprToElt (EnumElt l i _) e = EnumElt l i e

-- OTHER (Not handled)
ssaStmt _ s
  = convertError "ssaStmt" s


ssaAsgnStmt l1 l2 x@(Id l3 v) x' e
  | x == x'   = ExprStmt l1    (AssignExpr l2 OpAssign (LVar l3 v) e)
  | otherwise = VarDeclStmt l1 [VarDecl l2 x' (Just e)]

-- | Freshen annotation shortcut
--
fr_ = freshenAnn

-------------------------------------------------------------------------------------
ctorVisitor :: PPR r => [Id (AnnSSA r)] -> VisitorM (SSAM r) () () (AnnSSA r)
-------------------------------------------------------------------------------------
ctorVisitor ms =
    defaultVisitor { endStmt = es } { endExpr = ee } { mStmt = ts } { mExpr = te }
  where
    es FunctionStmt{}     = True
    es _                  = False
    ee FuncExpr{}         = True
    ee _                  = False

    te (AssignExpr la OpAssign (LDot ld (ThisRef _) s) e)
                          = AssignExpr <$> fr_ la
                                       <*> return OpAssign
                                       <*> (LVar <$> fr_ ld <**> mkCtorStr s)
                                       <*> return e
    te lv                 = return lv

    ts r@(ReturnStmt l _) = error "no return in ctor"
    ts r                  = return r

    -- ts r@(ReturnStmt l _) = maybeBlock <$> fr_ l <*> ((:[tracePP "ret" r]) <$> ctorExit l ms)


-------------------------------------------------------------------------------------
transSuper
  :: IsLocated a
  => a -> SsaEnv r -> [Expression (AnnSSA r)] -> SSAM r [Statement (AnnSSA r)]
-------------------------------------------------------------------------------------
transSuper l g es
  = do  svs     <- superVS parent
        flds    <- mapM asgnS fields
        let body = svs : flds
        l'      <- fr_ l
        return     [maybeBlock l' body]
  where
    fr     = fr_ l
    cha    = ssaCHA g

    fields | Just n <- curClass g
           = inheritedNonStaticFields cha n
           | otherwise
           = []

    parent | Just n <- curClass g
           , Just (TD d _ _) <- resolveType cha n
           , TS _ _ ([Gen (QN path name) _],_) <- d
           = case path of
               QP _ _ []     -> VarRef <$> fr <*> (Id <$> fr <**> F.symbolSafeString name)
               QP _ _ (y:ys) -> do
                  init <- VarRef <$> fr <*> (Id <$> fr <**> F.symbolSafeString y)
                  foldM (\e q -> DotRef <$> fr <*> return e
                                               <*> (Id <$> fr <**> F.symbolSafeString q)) init (ys ++ [name])
           | otherwise = ssaError $ bugSuperWithNoParent (srcPos l)

    superVS n = VarDeclStmt <$> fr <*> (single <$> superVD n)
    superVD n = VarDecl  <$> fr
                         <*> freshenIdSSA (builtinOpId BISuperVar)
                         <*> justM (NewExpr <$> fr <*> n <**> es)
    asgnS x = ExprStmt   <$> fr <*> asgnE x
    asgnE x = AssignExpr <$> fr
                         <*> return OpAssign
                         <*> (LDot <$> fr <*> (ThisRef <$> fr) <**> F.symbolSafeString x)
                         <*> (DotRef <$> fr
                                     <*> (VarRef <$> fr <*> freshenIdSSA (builtinOpId BISuperVar))
                                     <*> (Id <$> fr <**> F.symbolSafeString x))


-------------------------------------------------------------------------------------
ctorExit :: AnnSSA r -> [Id t] -> SSAM r (Statement (AnnSSA r))
-------------------------------------------------------------------------------------
ctorExit l ms = do
    ctorExit    <- VarRef <$> fr <*> freshenIdSSA (builtinOpId BICtorExit)
    es          <- mapM (VarRef <$> fr <**>) ms'
    exitC       <- CallExpr <$> fr <**> ctorExit <**> es
    ReturnStmt <$> fr <**> Just exitC
  where
    ms' = map (mkCtorId l) ms
    fr  = fr_ l


-- | Constructor Transformation
--
--  constructor() {
--    super(..);
--    this.x = 1;
--    this.y = "sting";
--    if () {  this.x = 2; }
--
--  }
--
--          ||
--          vv
--
--  constructor() {
--    [[ super(..); ]]
--
--    var _ctor_x_0 = 1;                        // preM
--    var _ctor_y_1 = "string"
--
--    if () { _ctor_x_2 = 2; }                  // bdM
--    // _ctor_x_3 = φ(_ctor_x_2,_ctor_x_0);
--
--    return _ctor_exit(_ctor_x_3,_ctor_y_1);   // exitM
--
--  }
--
-------------------------------------------------------------------------------------
ssaClassElt :: PPR r => SsaEnv r -> Statement (AnnSSA r) -> ClassElt (AnnSSA r)
                     -> SSAM r (ClassElt (AnnSSA r))
-------------------------------------------------------------------------------------
ssaClassElt g c (Constructor l xs bd0) = do

    fs          <- mapM symToVar fields

    -- `super(..)` must precede field initializations
    (sup, bd1)  <- case bd0 of
                     ExprStmt _ (CallExpr l (SuperRef _) es) : bbs -> do
                         s_   <- transSuper l g' es
                         -- XXX: Apply the "this.x = ..." transformation
                         -- here as well !!!
                         s_'  <- bdM fs s_
                         return (s_', bbs)
                     _ -> pure ([], bd0)

    pre         <- preM c
    bfs         <- bdM fs bd1
    efs         <- exitM fs
    bd2         <- pure (sup ++ pre ++ bfs ++ efs)
    (_, bd3)   <- ssaStmts g' bd2
    return       $ Constructor l xs bd3

  where

    g'           = initCallableSsaEnv g xs bd0

    symToVar     = freshenIdSSA . mkId . F.symbolString -- F.symbolSafeString
    cha          = ssaCHA g
    fields       | Just n <- curClass g
                 = sortBy c_sym (nonStaticFields cha n)   -- Sort alphabetically
                 | otherwise
                 = []
    exitM  fs    = single <$> ctorExit l fs

    bdM fs       = visitStmtsT (ctorVisitor fs) ()

    c_sym        = on compare show     -- XXX: Symbolic compare is on Symbol ID


-- | Initilization expression for instance variables is moved to the beginning
--   of the constructor.
ssaClassElt _ _ (MemberVarDecl l False x _)
  = return $ MemberVarDecl l False x Nothing

ssaClassElt g _ (MemberVarDecl l True x (Just e))
  = do z <- ssaExpr g e
       case z of
         (_, [], e') -> return $ MemberVarDecl l True x (Just e')
         _           -> ssaError $ errorEffectInFieldDef (srcPos l)

ssaClassElt _ _ (MemberVarDecl l True x Nothing)
  = ssaError $ errorUninitStatFld (srcPos l) x

ssaClassElt g _ (MemberMethDecl l s e xs body)
  = MemberMethDecl l s e xs <$> ssaFun g xs body


preM (ClassStmt _ _ cs)
  = do
        -- Initialized
        let (ls , xs , es) = unzip3 [(l, mkCtorId l x, e) | MemberVarDecl l False x (Just e) <- cs]
        -- Uninitialized
        let (ls1, xs1)     = unzip  [(l, mkCtorId l x)    | MemberVarDecl l False x Nothing  <- cs]

        -- Initialize the latter with `undefined`
        us                <- mapM (\l -> VarRef <$> fr l <*> freshenIdSSA undefinedId) ls1

        xs'               <- mapM freshenIdSSA xs
        xs1'              <- mapM freshenIdSSA xs1

        zipWith3M f (ls ++ ls1) (xs' ++ xs1') (es ++ us)
  where
    fr              = freshenAnn
    f l x e         = VarDeclStmt <$> fr l <*> (single <$> g l x e)
    g l x e         = VarDecl     <$> fr l <*> freshenIdSSA x <*> (Just <$> pure e)

preM _ = return []


-- | Expand: [[ if ( e1 || e2) { s1 } else { s2 } ]]
--
--      let r = false;
--      if ( [[ e1 ]] ) {       // IF1
--          r = true;           // RT1
--      }
--      else {
--          if ( [[ e2 ]] ) {   // IF2
--              r = true;       // RT2
--          }
--      }
--      if ( r ) {
--          [[ s1 ]]
--      }
--      else {
--          [[ s2 ]]
--      }
--
ssaExpandIfStmtInfixOr l e1 e2 s1 s2
  = do  n     <- ("lor_" ++) . show  <$> tick
        -- R1 ::= var r_NN = false;
        r     <- Id           <$> fr l <**> n
        fls   <- BoolLit      <$> fr l <**> False
        vd    <- VarDecl      <$> fr l <**> r <**> Just fls
        vs    <- VarDeclStmt  <$> fr l <**> [vd]
        -- RT1 ::= r = true;
        tru1  <- BoolLit      <$> fr l <**> True
        lv1   <- LVar         <$> fr l <**> n
        ae1   <- AssignExpr   <$> fr l <**> OpAssign <**> lv1 <**> tru1
        as1   <- ExprStmt     <$> fr l <**> ae1
        -- RT2 ::= r = true;
        tru2  <- BoolLit      <$> fr l <**> True
        lv2   <- LVar         <$> fr l <**> n
        ae2   <- AssignExpr   <$> fr l <**> OpAssign <**> lv2 <**> tru2
        as2   <- ExprStmt     <$> fr l <**> ae2
        -- IF2 ::= if ( e2 ) { r = true } else { }
        if2   <- IfSingleStmt <$> fr l <**> e2 <**> as2
        -- IF1 ::= if ( e1 ) { r = true } else { if ( e2 ) { r = true } }
        if1   <- IfStmt       <$> fr l <**> e1 <**> as1 <**> if2
        -- if ( r ) { s1 } else { s2 }
        r3    <- Id           <$> fr l <**> n
        v3    <- VarRef       <$> fr l <**> r3
        if3   <- IfStmt       <$> fr l <**> v3 <**> s1 <**> s2
        maybeBlock            <$> fr l <**> [vs, if1, if3]
  where
    fr = freshenAnn

-- | Expand: [[ if ( e1 && e2) { s1 } else { s2 } ]]
--
--      let r = true;
--      if ( [[ e1 ]] ) {
--          if ( [[ e2 ]] ) {
--
--          }
--          else {
--              r = false;
--          }
--      }
--      else {
--          r = false;
--      }
--      if ( r ) {
--          [[ s1 ]]
--      }
--      else {
--          [[ s2 ]]
--      }
--
ssaExpandIfStmtInfixAnd l e1 e2 s1 s2
  = do  n     <- ("land_" ++) . show  <$> tick
        -- var r_NN = true;
        r     <- Id           <$> fr l <**> n
        fls   <- BoolLit      <$> fr l <**> True
        vd    <- VarDecl      <$> fr l <**> r <**> Just fls
        vs    <- VarDeclStmt  <$> fr l <**> [vd]
        -- r = false;
        tru1  <- BoolLit      <$> fr l <**> False
        lv1   <- LVar         <$> fr l <**> n
        ae1   <- AssignExpr   <$> fr l <**> OpAssign <**> lv1 <**> tru1
        as1   <- ExprStmt     <$> fr l <**> ae1
        -- IF2 ::= if ( e2 ) {  } else { r = false; }
        emp   <- EmptyStmt    <$> fr l
        if2   <- IfStmt       <$> fr l <**> e2 <**> emp <**> as1
        -- AS2 ::= r = false;
        tru2  <- BoolLit      <$> fr l <**> False
        lv2   <- LVar         <$> fr l <**> n
        ae2   <- AssignExpr   <$> fr l <**> OpAssign <**> lv2 <**> tru2
        as2   <- ExprStmt     <$> fr l <**> ae2
        -- IF1 ::= if ( e1 ) { IF2 } else { AS2 }
        if1   <- IfStmt       <$> fr l <**> e1 <**> if2 <**> as2
        -- if ( r ) { s1 } else { s2 }
        r3    <- Id           <$> fr l <**> n
        v3    <- VarRef       <$> fr l <**> r3
        if3   <- IfStmt       <$> fr l <**> v3 <**> s1 <**> s2
        maybeBlock            <$> fr l <**> [vs, if1, if3]
  where
    fr = freshenAnn


infOp OpAssign         _ _  e = return e
infOp OpAssignAdd      l lv e = InfixExpr l OpAdd      <$> lvalExp lv <*> pure e
infOp OpAssignSub      l lv e = InfixExpr l OpSub      <$> lvalExp lv <*> pure e
infOp OpAssignMul      l lv e = InfixExpr l OpMul      <$> lvalExp lv <*> pure e
infOp OpAssignDiv      l lv e = InfixExpr l OpDiv      <$> lvalExp lv <*> pure e
infOp OpAssignMod      l lv e = InfixExpr l OpMod      <$> lvalExp lv <*> pure e
infOp OpAssignLShift   l lv e = InfixExpr l OpLShift   <$> lvalExp lv <*> pure e
infOp OpAssignSpRShift l lv e = InfixExpr l OpSpRShift <$> lvalExp lv <*> pure e
infOp OpAssignZfRShift l lv e = InfixExpr l OpZfRShift <$> lvalExp lv <*> pure e
infOp OpAssignBAnd     l lv e = InfixExpr l OpBAnd     <$> lvalExp lv <*> pure e
infOp OpAssignBXor     l lv e = InfixExpr l OpBXor     <$> lvalExp lv <*> pure e
infOp OpAssignBOr      l lv e = InfixExpr l OpBOr      <$> lvalExp lv <*> pure e

lvalExp (LVar l s)          = VarRef <$> fr_ l <*> (Id <$> fr_ l <*> pure s)
lvalExp (LDot l e s)        = DotRef <$> fr_ l <*> pure e <*> (Id <$> fr_ l <*> pure s)
lvalExp (LBracket l e1 e2)  = BracketRef <$> fr_ l <*> pure e1 <*> pure e2

-- -------------------------------------------------------------------------------------
-- presplice :: Maybe (Statement (AnnSSA r)) -> Statement (AnnSSA r) -> Statement (AnnSSA r)
-- -------------------------------------------------------------------------------------
-- presplice z s' = splice_ (getAnnotation s') z (Just s')

-- TODO: instantiate Monoid?
-------------------------------------------------------------------------------------
splice :: Statement a -> Maybe (Statement a) -> Statement a
-------------------------------------------------------------------------------------
splice s = splice_ (getAnnotation s) (Just s)

splice_ l Nothing Nothing    = EmptyStmt l
splice_ _ (Just s) Nothing   = s
splice_ _ Nothing (Just s)   = s
splice_ _ (Just s) (Just s') = seqStmt (getAnnotation s) s s'

seqStmt _ (BlockStmt l s) (BlockStmt _ s') = maybeBlock l (s   ++ s'  )
seqStmt l (BlockStmt _ s) s'               = maybeBlock l (s   ++ [s'])
seqStmt l s               (BlockStmt _ s') = maybeBlock l ([s] ++ s'  )
seqStmt l s s'                             = maybeBlock l [s, s']

-------------------------------------------------------------------------------------
prefixStmt :: a -> [Statement a] -> Statement a -> Statement a
-------------------------------------------------------------------------------------
prefixStmt l ss s = maybeBlock l (ss ++ [s])

-------------------------------------------------------------------------------------
maybeBlock :: a -> [Statement a] -> Statement a
-------------------------------------------------------------------------------------
maybeBlock l [ ]  = EmptyStmt l
maybeBlock _ [s]  = s
maybeBlock l ss   = BlockStmt l ss

-------------------------------------------------------------------------------------
flattenBlock :: [Statement t] -> [Statement t]
-------------------------------------------------------------------------------------
flattenBlock = concatMap f
  where
    f (BlockStmt _ ss) = ss
    f s                = [s]

-------------------------------------------------------------------------------------
ssaExpr :: PPR r => SsaEnv r -> Expression (AnnSSA r)
                 -> SSAM r (SsaEnv r, [Statement (AnnSSA r)], Expression (AnnSSA r))
-------------------------------------------------------------------------------------

ssaExpr g e@(IntLit _ _)
  = return (g, [], e)

ssaExpr g e@(NumLit _ _)
  = return (g, [], e)

ssaExpr g e@(HexLit _ _)
  = return (g, [], e)

ssaExpr g e@(BoolLit _ _)
  = return (g, [], e)

ssaExpr g e@(StringLit _ _)
  = return (g, [], e)

ssaExpr g e@(NullLit _)
  = return (g, [], e)

ssaExpr g e@(ThisRef _)
  = return (g, [], e)

ssaExpr g e@(SuperRef _)
  = return (g, [], e)

ssaExpr g   (ArrayLit l es) = do
    (g', es') <- ssaPureExprs g es
    return (g', [], ArrayLit l es')

-- | arguemnts ==> __getArguemnts()
--
ssaExpr g (VarRef l x)
  | isBIArgumentsVar g x
  = do  aId   <- freshenIdSSA (getArgId l)
        aVr   <- VarRef   <$> freshenAnn l <*> pure aId
        cExp  <- CallExpr <$> freshenAnn l <*> pure aVr <*> pure []
        ssaExpr g cExp

 | otherwise
  = (g,[],) <$> ssaVarRef g l x

ssaExpr g (CondExpr l c e1 e2)
  = do (gc, sc, c') <- ssaExpr g c
       (g1, e1')    <- ssaPureExpr gc e1
       (g2, e2')    <- ssaPureExpr g1 e2
       return (g2, sc, CondExpr l c' e1' e2')

ssaExpr g (PrefixExpr l o e) = do
    (g', ss, e') <- ssaExpr g e
    return (g', ss, PrefixExpr l o e')

ssaExpr _ e@(InfixExpr l OpLOr _ _)
  = ssaError $ unimplementedInfix l e

ssaExpr g (InfixExpr l o e1 e2) = do
    (g1, s1, e1') <- ssaExpr g e1
    (g2, s2, e2') <- ssaExpr g1 e2
    return $ (g2, s1 ++ s2, InfixExpr l o e1' e2')

ssaExpr _ e@(CallExpr _ (SuperRef _) _)
  = ssaError $ bugSuperNotHandled (srcPos e) e

ssaExpr g (CallExpr l e es) = do
    (g1, s, e')   <- ssaExpr g e
    (gs, ss, es') <- ssaExprs g1 es
    return (gs, s ++ ss, CallExpr l e' es')

ssaExpr g (ObjectLit l ps) = do
    (g', ss, es') <- ssaExprs g es
    return (g', ss, ObjectLit l (zip fs es'))
  where
    (fs, es) = unzip ps

ssaExpr g (DotRef l e i) = do
    (g', s, e')   <- ssaExpr g e
    return (g', s, DotRef l e' i)

ssaExpr g (BracketRef l e1 e2) = do
    (g1, s1, e1') <- ssaExpr g e1
    (g2, s2, e2') <- ssaExpr g1 e2
    return (g2, s1 ++ s2, BracketRef l e1' e2')

ssaExpr g (NewExpr l e es) = do
    (g1, s, e')   <- ssaExpr g e
    (gs, ss, es') <- ssaExprs g1 es
    return (gs, s ++ ss, NewExpr l e' es')

ssaExpr g (Cast l e) = do
    (g', s, e')   <- ssaExpr g e
    return (g', s, Cast l e')

ssaExpr g (FuncExpr l fo xs bd) = do
    bd' <- ssaFun g xs bd
    return (g, [], FuncExpr l fo xs bd')

-- x = e
ssaExpr g (AssignExpr l OpAssign (LVar lv v) e) =
    ssaAsgnExpr g l lv (Id lv v) e
    -- (g', s, x', e') <- ssaAsgn g l x e
    -- return             (g', s ++ [ssaAsgnStmt l lv x x' e'], e')

-- e1.f = e2
ssaExpr g (AssignExpr l OpAssign (LDot ll e1 f) e2) = do
    (g1, s1, e1') <- ssaExpr g e1
    (g2, s2, e2') <- ssaExpr g1 e2
    return (g2, s1 ++ s2, AssignExpr l OpAssign (LDot ll e1' f) e2')

-- e1[e2] = e3
ssaExpr g (AssignExpr l OpAssign (LBracket ll e1 e2) e3) = do
    (g1, s1, e1') <- ssaExpr g  e1
    (g2, s2, e2') <- ssaExpr g1 e2
    (g3, s3, e3') <- ssaExpr g2 e3
    return (g3, s1 ++ s2 ++ s3, AssignExpr l OpAssign (LBracket ll e1' e2') e3')

-- lv += e
--
-- XXX: only allow vars on the LHS to avoid side-effects
--
ssaExpr g (AssignExpr l op lv@(LVar _ _) e) = do
    lv'   <- lvalExp lv
    rhs   <- InfixExpr <$> fr_ l <*> pure (assignInfix op) <*> pure lv' <*> pure e
    ssaExpr g (AssignExpr l OpAssign lv rhs)

-- x++ ==> [x1 = x0 + 1], x1
ssaExpr g (UnaryAssignExpr l uop (LVar lv v))
  = do let x        = Id lv v
       x0          <- ssaVarRef g l x
       (x1, g')    <- updSsaEnv g l x
       let (eI, eO) = unaryExprs l uop x0 (VarRef l x1)
       return         (g', [ssaAsgnStmt l lv x x1 eI], eO)

-- lv++
ssaExpr g (UnaryAssignExpr l uop lv)
  = do (g', lv')  <- ssaLval g lv
       lv''       <- lvalExp lv'
       let e'      = unaryExpr l uop lv''
       return        (g', [], AssignExpr l OpAssign lv e')

ssaExpr _ e
  = convertError "ssaExpr" e


ssaAsgnExpr g l lx x e
  = do (g', s, x', e') <- ssaAsgn g l x e
       return         (g', s ++ [ssaAsgnStmt l lx x x' e'], e')

--------------------------------------------------------------------------------
ssaLval :: PPR r
  => SsaEnv r -> LValue (AnnSSA r) -> SSAM r (SsaEnv r, LValue (AnnSSA r))
--------------------------------------------------------------------------------
ssaLval g (LVar lv v)
  = do VarRef _ (Id _ v') <- ssaVarRef g lv (Id lv v)
       return (g, LVar lv v')

ssaLval g (LDot ll e f)
  = do (g', e') <- ssaPureExpr g e
       return (g', LDot ll e' f)

ssaLval g (LBracket ll e1 e2)
  = do (g1, e1') <- ssaPureExpr g e1
       (g2, e2') <- ssaPureExpr g1 e2
       return  (g2, LBracket ll e1' e2')


--------------------------------------------------------------------------------
-- | Helpers for gathering assignments for purifying effectful-expressions
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
ssaSeq :: (g -> e -> SSAM r (g, [s], a)) -> g -> [e] -> SSAM r (g, [s], [a])
--------------------------------------------------------------------------------
ssaSeq f      = go
  where
    go g []     = return (g, [], [])
    go g (e:es) = do  (g1, ss1, e' )  <- f g e
                      (g2, ss2, es')  <- go g1 es
                      return (g2, ss1 ++ ss2, e':es')

ssaExprs = ssaSeq ssaExpr

--------------------------------------------------------------------------------
ssaPureExprs :: PPR r => SsaEnv r -> [Expression (AnnSSA r)]
             -> SSAM r (SsaEnv r, [Expression (AnnSSA r)])
--------------------------------------------------------------------------------
ssaPureExprs    = go
  where
    go g []     = return (g, [])
    go g (e:es) = do  (g1, e' )  <- ssaPureExpr g e
                      (g2, es')  <- go g1 es
                      return (g2, e':es')


--
-- ssaExpr1 g = case1 . ssaExprs g
-- ssaExpr2 g = case2 . ssaExprs g
-- ssaExpr3 g = case3 . ssaExprs g
--
-- ssaPureExprWith θ g e = snd <$> ssaWith θ (fmap (True,) . ssaPureExpr g) e
--
-- -------------------------------------------------------------------------------------
-- ssaWith :: Env (Var r) -> (a -> SSAM r (Bool, b)) -> a -> SSAM r (Maybe (Env (Var r)), b)
-- -------------------------------------------------------------------------------------
-- ssaWith θ f x
--   = do  setSsaVars θ
--         (b, x') <- f x
--         (,x') <$> go b
--   where
--     go b | b         = Just <$> getSsaVars
--          | otherwise = pure Nothing

ssaPureExpr :: PPR r => SsaEnv r -> Expression (AnnSSA r) -> SSAM r (SsaEnv r, Expression (AnnSSA r))
ssaPureExpr g e = do
  (g', s, e') <- ssaExpr g e
  case s of
    []     -> return (g', e')
    _      -> ssaError $ errorUpdateInExpr (srcPos e) e

--------------------------------------------------------------------------------
-- | Dealing with Generic Assignment Expressions
--------------------------------------------------------------------------------
assignInfix :: AssignOp -> InfixOp
assignInfix OpAssignAdd      = OpAdd
assignInfix OpAssignSub      = OpSub
assignInfix OpAssignMul      = OpMul
assignInfix OpAssignDiv      = OpDiv
assignInfix OpAssignMod      = OpMod
assignInfix OpAssignLShift   = OpLShift
assignInfix OpAssignSpRShift = OpSpRShift
assignInfix OpAssignZfRShift = OpZfRShift
assignInfix OpAssignBAnd     = OpBAnd
assignInfix OpAssignBXor     = OpBXor
assignInfix OpAssignBOr      = OpBOr
assignInfix o                = error $ "assignInfix called with " ++ ppshow o


--------------------------------------------------------------------------------
-- | Dealing with Unary Expressions
--------------------------------------------------------------------------------

unaryExprs l u xOld xNew = (unaryExpr l u xOld, unaryId xOld xNew u)

unaryExpr l u e1         = InfixExpr l bop e1 (IntLit l 1)
  where
    bop                  = unaryBinOp u

unaryBinOp PrefixInc     = OpAdd
unaryBinOp PostfixInc    = OpAdd
unaryBinOp _             = OpSub
unaryId _ x PrefixInc    = x
unaryId _ x PrefixDec    = x
unaryId x _ _            = x


--------------------------------------------------------------------------------
ssaVarDecl :: PPR r
           => SsaEnv r -> VarDecl (AnnSSA r)
           -> SSAM r (SsaEnv r, [Statement (AnnSSA r)], VarDecl (AnnSSA r))
--------------------------------------------------------------------------------
ssaVarDecl g (VarDecl l x (Just e)) = do
    (g1, s, e') <- ssaExpr g e
    (x', g2)    <- initSsaVar g1 l x
    return    (g2, s, VarDecl l x' (Just e'))

ssaVarDecl g v@(VarDecl l x Nothing) =
  case a of
    Ambient -> return (g, [], VarDecl l x Nothing)
    RdOnly  -> ssaError $ errorReadOnlyUninit l x
    _       -> do vr <- VarRef <$> fr_ l <*> freshenIdSSA undefinedId
                  ssaVarDecl g (VarDecl l x (Just vr))
  where
    a = varDeclToAsgn v

--------------------------------------------------------------------------------
ssaVarRef :: PPR r
  => SsaEnv r -> AnnSSA r -> Id (AnnSSA r) -> SSAM r (Expression (AnnSSA r))
--------------------------------------------------------------------------------
ssaVarRef g l x
  = case getAssignability g x WriteLocal of
      WriteGlobal  -> return e
      Ambient      -> return e
      RdOnly       -> return e
      WriteLocal   -> case findSsaEnv g x of
                        Just t   -> return   $ VarRef l t
                        Nothing  -> return   $ VarRef l x
      ForeignLocal -> ssaError $ errorForeignLocal (srcPos x) x
      ReturnVar    -> ssaError $ errorSSAUnboundId (srcPos x) x
    where
       e = VarRef l x

--------------------------------------------------------------------------------
ssaAsgn :: PPR r
  => SsaEnv r -> AnnSSA r -> Id (AnnSSA r) -> Expression (AnnSSA r)
  -> SSAM r (SsaEnv r, [Statement (AnnSSA r)], Id (AnnSSA r), Expression (AnnSSA r))
--------------------------------------------------------------------------------
ssaAsgn g l x e  = do
    (g1, s, e') <- ssaExpr g e
    (x', g2)    <- updSsaEnv g1 l x
    return (g2, s, x', e')


type SsaEnvO r = Maybe (SsaEnv r)
type StmtO   r = Maybe (Statement (AnnSSA r))

--
--    if (x > 0) {
--        x = 1;        // S1
--    } else {
--        x = 2;        // S2
--    }
--    return x;
--
--    ==>
--
--    if (x0 > 0) {
--        x1 = 1;       // ssaStmt S1
--        x3 = x1;      // Φ-asgn-1
--    } else {
--        x2 = 2;       // ssaStmt S2
--        x3 = x2;      // Φ-asgn-2
--    }
--    return x3;
--
--------------------------------------------------------------------------------
envJoin :: PPR r => AnnSSA r -> SsaEnv r -> SsaEnvO r -> SsaEnvO r
                 -> SSAM r (SsaEnvO r, StmtO r, StmtO r)
--------------------------------------------------------------------------------
envJoin _ _ Nothing Nothing     = return (Nothing, Nothing, Nothing)
envJoin _ _ Nothing (Just g)    = return (Just g , Nothing, Nothing)
envJoin _ _ (Just g) Nothing    = return (Just g , Nothing, Nothing)
envJoin l g (Just g1) (Just g2) = do
    (φs, s1, s2) <- unzip3 <$> mapM phiAsgn φBinds
    return ( Just (g { mSSA = envAdds φs γ })     -- Update Φ-var bindings in env
           , Just (maybeBlock l s1)
           , Just (maybeBlock l s2))
  where

    γ   = mSSA g
    γ1  = mSSA g1
    γ2  = mSSA g2

    (_, δ1, _)  = envProgress γ γ1
    (_, δ2, _)  = envProgress γ γ2

    -- The latest version of each Φ-var in each branch
    -- (x, (x1 (branch 1), x2 (branch 2)))
    φBinds      = catMaybes
                $ map (\k -> do t1 <- envFindTy k γ1
                                t2 <- envFindTy k γ2
                                return (k, (t1, t2)))
                $ envKeys (δ1 `mappend` δ2)

    phiAsgn (x, (x1, x2)) = do
        xf       <- freshenIdSSA x
        (x', _)  <- updSsaEnv g l xf
        addAnn l (PhiVar x')
        s1      <- mkPhiAsgn l x' x1
        s2      <- mkPhiAsgn l x' x2
        return     ((xf, x'), s1, s2)


-- | `envProgress γ γ'`: given two environments `γ` and `γ'` returns the triple:
--
--    * Env of vars that remain unchainged (in terms of SSA)
--
--    * Env of vars that change (bound to their 'before' and 'after' version)
--
--    * The new vars introduces in the between `γ` and `γ'`
--
-------------------------------------------------------------------------------------
envProgress :: Env (Var r) -> Env (Var r) -> (Env (Var r), Env (Var r, Var r), [Id SrcSpan])
-------------------------------------------------------------------------------------
envProgress γ γ' = (σ, δ, ν)
  where
    σ          = envRights θ                           -- Unchanged vars
    δ          = envLefts  θ                           -- Updated vars
    ν          = envKeys (envDiff γ' γ)                -- new vars
    θ          = envIntersectWith meet γ γ'

    x `meet` y | x == y    = Right x
               | otherwise = Left (x, y)


-- XXX: Allowing the phi-var assignment to hold a unique reference, since we
--      know this will not be escaping.
-------------------------------------------------------------------------------------
mkPhiAsgn :: AnnSSA r -> Id (AnnSSA r) -> Id (AnnSSA r) -> SSAM r (Statement (AnnSSA r))
-------------------------------------------------------------------------------------
mkPhiAsgn l x y = do
    vy    <- enableUnique <$> (VarRef  <$> fl <*> freshenIdSSA y)
    vd    <-                   VarDecl <$> fl <*> freshenIdSSA x
                                              <*> (Just <$> pure vy)
    VarDeclStmt <$> fl <*> (single <$> pure vd)
  where
    fl = freshenAnn l


-- -- | `getLoopPhis g b` returns a list with the Φ-vars of the body `b`:
-- --
-- --    * the source variable, and
-- --    * the SSA-ed version before the loop body.
-- --
-- -------------------------------------------------------------------------------------
-- -- getLoopPhis :: SsaEnv r -> Statement (AnnSSA r) -> SSAM r [(Id SrcSpan, Var r)]
-- -------------------------------------------------------------------------------------
-- getLoopPhis g b = do
--     (go', _)  <- tryAction (ssaStmt g b)
--     case go' of
--       Just g' -> return $ diff (mSSA g) (mSSA g')
--       Nothing -> return []
--   where
--     x `meet` y | x == y    = Right x        -- No operation was performed on this var
--                | otherwise = Left  x        -- Φ-vars (in the beginning of the loop)
--
--     diff γ γ'  = envToList $ envLefts $ envIntersectWith meet γ γ'

-------------------------------------------------------------------------------------
ssaForLoop :: PPR r
           => SsaEnv r
           -> AnnSSA r
           -> [VarDecl (AnnSSA r)]
           -> Maybe (Expression (AnnSSA r))
           -> Maybe (Statement (AnnSSA r))
           -> Statement (AnnSSA r)
           -> SSAM r (Maybe (SsaEnv r), Statement (AnnSSA r))
-------------------------------------------------------------------------------------
ssaForLoop g l vds cOpt incExpOpt b =
  do
    (b, sts') <- ssaStmts g sts
    return     $ (b, maybeBlock l sts')
  where
    sts        = [VarDeclStmt l vds, WhileStmt l c bd]
    bd         = maybeBlock bl $ [b] ++ catMaybes [incExpOpt]
    bl         = getAnnotation b
    c          = maybe (BoolLit l True) id cOpt

-------------------------------------------------------------------------------------
ssaForLoopExpr :: PPR r
               => SsaEnv r
               -> AnnSSA r
               -> Expression (AnnSSA r)
               -> Maybe (Expression (AnnSSA r))
               -> Maybe (Statement (AnnSSA r))
               -> Statement (AnnSSA r)
               -> SSAM r (Maybe (SsaEnv r), Statement (AnnSSA r))
-------------------------------------------------------------------------------------
ssaForLoopExpr g l exp cOpt incExpOpt b =
  do
    l1        <- fr_ l
    (b, sts') <- ssaStmts g [ExprStmt l1 exp, WhileStmt l c bd]
    l2        <- fr_ l
    return     $ (b, maybeBlock l2 sts')
  where
    bd         = maybeBlock bl $ [b] ++ catMaybes [incExpOpt]
    bl         = getAnnotation b
    c          = maybe (BoolLit l True) id cOpt

