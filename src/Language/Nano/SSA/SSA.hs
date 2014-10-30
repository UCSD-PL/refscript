{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}

module Language.Nano.SSA.SSA (ssaTransform) where

import           Control.Arrow                           ((***))
import           Control.Applicative                     ((<$>), (<*>))
import           Control.Monad
import           Data.Default
import           Data.Data
import           Data.Maybe                              (catMaybes, maybeToList) 
import qualified Data.List                               as L
import qualified Data.IntSet                             as I
import qualified Data.IntMap.Strict                      as IM
import qualified Data.HashSet                            as S
import           Data.Typeable                           ()

import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations

import qualified Language.Fixpoint.Errors                as E
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types                 as F

import           Language.Nano.Annots
import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Misc
import           Language.Nano.Program
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.SSA.Types
import           Language.Nano.SSA.SSAMonad
import           Language.Nano.Visitor     

import           Debug.Trace                        hiding (traceShow)

-- FIXME : SSA needs a proper environment like TC an Liquid

----------------------------------------------------------------------------------
ssaTransform :: (PP r, F.Reftable r, Data r) 
             => NanoBareR r -> IO (Either (F.FixResult E.Error) (NanoSSAR r))
----------------------------------------------------------------------------------
ssaTransform p = return . execute p . ssaNano $ p 

-- | `ssaNano` Perfroms SSA transformation of the input program. The output
-- program is patched (annotated per AST) with information about:
-- * SSA-phi nodes
-- * Spec annotations (functions, global variable declarations)
-- * Type annotations (variable declarations (?), class elements)
----------------------------------------------------------------------------------
ssaNano :: Data r => NanoBareR r -> SSAM r (NanoSSAR r)
----------------------------------------------------------------------------------
ssaNano p@(Nano { code = Src fs })
  = do setGlobs $ allGlobs
       setMeas  $ S.fromList $ F.symbol <$> envIds (consts p)
       withAssignability ReadOnly ros         $ 
         withAssignability WriteLocal wls     $ 
           withAssignability WriteGlobal wgs  $ 
            do  (_,fs')  <- ssaStmts fs
                ssaAnns  <- getAnns
                ast_cnt  <- getAstCount
                return    $ p { code   = Src $ (patch ssaAnns <$>) <$> fs' 
                              , max_id = ast_cnt }
    where
      allGlobs              = I.fromList  $ getAnnotation  <$> fmap ann_id <$> writeGlobalVars fs
      (ros, wgs, wls)       = variablesInScope allGlobs fs
      patch ms (Ann i l fs) = Ann i l (fs ++ IM.findWithDefault [] i ms)


---------------------------------------------------------------------------------------
variablesInScope :: Data r => I.IntSet -> [Statement (AnnSSA r)] -> ([Var r], [Var r], [Var r])
---------------------------------------------------------------------------------------
variablesInScope gs fs = (ros, wgs, wls)
  where
    vs            = hoistVarDecls fs
    (wgs, wls)    = L.partition ((`I.member` gs) . ann_id . getAnnotation) vs 
    ros           = hoistReadOnly fs

-------------------------------------------------------------------------------------
ssaFun :: Data r => AnnSSA r -> [Var r] -> [Statement (AnnSSA r)] -> SSAM r [Statement (AnnSSA r)]
-------------------------------------------------------------------------------------
ssaFun l xs body
  = do  θ <- getSsaEnv
        glbs <- getGlobs
        let (ros, wgs, wls) = (`variablesInScope` body) glbs
        withAssignability ReadOnly (ssaEnvIds θ)   $                 -- Variables from OUTER scope are UNASSIGNABLE
          withAssignability ReadOnly ros        $ 
            withAssignability WriteGlobal wgs   $ 
              withAssignability WriteLocal wls  $ 
            do  arg         <- argId    <$> freshenAnn l
                ret         <- returnId <$> freshenAnn l
                setSsaEnv    $ extSsaEnv (arg: ret : xs) θ  -- Extend SsaEnv with formal binders
                (_, body')  <- ssaStmts body                -- Transform function
                setSsaEnv θ                                 -- Restore Outer SsaEnv
                return        $ body'

-------------------------------------------------------------------------------------
ssaSeq :: (a -> SSAM r (Bool, a)) -> [a] -> SSAM r (Bool, [a])
-------------------------------------------------------------------------------------
ssaSeq f            = go True
  where
    go False zs     = return (False, zs)
    go b     []     = return (b    , [])
    go True  (x:xs) = do (b , y)  <- f x
                         (b', ys) <- go b xs
                         return      (b', y:ys)

-------------------------------------------------------------------------------------
ssaStmts :: Data r => [Statement (AnnSSA r)] -> SSAM r (Bool, [Statement (AnnSSA r)])
-------------------------------------------------------------------------------------------
ssaStmts ss = mapSnd flattenBlock <$> ssaSeq ssaStmt ss


-----------------------------------------------------------------------------------
ssaStmt :: Data r => Statement (AnnSSA r) -> SSAM r (Bool, Statement (AnnSSA r))
-------------------------------------------------------------------------------------
-- skip
ssaStmt s@(EmptyStmt _) 
  = return (True, s)

-- declare function foo(...): T;
ssaStmt s@(FuncAmbDecl _ _ _) 
  = return (True, s)

ssaStmt s@(FuncOverload _ _ _) 
  = return (True, s)

-- interface IA<V> extends IB<T> { ... }
ssaStmt s@(IfaceStmt _ _) 
  = return (True, s)

-- x = e
ssaStmt (ExprStmt l1 (AssignExpr l2 OpAssign (LVar l3 v) e)) = do
    let x        = Id l3 v
    (s, x', e') <- ssaAsgn l2 x e
    return         (True, prefixStmt l1 s $ ssaAsgnStmt l1 l2 x x' e')

-- e
ssaStmt (ExprStmt l e) = do
    (s, e') <- ssaExpr e
    return (True, prefixStmt l s $ ExprStmt l e')

-- s1;s2;...;sn
ssaStmt (BlockStmt l stmts) = do
    (b, stmts') <- ssaStmts stmts
    return (b,  BlockStmt l $ flattenBlock stmts')

-- if b { s1 }
ssaStmt (IfSingleStmt l b s)
  = ssaStmt (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
ssaStmt (IfStmt l e s1 s2) = do
    (se, e')     <- ssaExpr e
    θ            <- getSsaEnv
    φ            <- getSsaEnvGlob
    (θ1, s1')    <- ssaWith θ φ ssaStmt s1
    (θ2, s2')    <- ssaWith θ φ ssaStmt s2
    (phis, θ', φ1, φ2) <- envJoin l θ1 θ2
    case θ' of 
      Just θ''   -> do  setSsaEnv     $ θ''
                        -- FIX THE JOINED TYPE BY DOING AN EXTRA ASSIGNMENT 
                        -- THAT WILL BECOME AN UPCAST LATER
                        latest       <- catMaybes <$> mapM findSsaEnv phis
                        new          <- mapM (updSsaEnv l) phis
                        
                        addAnn l      $ PhiPost $ zip3 phis latest new 

                        let stmt'     = -- postfixStmt l [postVstmt] 
                                        prefixStmt l se
                                      $ IfStmt l e' (splice s1' φ1) (splice s2' φ2)
                        return        $ (True,  stmt')

      Nothing    ->     let stmt'     = prefixStmt l se
                                      $ IfStmt l e' (splice s1' φ1) (splice s2' φ2) in
                        return (False, stmt')
  {- where
    dbg (Just θ) = trace ("SSA ENV: " ++ ppshow θ) (Just θ) -}

-- while c { b }
ssaStmt (WhileStmt l cnd body)
  = do  (xs, x0s)         <- unzip . map (\(x, (SI xo,_)) -> (x, xo)) <$> getLoopPhis body

        xs'               <- mapM freshenIdSSA xs
        as                <- mapM getAssignability xs'
        let (l1s, l0s,_)  = unzip3 $ filter ((== WriteLocal) . thd3) (zip3 xs' x0s as)

        -- SSA only the WriteLocal variables - globals will remain the same.
       
        l1s'              <- mapM (updSsaEnv l) l1s
        θ1                <- getSsaEnv
        (sc, cnd')        <- ssaExpr cnd
        when (not $ null sc) (ssaError $ errorUpdateInExpr (srcPos l) cnd)
        (t, body')        <- ssaStmt body
        θ2                <- getSsaEnv

        -- SSA only the WriteLocal variables - globals will remain the same.
       
        let l2s            = [ x2 | (Just (SI x2), WriteLocal) <- mapFst (`envFindTy` θ2) <$> zip xs as ]
        addAnn l           $ PhiVar l1s'
        setSsaEnv          $ θ1
        l'                <- freshenAnn l
        let body''         = body' `splice` asgn l' (mkNextId <$> l1s') l2s
        l''               <- freshenAnn l
        return             $ (t, asgn l'' l1s' l0s `presplice` WhileStmt l cnd' body'')
    where
        asgn _  [] _       = Nothing
        asgn l' ls rs      = Just $ BlockStmt l' $ zipWith (mkPhiAsgn l') ls rs


ssaStmt (ForStmt _  NoInit _ _ _ )     =
    errorstar "unimplemented: ssaStmt-for-01"

ssaStmt (ForStmt l v cOpt (Just (UnaryAssignExpr l1 o lv)) b) =
    ssaStmt (ForStmt l v cOpt (Just $ AssignExpr l1 (op o) lv (IntLit l1 1)) b)
  where
    op PrefixInc   = OpAssignAdd
    op PrefixDec   = OpAssignSub
    op PostfixInc  = OpAssignAdd
    op PostfixDec  = OpAssignSub

ssaStmt (ForStmt l (VarInit vds) cOpt (Just e@(AssignExpr l1 _ _ _)) b) =
    ssaForLoop l vds cOpt (Just $ ExprStmt l1 (expand e)) b
  where
    expand (AssignExpr l1 o lv e) = AssignExpr l1 OpAssign lv (infOp o l1 lv e)
    expand _ = errorstar "unimplemented: expand assignExpr"

ssaStmt (ForStmt l (VarInit vds) cOpt (Just i) b) =
    ssaForLoop l vds cOpt (Just $ ExprStmt (getAnnotation i) i) b

ssaStmt (ForStmt l (VarInit vds) cOpt Nothing  b) =
    ssaForLoop l vds cOpt Nothing b


ssaStmt (ForStmt l (ExprInit ei) cOpt (Just e@(AssignExpr l1 _ _ _)) b) =
    ssaForLoopExpr l ei cOpt (Just $ ExprStmt l1 (expand e)) b
  where
    expand (AssignExpr l1 o lv e) = AssignExpr l1 OpAssign lv (infOp o l1 lv e)
    expand _ = errorstar "unimplemented: expand assignExpr"

ssaStmt (ForStmt l (ExprInit e) cOpt (Just i) b) =
    ssaForLoopExpr l e cOpt (Just $ ExprStmt (getAnnotation i) i) b

ssaStmt (ForStmt l (ExprInit e) cOpt Nothing  b) =
    ssaForLoopExpr l e cOpt Nothing b


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

ssaStmt (ForInStmt l (ForInVar v) e b) =
    do  init_  <- initArr
        for_   <- forStmt
        ssaStmt $ BlockStmt l [init_, for_]
  where
    fr_         = freshenAnn
    fr          = fr_ l 
    biForInKeys = return $ builtinId "BIForInKeys"

    initArr     = vStmt        $  VarDecl <$> fr 
                                          <*> keysArr 
                                          <*> (Just <$> (CallExpr <$> fr 
                                                                  <*> (VarRef <$> fr <*> biForInKeys)
                                                                  <*> (return [e])))
    initIdx     = VarDecl     <$> fr 
                              <*> keysIdx 
                              <*> (Just      <$> (IntLit  <$> fr <*> return 0))
    condition   = Just        <$> (InfixExpr <$> fr 
                                             <*> return OpLT 
                                             <*> (VarRef  <$> fr <*> keysIdx) 
                                             <*> (DotRef  <$> fr 
                                                          <*> (VarRef <$> fr <*> keysArr)
                                             <*> (Id      <$> fr 
                                                          <*> return "length")))
    increment   = Just        <$> (UnaryAssignExpr       
                                             <$> fr
                                             <*> return PostfixInc 
                                             <*> (LVar    <$> fr 
                                                          <*> (unId <$> keysIdx)))
    accessKeys  = vStmt        $   VarDecl <$> fr 
                                           <*> return v 
                                           <*> (Just <$> (BracketRef <$> fr 
                                                                     <*> (VarRef <$> fr <*> keysArr) 
                                                                     <*> (VarRef <$> fr <*> keysIdx)))
    forStmt     = ForStmt     <$> fr 
                              <*> (VarInit <$> single <$> initIdx) 
                              <*> condition 
                              <*> increment 
                              <*> (BlockStmt <$> fr 
                                             <*> ( (:[b]) <$> accessKeys))

    vStmt v     = VarDeclStmt <$> fr <*> (single <$> v)

    keysArr     = return $ mkKeysId    v 
    keysIdx     = return $ mkKeysIdxId v

    mkId s      = Id (Ann def def def) s
    builtinId s = mkId ("builtin_" ++ s)



-- var x1 [ = e1 ]; ... ; var xn [= en];
ssaStmt (VarDeclStmt l ds) = do
    stvds' <- mapM ssaVarDecl ds
    return    (True, mkStmt $ foldr crunch ([], []) stvds') 
  where
    crunch ([], d) (ds, ss') = (d:ds, ss')
    crunch (ss, d) (ds, ss') = ([]  , mkStmts l ss (d:ds) ss')
    mkStmts l ss ds ss'      = ss ++ VarDeclStmt l ds : ss'
    mkStmt (ds', [] )        = VarDeclStmt l ds'
    mkStmt (ds', ss')        = BlockStmt l $ mkStmts l [] ds' ss'

-- return e
ssaStmt s@(ReturnStmt _ Nothing)
  = return (False, s)

-- return e
ssaStmt (ReturnStmt l (Just e)) = do
    (s, e') <- ssaExpr e
    return (False, prefixStmt l s $ ReturnStmt l (Just e'))

-- throw e
ssaStmt (ThrowStmt l e) = do
    (s, e') <- ssaExpr e
    return (False, prefixStmt l s $ ThrowStmt l e')


-- function f(...){ s }
ssaStmt (FunctionStmt l f xs bd)
  = (True,) <$> FunctionStmt l f xs <$> (ssaFun l xs bd)

-- switch (e) { ... }
ssaStmt (SwitchStmt l e xs)
  = do
      id <- updSsaEnv (an e) (Id (an e) "__switchVar")
      let go (l, e, s) i = IfStmt (an s) (InfixExpr l OpStrictEq (VarRef l id) e) s i
      mapSnd (BlockStmt l) <$> ssaStmts
        [ VarDeclStmt (an e) [VarDecl (an e) id (Just e)], foldr go z sss ]
  where
      an                   = getAnnotation
      sss                  = [ (l, e, BlockStmt l $ remBr ss) | CaseClause l e ss <- xs ]
      z                    = headWithDefault (EmptyStmt l) [BlockStmt l $ remBr ss | CaseDefault l ss <- xs]

      remBr                = filter (not . isBr) . flattenBlock
      isBr (BreakStmt _ _) = True
      isBr _               = False
      headWithDefault a [] = a
      headWithDefault _ xs = head xs

-- class A extends B implements I,J,... { ... }
--
--  FIXME: fix env here.
--
ssaStmt (ClassStmt l n e is bd)
  = do  bd' <- mapM (ssaClassElt fields) $ ctor ++ bd
        return (True, ClassStmt l n e is bd')
  where
    -- Only gather non-static fields
    fields = [(i,eo)  | MemberVarDecl _ False i eo <- bd] 
    ctor   | null [() | Constructor{} <- bd ] = [Constructor l [] []]
           | otherwise                        = []

--
--  FIXME: fix env here.
--
ssaStmt (ModuleStmt l n body)
  = do  θ <- getSsaEnv
        (ros, wgs, wls) <- (`variablesInScope` body) <$> getGlobs
        withAssignability ReadOnly (ssaEnvIds θ)   $           -- Variables from OUTER scope are UNASSIGNABLE
          withAssignability ReadOnly ros        $ 
            withAssignability WriteGlobal wgs   $ 
              withAssignability WriteLocal wls  $ 
            do  (_, body')   <- ssaStmts body               -- Transform function
                setSsaEnv θ                                 -- Restore Outer SsaEnv
                return        $ (True, ModuleStmt l n body')

ssaStmt (EnumStmt l n es) 
  = return (True, EnumStmt l n es) 

-- OTHER (Not handled)
ssaStmt s
  = convertError "ssaStmt" s


postfixIfStmt l (x, Just rhs) 
  = VarDecl <$> freshenAnn l 
            <*> updSsaEnv l x 
            <*> (Just <$> (VarRef <$> freshenAnn l <*> return rhs))


ssaAsgnStmt l1 l2 x@(Id l3 v) x' e' 
  | x == x'   = ExprStmt l1    (AssignExpr l2 OpAssign (LVar l3 v) e')
  | otherwise = VarDeclStmt l1 [VarDecl l2 x' (Just e')]

-------------------------------------------------------------------------------------
ctorVisitor :: Data r => AnnSSA r -> [Id (AnnSSA r)] -> VisitorM (SSAM r) () () (AnnSSA r)
-------------------------------------------------------------------------------------
ctorVisitor l ms          = defaultVisitor { mStmt = ts } { mExpr = te }
  where
    ss                    = S.fromList $ F.symbol <$> ms

    te ae@(AssignExpr la OpAssign (LDot ld (ThisRef _) s) e)
      | F.symbol s `S.member` ss
                          = AssignExpr <$> fr_ la 
                                       <*> return OpAssign 
                                       <*> (LVar <$> fr_ ld <*> return (mkCtorStr s))
                                       <*> return e
      | otherwise         = return ae
    te lv                 = return lv

    ts r@(ReturnStmt l _) = BlockStmt <$> fr <*> ((++ [r]) <$> mapM (ctorExit l) ms)
    ts r                  = return $ r

    fr_                   = freshenAnn
    fr                    = fr_ l 

ctorExit l s = ExprStmt <$> fr <*> 
                 (AssignExpr <$> fr 
                             <*> return OpAssign
                             <*> (LDot   <$> fr 
                                         <*> (ThisRef <$> fr) 
                                         <*> return (F.symbolString $ F.symbol s))
                             <*> (VarRef <$> fr 
                                         <*> ((`mkCtorId` s) <$> fr)))
  where
    fr                    = freshenAnn l 

-------------------------------------------------------------------------------------
ssaClassElt :: Data r 
            => [(Id (AnnSSA r), Maybe (Expression (AnnSSA r)))] 
            -> ClassElt (AnnSSA r) 
            -> SSAM r (ClassElt (AnnSSA r))
-------------------------------------------------------------------------------------
ssaClassElt flds (Constructor l xs bd0)
  = do θ <- getSsaEnv
       (ros, wgs, wls) <- (`variablesInScope` bd0) <$> getGlobs
       withAssignability ReadOnly (ssaEnvIds θ) $               -- Variables from OUTER scope are NON-ASSIGNABLE
          withAssignability ReadOnly ros        $               -- ReadOnly in scope
            withAssignability WriteGlobal wgs   $               -- Globals in scope
              withAssignability WriteLocal wls  $               -- Locals in scope
         do setSsaEnv     $ extSsaEnv xs θ                      -- Extend SsaEnv with formal binders
            initStmts    <- mapM initStmt fldNms
            bd1          <- visitStmtsT (ctorVisitor l fldNms) () bd0
            exitStmts    <- mapM (ctorExit l) fldNms
            (_, bd2)     <- ssaStmts $ initStmts ++ bd1 ++ exitStmts  -- Transform function
            setSsaEnv θ                                         -- Restore Outer SsaEnv
            return        $ Constructor l xs bd2
  where
    fldNms      = fst <$> flds
    initStmt s  = VarDeclStmt <$> fr <*> (single <$> initVd s)
    initVd i    = case [ eo | (v, eo) <- flds, i == v ] of
                    [Just e] -> VarDecl <$> fr
                                        <*> return (mkCtorId l i) 
                                        <*> return (Just e)
                    [_]      -> VarDecl <$> fr
                                        <*> return (mkCtorId l i) 
                                        <*> (Just <$> (VarRef <$> fr_ l 
                                                              <*> (Id <$> fr_ l 
                                                              <*> return "undefined")))
                    _        -> ssaError $ bugSSAConstructorInit (srcPos l) 
    fr_         = freshenAnn
    fr          = fr_ l 

-- | Initilization expression for instance variables is moved to the beginning 
--   of the constructor.
ssaClassElt _ (MemberVarDecl l False x _) = return $ MemberVarDecl l False x Nothing

ssaClassElt _ (MemberVarDecl l True x (Just e))
  = do z <- ssaExpr e
       case z of 
         ([], e') -> return $ MemberVarDecl l True x (Just e')
         _        -> ssaError $ errorEffectInFieldDef (srcPos l)

ssaClassElt _ (MemberVarDecl l True x Nothing)
  = ssaError $ errorUninitStatFld (srcPos l) x

ssaClassElt _ (MemberMethDef l s e xs body)
  = do θ <- getSsaEnv
       (ros, wgs, wls) <- (`variablesInScope` body) <$> getGlobs
       withAssignability ReadOnly (ssaEnvIds θ) $               -- Variables from OUTER scope are NON-ASSIGNABLE
          withAssignability ReadOnly ros        $               -- ReadOnly in scope
            withAssignability WriteGlobal wgs   $               -- Globals in scope
              withAssignability WriteLocal wls  $               -- Locals in scope
         do setSsaEnv     $ extSsaEnv ((returnId l) : xs) θ     -- Extend SsaEnv with formal binders
            (_, body')   <- ssaStmts body                       -- Transform function
            setSsaEnv θ                                         -- Restore Outer SsaEnv
            return        $ MemberMethDef l s e xs body'
ssaClassElt _ m@(MemberMethDecl _ _ _ _ ) = return m

infOp OpAssign         _ _  = id
infOp OpAssignAdd      l lv = InfixExpr l OpAdd      (lvalExp lv)
infOp OpAssignSub      l lv = InfixExpr l OpSub      (lvalExp lv)
infOp OpAssignMul      l lv = InfixExpr l OpMul      (lvalExp lv)
infOp OpAssignDiv      l lv = InfixExpr l OpDiv      (lvalExp lv)
infOp OpAssignMod      l lv = InfixExpr l OpMod      (lvalExp lv)
infOp OpAssignLShift   l lv = InfixExpr l OpLShift   (lvalExp lv)
infOp OpAssignSpRShift l lv = InfixExpr l OpSpRShift (lvalExp lv)
infOp OpAssignZfRShift l lv = InfixExpr l OpZfRShift (lvalExp lv)
infOp OpAssignBAnd     l lv = InfixExpr l OpBAnd     (lvalExp lv)
infOp OpAssignBXor     l lv = InfixExpr l OpBXor     (lvalExp lv)
infOp OpAssignBOr      l lv = InfixExpr l OpBOr      (lvalExp lv)

lvalExp (LVar l s)          = VarRef l (Id l s)
lvalExp (LDot l e s)        = DotRef l e (Id l s)
lvalExp (LBracket l e1 e2)  = BracketRef l e1 e2

-------------------------------------------------------------------------------------
presplice :: Maybe (Statement (AnnSSA r)) -> Statement (AnnSSA r) -> Statement (AnnSSA r)
-------------------------------------------------------------------------------------
presplice z s' = splice_ (getAnnotation s') z (Just s')

-------------------------------------------------------------------------------------
splice :: Statement a -> Maybe (Statement a) -> Statement a 
-------------------------------------------------------------------------------------
splice s z = splice_ (getAnnotation s) (Just s) z

splice_ l Nothing Nothing    = EmptyStmt l
splice_ _ (Just s) Nothing   = s
splice_ _ Nothing (Just s)   = s
splice_ _ (Just s) (Just s') = seqStmt (getAnnotation s) s s'


seqStmt _ (BlockStmt l s) (BlockStmt _ s') = BlockStmt l (s ++ s')
seqStmt l s s'                             = BlockStmt l [s, s']

-------------------------------------------------------------------------------------
prefixStmt :: a -> [Statement a] -> Statement a -> Statement a 
-------------------------------------------------------------------------------------
prefixStmt _ [] s = s 
prefixStmt l ss s = BlockStmt l $ flattenBlock $ ss ++ [s]

-------------------------------------------------------------------------------------
postfixStmt :: a -> [Statement a] -> Statement a -> Statement a 
-------------------------------------------------------------------------------------
postfixStmt _ [] s = s 
postfixStmt l ss s = BlockStmt l $ flattenBlock $ [s] ++ ss


-------------------------------------------------------------------------------------
flattenBlock :: [Statement t] -> [Statement t]
-------------------------------------------------------------------------------------
flattenBlock = concatMap f
  where
    f (BlockStmt _ ss) = ss
    f s                = [s ]

-------------------------------------------------------------------------------------
ssaWith :: SsaEnv r         -- Local
        -> SsaEnv r         -- Global
        -> (a -> SSAM r (Bool, b)) 
        -> a 
        -> SSAM r (Maybe (SsaEnv r, SsaEnv r), b)
-------------------------------------------------------------------------------------
ssaWith θ φ f x = do
  setSsaEnv θ
  setSsaEnvGlob φ
  (b, x') <- f x
  (, x')  <$> (if b then Just <$> ((,) <$> getSsaEnv <*> getSsaEnvGlob) else return Nothing)


-------------------------------------------------------------------------------------
ssaExpr ::  Data r => Expression (AnnSSA r) -> SSAM r ([Statement (AnnSSA r)], Expression (AnnSSA r))
-------------------------------------------------------------------------------------

ssaExpr e@(IntLit _ _)
  = return ([], e)

ssaExpr e@(BoolLit _ _)
  = return ([], e)

ssaExpr e@(StringLit _ _)
  = return ([], e)

ssaExpr e@(NullLit _)
  = return ([], e)

ssaExpr e@(ThisRef _)
  = return ([], e)

ssaExpr e@(SuperRef _)
  = return ([], e)

ssaExpr   (ArrayLit l es)
  = ssaExprs (ArrayLit l) es

ssaExpr (VarRef l x)
  = ([],) <$> ssaVarRef l x

ssaExpr (CondExpr l c e1 e2)
  = do (sc, c') <- ssaExpr c
       θ        <- getSsaEnv
       φ        <- getSsaEnvGlob
       e1'      <- ssaPureExprWith θ φ e1
       e2'      <- ssaPureExprWith θ φ e2
       return (sc, CondExpr l c' e1' e2')

ssaExpr (PrefixExpr l o e)
  = ssaExpr1 (PrefixExpr l o) e

ssaExpr (InfixExpr l OpLOr e1 e2)
  = do  l' <- freshenAnn l 
        vid <- Id <$> freshenAnn l <*> (return $ "__InfixExpr_OpLOr_" ++ show (ann_id l'))
        vr  <- VarRef <$> freshenAnn l <*> return vid
        vd  <- VarDecl <$> freshenAnn l <*> return vid <*> return (Just e1)
        vs  <- VarDeclStmt <$> freshenAnn l <*> return [vd]
        (_, vs') <- ssaStmt vs
        (ss,e') <- ssaExpr (CondExpr l vr vr e2) 
        return  $ (vs':ss, e')
 
ssaExpr (InfixExpr l o e1 e2)
  = ssaExpr2 (InfixExpr l o) e1 e2

ssaExpr (CallExpr l e es)
  = ssaExprs (\es' -> CallExpr l (head es') (tail es')) (e : es)

ssaExpr (ObjectLit l ps)
  = ssaExprs (ObjectLit l . zip fs) es
  where
    (fs, es) = unzip ps

ssaExpr (DotRef l e i)
  = ssaExpr1 (\e' -> DotRef l e' i) e 

ssaExpr (BracketRef l e1 e2)
  = ssaExpr2 (BracketRef l) e1 e2

ssaExpr (NewExpr l e es)
  = ssaExprs(\es' -> NewExpr l (head es') (tail es')) (e:es) 

ssaExpr (Cast l e)
  = ssaExpr1 (Cast l) e

ssaExpr (FuncExpr l fo xs bd)
  = ([],) . FuncExpr l fo xs <$> ssaFun l xs bd

-- x = e
ssaExpr (AssignExpr l OpAssign (LVar lx v) e)
  = ssaAsgnExpr l lx (Id lx v) e
       
-- e1.f = e2
ssaExpr (AssignExpr l OpAssign (LDot ll e1 f) e2)
  = ssaExpr2 (\e1' e2' -> AssignExpr l OpAssign (LDot ll e1' f) e2') e1 e2

-- e1[e2] = e3
ssaExpr (AssignExpr l OpAssign (LBracket ll e1 e2) e3)
  = ssaExpr3 (\e1' e2' e3' -> AssignExpr l OpAssign (LBracket ll e1' e2') e3') e1 e2 e3

-- lv += e
ssaExpr (AssignExpr l op lv e)
  = ssaExpr (AssignExpr l OpAssign lv rhs)
  where
    rhs = InfixExpr l (assignInfix op) (lvalExp lv) e
        
-- x++
ssaExpr (UnaryAssignExpr l uop (LVar lv v))
  = do let x           = Id lv v
       xOld           <- ssaVarRef l x
       xNew           <- updSsaEnv l x
       let (eIn, eOut) = unaryExprs l uop xOld (VarRef l xNew)
       return  ([ssaAsgnStmt l lv x xNew eIn], eOut)

-- lv++
ssaExpr (UnaryAssignExpr l uop lv)
  = do lv'   <- ssaLval lv
       let e' = unaryExpr l uop (lvalExp lv')
       return ([], AssignExpr l OpAssign lv' e')
       
ssaExpr e
  = convertError "ssaExpr" e

ssaAsgnExpr l lx x e
  = do (s, x', e') <- ssaAsgn l x e
       return         (s ++ [ssaAsgnStmt l lx x x' e'], e')

-----------------------------------------------------------------------------
ssaLval    ::  Data r => LValue (AnnSSA r) -> SSAM r (LValue (AnnSSA r))
-----------------------------------------------------------------------------
ssaLval (LVar lv v)
  = do VarRef _ (Id _ v') <- ssaVarRef lv (Id lv v)
       return (LVar lv v')

ssaLval (LDot ll e f)
  = (\e' -> LDot ll e' f) <$> ssaPureExpr e

ssaLval (LBracket ll e1 e2)
  = LBracket ll <$> ssaPureExpr e1 <*> ssaPureExpr e2
  

--------------------------------------------------------------------------
-- | Helpers for gathering assignments for purifying effectful-expressions
--------------------------------------------------------------------------
 
ssaExprs f es       = (concat *** f) . unzip <$> mapM ssaExpr es

ssaExpr1 = case1 ssaExprs -- (\case [e'] -> f e') [e]
ssaExpr2 = case2 ssaExprs -- (\case [e1', e2'] -> f e1' e2') [e1, e2]
ssaExpr3 = case3 ssaExprs -- (\case [e1', e2', e3'] -> f e1' e2' e3') [e1, e2, e3]
ssaPureExprWith θ φ e = snd <$> ssaWith θ φ (fmap (True,) . ssaPureExpr) e

ssaPureExpr e = do
  (s, e') <- ssaExpr e
  case s of
    []     -> return e'
    _      -> ssaError $ errorUpdateInExpr (srcPos e) e 

--------------------------------------------------------------------------
-- | Dealing with Generic Assignment Expressions
--------------------------------------------------------------------------
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


--------------------------------------------------------------------------
-- | Dealing with Unary Expressions
--------------------------------------------------------------------------
       
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


-------------------------------------------------------------------------------------
ssaVarDecl :: Data r => VarDecl (AnnSSA r) -> SSAM r ([Statement (AnnSSA r)], VarDecl (AnnSSA r))
-------------------------------------------------------------------------------------
ssaVarDecl (VarDecl l x (Just e)) = do
    (s, x', e') <- ssaAsgn l x e
    return    (s, VarDecl l x' (Just e'))

ssaVarDecl (VarDecl l x Nothing) = do
    x' <- updSsaEnv l x
    return    ([], VarDecl l x' Nothing)

------------------------------------------------------------------------------------------
ssaVarRef ::  Data r => AnnSSA r -> Id (AnnSSA r) -> SSAM r (Expression (AnnSSA r))
------------------------------------------------------------------------------------------
ssaVarRef l x
  = do getAssignability x >>= \case
         WriteGlobal -> return e
         ReadOnly    -> maybe e  (VarRef l) <$> findSsaEnv x
         WriteLocal  -> findSsaEnv x >>= \case
             Just t  -> return   $ VarRef l t
             Nothing -> return   $ VarRef l x -- ssaError $ errorSSAUnboundId (srcPos x) x
         ImportDecl  -> ssaError $ errorSSAUnboundId (srcPos x) x
         ReturnVar   -> ssaError $ errorSSAUnboundId (srcPos x) x
         ThisVar     -> ssaError $ errorSSAUnboundId (srcPos x) x
    where
       e = VarRef l x
 

------------------------------------------------------------------------------------
ssaAsgn :: Data r => AnnSSA r -> Id (AnnSSA r) -> Expression (AnnSSA r) ->
           SSAM r ([Statement (AnnSSA r)], Id (AnnSSA r), Expression (AnnSSA r))
------------------------------------------------------------------------------------
ssaAsgn l x e  = do
    (s, e') <- ssaExpr e
    x'      <- updSsaEnv l x
    return (s, x', e')


-------------------------------------------------------------------------------------
-- envJoin :: Data r => AnnSSA r -> Maybe (SsaEnv r) -> Maybe (SsaEnv r)
--         -> SSAM r ( Maybe (SsaEnv r),
--                     Maybe (Statement (AnnSSA r)),
--                     Maybe (Statement (AnnSSA r)))
-------------------------------------------------------------------------------------
envJoin _ Nothing Nothing     = return ([], Nothing, Nothing, Nothing)
envJoin _ Nothing (Just θ)    = return ([], Just $ fst θ , Nothing, Nothing)
envJoin _ (Just θ) Nothing    = return ([], Just $ fst θ , Nothing, Nothing)
envJoin l (Just θ1) (Just θ2) = envJoin' l θ1 θ2

envJoin' l (θ1,φ1) (θ2,φ2) = do
    setSsaEnv θ'                          -- Keep Common binders
    phis       <- mapM (mapFstM freshenIdSSA) (envToList $ envLefts θ)
    gl_phis    <- mapM (mapFstM freshenIdSSA) (envToList $ envLefts φ)
    stmts      <- forM (phis ++ gl_phis) $ phiAsgn l   -- Adds Phi-Binders, Phi Annots, Return Stmts
    θ''        <- getSsaEnv
    let (s1,s2) = unzip stmts
    return (fst <$> phis, Just θ'', Just $ BlockStmt l s1, Just $ BlockStmt l s2)
  where
    φ           = envIntersectWith meet φ1 φ2
    θ           = envIntersectWith meet θ1 θ2
    θ'          = envRights θ
    meet        = \x1 x2 -> if x1 == x2 then Right x1 else Left (x1, x2)

phiAsgn l (x, (SI x1, SI x2)) = do
    (a,x') <- updSsaEnv' l x                       -- Generate FRESH phi name
    addAnn l (PhiVar [x'])                         -- RECORD x' as PHI-Var at l
    case a of 
      WriteLocal -> let s1 = mkPhiAsgn l x' x1 in  -- Create Phi-Assignments
                    let s2 = mkPhiAsgn l x' x2 in  -- for both branches
                    return (s1, s2)
      _          -> return (EmptyStmt def, EmptyStmt def)

mkPhiAsgn :: AnnSSA r -> Id (AnnSSA r) -> Id (AnnSSA r) -> Statement (AnnSSA r) 
mkPhiAsgn l x y = VarDeclStmt l [VarDecl l x (Just $ VarRef l y)]


-- Get the phi vars starting from an SSAEnv @θ@ and going through the statement @b@.
-- getLoopPhis :: Statement (AnnSSA r) -> SSAM r [Var r]
getLoopPhis b = do
    θ     <- getSsaEnv
    θ'    <- names <$> snd <$> tryAction (ssaStmt b)
    let xs = envToList (envLefts $ envIntersectWith meet θ θ')
    return xs
  where
    meet x x' = if x == x' then Right x else Left (x, x')


-------------------------------------------------------------------------------------
ssaForLoop :: Data r => (AnnSSA r) -> [VarDecl (AnnSSA r)] -> Maybe (Expression (AnnSSA r)) 
  -> Maybe (Statement (AnnSSA r)) -> Statement (AnnSSA r) -> SSAM r (Bool, Statement (AnnSSA r))
-------------------------------------------------------------------------------------
ssaForLoop l vds cOpt incExpOpt b =
  do
    (b, sts') <- ssaStmts sts
    return     $ (b, BlockStmt l sts')
  where
    sts        = [VarDeclStmt l vds, WhileStmt l c bd]
    bd         = BlockStmt bl $ [b] ++ catMaybes [incExpOpt]
    bl         = getAnnotation b
    c          = maybe (BoolLit l True) id cOpt

-------------------------------------------------------------------------------------
ssaForLoopExpr :: Data r => (AnnSSA r) -> Expression (AnnSSA r) -> Maybe (Expression (AnnSSA r)) 
  -> Maybe (Statement (AnnSSA r)) -> Statement (AnnSSA r) -> SSAM r (Bool, Statement (AnnSSA r))
-------------------------------------------------------------------------------------
ssaForLoopExpr l exp cOpt incExpOpt b =
  do
    l1        <- fr
    (b, sts') <- ssaStmts [ExprStmt l1 exp, WhileStmt l c bd]
    l2        <- fr
    return     $ (b, BlockStmt l2 sts')
  where
    bd         = BlockStmt bl $ [b] ++ catMaybes [incExpOpt]
    bl         = getAnnotation b
    c          = maybe (BoolLit l True) id cOpt
    fr_        = freshenAnn
    fr         = fr_ l 


-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
