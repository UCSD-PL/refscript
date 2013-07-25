{- LANGUAGE TypeSynonymInstances #-}
{- LANGUAGE FlexibleInstances    #-}
{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE ScopedTypeVariables  #-}

--
-- | Module that implements a visitor over the language ecmascript AST.
--
-- The implementation is heavily based on the one in CIL: 
--     Copyright (c) 2001-2002, 
--      *  George C. Necula    <necula@cs.berkeley.edu>
--      *  Scott McPeak        <smcpeak@cs.berkeley.edu>
--      *  Wes Weimer          <weimer@cs.berkeley.edu>
--  which can be found here: https://github.com/kerneis/cil
--  with several simplifications, e.g. no optimizations have been implemented in
--  this version.
--

module Language.Nano.Visitor.Visitor (
    NanoVisitor(..)
  , VisitAction(..)
  , visitProgram 
  , visitStatement 
  , visitExpression 
  , visitCaseClause 
  , visitCatchClause 
  , visitId 
  , visitForInit 
  , visitJavaScript 
  , visitInfixOp 
  , visitAssignOp 
  , visitUnaryAssignOp 
  , visitPrefixOp 
  , visitProp 
  , visitLValue 
  , visitVarDecl 
  , visitForInInit 

  , defaultVisitor

  , NanoVisitorM(..)
  , VisitActionM(..)
  , runVisitProgramM       
  , runVisitJavascriptM    
  , runVisitExpressionM    
  , runVisitStatementM     
  , runVisitCaseClauseM    
  , runVisitCatchClauseM   
  , runVisitIdM            
  , runVisitForInitM       
  , runVisitForInInitM     
  , runVisitVarDeclM       
  , runVisitPrefixOpM      
  , runVisitUnaryAssignOpM 
  , runVisitLValueM        
  , runVisitAssignOpM      
  , runVisitPropM          
  , runVisitInfixOpM       

  , defaultVisitorM

) where 

import           Control.Applicative            ((<$>))
import           Control.Monad.State
import           Language.Nano.Errors

import           Language.Nano.Typecheck.Types
import           Data.Traversable         as T
import           Language.ECMAScript3.Syntax

-------------------------------------------------------------------------------
-- | AST Visitor
-------------------------------------------------------------------------------

data VisitAction a =  SkipChildren 
                    | DoChildren
                    | ChangeTo a
                    | ChangeDoChildrenPost a  (a -> a)


data NanoVisitor a b = NV {
     vProgram       :: Nano a b      -> VisitAction (Nano a b)
  ,  vStatement     :: Statement a   -> VisitAction (Statement a)
  ,  vJavaScript    :: JavaScript a  -> VisitAction (JavaScript a)
  ,  vId            :: Id a          -> VisitAction (Id a)
  ,  vInfixOp       :: InfixOp       -> VisitAction InfixOp
  ,  vAssignOp      :: AssignOp      -> VisitAction AssignOp
  ,  vUnaryAssignOp :: UnaryAssignOp -> VisitAction UnaryAssignOp
  ,  vPrefixOp      :: PrefixOp      -> VisitAction PrefixOp
  ,  vProp          :: Prop a        -> VisitAction (Prop a)
  ,  vLValue        :: LValue a      -> VisitAction (LValue a)
  ,  vExpression    :: Expression a  -> VisitAction (Expression a)
  ,  vCaseClause    :: CaseClause a  -> VisitAction (CaseClause a)
  ,  vCatchClause   :: CatchClause a -> VisitAction (CatchClause a)
  ,  vVarDecl       :: VarDecl a     -> VisitAction (VarDecl a)
  ,  vForInit       :: ForInit a     -> VisitAction (ForInit a)
  ,  vForInInit     :: ForInInit a   -> VisitAction (ForInInit a)
  }


defaultVisitor = NV d d d d d d d d d d d d d d d d
  where
    d _ = DoChildren


-------------------------------------------------------------------------------
doVisit :: NanoVisitor a b -> VisitAction c -> (NanoVisitor a b -> c -> c) -> c -> c  
-------------------------------------------------------------------------------
doVisit  vis action children n = 
  case action of
    SkipChildren              -> n
    ChangeTo n'               -> n'
    DoChildren                -> children vis n
    ChangeDoChildrenPost n' f -> f $ children vis n'


-------------------------------------------------------------------------------
visitProgram :: NanoVisitor a b -> Nano a b -> Nano a b 
-------------------------------------------------------------------------------
visitProgram vis pgm = doVisit vis (vProgram vis pgm) ch pgm
  where ch vis p@(Nano {code = Src fs}) = p { code = Src $ visitStatement vis <$> fs }


-------------------------------------------------------------------------------
visitStatement :: NanoVisitor a b -> Statement a -> Statement a
-------------------------------------------------------------------------------
visitStatement vis st = doVisit vis (vStatement vis st) ch st
  where
    ch vis s = 
      let ve  = visitExpression  vis
          vs  = visitStatement   vis
          vc  = visitCaseClause  vis
          vcc = visitCatchClause vis
          vi  = visitId          vis
          vfi = visitForInit     vis
          vfii= visitForInInit   vis
          vd  = visitVarDecl     vis
      in case s of
        BlockStmt l sts         -> BlockStmt l $ vs <$> sts
        EmptyStmt l             -> EmptyStmt l
        ExprStmt l e            -> ExprStmt l $ ve e
        IfStmt l e s1 s2        -> IfStmt l (ve e) (vs s1) (vs s2)
        IfSingleStmt l e s      -> IfSingleStmt l (ve e) (vs s)
        SwitchStmt l e cs       -> SwitchStmt l (ve e) (vc <$> cs)
        WhileStmt l e s         -> WhileStmt l (ve e) (vs s)
        DoWhileStmt l s e       -> DoWhileStmt l (vs s) (ve e)
        BreakStmt l mid         -> BreakStmt l (vi <$> mid)
        ContinueStmt l mid      -> ContinueStmt l (vi <$> mid)
        LabelledStmt l id s     -> LabelledStmt l (vi id) (vs s)
        ForInStmt l fi e s      -> ForInStmt l (vfii fi) (ve e) (vs s)
        ForStmt l fii me1 me2 s -> ForStmt l (vfi fii) (ve <$> me1) (ve <$> me2) (vs s)
        TryStmt l s mcc ms      -> TryStmt l (vs s) (vcc <$> mcc) (vs <$> ms)
        ThrowStmt l e           -> ThrowStmt l $ ve e
        ReturnStmt l me         -> ReturnStmt l $ ve <$> me
        WithStmt l e s          -> WithStmt l (ve e) (vs s)
        VarDeclStmt l vds       -> VarDeclStmt l $ vd <$> vds
        FunctionStmt l i is ss  -> FunctionStmt l (vi i) (vi <$> is) (vs <$> ss)


-------------------------------------------------------------------------------
visitExpression :: NanoVisitor a b -> Expression a -> Expression a
-------------------------------------------------------------------------------
visitExpression vis e = doVisit vis (vExpression vis $ tracePP "vExpression" e) ch e
  where 
    ch vis e = 
      let ve   = visitExpression    vis
          vs   = visitStatement     vis
          vi   = visitId            vis
          vpo  = visitPrefixOp      vis
          vuao = visitUnaryAssignOp vis
          vlv  = visitLValue        vis
          vao  = visitAssignOp      vis
          vp   = visitProp          vis
          vio  = visitInfixOp       vis
      in case e of
        StringLit l s            -> StringLit l s
        RegexpLit l s b1 b2      -> RegexpLit l s b1 b2
        NumLit l d               -> NumLit l d
        IntLit l i               -> IntLit l i
        BoolLit l b              -> BoolLit l b
        NullLit l                -> NullLit l
        ArrayLit l es            -> ArrayLit l $ ve <$> es
        ObjectLit l pes          -> ObjectLit l $ (\(p,e) -> (vp p, ve e)) <$> pes
        ThisRef l                -> ThisRef l
        VarRef l i               -> VarRef l $ vi i
        DotRef l e i             -> DotRef l (ve e) (vi i)
        BracketRef l e1 e2       -> BracketRef l (ve e1) (ve e2)
        NewExpr l e es           -> NewExpr l (ve e) (ve <$> es)
        PrefixExpr l po e        -> PrefixExpr l (vpo po) (ve e)
        UnaryAssignExpr l uao lv -> UnaryAssignExpr l (vuao  uao) (vlv lv)
        InfixExpr l io e1 e2     -> InfixExpr l (vio io) (ve e1) (ve e2)
        CondExpr l e1 e2 e3      -> CondExpr l (ve e1) (ve e2) (ve e3)
        AssignExpr l ao lv e     -> AssignExpr l (vao ao) (vlv lv) (ve e)
        ListExpr l es            -> ListExpr l (ve <$> es)
        CallExpr l e es          -> CallExpr l (ve e) (ve <$> es)
        FuncExpr l mi is ss      -> FuncExpr l (vi <$> mi) (vi <$> is) (vs <$> ss)
        Cast l e                 -> Cast l $ ve e
        DeadCast l e             -> DeadCast l $ ve e
      
            
-------------------------------------------------------------------------------
visitCaseClause :: NanoVisitor a b -> CaseClause a -> CaseClause a
-------------------------------------------------------------------------------
visitCaseClause vis c = doVisit vis (vCaseClause vis c) ch c
  where 
    ch vis cc = 
      let ve   = visitExpression    vis
          vs   = visitStatement     vis
      in case cc of 
        CaseClause l e ss -> CaseClause l (ve e) (vs <$> ss)
        CaseDefault l ss  -> CaseDefault l (vs <$> ss)
                   

-------------------------------------------------------------------------------
visitCatchClause :: NanoVisitor a b -> CatchClause a -> CatchClause a
-------------------------------------------------------------------------------
visitCatchClause vis c = doVisit vis (vCatchClause vis c) ch c
  where 
    ch vis c = 
      let vi   = visitId            vis
          vs   = visitStatement     vis
      in case c of 
        CatchClause l i s -> CatchClause l (vi i) (vs s)
            

-------------------------------------------------------------------------------
visitId :: NanoVisitor a b -> Id a -> Id a
-------------------------------------------------------------------------------
visitId vis i = doVisit vis (vId vis i) (flip const) i
            

-------------------------------------------------------------------------------
visitForInit :: NanoVisitor a b -> ForInit a -> ForInit a
-------------------------------------------------------------------------------
visitForInit vis c = doVisit vis (vForInit vis c) ch c
  where 
    ch vis i = 
      let vd = visitVarDecl vis 
          ve = visitExpression vis
      in case i of 
        NoInit      -> NoInit
        VarInit vds -> VarInit (vd <$> vds)
        ExprInit e  -> ExprInit (ve e)         
            

-------------------------------------------------------------------------------
visitJavaScript    :: NanoVisitor a b -> JavaScript a  -> JavaScript a
-------------------------------------------------------------------------------
visitJavaScript vis js = doVisit vis (vJavaScript vis js) ch js
  where 
    ch vis (Script l ss) = Script l (visitStatement vis <$> ss) 


-------------------------------------------------------------------------------
visitInfixOp       :: NanoVisitor a b -> InfixOp -> InfixOp
-------------------------------------------------------------------------------
visitInfixOp vis io = doVisit vis (vInfixOp vis io) (flip const) io


-------------------------------------------------------------------------------
visitAssignOp      :: NanoVisitor a b -> AssignOp -> AssignOp
-------------------------------------------------------------------------------
visitAssignOp vis ao = doVisit vis (vAssignOp vis ao) (flip const) ao


-------------------------------------------------------------------------------
visitUnaryAssignOp :: NanoVisitor a b -> UnaryAssignOp -> UnaryAssignOp 
-------------------------------------------------------------------------------
visitUnaryAssignOp vis uao = doVisit vis (vUnaryAssignOp vis uao) (flip const) uao


-------------------------------------------------------------------------------
visitPrefixOp      :: NanoVisitor a b -> PrefixOp -> PrefixOp
-------------------------------------------------------------------------------
visitPrefixOp vis po = doVisit vis (vPrefixOp vis po) (flip const) po


-------------------------------------------------------------------------------
visitProp          :: NanoVisitor a b -> Prop a -> Prop a
-------------------------------------------------------------------------------
visitProp vis p = doVisit vis (vProp vis p) ch p
  where 
    ch vis p = 
      let vid = visitId vis
      in case p of 
        PropId l i      -> PropId l (vid i)
        PropString l s  -> PropString l s
        PropNum l n     -> PropNum l n


-------------------------------------------------------------------------------
visitLValue        :: NanoVisitor a b -> LValue a -> LValue a
-------------------------------------------------------------------------------
visitLValue vis lv = doVisit vis (vLValue vis lv) ch lv
  where 
    ch vis lv = 
      let ve = visitExpression vis  
      in case lv of 
        LVar l s          -> LVar l s 
        LDot l e s        -> LDot l (ve e) s
        LBracket l e1 e2  -> LBracket l (ve e1) (ve e2)


-------------------------------------------------------------------------------
visitVarDecl       :: NanoVisitor a b -> VarDecl a -> VarDecl a
-------------------------------------------------------------------------------
visitVarDecl vis vd = doVisit vis (vVarDecl vis vd) ch vd
  where 
    ch vis (VarDecl l i me) = VarDecl l (visitId vis i) (visitExpression vis <$> me)


-------------------------------------------------------------------------------
visitForInInit     :: NanoVisitor a b -> ForInInit a   -> ForInInit a   
-------------------------------------------------------------------------------
visitForInInit vis fi = doVisit vis (vForInInit vis fi) ch fi
  where 
    ch vis (ForInVar i)   = ForInVar  $ visitId vis i
    ch vis (ForInLVal lv) = ForInLVal $ visitLValue vis lv



-------------------------------------------------------------------------------
-- | Monadic AST Visitor
-------------------------------------------------------------------------------

{- 
 - | Using a visit monad:
 - The problem here is that we cannot create a visit class, because the
 - instances will have to refer to a b, which are quantified over in VT.
 -
 -}
data VisitActionM m a b c =  SkipChildrenM 
                           | DoChildrenM
                           | ChangeToM c
                           -- TODO: fix this: VT should not be exposed
                           | ChangeDoChildrenPostM (c, c -> VT m a b c)


data NanoVisitorM m a b = NVM {
     vProgramM       :: Nano a b      -> VisitActionM m a b (Nano a b)
  ,  vStatementM     :: Statement a   -> VisitActionM m a b (Statement a)
  ,  vJavaScriptM    :: JavaScript a  -> VisitActionM m a b (JavaScript a)
  ,  vIdM            :: Id a          -> VisitActionM m a b (Id a)
  ,  vInfixOpM       :: InfixOp       -> VisitActionM m a b InfixOp
  ,  vAssignOpM      :: AssignOp      -> VisitActionM m a b AssignOp
  ,  vUnaryAssignOpM :: UnaryAssignOp -> VisitActionM m a b UnaryAssignOp
  ,  vPrefixOpM      :: PrefixOp      -> VisitActionM m a b PrefixOp
  ,  vPropM          :: Prop a        -> VisitActionM m a b (Prop a)
  ,  vLValueM        :: LValue a      -> VisitActionM m a b (LValue a)
  ,  vExpressionM    :: Expression a  -> VisitActionM m a b (Expression a)
  ,  vCaseClauseM    :: CaseClause a  -> VisitActionM m a b (CaseClause a)
  ,  vCatchClauseM   :: CatchClause a -> VisitActionM m a b (CatchClause a)
  ,  vVarDeclM       :: VarDecl a     -> VisitActionM m a b (VarDecl a)
  ,  vForInitM       :: ForInit a     -> VisitActionM m a b (ForInit a)
  ,  vForInInitM     :: ForInInit a   -> VisitActionM m a b (ForInInit a)
  }


type VT m a b = StateT (NanoVisitorM m a b) m 


defaultVisitorM = NVM d d d d d d d d d d d d d d d d
  where
    d _ = DoChildrenM


-------------------------------------------------------------------------------
doVisitM :: (Functor m, Monad m) => 
     (NanoVisitorM m a b -> c -> VisitActionM m a b c) 
  -> (c -> VT m a b c) 
  -> c 
  -> VT m a b c
-------------------------------------------------------------------------------
doVisitM op children n = 
  do  v <- get
      case op v n of
        SkipChildrenM                -> return n
        ChangeToM n'                 -> return n'
        DoChildrenM                  -> children n
        ChangeDoChildrenPostM (n, f) -> children n >>= f 
 
-- The class version will not work right ...
{-
class Visitable n where 
  visit :: (Functor m, Monad m) => forall a b . n -> VT m a b n

instance Visitable (Nano a b) where
  visit pgm = 
    do  p <- vProgramM <$> get
        doVisitM (p pgm) ch pgm
    where
      ch p@(Nano {code = Src fs}) = 
        do  cd <- T.mapM visitStatementM fs
            return $ p { code = Src cd }
-}

-- We can still go without the class:

runVisitProgramM       :: ( Functor m, Monad m ) => NanoVisitorM m a b -> Nano a b          -> m ( Nano a b         )
runVisitJavascriptM    :: ( Functor m, Monad m ) => NanoVisitorM m a b -> JavaScript      a -> m ( JavaScript a     )
runVisitExpressionM    :: ( Functor m, Monad m ) => NanoVisitorM m a b -> Expression      a -> m ( Expression     a )
runVisitStatementM     :: ( Functor m, Monad m ) => NanoVisitorM m a b -> Statement       a -> m ( Statement      a )
runVisitCaseClauseM    :: ( Functor m, Monad m ) => NanoVisitorM m a b -> CaseClause      a -> m ( CaseClause     a )
runVisitCatchClauseM   :: ( Functor m, Monad m ) => NanoVisitorM m a b -> CatchClause     a -> m ( CatchClause    a )
runVisitIdM            :: ( Functor m, Monad m ) => NanoVisitorM m a b -> Id              a -> m ( Id             a )
runVisitForInitM       :: ( Functor m, Monad m ) => NanoVisitorM m a b -> ForInit         a -> m ( ForInit        a )
runVisitForInInitM     :: ( Functor m, Monad m ) => NanoVisitorM m a b -> ForInInit       a -> m ( ForInInit      a )
runVisitVarDeclM       :: ( Functor m, Monad m ) => NanoVisitorM m a b -> VarDecl         a -> m ( VarDecl        a )
runVisitPrefixOpM      :: ( Functor m, Monad m ) => NanoVisitorM m a b -> PrefixOp          -> m ( PrefixOp         )
runVisitUnaryAssignOpM :: ( Functor m, Monad m ) => NanoVisitorM m a b -> UnaryAssignOp     -> m ( UnaryAssignOp    )
runVisitLValueM        :: ( Functor m, Monad m ) => NanoVisitorM m a b -> LValue          a -> m ( LValue         a )
runVisitAssignOpM      :: ( Functor m, Monad m ) => NanoVisitorM m a b -> AssignOp          -> m ( AssignOp         )
runVisitPropM          :: ( Functor m, Monad m ) => NanoVisitorM m a b -> Prop            a -> m ( Prop           a )
runVisitInfixOpM       :: ( Functor m, Monad m ) => NanoVisitorM m a b -> InfixOp           -> m ( InfixOp          )

runVisitProgramM       v  n = fst <$> runStateT (visitProgramM        n) v
runVisitJavascriptM    v  n = fst <$> runStateT (visitJavaScriptM     n) v
runVisitExpressionM    v  n = fst <$> runStateT (visitExpressionM     n) v
runVisitStatementM     v  n = fst <$> runStateT (visitStatementM      n) v
runVisitCaseClauseM    v  n = fst <$> runStateT (visitCaseClauseM     n) v
runVisitCatchClauseM   v  n = fst <$> runStateT (visitCatchClauseM    n) v
runVisitIdM            v  n = fst <$> runStateT (visitIdM             n) v
runVisitForInitM       v  n = fst <$> runStateT (visitForInitM        n) v
runVisitForInInitM     v  n = fst <$> runStateT (visitForInInitM      n) v
runVisitVarDeclM       v  n = fst <$> runStateT (visitVarDeclM        n) v
runVisitPrefixOpM      v  n = fst <$> runStateT (visitPrefixOpM       n) v
runVisitUnaryAssignOpM v  n = fst <$> runStateT (visitUnaryAssignOpM  n) v
runVisitLValueM        v  n = fst <$> runStateT (visitLValueM         n) v
runVisitAssignOpM      v  n = fst <$> runStateT (visitAssignOpM       n) v
runVisitPropM          v  n = fst <$> runStateT (visitPropM           n) v
runVisitInfixOpM       v  n = fst <$> runStateT (visitInfixOpM        n) v


-- | Shorthand aliases:
ve   n = visitExpressionM    $ tracePP "Visiting Expression" n
vs   n = visitStatementM     $ tracePP "Visiting Statement" n
vc   n = visitCaseClauseM                         n
vcc  n = visitCatchClauseM                        n
vi   n = visitIdM            $ tracePP "Visiting Id" n
vfi  n = visitForInitM                            n
vfii n = visitForInInitM                          n
vd   n = visitVarDeclM                            n
vpo  n = visitPrefixOpM      $ tracePP "Visiting PrefixOp" n
vuao n = visitUnaryAssignOpM                      n
vlv  n = visitLValueM        $ tracePP "Visiting LValue" n
vao  n = visitAssignOpM      $ tracePP "Visiting AssignOp" n
vp   n = visitPropM          $ tracePP "Visiting Prop" n
vio  n = visitInfixOpM       $ tracePP "Visiting InfixOp" n


-------------------------------------------------------------------------------
visitProgramM :: (Functor m, Monad m) => Nano a b -> VT m a b (Nano a b)
-------------------------------------------------------------------------------
visitProgramM = doVisitM vProgramM ch
  where
    ch p@(Nano {code = Src fs}) = 
      do  cd <- T.mapM visitStatementM fs
          return $ p { code = Src cd }


-------------------------------------------------------------------------------
visitStatementM :: (Functor m, Monad m) =>  Statement a -> VT m a b (Statement a)
-------------------------------------------------------------------------------
visitStatementM = doVisitM vStatementM ch
  where
    ch (BlockStmt l sts        ) = BlockStmt l <$> T.mapM vs sts
    ch (EmptyStmt l            ) = return $ EmptyStmt l
    ch (ExprStmt l e           ) = ExprStmt l <$> ve e
    ch (IfStmt l e s1 s2       ) = liftM3 (IfStmt l) (ve e) (vs s1) (vs s2)
    ch (IfSingleStmt l e s     ) = liftM2 (IfSingleStmt l) (ve e) (vs s)
    ch (SwitchStmt l e cs      ) = liftM2 (SwitchStmt l) (ve e) (T.mapM vc cs)
    ch (WhileStmt l e s        ) = liftM2 (WhileStmt l) (ve e) (vs s)
    ch (DoWhileStmt l s e      ) = liftM2 (DoWhileStmt l) (vs s) (ve e)
    ch (BreakStmt l mid        ) = BreakStmt l <$> T.mapM vi mid
    ch (ContinueStmt l mid     ) = ContinueStmt l <$> T.mapM vi mid
    ch (LabelledStmt l id s    ) = liftM2 (LabelledStmt l) (vi id) (vs s)
    ch (ForInStmt l fi e s     ) = liftM3 (ForInStmt l) (vfii fi) (ve e) (vs s)
    ch (ForStmt l fii me1 me2 s) = liftM4 (ForStmt l) (vfi fii) (T.mapM ve me1) (T.mapM ve me2) (vs s)
    ch (TryStmt l s mcc ms     ) = liftM3 (TryStmt l) (vs s) (T.mapM vcc mcc) (T.mapM vs ms)
    ch (ThrowStmt l e          ) = ThrowStmt l <$> ve e
    ch (ReturnStmt l me        ) = ReturnStmt l <$> T.mapM ve me
    ch (WithStmt l e s         ) = liftM2 (WithStmt l) (ve e) (vs s)
    ch (VarDeclStmt l vds      ) = VarDeclStmt l <$> T.mapM vd vds
    ch (FunctionStmt l i is ss ) = liftM3 (FunctionStmt l) (vi i) (T.mapM vi is) (T.mapM vs ss)


-------------------------------------------------------------------------------
visitExpressionM :: (Functor m, Monad m) =>  Expression a -> VT m a b (Expression a)
-------------------------------------------------------------------------------
visitExpressionM =  doVisitM vExpressionM ch
  where 
    ch (StringLit l s           ) = return $ StringLit l s
    ch (RegexpLit l s b1 b2     ) = return $ RegexpLit l s b1 b2
    ch (NumLit l d              ) = return $ NumLit l d
    ch (IntLit l i              ) = return $ IntLit l i
    ch (BoolLit l b             ) = return $ BoolLit l b
    ch (NullLit l               ) = return $ NullLit l
    ch (ArrayLit l es           ) = ArrayLit l <$> T.mapM ve es
    ch (ObjectLit l pes         ) = ObjectLit l <$> T.mapM (\(p,e) -> liftM2 (,) (vp p) (ve e)) pes
    ch (ThisRef l               ) = return $ ThisRef l
    ch (VarRef l i              ) = VarRef l <$> vi i
    ch (DotRef l e i            ) = liftM2 (DotRef l) (ve e) (vi i)
    ch (BracketRef l e1 e2      ) = liftM2 (BracketRef l) (ve e1) (ve e2)
    ch (NewExpr l e es          ) = liftM2 (NewExpr l) (ve e) (T.mapM ve es)
    ch (PrefixExpr l po e       ) = liftM2 (PrefixExpr l) (vpo po) (ve e)
    ch (UnaryAssignExpr l uao lv) = liftM2 (UnaryAssignExpr l) (vuao  uao) (vlv lv)
    ch (InfixExpr l io e1 e2    ) = liftM3 (InfixExpr l) (vio io) (ve e1) (ve e2)
    ch (CondExpr l e1 e2 e3     ) = liftM3 (CondExpr l) (ve e1) (ve e2) (ve e3)
    ch (AssignExpr l ao lv e    ) = liftM3 (AssignExpr l) (vao ao) (vlv lv) (ve e)
    ch (ListExpr l es           ) = ListExpr l <$> T.mapM ve es
    ch (CallExpr l e es         ) = liftM2 (CallExpr l) (ve e) (T.mapM ve es)
    ch (FuncExpr l mi is ss     ) = liftM3 (FuncExpr l) (T.mapM vi mi) (T.mapM vi is) (T.mapM vs ss)
    ch (Cast l e                ) = Cast l <$> ve e
    ch (DeadCast l e            ) = DeadCast l <$> ve e


-------------------------------------------------------------------------------
visitCaseClauseM :: (Functor m, Monad m) =>  CaseClause a -> VT m a b (CaseClause a)
-------------------------------------------------------------------------------
visitCaseClauseM = doVisitM vCaseClauseM ch
  where 
    ch (CaseClause l e ss) = liftM2 (CaseClause l) (ve e) (T.mapM vs ss)
    ch (CaseDefault l ss ) = CaseDefault l <$> T.mapM vs ss


-------------------------------------------------------------------------------
visitCatchClauseM :: (Functor m, Monad m) =>  CatchClause a -> VT m a b (CatchClause a)
-------------------------------------------------------------------------------
visitCatchClauseM = doVisitM vCatchClauseM ch
  where 
    ch (CatchClause l i s) = liftM2 (CatchClause l) (vi i) (vs s)


-------------------------------------------------------------------------------
visitIdM :: (Functor m, Monad m) =>  Id a -> VT m a b (Id a)
-------------------------------------------------------------------------------
visitIdM =  doVisitM vIdM return


-------------------------------------------------------------------------------
visitForInitM :: (Functor m, Monad m) => ForInit a -> VT m a b (ForInit a)
-------------------------------------------------------------------------------
visitForInitM = doVisitM vForInitM ch
  where 
    ch NoInit        = return NoInit
    ch (VarInit vds) = VarInit <$> T.mapM vd vds
    ch (ExprInit e)  = ExprInit <$> ve e


-------------------------------------------------------------------------------
visitJavaScriptM :: (Functor m, Monad m) =>  JavaScript a  -> VT m a b (JavaScript a)
-------------------------------------------------------------------------------
visitJavaScriptM = doVisitM vJavaScriptM ch
  where 
    ch (Script l ss) = Script l <$> T.mapM visitStatementM ss


-------------------------------------------------------------------------------
visitInfixOpM :: (Functor m, Monad m) =>  InfixOp -> VT m a b InfixOp
-------------------------------------------------------------------------------
visitInfixOpM = doVisitM vInfixOpM return


-------------------------------------------------------------------------------
visitAssignOpM :: (Functor m, Monad m) =>  AssignOp -> VT m a b AssignOp
-------------------------------------------------------------------------------
visitAssignOpM =  doVisitM vAssignOpM return


-------------------------------------------------------------------------------
visitUnaryAssignOpM :: (Functor m, Monad m) =>  UnaryAssignOp -> VT m a b UnaryAssignOp 
-------------------------------------------------------------------------------
visitUnaryAssignOpM = doVisitM vUnaryAssignOpM return


-------------------------------------------------------------------------------
visitPrefixOpM :: (Functor m, Monad m) =>  PrefixOp -> VT m a b PrefixOp
-------------------------------------------------------------------------------
visitPrefixOpM = doVisitM vPrefixOpM return


-------------------------------------------------------------------------------
visitPropM :: (Functor m, Monad m) =>  Prop a -> VT m a b (Prop a)
-------------------------------------------------------------------------------
visitPropM = doVisitM vPropM ch
  where 
    ch p = 
      let vid = visitIdM
      in case p of 
        PropId l i      -> PropId l <$> vid i
        PropString l s  -> return $ PropString l s
        PropNum l n     -> return $ PropNum l n


-------------------------------------------------------------------------------
visitLValueM :: (Functor m, Monad m) =>  LValue a -> VT m a b (LValue a)
-------------------------------------------------------------------------------
visitLValueM = doVisitM vLValueM ch
  where 
    ch lv = 
      let ve = visitExpressionM
      in case lv of 
        LVar l s          -> return $ LVar l s 
        LDot l e s        -> liftM2 (LDot l) (ve e) (return s)
        LBracket l e1 e2  -> liftM2 (LBracket l) (ve e1) (ve e2)


-------------------------------------------------------------------------------
visitVarDeclM :: (Functor m, Monad m) =>  VarDecl a -> VT m a b (VarDecl a)
-------------------------------------------------------------------------------
visitVarDeclM = doVisitM vVarDeclM ch
  where 
    ch (VarDecl l i me) = liftM2 (VarDecl l) (vi i) (T.mapM ve me)


-------------------------------------------------------------------------------
visitForInInitM :: (Functor m, Monad m) =>  ForInInit a -> VT m a b (ForInInit a)
-------------------------------------------------------------------------------
visitForInInitM = doVisitM vForInInitM ch
  where 
    ch (ForInVar i)   = ForInVar  <$> vi i
    ch (ForInLVal lv) = ForInLVal <$> vlv lv




{-
 -- | Attempt using class for visit
 --
data VisitActionM m c = SkipChildrenM 
                      | DoChildrenM
                      | ChangeToM c
                      | ChangeDoChildrenPostM (c, (c -> m c))


data NanoVisitorM m a b = NVM {
     vProgramM       :: Nano a b      -> VisitActionM m (Nano a b)
  ,  vStatementM     :: Statement a   -> VisitActionM m (Statement a)
  ,  vJavaScriptM    :: JavaScript a  -> VisitActionM m (JavaScript a)
  ,  vIdM            :: Id a          -> VisitActionM m (Id a)
  ,  vInfixOpM       :: InfixOp       -> VisitActionM m InfixOp
  ,  vAssignOpM      :: AssignOp      -> VisitActionM m AssignOp
  ,  vUnaryAssignOpM :: UnaryAssignOp -> VisitActionM m UnaryAssignOp
  ,  vPrefixOpM      :: PrefixOp      -> VisitActionM m PrefixOp
  ,  vPropM          :: Prop a        -> VisitActionM m (Prop a)
  ,  vLValueM        :: LValue a      -> VisitActionM m (LValue a)
  ,  vExpressionM    :: Expression a  -> VisitActionM m (Expression a)
  ,  vCaseClauseM    :: CaseClause a  -> VisitActionM m (CaseClause a)
  ,  vCatchClauseM   :: CatchClause a -> VisitActionM m (CatchClause a)
  ,  vVarDeclM       :: VarDecl a     -> VisitActionM m (VarDecl a)
  ,  vForInitM       :: ForInit a     -> VisitActionM m (ForInit a)
  ,  vForInInitM     :: ForInInit a   -> VisitActionM m (ForInInit a)
  }


-------------------------------------------------------------------------------
doVisitM :: (Functor m, Monad m) => NanoVisitorM m a b -> VisitActionM m c -> (NanoVisitorM m a b -> c -> m c) -> c -> m c
-------------------------------------------------------------------------------
doVisitM  vis action children n = 
  case action of
    SkipChildrenM                 -> return n
    ChangeToM n'                  -> return n'
    DoChildrenM                   -> children vis n
    ChangeDoChildrenPostM (n', f) -> (children vis n') >>= f 


class Visitable n where
  visit :: (Functor m, Monad m) =>  NanoVisitorM m a b -> n -> m n

instance Visitable (Nano a b) where 
  visit vis pgm = doVisitM vis (vProgramM vis pgm) ch pgm
    where 
      ch vis p@(Nano {code = Src fs}) = 
        do  cd <- T.mapM (visit vis) fs
            return $ p { code = Src cd }
 


-------------------------------------------------------------------------------
visitProgramM :: (Functor m, Monad m) =>  NanoVisitorM m a b -> Nano a b -> m (Nano a b)
-------------------------------------------------------------------------------
visitProgramM vis pgm = doVisitM vis (vProgramM vis pgm) ch pgm
  where 
    ch vis p@(Nano {code = Src fs}) = 
      do  cd <- T.mapM (visit vis) fs
          return $ p { code = Src cd }


-------------------------------------------------------------------------------
-- visitStatementM :: (Functor m, Monad m) =>  NanoVisitorM m a b -> Statement a -> m (Statement a)
-------------------------------------------------------------------------------
instance Visitable (Statement a) where
  visit vis st = doVisitM vis (visit vis st) ch st
    where
      ch vis s = let vv  = visit vis in case s of
        BlockStmt l sts         -> BlockStmt l <$> T.mapM vv sts
        EmptyStmt l             -> return $ EmptyStmt l
        ExprStmt l e            -> ExprStmt l <$> vv e
        IfStmt l e s1 s2        -> liftM3 (IfStmt l) (vv e) (vv s1) (vv s2)
        IfSingleStmt l e s      -> liftM2 (IfSingleStmt l) (vv e) (vv s)
        SwitchStmt l e cs       -> liftM2 (SwitchStmt l) (vv e) (T.mapM vv cs)
        WhileStmt l e s         -> liftM2 (WhileStmt l) (vv e) (vv s)
        DoWhileStmt l s e       -> liftM2 (DoWhileStmt l) (vv s) (vv e)
        {-BreakStmt l mid         -> BreakStmt l <$> (T.mapM vi mid)-}
        {-ContinueStmt l mid      -> ContinueStmt l <$> (T.mapM vi mid)-}
        LabelledStmt l id s     -> liftM2 (LabelledStmt l) (vv id) (vv s)
        ForInStmt l fi e s      -> liftM3 (ForInStmt l) (vv fi) (vv e) (vv s)
        {-ForStmt l fii me1 me2 s -> liftM4 (ForStmt l) (vfi fii) (T.mapM ve me1) (T.mapM ve me2) (vs s)-}
        {-TryStmt l s mcc ms      -> liftM2 (TryStmt l) (vs s) (T.mapM vcc mcc) (T.mapM vs ms)-}
        {-ThrowStmt l e           -> ThrowStmt l <$> ve e-}
        {-ReturnStmt l me         -> ReturnStmt l <$> (T.mapM ve me)-}
        {-WithStmt l e s          -> liftM2 (WithStmt l) (ve e) (vs s)-}
        {-VarDeclStmt l vds       -> liftM2 VarDeclStmt l <$> vd <$> vds-}
        {-FunctionStmt l i is ss  -> FunctionStmt l (vi i) (vi <$> is) (vs <$> ss)-}
              
-}

