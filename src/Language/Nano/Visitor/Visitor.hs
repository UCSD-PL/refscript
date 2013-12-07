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

) where 

import           Control.Applicative            ((<$>))
import           Language.Nano.Errors

import           Language.Nano.Typecheck.Types
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


