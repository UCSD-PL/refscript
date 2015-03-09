{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverlappingInstances     #-}

-- | Pretty-printing JavaScript.
module Language.Nano.Syntax.PrettyPrint
  ( 
  javaScript
  , renderStatements
  , renderExpression
  , PP (..)
  ) where

import Text.PrettyPrint.HughesPJ
import Language.Nano.Syntax
import Prelude hiding (maybe)

------------------------------------------------------------------------------

class PP a where 
  pp :: a -> Doc

instance PP [Statement a] where 
  pp = stmtList 

instance PP (Expression a) where 
  pp = ppExpression True

instance PP (Statement a) where 
  pp = ppStatement

instance PP (CaseClause a) where
  pp = caseClause

instance PP [(CaseClause a)] where
  pp = caseClauseList
  
instance PP (ClassElt a)  where
  pp = ppClassElt 

instance PP (ForInit a) where 
  pp = ppForInit 

instance PP (ForInInit a) where 
  pp = ppForInInit

instance PP (LValue a) where 
  pp = ppLValue 

instance PP InfixOp where 
  pp = infixOp 

instance PP AssignOp where 
  pp = assignOp

instance PP PrefixOp where 
  pp = prefixOp

instance PP (Prop a) where
  pp = prop

instance (PP a, PP b) => PP (a,b) where
  pp (x, y) = (pp x) <+> (text ":") <+> (pp y)

instance (PP a, PP b, PP c) => PP (a,b,c) where
  pp (x, y, z) = (pp x) <+> (text ":") <+> (pp y) <+> (text ":") <+> (pp z)

----------------------------------------------------------------------------



-- | Renders a list of statements as a 'String'
renderStatements :: [Statement a] -> String
renderStatements = render . stmtList

-- | Renders a list of statements as a 'String'
renderExpression :: Expression a -> String
renderExpression = render . (ppExpression True)

-- Displays the statement in { ... }, unless it is a block itself.
inBlock:: Statement a -> Doc
inBlock s@(BlockStmt _ _) = ppStatement s
inBlock s                 = ssAsBlock [s]

-- ssAsBlock :: [Statement a] -> Doc
asBlock f ss = lbrace $+$ nest 2 (f ss) $$ rbrace

ssAsBlock = asBlock stmtList

classEltAsBlock = asBlock classEltList

ppId (Id _ str) = text str

ppEnumElt (EnumElt _ (Id _ str) e) = text str <+> text "=" <+> ppExpression True e

ppForInit :: ForInit a -> Doc
ppForInit t = case t of
  NoInit     -> empty
  VarInit vs -> text "var" <+> cat (punctuate comma $ map (ppVarDecl False) vs)
  ExprInit e -> ppExpression False e

ppForInInit :: ForInInit a -> Doc  
ppForInInit t = case t of
  ForInVar id   -> text "var" <+> ppId id
  ForInLVal lv -> ppLValue lv

caseClause :: CaseClause a -> Doc
caseClause (CaseClause _ e ss) =
  text "case" $+$ ppExpression True e <+> colon $$ nest 2 (stmtList ss)
caseClause (CaseDefault _ ss) =
  text "default:" $$ nest 2 (stmtList ss)

ppVarDecl :: Bool -> VarDecl a -> Doc
ppVarDecl hasIn vd = case vd of
  VarDecl _ id Nothing  -> ppId id
  VarDecl _ id (Just e) -> ppId id <+> equals <+> ppAssignmentExpression hasIn e

ppStatement :: Statement a -> Doc
ppStatement s = case s of
  BlockStmt _ ss -> ssAsBlock ss
  EmptyStmt _ -> semi
  ExprStmt _ e@(CallExpr _ (FuncExpr {}) _ ) -> 
    parens (ppExpression True e) <> semi
  ExprStmt _ e -> ppExpression True e <> semi
  IfSingleStmt _ test cons -> text "if" <+> 
                              parens (ppExpression True test) $$ 
                              ppStatement cons
  IfStmt _ test cons alt -> text "if" <+> parens (ppExpression True test) $$ 
                            ppStatement cons $$ text "else" <+> ppStatement alt
  SwitchStmt _ e cases ->
    text "switch" <+> parens (ppExpression True e) $$ 
    braces (nest 2 (vcat (map caseClause cases)))
  WhileStmt _ test body -> text "while" <+> parens (ppExpression True test) $$
                           ppStatement body
  ReturnStmt _ Nothing -> text "return"
  ReturnStmt _ (Just e) -> text "return" <+> ppExpression True e
  DoWhileStmt _ s e -> 
    text "do" $$ 
    (ppStatement s <+> text "while" <+> parens (ppExpression True e) <> semi)
  BreakStmt _ Nothing ->  text "break" <> semi
  BreakStmt _ (Just label) -> text "break" <+> ppId label <> semi
  ContinueStmt _ Nothing -> text "continue" <> semi
  ContinueStmt _ (Just label) -> text"continue" <+> ppId label <> semi
  LabelledStmt _ label s -> ppId label <> colon $$ ppStatement s
  ForInStmt p init e body -> 
    text "for" <+> 
    parens (ppForInInit init <+> text "in" <+> ppExpression True e) $+$ 
    ppStatement body
  ForStmt _ init incr test body ->
    text "for" <+> 
    parens (ppForInit init <> semi <+> maybe incr (ppExpression True) <> 
            semi <+> maybe test (ppExpression True)) $$ 
    ppStatement body
  TryStmt _ stmt mcatch mfinally ->
    text "try" $$ inBlock stmt $$ ppCatch $$ ppFinally 
    where ppFinally = case mfinally of
            Nothing -> empty
            Just stmt -> text "finally" <> inBlock stmt
          ppCatch = case mcatch of
            Nothing -> empty
            Just (CatchClause _ id s) -> 
              text "catch" <+> (parens.ppId) id <+> inBlock s
  ThrowStmt _ e -> text "throw" <+> ppExpression True e <> semi
  WithStmt _ e s -> text "with" <+> parens (ppExpression True e) $$ ppStatement s
  VarDeclStmt _ decls ->
    text "var" <+> cat (punctuate comma (map (ppVarDecl True) decls)) <> semi
  FunctionStmt _ name args body ->
    text "function" <+> ppId name <> 
    parens (cat $ punctuate comma (map ppId args)) $$ 
    ssAsBlock body
  ClassStmt _ name ext imp body -> 
    text "class" <+> ppId name  <+> 
    ( case ext of 
        Just e  -> text "extends" <+> ppId e
        Nothing -> text "") <+>
    ( case imp of 
        [] -> text ""
        is -> text "implements" <+> cat (punctuate comma (map ppId is))) $$
    classEltAsBlock body

  FuncAmbDecl _ name args ->
    text "declare function" <+> ppId name <> 
    parens (cat $ punctuate comma (map ppId args))
  FuncOverload _ name args ->
    text "function" <+> ppId name <> 
    parens (cat $ punctuate comma (map ppId args))
  ModuleStmt _ name body ->
    text "module" <+> ppId name $$ ssAsBlock body
  IfaceStmt _ x -> text "// interface" <+> ppId x
  EnumStmt _ name elts -> text "enumeration" <+> ppId name <+>
    braces (cat $ punctuate comma (map ppEnumElt elts)) 

ppClassElt :: ClassElt a -> Doc
ppClassElt (Constructor _ args body) = 
  text "constructor" <>
  parens (cat $ punctuate comma (map ppId args)) $$ 
  ssAsBlock body
ppClassElt (MemberVarDecl a s name eo) = 
  text (ite s " static " "") <> ppVarDecl True (VarDecl a name eo)
ppClassElt (MemberMethDef _ s name args body) = 
  text ({-ite m "public" "private" ++ -}ite s " static" "") <+> 
  ppId name <> 
  parens (cat $ punctuate comma (map ppId args)) $$ 
  ssAsBlock body
ppClassElt (MemberMethDecl _ s name args) = 
  text ({-ite m "public" "private" ++ -}ite s " static" "") <+> 
  ppId name <> 
  parens (cat $ punctuate comma (map ppId args)) 



ite True a _  = a 
ite False _ a = a 

stmtList :: [Statement a] -> Doc
stmtList = vcat . map ppStatement

classEltList :: [ClassElt a] -> Doc
classEltList = vcat . map ppClassElt

caseClauseList :: [CaseClause a] -> Doc
caseClauseList = vcat . map caseClause

prop :: Prop a -> Doc
prop p = case p of
  PropId _ id -> ppId id
  PropString _ str -> doubleQuotes (text (jsEscape str))
  PropNum _ n -> text (show n)

infixOp op = text $ case op of
  OpMul -> "*"
  OpDiv -> "/"
  OpMod -> "%" 
  OpAdd -> "+" 
  OpSub -> "-"
  OpLShift -> "<<"
  OpSpRShift -> ">>"
  OpZfRShift -> ">>>"
  OpLT -> "<"
  OpLEq -> "<="
  OpGT -> ">"
  OpGEq -> ">="
  OpIn -> "in"
  OpInstanceof -> "instanceof"
  OpEq -> "=="
  OpNEq -> "!="
  OpStrictEq -> "==="
  OpStrictNEq -> "!=="
  OpBAnd -> "&"
  OpBXor -> "^"
  OpBOr -> "|"
  OpLAnd -> "&&"
  OpLOr -> "||"


prefixOp op = text $ case op of
  PrefixLNot -> "!"
  PrefixBNot -> "~"
  PrefixPlus -> "+"
  PrefixMinus -> "-"
  PrefixTypeof -> "typeof"
  PrefixVoid -> "void"
  PrefixDelete -> "delete"


assignOp op = text $ case op of
  OpAssign -> "="
  OpAssignAdd -> "+="
  OpAssignSub -> "-="
  OpAssignMul -> "*="
  OpAssignDiv -> "/="
  OpAssignMod -> "%="
  OpAssignLShift -> "<<="
  OpAssignSpRShift -> ">>="
  OpAssignZfRShift -> ">>>="
  OpAssignBAnd -> "&="
  OpAssignBXor -> "^="
  OpAssignBOr -> "|="

-- Based on:
--   http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Guide:Literals
jsEscape:: String -> String
jsEscape "" = ""
jsEscape (ch:chs) = sel ch ++ jsEscape chs where
    sel '\b' = "\\b"
    sel '\f' = "\\f"
    sel '\n' = "\\n"
    sel '\r' = "\\r"
    sel '\t' = "\\t"
    sel '\v' = "\\v"
    sel '\'' = "\\'"
    sel '\"' = "\\\""
    sel '\\' = "\\\\"
    sel x    = [x]
    -- We don't have to do anything about \X, \x and \u escape sequences.
    
regexpEscape :: String -> String
regexpEscape "" = ""
regexpEscape "\\" = "\\\\"
regexpEscape ('\\':c:rest) = '\\':c:(regexpEscape rest)
regexpEscape ('/':rest) = '\\':'/':regexpEscape rest
regexpEscape (c:rest)   = c:regexpEscape rest

ppLValue :: LValue a -> Doc
ppLValue (LVar _ x) = text x
ppLValue (LDot _ e x) = ppMemberExpression e <> text "." <> text x
ppLValue (LBracket _ e1 e2) = ppMemberExpression e1 <> 
                              brackets (ppExpression True e2)

-- 11.1
ppPrimaryExpression :: Expression a -> Doc
ppPrimaryExpression e = case e of
  ThisRef _ -> text "this"
  VarRef _ id -> ppId id
  NullLit _ -> text "null"
  BoolLit _ True -> text "true"
  BoolLit _ False -> text "false"
  NumLit  _ n -> text (show n)
  HexLit  _ s -> text (show s)
  IntLit _ n ->  text (show n)
  StringLit _ str -> doubleQuotes (text (jsEscape str))
  RegexpLit _ reg g ci -> text "/" <> (text (regexpEscape reg)) <> text "/" <>
                          (if g then text "g" else empty) <> 
                          (if ci then text "i" else empty)
  ArrayLit _ es -> 
    brackets $ cat $ punctuate comma (map (ppAssignmentExpression True) es)
  ObjectLit _ xs ->  
    braces (hsep (punctuate comma (map pp' xs))) where
      pp' (n,v) =  prop n <> colon <+> ppAssignmentExpression True v
  _ -> parens $ ppExpression True e

-- 11.2
ppMemberExpression :: Expression a -> Doc
ppMemberExpression e = case e of
  FuncExpr _ name params body -> 
    text "function" <+> maybe name ppId <+>
    parens (cat $ punctuate comma (map ppId params)) $$ 
    ssAsBlock body
  DotRef _ obj id -> ppMemberExpression obj <> text "." <> ppId id
  BracketRef _ obj key -> 
    ppMemberExpression obj <> brackets (ppExpression True key)  
  NewExpr _ ctor args -> 
    text "new" <+> ppMemberExpression ctor <> ppArguments args
  _ -> ppPrimaryExpression e

ppCallExpression :: Expression a -> Doc
ppCallExpression e = case e of
  CallExpr _ f args -> ppCallExpression f <> ppArguments args
  DotRef _ obj id -> ppCallExpression obj <> text "." <> ppId id
  BracketRef _ obj key ->ppCallExpression obj <> brackets (ppExpression True key)
  _ -> ppMemberExpression e  
    
ppArguments :: [Expression a] -> Doc
ppArguments es = 
  parens $ cat $ punctuate comma (map (ppAssignmentExpression True) es)

ppLHSExpression :: Expression a -> Doc
ppLHSExpression = ppCallExpression

-- 11.3
ppPostfixExpression :: Expression a -> Doc
ppPostfixExpression e = case e of
  UnaryAssignExpr _ PostfixInc e' -> ppLValue e' <> text "++"
  UnaryAssignExpr _ PostfixDec e' -> ppLValue e' <> text "--"
  _ -> ppLHSExpression e
  
-- 11.4
ppUnaryExpression :: Expression a -> Doc
ppUnaryExpression e = case e of
  PrefixExpr _ op e' -> prefixOp op <+> ppUnaryExpression e'
  UnaryAssignExpr _ PrefixInc e' -> text "++" <> ppLValue e'
  UnaryAssignExpr _ PrefixDec e' -> text "--" <> ppLValue e'
  _ -> ppPostfixExpression e

-- 11.5
ppMultiplicativeExpression :: Expression a -> Doc
ppMultiplicativeExpression e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpMul, OpDiv, OpMod] -> 
    ppMultiplicativeExpression e1 <+> infixOp op <+> ppUnaryExpression e2
  _ -> ppUnaryExpression e
  
-- 11.6
ppAdditiveExpression :: Expression a -> Doc
ppAdditiveExpression e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpAdd, OpSub] -> 
    ppAdditiveExpression e1 <+> infixOp op <+> ppMultiplicativeExpression e2
  _ -> ppMultiplicativeExpression e

-- 11.7
ppShiftExpression :: Expression a -> Doc
ppShiftExpression e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpLShift, OpSpRShift, OpZfRShift] -> 
    ppShiftExpression e1 <+> infixOp op <+> ppAdditiveExpression e2  
  _ -> ppAdditiveExpression e

-- 11.8.  
-- | @ppRelationalExpression True@ is RelationalExpression,
-- @ppRelationalExpression False@ is RelationalExpressionNoIn
ppRelationalExpression :: Bool -> Expression a -> Doc
ppRelationalExpression hasIn e = 
  let opsNoIn = [OpLT, OpGT, OpLEq, OpGEq, OpInstanceof]
      ops     = if hasIn then OpIn:opsNoIn else opsNoIn
  in case e of    
    InfixExpr _ op e1 e2 | op `elem` ops -> 
      ppRelationalExpression hasIn e1 <+> infixOp op <+> ppShiftExpression e2
    _ -> ppShiftExpression e
    
-- 11.9
ppEqualityExpression :: Bool -> Expression a -> Doc
ppEqualityExpression hasIn e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpEq, OpNEq, OpStrictEq, OpStrictNEq] ->
    ppEqualityExpression hasIn e1 <+> infixOp op <+> 
    ppRelationalExpression hasIn e2
  _ -> ppRelationalExpression hasIn e
  
-- 11.10
ppBitwiseANDExpression :: Bool -> Expression a -> Doc
ppBitwiseANDExpression hasIn e = case e of
  InfixExpr _ op@OpBAnd e1 e2 -> ppBitwiseANDExpression hasIn e1 <+> 
                                 infixOp op <+>
                                 ppEqualityExpression hasIn e2
  _ -> ppEqualityExpression hasIn e
  
ppBitwiseXORExpression :: Bool -> Expression a -> Doc
ppBitwiseXORExpression hasIn e = case e of
  InfixExpr _ op@OpBXor e1 e2 -> ppBitwiseXORExpression hasIn e1 <+>
                                 infixOp op <+>
                                 ppBitwiseANDExpression hasIn e2
  _ -> ppBitwiseANDExpression hasIn e
  
ppBitwiseORExpression :: Bool -> Expression a -> Doc
ppBitwiseORExpression hasIn e = case e of
  InfixExpr _ op@OpBOr e1 e2 -> ppBitwiseORExpression hasIn e1 <+>
                                infixOp op <+>
                                ppBitwiseXORExpression hasIn e2
  _ -> ppBitwiseXORExpression hasIn e

-- 11.11
ppLogicalANDExpression :: Bool -> Expression a -> Doc
ppLogicalANDExpression hasIn e = case e of
  InfixExpr _ op@OpLAnd e1 e2 -> ppLogicalANDExpression hasIn e1 <+>
                                 infixOp op <+>
                                 ppBitwiseORExpression hasIn e2
  _ -> ppBitwiseORExpression hasIn e                                 
                                 
ppLogicalORExpression :: Bool -> Expression a -> Doc
ppLogicalORExpression hasIn e = case e of
  InfixExpr _ op@OpLOr e1 e2 -> ppLogicalORExpression hasIn e1 <+>
                                infixOp op <+>
                                ppLogicalANDExpression hasIn e2
  _ -> ppLogicalANDExpression hasIn e
  
-- 11.12
ppConditionalExpression :: Bool -> Expression a -> Doc
ppConditionalExpression hasIn e = case e of
  CondExpr _ c et ee -> ppLogicalORExpression hasIn c <+> text "?" <+> 
                        ppAssignmentExpression hasIn et <+> colon <+>
                        ppAssignmentExpression hasIn ee
  _ -> ppLogicalORExpression hasIn e

-- 11.13
ppAssignmentExpression :: Bool -> Expression a -> Doc
ppAssignmentExpression hasIn e = case e of
  AssignExpr _ op l r -> ppLValue l <+> assignOp op <+> 
                         ppAssignmentExpression hasIn r
  _ -> ppConditionalExpression hasIn e
  
-- 11.14
ppListExpression :: Bool -> Expression a -> Doc
ppListExpression hasIn e = case e of
  ListExpr _ es -> cat $ punctuate comma (map (ppExpression hasIn) es)
  _ -> ppAssignmentExpression hasIn e

-- PV Adding new levels for Cast
ppUserCastExpression :: Bool -> Expression a -> Doc
ppUserCastExpression hasIn e = case e of
  Cast _ e  -> text "UserCast" <> (parens $ ppExpression False e)
  _         -> ppCastExpression hasIn e

-- PV Adding new levels for Cast_
ppCastExpression :: Bool -> Expression a -> Doc
ppCastExpression hasIn e = case e of
  Cast_ _ e -> text "Cast" <> (parens $ ppExpression False e)
  _         -> ppListExpression hasIn e


-- PV Adding new levels for Super
ppExpression :: Bool -> Expression a -> Doc
ppExpression hasIn e = case e of
  SuperRef _ -> text "super"
  _          -> ppUserCastExpression hasIn e


maybe :: Maybe a -> (a -> Doc) -> Doc
maybe Nothing  _ = empty
maybe (Just a) f = f a

-- | Renders a JavaScript program as a document, the show instance of
-- 'Doc' will pretty-print it automatically
javaScript :: JavaScript a -> Doc
javaScript (Script _ ss) = stmtList ss
