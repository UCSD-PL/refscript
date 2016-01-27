{-# LANGUAGE FlexibleInstances #-}

-- | Pretty-printing JavaScript.
module Language.Rsc.Pretty.Syntax (
    javaScript
  , renderStatements
  , renderExpression
) where

import           Control.Applicative        ((<$>))
import           Data.Maybe                 (catMaybes, fromMaybe)
import qualified Language.Fixpoint.Types    as F
import           Language.Rsc.AST.Syntax
import           Language.Rsc.Names
import           Language.Rsc.Pretty.Common
import           Prelude                    hiding (maybe)
import           Text.PrettyPrint.HughesPJ

instance {-# OVERLAPPING #-} PP [Statement a] where
  pp = stmtList

instance PP (Expression a) where
  pp = ppExpression True

instance PP (Statement a) where
  pp = fromMaybe (text "") . ppStatement

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

instance PP (Id a) where
  pp (Id _ x) = text x

instance PP (VarDecl a) where
  pp = ppVarDecl True


indentationLevel = 4

-- | Renders a list of statements as a 'String'
renderStatements :: [Statement a] -> String
renderStatements = render . stmtList

-- | Renders a list of statements as a 'String'
renderExpression :: Expression a -> String
renderExpression = render . (ppExpression True)

inBlock:: Statement a -> Doc
inBlock (BlockStmt _ ss) = stmtList ss
inBlock s                = stmtList [s]

asBlock  f ss = lbrace $+$ nest indentationLevel (f ss) $$ rbrace

ssAsBlock :: [Statement a] -> Doc
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
  text "case" $+$ ppExpression True e <+> colon $$ nest indentationLevel (stmtList ss)
caseClause (CaseDefault _ ss) =
  text "default:" $$ nest indentationLevel (stmtList ss)

ppVarDecl :: Bool -> VarDecl a -> Doc
ppVarDecl hasIn vd = case vd of
  VarDecl _ id Nothing  -> ppId id
  VarDecl _ id (Just e) -> ppId id <+> equals <+> ppAssignmentExpression hasIn e

ppStatement :: Statement a -> Maybe Doc
ppStatement s = case s of
  BlockStmt _ ss            -> Just $ ssAsBlock ss
  EmptyStmt _               -> Nothing
  ExprStmt _ e@(CallExpr _ FuncExpr{} _)
                            -> Just $ parens (ppExpression True e) <> semi
  ExprStmt _ e              -> Just $ ppExpression True e <> semi
  IfSingleStmt _ test cons  -> Just $ text "if" <+>
                                      parens (ppExpression True test) $$
                                      pp cons
  IfStmt _ test cons alt    -> Just $ headerWithBlock (text "if" <+> parens (ppExpression True test)) (inBlock cons) $$
                                      headerWithBlock (text "else") (inBlock alt)
  SwitchStmt _ e cases      -> Just $ text "switch" <+> parens (ppExpression True e) $$
                                      braces (nest indentationLevel (vcat (map caseClause cases)))
  WhileStmt _ test body     -> Just $ text "while" <+> parens (ppExpression True test) $$ pp body
  ReturnStmt _ Nothing      -> Just $ text "return" <> semi
  ReturnStmt _ (Just e)     -> Just $ text "return" <+> ppExpression True e <> semi
  DoWhileStmt _ s e         -> Just $ text "do" $$ pp s <+> text "while" <+>
                                      parens (ppExpression True e) <> semi
  BreakStmt _ Nothing       -> Just $ text "break" <> semi
  BreakStmt _ (Just label)  -> Just $ text "break" <+> ppId label <> semi
  ContinueStmt _ Nothing    -> Just $ text "continue" <> semi
  ContinueStmt _ (Just lbl) -> Just $ text "continue" <+> ppId lbl <> semi
  LabelledStmt _ label s    -> Just $ ppId label <> colon $$ pp s
  ForInStmt p init e body   -> Just $ text "for" <+>
                                      parens (ppForInInit init <+> text "in" <+> ppExpression True e) $+$
                                      pp body
  ForStmt _ init incr test body
                            -> Just $ text "for" <+>
                                      parens (ppForInit init <> semi <+>
                                              maybe incr (ppExpression True) <> semi <+>
                                              maybe test (ppExpression True)) $$
                                      pp body
  TryStmt _ stmt mcatch mfinally
                            -> Just $ text "try" $$ inBlock stmt $$ ppCatch $$ ppFinally
    where ppFinally = case mfinally of
                        Nothing -> empty
                        Just stmt -> text "finally" <> inBlock stmt
          ppCatch   = case mcatch of
                        Nothing -> empty
                        Just (CatchClause _ id s) -> text "catch" <+>
                                                     (parens.ppId) id <+> inBlock s
  ThrowStmt _ e             -> Just $ text "throw" <+> ppExpression True e <> semi
  WithStmt _ e s            -> Just $ text "with" <+> parens (ppExpression True e) $$ pp s
  VarDeclStmt _ decls       -> Just $ text "var" <+>
                                      cat (punctuate comma (map (ppVarDecl True) decls)) <>
                                      semi
  FunctionStmt _ name args (Just body)
                            -> Just $ headerWithBlock
                                        ( text "function" <+> ppId name <>
                                          parens (cat $ punctuate comma (map ppId args)))
                                        (stmtList body)

  FunctionStmt _ name args Nothing
                            -> Nothing
  ClassStmt _ name body     -> Just $ headerWithBlock (text "class"  <+> ppId name) (classEltList body)
  ModuleStmt _ name body    -> Just $ headerWithBlock (text "module" <+> ppId name) (stmtList     body)
  InterfaceStmt _ x         -> Nothing
  EnumStmt _ name elts      -> Just $ text "enumeration" <+> ppId name <+>
                                      braces (cat $ punctuate comma (map ppEnumElt elts))

headerWithBlock header block
  = header <+> lbrace $$ nest indentationLevel block $$ rbrace


ppClassElt :: ClassElt a -> Doc
ppClassElt (Constructor _ args body) =
  headerWithBlock (text "constructor" <> parens (cat $ punctuate comma (map ppId args)))
                  (stmtList body)

ppClassElt (MemberVarDecl a s name eo) =
  ifStatic s <+> ppVarDecl True (VarDecl a name eo)

ppClassElt (MemberMethDecl _ s name args body) =
  headerWithBlock (ifStatic s <+> ppId name <> parens (cat $ punctuate comma (map ppId args)))
                  (stmtList body)

ifStatic s | s = pp "static" | otherwise = empty

stmtList :: [Statement a] -> Doc
stmtList = vcat . catMaybes . map ppStatement

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

instance PP BuiltinOp where
  pp = text . show

instance PP SyntaxKind where
  pp FuncDeclKind    = text "FuncDeclKind"
  pp MethDeclKind    = text "MethDeclKind"
  pp FieldDeclKind   = text "FieldDeclKind"
  pp CtorDeclKind    = text "CtorDeclKind"
  pp VarDeclKind     = text "VarDeclKind"
  pp ClassDeclKind   = text "ClassDeclKind"
  pp ModuleDeclKind  = text "ModuleDeclKind"
  pp EnumDeclKind    = text "EnumDeclKind"

