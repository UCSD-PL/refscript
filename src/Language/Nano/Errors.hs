{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveDataTypeable        #-}

module Language.Nano.Errors where

import Debug.Trace
import Text.Printf 
import Text.PrettyPrint.HughesPJ
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Parser.Type      (SourceSpan (..))
import Language.Fixpoint.Errors
import Language.Fixpoint.PrettyPrint


mkErr = err . sourceSpanSrcSpan 

sourceSpanSrcSpan sp = SS (sp_begin sp) (sp_end sp)

--------------------------------------------------------------------------------
-- | SourceSpans ---------------------------------------------------------------
--------------------------------------------------------------------------------

instance PP SrcSpan where
  pp    = pprint

instance PP SourceSpan where 
  pp    = pp . tx where tx (Span x y) = SS x y

---------------------------------------------------------------------
ppshow :: (PP a) => a -> String
---------------------------------------------------------------------
ppshow = render . pp

---------------------------------------------------------------------
tracePP     ::  (PP a) => String -> a -> a
---------------------------------------------------------------------
tracePP s x = trace (printf "\nTrace: [%s]: %s" s (ppshow x)) x

instance PP a => PP (Either String a) where 
  pp (Left s)  = text $ "ERROR!" ++ s
  pp (Right x) = pp x 

instance PP String where
  pp = text

instance PP Error where
  pp = pprint


---------------------------------------------------------------------------
-- | Constructing Errors --------------------------------------------------
---------------------------------------------------------------------------


bug' l s                  = err   l $ "BUG: " ++ s 
bug l s                   = mkErr l $ "BUG: " ++ s 
bugBadPhi l t1s t2s       = mkErr l $ printf "BUG: Unbalanced Phi at %s \n %s \n %s" (ppshow l) (ppshow t1s) (ppshow t2s)
bugBadSubtypes l x        = mkErr l $ printf "BUG: Unexpected Subtyping Constraint \n %s" (ppshow x)
bugMalignedFields l s s'  = mkErr l $ printf "BUG: [%s] \n CGMonad: fields not aligned: '%s' and '%s'" (ppshow l) (ppshow s) (ppshow s')
bugMalignedFields' l t t' = mkErr l $ render $ text "Misaligned Fields:"
                                             $+$ text "  t1 =" <+> pp t
                                             $+$ text "  t2 =" <+> pp t'

bugUnknownAlias l x       = mkErr l $ printf "BUG: Unknown definition for alias %s" (ppshow x)
bugUnboundPhiVar l x      = mkErr l $ printf "BUG: Phi Variable %s is unbound" (ppshow x)
bugUnboundVariable l x    = mkErr l $ printf "BUG: Variable %s is unbound in environment at %s" (ppshow x) (ppshow l)
bugUnboundFunction γ l x  = mkErr l $ printf "BUG: Function %s is unbound in environment %s at %s" (ppshow x) (ppshow γ) (ppshow l)
bugMultipleAnnots l x     = mkErr l $ printf "BUG: Multiple variable annotations for: %s" (ppshow x)
bugMissingTypeArgs l      = mkErr l $ printf "BUG: Missing Type Arguments at %s" (ppshow l)
bugTBodiesOccur l s       = mkErr l $ printf "BUG: There should be no TBodies herie %s" s
bugBadUnions l s          = mkErr l $ printf "BUG: No unions should be found here (%s)" s
bugBadFunction l          = mkErr l $ printf "BUG: No function expression was found"
bugUnknown l thing x      = mkErr l $ printf "BUG: Cannot find %s %s" thing (ppshow x) 

bugMissingClsMethAnn l x  = mkErr l $ printf "BUG: Cannot find type for %s in defined class" (ppshow x)
bugMissingClsType    l x  = mkErr l $ printf "BUG: Cannot find type for class %s" (ppshow x)

errorCyclicDefs l x stk   = mkErr l $ printf "Cyclic definitions: %s in %s" (ppshow x) (ppshow stk)
errorArgName l x y        = mkErr l $ printf "Wrong Parameter Name at %s: Saw %s but Expected %s" (ppshow l) (ppshow x) (ppshow y)  
errorMissingSpec l f      = mkErr l $ printf "Missing Signature For %s defined at %s" (ppshow f) (ppshow l)
errorDuplicate i l l'     = mkErr l $ printf "Duplicate Specification for %s:\n  %s \n  %s" (ppshow i) (ppshow l) (ppshow l')
errorArgMismatch l        = mkErr l $ printf "Mismatch in Number of Args in Call" 
errorMultipleCasts l cs   = mkErr l $ render $ text "Multiple Casts: " <+> (vcat (map pp cs)) 
errorNoMatchCallee l ts t = mkErr l $ render $   text "No matching callee type!" 
                                             $+$ text "Argument Types: " <+> pp ts 
                                             $+$ text "Function Type : " <+> pp t
errorMissingReturn l      = mkErr l $ printf "BUG: Missing Return statement at %s" (ppshow l)

errorNonFunction l f t    = mkErr l $ render $ text "Non-function type " $+$ pp f <+> text "::" <+> pp t

errorEnvJoin l x t1 t2    = mkErr l $ printf "Variable '%s' has different types ('%s' and '%s') when joining environments." (ppshow x) (ppshow t1) (ppshow t2)

errorUnboundId l x        = mkErr l $ printf "Identifier %s unbound" (ppshow x) 
errorUnboundType l x      = mkErr l $ printf "Type identifier \'%s\' unbound" (ppshow x)
errorUnboundIdEnv l x t   = mkErr l $ printf "ZOGBERT Identifier %s unbound in %s" (ppshow x) (ppshow t)
errorWrongType l m e t t' = mkErr l $ printf "%s -- unexpected type for %s :: %s expected %s" m (ppshow e) (ppshow t) (ppshow t')
errorJoin l x t t'        = mkErr l $ printf "Conflicting join for %s \n   %s\n   %s" (ppshow x) (ppshow t) (ppshow t') 
errorJoinSubsts l θ θ'    = mkErr l $ printf "Cannot join substs: %s\nand\n%s" (ppshow θ) (ppshow θ')
errorUnification l t t'   = mkErr l $ printf "Cannot unify types: %s and %s" (ppshow t) (ppshow t')
errorBoundTyVar l a t     = mkErr l $ printf "Cannot unify bound type parameter %s with %s" (ppshow a) (ppshow t)
errorFreeTyVar l t        = mkErr l $ printf "Type not fully instantiated: %s" (ppshow t)
errorWriteImmutable l x   = mkErr l $ render $ text "Cannot write variable outside local-scope" <+> pp x
                                             $+$ text "Add type annotation to indicate it is globally writable"

errorInvalidTopStmt l x   = mkErr l $ printf "Invalid top-level statement: %s" (ppshow x) 
errorOccursCheck l a t    = mkErr l $ printf "Occurs check fails: %s in %s" (ppshow a) (ppshow t)
errorRigidUnify l a t     = mkErr l $ printf "Cannot unify rigid variable %s with %s" (ppshow a) (ppshow t) 
errorSubType l m t t'     = mkErr l $ printf "%s -- Type %s is not a subtype of %s" m (ppshow t) (ppshow t')
errorCast l m e t         = mkErr l $ printf "%s -- Cannot cast non-variable expression: %s to %s" m (ppshow e) (ppshow t)
errorObjSubtyping l t t'  = mkErr l $ printf "Object type: %s is not a subtype of %s" (ppshow t) (ppshow t')
errorObjectAccess l e t   = mkErr l $ printf "Dot notation on non object expression %s :: %s" (ppshow e) (ppshow t)
errorObjectTAccess l t    = mkErr l $ printf "Dot notation not permitted on expressions of type %s" (ppshow t)
errorObjectBinding l      = mkErr l $ printf "Field does not exist in object" 
errorNullUndefined l      = mkErr l $ printf "Null type is not a subtype of undefined"
errorUniqueTypeParams l   = mkErr l $ printf "Only unique type paramteres are allowed"
errorBracketAccess l t f  = mkErr l $ printf "Cannot access field \"%s\" of type: %s" (ppshow f) (ppshow t)
errorAnnotation l e t ta  = mkErr l $ printf "Type %s does not satisfy annotation %s at expression %s." (ppshow t) (ppshow ta) (ppshow e)
errorMissingAnnot l s     = mkErr l $ printf "Missing type annotation for %s" s
errorBadAnnot l s1 s2     = mkErr l $ printf "Type annotation for %s needs to be of %s type" (ppshow s1) (ppshow s2)
errorLiquid l             = mkErr l $ printf "Liquid Type Error at %s" (ppshow l)
errorESC l                = mkErr l $ printf "ESC Error at %s" (ppshow l)
errorMultipleTypeArgs l   = mkErr l $ printf "Multiple Type Args"
errorDownCast l t         = mkErr l $ printf "Downcast: %s" (ppshow t) 
errorDeadCast l           = mkErr l $ printf "Deadcast"
errorTypeAssign l t1 t2   = mkErr l $ printf "Cannot assign type %s to %s" (ppshow t1) (ppshow t2)
errorBracketAssign l x    = mkErr l $ printf "Invalid bracket assignment %s" (ppshow x) 
errorPropRead  l x1 x2    = mkErr l $ printf "Invalid property read object: %s property: %s" (ppshow x1) (ppshow x2) 
errorArrayLit     l x     = mkErr l $ printf "Invalid array literal %s" (ppshow x) 
errorClassExtends l x y s = mkErr l $ printf "Class %s cannot extend class %s: types for property %s are incompatible" (ppshow x) (ppshow y) (ppshow s)
errorClEltAnMissing l c s = mkErr l $ printf "Class '%s' is missing an annotation for element '%s'." (ppshow c) (ppshow s)
errorVarDeclAnnot l x     = mkErr l $ printf "Variable definition of '%s' with neither type annotation nor initialization is not supported." (ppshow x)
errorConstNonFunc l x     = mkErr l $ printf "Constructor for class '%s' does not have a function type." (ppshow x)
errorConstMissing l x     = mkErr l $ printf "Constructor for class '%s' does is missing." (ppshow x)
errorClassMissing l x     = mkErr l $ printf "Cannot call 'new' on non-existing class '%s'." (ppshow x)
errorBadPAlias l p nx ne  = mkErr l $ printf "Invalid predicate alias application: %s \nExpected %d arguments, but got %d." (ppshow p) nx ne 
errorBadTAlias l t nt ne nα nx  
                          = mkErr l $ printf "Invalid type alias application: %s \nExpected %d type, %d value arguments, but got %d and %d" (ppshow t) nα nx nt ne  

errorConvDef l t1 t2      = mkErr l $ printf "Cannot convert types:\n%s\nand\n%s" (ppshow t1) (ppshow t2)
errorConvDefDepth l t1 t2 = mkErr l $ printf "No deep subtyping: '%s' and '%s'" (ppshow t1) (ppshow t2)
errorConvDefInvar l t1 t2 = mkErr l $ printf "Only invariant interface subtyping:\n%s\nand\n%s" (ppshow t1) (ppshow t2)
errorMissFlds l t1 t2 x   = mkErr l $ printf "Cannot convert:\n%s\n\nto\n\n%s\n\nType\n%s\nis missing fields %s." 
                                                  (ppshow t1) (ppshow t2) (ppshow t1) (ppshow x)
errorSuper l              = mkErr l $ printf "Cannot resolve reference to super." 
errorSuperParentMissing l = mkErr l $ printf "Calling super when parent class missing." 
