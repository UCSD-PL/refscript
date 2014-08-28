{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE OverlappingInstances      #-}

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
-- | Bugs
---------------------------------------------------------------------------
unimplemented l s x           = mkErr l $ printf "Unimplemented %s: %s" (ppshow s) (ppshow x)
unsupportedConsTy l t         = mkErr l $ printf "Unsupported constructor type %s" (ppshow t)
unsupportedNonSingleConsTy l  = mkErr l $ printf "Only a single constructor signature is supported."
unsupportedDotRef l t         = mkErr l $ printf "Unsupported dot reference %s" (ppshow t)
unsupportedConvFun l t1 t2    = mkErr l $ printf "Unsupported case in convertFun:\n%s\nvs\n%s" (ppshow t1) (ppshow t2)

bug' l s                      = err   l $ "BUG: " ++ s 
bug l s                       = mkErr l $ "BUG: " ++ s 
impossible l s                = mkErr l $ "IMPOSSIBLE" ++ s 
bugBadSubtypes l t1 t2        = mkErr l $ printf "BUG: Unexpected Subtyping Constraint\n%s <: %s" (ppshow t1) (ppshow t2)
bugMalignedFields l s s'      = mkErr l $ printf "BUG: [%s] \n CGMonad: fields not aligned: '%s' and '%s'" (ppshow l) (ppshow s) (ppshow s')

bugUnknownAlias l x           = mkErr l $ printf "BUG: Unknown definition for alias %s" (ppshow x)
bugUnboundPhiVar l x          = mkErr l $ printf "BUG: Phi Variable %s is unbound" (ppshow x)
bugUnboundVariable l   x      = mkErr l $ printf "BUG: Variable '%s' is unbound" (ppshow x)
bugMissingTypeArgs l          = mkErr l $ printf "BUG: Missing Type Arguments at %s" (ppshow l)
bugUnknown l thing x          = mkErr l $ printf "BUG: Cannot find '%s' in '%s'" thing (ppshow x) 
bugMissingModule l x          = mkErr l $ printf "BUG: Cannot find module '%s'" (ppshow x) 
bugCallTo l x es              = mkErr l $ printf "BUG: Bug at call to '%s' with args '%s'" (ppshow x) (ppshow es)
bugMultipleCasts l e          = mkErr l $ printf "BUG: Found multple casts on expression '%s'" (ppshow e)
bugNoAnnotForGlob l x         = mkErr l $ printf "BUG: No type annotation found for global variable '%s'" (ppshow x)

bugClassDefNotFound l x       = mkErr l $ printf "BUG: Class definition for '%s' not found." (ppshow x)
bugEnvFindTy l x              = mkErr l $ printf "BUG: envFindTy failed to find binding '%s'" (ppshow x)
bugZipType l t1 t2            = mkErr l $ printf "BUG: zipType of types '%s' and '%s'" (ppshow t1) (ppshow t2)


---------------------------------------------------------------------------
-- | Nano
---------------------------------------------------------------------------
errorInvalidTopStmt l x       = mkErr l $ printf "Invalid top-level statement: %s" (ppshow x) 
errorDuplicate i l l'         = mkErr l $ printf "Duplicate Specification for %s:\n  %s \n  %s" (ppshow i) (ppshow l) (ppshow l')


---------------------------------------------------------------------------
-- | SSA
---------------------------------------------------------------------------
errorWriteImmutable l x       = mkErr l $ printf "Cannot assign to local variable '%s' outside local-scope. " (ppshow x)
                                       ++ printf "Add a type annotation to indicate it is globally writable." 
errorSSAUnboundId l x         = mkErr l $ printf "SSA: Identifier '%s' unbound" (ppshow x) 

errorUpdateInExpr l e       = mkErr l $ printf "Unsupported: assignment in If-then-else expression %s" (ppshow e)
---------------------------------------------------------------------------
-- | TC 
---------------------------------------------------------------------------

-- Unification
errorRigidUnify l a t         = mkErr l $ printf "Cannot unify rigid variable '%s' with '%s'" (ppshow a) (ppshow t)
errorOccursCheck l a t        = mkErr l $ printf "Occurs check fails: %s in %s" (ppshow a) (ppshow t)
errorFreeTyVar l t            = mkErr l $ printf "Type not fully instantiated: %s" (ppshow t)
errorUnification l t t'       = mkErr l $ printf "Cannot unify types: %s and %s" (ppshow t) (ppshow t')
errorMergeSubst l t t'        = mkErr l $ printf "At merging substitutions cannot unify types: %s and %s" (ppshow t) (ppshow t')
errorUniqueTypeParams l       = mkErr l $ printf "Only unique type paramteres are allowed"

-- Subtyping
errorDownCast l t1 t2         = mkErr l $ printf "Downcast: %s => %s" (ppshow t1) (ppshow t2)
errorClassExtends l x y s     = mkErr l $ printf "Type '%s' cannot extend type '%s'.Types of elements %s are incompatible."   
                                                   (ppshow x) (ppshow y) (ppshow s)
errorIncompMutTy l t t'       = mkErr l $ printf "Types '%s' and '%s' have incompatible mutabilities." (ppshow t) (ppshow t')
errorIncompMutElt l t t'      = mkErr l $ printf "Elements '%s' and '%s' have incompatible mutabilities." (ppshow t) (ppshow t')
errorConstrMissing l t        = mkErr l $ printf "Could not find constructor for type '%s'." (ppshow t)
errorSubtype l t t'           = mkErr l $ printf "Type \n%s\n is not a subtype of\n%s" (ppshow t) (ppshow t')
errorTClassSubtype l s s'     = mkErr l $ printf "Type 'typeof %s' is not a subtype of 'typeof %s'" (ppshow s) (ppshow s')
errorTModule l s s'           = mkErr l $ printf "Modules '%s' and '%s' are incompatible." (ppshow s) (ppshow s')
errorUnionSubtype l t t'      = mkErr l $ printf "Union type '%s' is not a subtype of '%s'" (ppshow t) (ppshow t')
errorObjSubtype l t t'        = mkErr l $ printf "Object type '%s' is not a subtype of '%s'" (ppshow t) (ppshow t')
errorFuncSubtype l t t'       = mkErr l $ printf "Function type '%s' is not a subtype of '%s'" (ppshow t) (ppshow t')

-- Typechecking
errorCallNotSup l fn es ts    = mkErr l $ printf "Cannot call '%s' with argument(s): %s of type %s" (ppshow fn) (ppshow es) (ppshow ts)
errorCallMatch l fn ts        = mkErr l $ printf "Could not match call to '%s' to a particular signature. Argument(s) with types '%s' are invalid." (ppshow fn) (ppshow ts)
errorCallReceiver l e f       = mkErr l $ printf "Could not call method '%s' of '%s'." (ppshow f) (ppshow e)
errorTypeArgsNum l n p q      = mkErr l $ printf "Type %s expects %s arguments but %s were provided" (ppshow n) (ppshow p) (ppshow q)
errorClassMissing l x         = mkErr l $ printf "Cannot call 'new' on non-existing class '%s'." (ppshow x)
errorParentClassMissing l x y = mkErr l $ printf "Class '%s' cannot extend missing class '%s'." (ppshow x) (ppshow y)
errorClassAnnotMissing l x    = mkErr l $ printf "Cannot find annotation for class '%s'." (ppshow x)
errorClassEltAnnot l c s      = mkErr l $ printf "Class '%s' needs to have a single annotation for element '%s'." (ppshow c) (ppshow s)
errorUnboundIdEnv l x t       = mkErr l $ printf "ZOGBERT Identifier '%s' unbound in %s" (ppshow x) (ppshow t)
errorUnboundType l x          = mkErr l $ printf "Type identifier '%s' unbound" (ppshow x)
errorUnboundId l x            = mkErr l $ printf "Identifier '%s' is unbound" (ppshow x) 
errorEnvJoin l x t1 t2        = mkErr l $ printf "Variable '%s' has different types ('%s' and '%s') when joining environments." (ppshow x) (ppshow t1) (ppshow t2)
errorArgMismatch l            = mkErr l $ printf "Mismatch in Number of arguments in signature" 
errorArgName l x y            = mkErr l $ printf "Wrong Parameter Name at %s: Saw %s but Expected %s" (ppshow l) (ppshow x) (ppshow y)  
errorExtractNonFld l f x t    = mkErr l $ printf "Cannot extract non-field '%s' from object '%s' of type '%s'." (ppshow f) (ppshow x) (ppshow t)
errorMissingFld l f t         = mkErr l $ printf "Field '%s' is missing from type '%s'." (ppshow f) (ppshow t)
errorNonSingleFuncAnn l       = mkErr l $ printf "Anonymous function needs to have exactly one type annotation."
errorUnfoldType l t           = mkErr l $ printf "Could not unfold type '%s'." (ppshow t)
errorUnresolvedType l t       = mkErr l $ printf "Could not resolve type '%s'." (ppshow t)
errorUnresolvedTypes l t1 t2  = mkErr l $ printf "Could not resolve types '%s' and '%s'." (ppshow t1) (ppshow t2)
errorConsSigMissing l t       = mkErr l $ printf "Constructor signature for '%s' is missing." (ppshow t)
errorModuleExport l m x       = mkErr l $ printf "Module '%s' does not export '%s'." (ppshow m) (ppshow x)


---------------------------------------------------------------------------
-- | LIQUID
---------------------------------------------------------------------------
errorCyclicDefs l x stk       = mkErr l $ printf "Cyclic definitions: %s in %s" (ppshow x) (ppshow stk)
errorBadTAlias l t nt ne a m  = mkErr l $ printf "Invalid type alias application: %s \nExpected %d type, %d value arguments, but got %d and %d" (ppshow t) a m nt ne  
errorBadPAlias l p nx ne      = mkErr l $ printf "Invalid predicate alias application: %s \nExpected %d arguments, but got %d." (ppshow p) nx ne 
errorLiquid l                 = mkErr l $ printf "Liquid Type Error" 
errorNoMatchCallee l fn ts t  = mkErr l $ printf "No matching callee type for '%s'.\nArgument Types: %s\nFunction Type: %s" (ppshow fn) (ppshow ts) (ppshow t)
errorMultipleCasts l cs       = mkErr l $ render $ text "Multiple Casts: " <+> (vcat (map pp cs)) 

 
---------------------------------------------------------------------------
-- | Pervasive (typechecking TC and Liquid)
---------------------------------------------------------------------------
errorSuper l                  = mkErr l $ printf "Cannot resolve reference to super." 
errorMissingFields l t1 t2 x  = mkErr l $ printf "Cannot convert: %s to %s. Type %s is missing fields %s." (ppshow t1) (ppshow t2) (ppshow t1) (ppshow x) 
errorVarDeclAnnot l x         = mkErr l $ printf "Variable definition of '%s' with neither type annotation nor initialization is not supported." (ppshow x)
errorMissingAnnot l s         = mkErr l $ printf "Missing type annotation for %s" s
errorNonFunction l f t        = mkErr l $ printf "Non-function type: %s :: %s " (ppshow f) (ppshow t)
errorMissingReturn l          = mkErr l $ printf "Missing Return statement."
errorMissingSpec l f          = mkErr l $ printf "Missing signature for '%s'" (ppshow f)

