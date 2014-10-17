{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE OverlappingInstances      #-}

module Language.Nano.Errors where

import Debug.Trace
import Text.Printf 
import Text.PrettyPrint.HughesPJ
import Language.Nano.Locations
import Language.ECMAScript3.PrettyPrint
import Language.Fixpoint.Errors
import Language.Fixpoint.PrettyPrint


mkErr = err . sourceSpanSrcSpan 

---------------------------------------------------------------------
ppshow :: (PP a) => a -> String
---------------------------------------------------------------------
ppshow = render . pp

---------------------------------------------------------------------
tracePP     ::  (PP a) => String -> a -> a
---------------------------------------------------------------------
tracePP s x = trace (printf "\nTrace: [%s]: %s" s (ppshow x)) x

---------------------------------------------------------------------
ltracePP     ::  (PP a, IsLocated l) => l -> String -> a -> a
---------------------------------------------------------------------
ltracePP l s x = trace (printf "\nTrace: [%s: %s]: %s" (ppshow (srcPos l)) s (ppshow x)) x


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
unsupportedStaticNoInit l x   = mkErr l $ printf "Unsupported uninitialized static field '%s'." (ppshow x)
unsupportedUnionTVar l t      = mkErr l $ printf "Unsupported multiple type variables in union '%s'." (ppshow t)

bug' l s                      = err   l $ printf "BUG: %s" s 
bug l s                       = mkErr l $ printf "BUG: %s" s 
impossible l s                = mkErr l $ printf "IMPOSSIBLE: %s" s 
bugBadSubtypes l t1 t2        = mkErr l $ printf "BUG: Unexpected Subtyping Constraint\n%s <: %s" (ppshow t1) (ppshow t2)
bugMalignedFields l s s'      = mkErr l $ printf "BUG: Fields not aligned: '%s' and '%s'" (ppshow s) (ppshow s')
bugFlattenType l s            = mkErr l $ printf "BUG: Could not flatten type '%s'" (ppshow s)
bugWeakenAncestors l x1 x2    = mkErr l $ printf "BUG: When weakeninig from '%s' to '%s' (or the inverse)." (ppshow x1) (ppshow x2)

bugUnknownAlias l x           = mkErr l $ printf "BUG: Unknown definition for alias %s" (ppshow x)
bugUnboundPhiVar l x          = mkErr l $ printf "BUG: Phi Variable %s is unbound" (ppshow x)
bugUnboundVariable l   x      = mkErr l $ printf "BUG: Variable '%s' is unbound" (ppshow x)
bugMissingTypeArgs l          = mkErr l $ printf "BUG: Missing Type Arguments at %s" (ppshow l)
bugUnknown l thing x          = mkErr l $ printf "BUG: Cannot find '%s' in '%s'" thing (ppshow x) 
bugMissingModule l x          = mkErr l $ printf "BUG: Cannot find module '%s'" (ppshow x) 
bugCallTo l x es              = mkErr l $ printf "BUG: Bug at call to '%s' with args '%s'" (ppshow x) (ppshow es)
bugMultipleCasts l e          = mkErr l $ printf "BUG: Found multple casts on expression '%s'" (ppshow e)
bugNoCasts l e                = mkErr l $ printf "BUG: No casts found for expression '%s'" (ppshow e)
bugNoAnnotForGlob l x         = mkErr l $ printf "BUG: No type annotation found for global variable '%s'" (ppshow x)
bugCondExprSigParse l         = mkErr l $ printf "BUG: In parsing conditional expression signature."
bugEltSubt l f1 f2            = mkErr l $ printf "BUG: Cannot subtype type members '%s' and '%s'." (ppshow f1) ( ppshow f2)
bugSSAConstructorInit l       = mkErr l $ printf "BUG: Multiple definition of the same field." 
bugClassInstVarInit l x       = mkErr l $ printf "BUG: Instance variable '%s' initialization should have been moved to constructor." (ppshow x)
bugNestedCasts l e            = mkErr l $ printf "BUG: Nested casts on expression '%s'." (ppshow e)
bugMemberMethDecl l s         = mkErr l $ printf "BUG: MemberMethDecls are not allowed at %s." (ppshow s)

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
errorUpdateInExpr l e         = mkErr l $ printf "Unsupported: assignment in If-then-else expression %s" (ppshow e)
errorEffectInFieldDef l       = mkErr l $ printf "Cannot have effects in field initialization."
errorUninitStatFld l x        = mkErr l $ printf "Uninitialized static member '%s' is not allowed." (ppshow x)


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
errorUserCast l e t           = mkErr l $ printf "User cast of type '%s' on '%s' failed." (ppshow t) (ppshow e)
errorDownCast l t1 t2         = mkErr l $ printf "Downcast: %s => %s" (ppshow t1) (ppshow t2)
errorClassExtends l x y t1 t2 = mkErr l $ printf "Type '%s' cannot extend type '%s'.\nType for '%s':\n%s\nType for '%s':\n%s" (ppshow x) (ppshow y) 
                                                  (ppshow x) (ppshow t1) (ppshow y) (ppshow t2)
errorWidthSubtyping l es es'  = mkErr l $ printf "Invalid width subtyping between types with elements '%s' and '%s'." (ppshow es) (ppshow es')
errorIncompMutTy l t t'       = mkErr l $ printf "Types '%s' and '%s' have incompatible mutabilities." (ppshow t) (ppshow t')
errorIncompMutElt l t t'      = mkErr l $ printf "Elements '%s' and '%s' have incompatible mutabilities." (ppshow t) (ppshow t')
errorConstrMissing l t        = mkErr l $ printf "Could not find constructor for type '%s'." (ppshow t)
errorSubtype l t t'           = mkErr l $ printf "Type \n%s\n is not a subtype of\n%s" (ppshow t) (ppshow t')
errorTClassSubtype l s s'     = mkErr l $ printf "Type 'typeof %s' is not a subtype of 'typeof %s'" (ppshow s) (ppshow s')
errorTEnumSubtype l s s'      = mkErr l $ printf "Type 'enum %s' is not a subtype of 'enum %s'" (ppshow s) (ppshow s')
errorTModule l s s'           = mkErr l $ printf "Modules '%s' and '%s' are incompatible." (ppshow s) (ppshow s')
errorUnionSubtype l t t'      = mkErr l $ printf "Union type '%s' is not a subtype of '%s'" (ppshow t) (ppshow t')
errorObjSubtype l t t'        = mkErr l $ printf "Object type '%s' is not a subtype of '%s'" (ppshow t) (ppshow t')
errorFuncSubtype l t t'       = mkErr l $ printf "Function type '%s' is not a subtype of '%s'" (ppshow t) (ppshow t')

-- Typechecking
errorCallNotSup l fn ft es ts = mkErr l $ printf "Cannot call '%s' of type '%s' with argument(s) %s of type %s." (ppshow fn) (ppshow ft) (ppshow es) (ppshow ts)
errorCallNotFound l e f       = mkErr l $ printf "Cannot find callable property '%s' of object '%s'." (ppshow f) (ppshow e)
errorCallMatch l fn ts        = mkErr l $ printf "Could not match call to '%s' to a particular signature. Argument(s) with types '%s' are invalid." (ppshow fn) (ppshow ts)
errorCallReceiver l e f       = mkErr l $ printf "Could not call method '%s' of '%s'." (ppshow f) (ppshow e)
errorTypeArgsNum l n p q      = mkErr l $ printf "Type %s expects %s arguments but %s were provided" (ppshow n) (ppshow p) (ppshow q)
errorClassMissing l x         = mkErr l $ printf "Cannot call 'new' on non-existing class '%s'." (ppshow x)
errorParentClassMissing l x y = mkErr l $ printf "Class '%s' cannot extend missing class '%s'." (ppshow x) (ppshow y)
errorClassAnnotMissing l x    = mkErr l $ printf "Cannot find annotation for class '%s'." (ppshow x)
errorClassEltAnnot l c s      = mkErr l $ printf "Class '%s' needs to have a single annotation for element '%s'." (ppshow c) (ppshow s)
errorUnboundIdEnv l x t       = mkErr l $ printf "ZOGBERT Identifier '%s' unbound in %s" (ppshow x) (ppshow t)
errorUnboundName l x          = mkErr l $ printf "Name '%s' is unbounded." (ppshow x)
errorUnboundId l x            = mkErr l $ printf "Identifier '%s' is unbound" (ppshow x) 
errorEnvJoin l x t1 t2        = mkErr l $ printf "Variable '%s' has different types ('%s' and '%s') when joining environments." (ppshow x) (ppshow t1) (ppshow t2)
errorEnvJoinUnif l x t1 t2    = mkErr l $ printf "Error in unifying types '%s' and '%s' for variable '%s' when joining environments." (ppshow t1) (ppshow t2) (ppshow x)
errorArgMismatch l            = mkErr l $ printf "Mismatch in Number of arguments in signature" 
errorArgName l x y            = mkErr l $ printf "Wrong Parameter Name at %s: Saw %s but Expected %s" (ppshow l) (ppshow x) (ppshow y)  
errorExtractNonFld l f x t    = mkErr l $ printf "Cannot extract non-field '%s' from object '%s' of type '%s'." (ppshow f) (ppshow x) (ppshow t)
errorMissingFld l f t         = mkErr l $ printf "Field '%s' is missing from type '%s'." (ppshow f) (ppshow t)
errorNoFuncAnn l              = mkErr l $ printf "No type annotation or contextual type for anonymous function."
errorUnfoldType l t           = mkErr l $ printf "Could not unfold type '%s'." (ppshow t)
errorUnresolvedType l t       = mkErr l $ printf "Could not resolve type '%s'." (ppshow t)
errorUnresolvedTypes l t1 t2  = mkErr l $ printf "Could not resolve types '%s' and '%s'." (ppshow t1) (ppshow t2)
errorConsSigMissing l t       = mkErr l $ printf "Constructor signature for '%s' is missing." (ppshow t)
errorModuleExport l m x       = mkErr l $ printf "Module '%s' does not export '%s'." (ppshow m) (ppshow x)

errorDeadCast l t1 t2         = mkErr l $ printf "Cannot convert '%s' to '%s'" (ppshow t1) (ppshow t2)

---------------------------------------------------------------------------
-- | LIQUID
---------------------------------------------------------------------------
errorCyclicDefs l x stk       = mkErr l $ printf "Cyclic definitions: %s in %s" (ppshow x) (ppshow stk)
errorBadTAlias l t nt ne a m  = mkErr l $ printf "Invalid type alias application: %s \nExpected %d type, %d value arguments, but got %d and %d" (ppshow t) a m nt ne  
errorTAliasNumArgs l na nx n  = mkErr l $ printf "Invalid type alias application: Expected %d type, %d value arguments, but %d" na nx n  
errorTAliasMismatch l t a      = mkErr l $ printf "Invalid type alias application %s : Cannot convert %s into value argument" (ppshow t) (ppshow a)
                                
errorBadPAlias l p nx ne      = mkErr l $ printf "Invalid predicate alias application: %s \nExpected %d arguments, but got %d." (ppshow p) nx ne 
errorLiquid l                 = mkErr l $ printf "Liquid Type Error" 
errorNoMatchCallee l fn ts t  = mkErr l $ printf "No matching callee type for '%s'.\nArgument Types: %s\nFunction Type: %s" (ppshow fn) (ppshow ts) (ppshow t)
errorMultipleCasts l cs       = mkErr l $ printf "Multiple Casts: %s" (ppshow cs) 
errorUnsafeExtends l          = mkErr l $ printf "Unsafe Extends"
errorWellFormed l             = mkErr l $ printf "Well-formedness Error" 
 
---------------------------------------------------------------------------
-- | Pervasive (typechecking TC and Liquid)
---------------------------------------------------------------------------
errorSuper l                  = mkErr l $ printf "Cannot resolve reference to super." 
errorMissingFields l t1 t2 x  = mkErr l $ printf "Cannot convert %s to %s. Type %s is missing fields %s." (ppshow t1) (ppshow t2) (ppshow t1) (ppshow x) 
errorVarDeclAnnot l x         = mkErr l $ printf "Variable definition of '%s' can have at most one type annotation." (ppshow x)
errorMissingAnnot l s         = mkErr l $ printf "Missing type annotation for %s." s
errorNonFunction l f t        = mkErr l $ printf "Non-function type: %s :: %s." (ppshow f) (ppshow t)
errorMissingReturn l          = mkErr l $ printf "Missing Return statement."
errorMissingSpec l f          = mkErr l $ printf "Missing signature for '%s'." (ppshow f)
errorVariadic l f             = mkErr l $ printf "Cannot call variadic on type '%s'." (ppshow f)
errorVariadicNoArgs l f       = mkErr l $ printf "Cannot make variadic call '%s' without arguments." (ppshow f)
errorConflateTypeMembers l es = mkErr l $ printf "Cannot conflate type members '%s'." (ppshow es)  
