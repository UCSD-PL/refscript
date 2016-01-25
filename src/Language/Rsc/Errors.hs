{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Rsc.Errors where

import           Debug.Trace
import           Language.Rsc.AST.Syntax
import           Language.Rsc.Locations
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Typecheck.Types
import           Text.PrettyPrint.HughesPJ
import           Text.Printf

import           Language.Fixpoint.Misc
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Types.PrettyPrint


mkErr = err . sourceSpanSrcSpan

---------------------------------------------------------------------------
-- | Bugs
---------------------------------------------------------------------------
unimplemented l s x           = mkErr l $ printf "Unimplemented %s: %s" (ppshow s) (ppshow x)
unsupportedConsTy l t         = mkErr l $ printf "Unsupported constructor type %s" (ppshow t)
unsupportedNonSingleConsTy l  = mkErr l $ printf "Only a single constructor signature is supported."
unsupportedDotRef l t         = mkErr l $ printf "Unsupported dot reference %s" (ppshow t)
unsupportedConvFun l t1 t2    = mkErr l $ printf "Unsupported case in convertFun: '%s' and '%s'" (ppshow t1) (ppshow t2)
unsupportedSplitElt l t1 t2   = mkErr l $ printf "Unsupported case in splitTM: '%s' and '%s'" (ppshow t1) (ppshow t2)
unsupportedStaticNoInit l x   = mkErr l $ printf "Unsupported uninitialized static field '%s'." (ppshow x)
unsupportedUnionTVar l t      = mkErr l $ printf "Unsupported multiple type variables in union '%s'." (ppshow t)
unsupportedMethodComp l m m'  = mkErr l $ printf "Unsupported method comparison between '%s' and '%s'." (ppshow m) (ppshow m')
unsupportedUnionAccess l t f  = mkErr l $ printf "Unsupported access of field '%s' in union type '%s'." (ppshow f) (ppshow t)

bug' l s                      = err   l $ printf "BUG: %s" s
bug l s                       = mkErr l $ printf "BUG: %s" s
impossible l s                = mkErr l $ printf "IMPOSSIBLE: %s" s
bugBadSubtypes l t1 t2        = mkErr l $ printf "BUG: Unexpected Subtyping Constraint\n%s <: %s" (ppshow t1) (ppshow t2)
bugMalignedFields l s s'      = mkErr l $ printf "BUG: Fields not aligned: '%s' and '%s'" (ppshow s) (ppshow s')
bugExpandType l s             = mkErr l $ printf "BUG: Could not expand type '%s'" (ppshow s)
bugWeakenAncestors l x1 x2    = mkErr l $ printf "BUG: When weakeninig from '%s' to '%s' (or the inverse)." (ppshow x1) (ppshow x2)

bugUnknownAlias l x           = mkErr l $ printf "BUG: Unknown definition for alias %s" (ppshow x)
bugUnboundPhiVar l x          = mkErr l $ printf "BUG: Phi Variable %s is unbound" (ppshow x)
bugUnboundVariable l   x      = mkErr l $ printf "BUG: Variable '%s' is unbound" (ppshow x)
bugMissingTypeArgs l x        = mkErr l $ show $ text "BUG: Missing type arguments at call to" <+> pp x
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
bugConsSigMissing l           = mkErr l $ printf "BUG: Constructor signature missing."

bugClassDefNotFound l x       = mkErr l $ printf "BUG: Class definition for '%s' not found." (ppshow x)
bugEnvFindTy l x              = mkErr l $ printf "BUG: envFindTy failed to find binding '%s'" (ppshow x)
bugZipType l t1 t2            = mkErr l $ printf "BUG: zipType of types '%s' and '%s'" (ppshow t1) (ppshow t2)
bugGetMutability l t          = mkErr l $ printf "BUG: Cannot get mutability from type '%s'." (ppshow t)


---------------------------------------------------------------------------
-- | Rsc
---------------------------------------------------------------------------
errorInvalidTopStmt l x       = mkErr l $ printf "Invalid top-level statement: %s" (ppshow x)
errorDuplicate i l l'         = mkErr l $ printf "Duplicate specification for '%s':\n  %s \n  %s" (ppshow i) (ppshow l) (ppshow l')
errorDuplicateKey l x         = mkErr l $ printf "Duplicate key '%s' when merging creating environment" (ppshow x)
errorInvalidHex l x           = mkErr l $ printf "'%s' is not a valid HEX value" (ppshow x)
errorIllFormedType l t        = mkErr l $ printf "Type '%s' is ill-formed" (ppshow t)


---------------------------------------------------------------------------
-- | SSA
---------------------------------------------------------------------------
errorWriteImmutable l m x     = mkErr l $ printf "Cannot assign to %s-variable '%s'. " (ppshow m) (ppshow x)
                                       ++ printf "Add a type annotation to indicate it is globally writable."
errorSSAUnboundId l x         = mkErr l $ printf "SSA: Identifier '%s' unbound" (ppshow x)
errorUpdateInExpr l e         = mkErr l $ printf "Unsupported: assignment in If-then-else expression %s" (ppshow e)
errorEffectInFieldDef l       = mkErr l $ printf "Cannot have effects in field initialization."
errorUninitStatFld l x        = mkErr l $ printf "Uninitialized static member '%s' is not allowed." (ppshow x)
errorForeignLocal l x         = mkErr l $ printf "Cannot reference local (out-of-scope) variable '%s'" (ppshow x)
bugSuperNotHandled l e        = mkErr l $ printf "BUG: Expression '%s' should have been taken care of." (ppshow e)
bugSuperWithNoParent l        = mkErr l $ printf "BUG: Calling 'super()' in constructor of class with no parent."
unimplementedInfix l e        = mkErr l $ printf "UNIMPLEMENTED: Infix expression '%s' as standalone expression." (ppshow e)


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
errorUserCast l t e           = mkErr l $ printf "User cast of type '%s' on '%s' failed." (ppshow t) (ppshow e)
errorDownCast l t             = mkErr l $ printf "Downcast to '%s'" (ppshow t)
errorClassExtends l x y t1 t2 = mkErr l $ printf "Type '%s' cannot extend type '%s'. Type for '%s': '%s'. Type for '%s': '%s'" (ppshow x) (ppshow y)
                                                  (ppshow x) (ppshow t1) (ppshow y) (ppshow t2)
errorIncompatTypes l a b      = mkErr l $ printf "Type '%s' is not assignable to '%s'." (ppshow a) (ppshow b)
errorIncompatCovFields l a b  = mkErr l $ printf "Incompatible covariant fields when converting between elements: '%s' and '%s'." (ppshow a) (ppshow b)
errorIncompatInvFields l a b  = mkErr l $ printf "Incompatible invariant fields when converting between elements: '%s' and '%s'." (ppshow a) (ppshow b)
errorNonObjectType l t        = mkErr l $ printf "Type '%s' cannot be treated as an object type." (ppshow t)
errorIncompMutTy l t t'       = mkErr l $ printf "Types '%s' and '%s' have incompatible mutabilities." (ppshow t) (ppshow t')
errorIncompMethMut l m        = mkErr l $ printf "Mutability modifiers of method '%s' are incompatible." (ppshow m)
errorIncompCallSigs l t t'    = mkErr l $ printf "Types '%s' and '%s' have incompatible call signatures." (ppshow t) (ppshow t')
errorIncompCtorSigs l t t'    = mkErr l $ printf "Types '%s' and '%s' have incompatible constructor signatures." (ppshow t) (ppshow t')
errorIncompNIdxSigs l t t'    = mkErr l $ printf "Types '%s' and '%s' have incompatible numeric index signatures." (ppshow t) (ppshow t')
errorIncompSIdxSigs l t t'    = mkErr l $ printf "Types '%s' and '%s' have incompatible string index signatures." (ppshow t) (ppshow t')
errorObjectType l t t'        = mkErr l $ printf "Cannot treat object type '%s' nominally as type '%s'." (ppshow t) (ppshow t')
errorIncompMutElt l f m m'    = mkErr l $ printf "Incompatible assignability modifer for property '%s'. Wanted %s, but got %s." (ppshow f) (ppshow m') (ppshow m)
errorOptionalElt l p t t'     = mkErr l $ printf "Unmatching optionality values for property '%s' in types '%s' and '%s'." (ppshow p) (ppshow t) (ppshow t')
errorIncompatOptional l f     = mkErr l $ printf "Property '%s' has incompatible optionality modifiers." (ppshow f)
errorConstrMissing l t        = mkErr l $ printf "Could not find constructor for type '%s'." (ppshow t)
errorSubtype l t t'           = mkErr l $ printf "Type '%s' is not a subtype of '%s'" (ppshow t) (ppshow t')
errorTClassSubtype l s s'     = mkErr l $ printf "Type 'typeof %s' is not a subtype of 'typeof %s'" (ppshow s) (ppshow s')
errorTEnumSubtype l s s'      = mkErr l $ printf "Type 'enum %s' is not a subtype of 'enum %s'." (ppshow s) (ppshow s')
errorTModule l s s'           = mkErr l $ printf "Modules '%s' and '%s' are incompatible." (ppshow s) (ppshow s')
errorUnionSubtype l t t'      = mkErr l $ printf "Union type '%s' is not a subtype of '%s'" (ppshow t) (ppshow t')
errorObjSubtype l t t' fs     = mkErr l $ printf "Object type '%s' is not a subtype of '%s'. The latter is missing fields: %s." (ppshow t) (ppshow t')
                                                  (show $ intersperse comma $ map pp fs)
errorFuncSubtype l t t'       = mkErr l $ printf "Function type '%s' is not a subtype of '%s'" (ppshow t) (ppshow t')

-- Typechecking
errorCallNotSup l fn ft es ts = mkErr l $ show $ text "Cannot call"       <+> ticks (pp fn)                 <+>
                                                 text "with signature"    $+$ nest 2 (pp ft)                $+$
                                                 text "with argument(s):" <+> intersperse comma (map pp es) $+$
                                                 text "of type(s):"       <+> intersperse comma (map pp ts)

errorCallNotFound l e f       = mkErr l $ printf "Cannot find callable property '%s' of type '%s'." (ppshow f) (ppshow e)
errorCallMatch l fn ts        = mkErr l $ printf "Could not match call to '%s' to a particular signature. Argument(s) with types '%s' are invalid." (ppshow fn) (ppshow ts)
errorCallReceiver l e f       = mkErr l $ printf "Could not call method '%s' of '%s'." (ppshow f) (ppshow e)
errorTypeArgsNum l n p q      = mkErr l $ printf "Type %s expects %s arguments but %s were provided" (ppshow n) (ppshow p) (ppshow q)
errorClassMissing l x         = mkErr l $ printf "Cannot call 'new' on non-existing class '%s'." (ppshow x)
errorParentClassMissing l x y = mkErr l $ printf "Class '%s' cannot extend missing class '%s'." (ppshow x) (ppshow y)
errorClassAnnotMissing l x    = mkErr l $ printf "Cannot find annotation for class '%s'." (ppshow x)
errorClassEltAnnot l c s      = mkErr l $ printf "Class '%s' needs to have a single annotation for element '%s'." (ppshow c) (ppshow s)
errorUnboundIdEnv l x t       = mkErr l $ printf "ZOGBERT Identifier '%s' unbound in %s" (ppshow x) (ppshow t)
errorUnboundName l x          = mkErr l $ printf "Name '%s' is unbound." (ppshow x)
errorUnboundPath l x          = mkErr l $ printf "Path '%s' is unbound." (ppshow x)
errorUnboundId l x            = mkErr l $ printf "Identifier '%s' is unbound." (ppshow x)
errorEnvJoin l x t1 t2        = mkErr l $ printf "Variable '%s' has different types ('%s' and '%s') when joining environments." (ppshow x) (ppshow t1) (ppshow t2)
errorEnvJoinUnif l x t1 t2    = mkErr l $ printf "Error in unifying types '%s' and '%s' for variable '%s' when joining environments." (ppshow t1) (ppshow t2) (ppshow x)
errorArgMismatch l            = mkErr l $ printf "Mismatch in Number of arguments in signature"
errorArgName l x y            = mkErr l $ printf "Wrong Parameter Name at %s: Saw %s but Expected %s" (ppshow l) (ppshow x) (ppshow y)
errorExtractNonFld l f x t    = mkErr l $ printf "Cannot extract non-field '%s' from object '%s' of type '%s'." (ppshow f) (ppshow x) (ppshow t)
errorMissingFld l f t         = mkErr l $ printf "Field '%s' is missing from type '%s'." (ppshow f) (ppshow t)
errorNoFuncAnn l              = mkErr l $ printf "No type annotation or contextual type for anonymous function."
errorUnfoldType l t           = mkErr l $ printf "Could not unfold type '%s'." (ppshow t)
errorUnfoldTypes l t1 t2      = mkErr l $ printf "Could not unfold one of the types: '%s' and '%s'." (ppshow t1) (ppshow t2)
errorUnresolvedType l t       = mkErr l $ printf "Could not resolve type '%s'." (ppshow t)
errorUnresolvedTypes l t1 t2  = mkErr l $ printf "Could not resolve types '%s' and '%s'." (ppshow t1) (ppshow t2)
errorConsSigMissing l t       = mkErr l $ printf "Constructor signature for '%s' is missing." (ppshow t)
errorModuleExport l m x       = mkErr l $ printf "Module '%s' does not export '%s'." (ppshow m) (ppshow x)
errorDeadCast l t1 t2         = mkErr l $ printf "Generic deadcast error when converting from type '%s' to '%s'" (ppshow t1) (ppshow t2)
errorUqMutSubtyping l e t rt  = mkErr l $ printf "No subtyping allowed at unique mutability when returning expression '%s' of type '%s' to type '%s'." (ppshow e) (ppshow t) (ppshow rt)
errorTypeParamConstr l f t c  = mkErr l $ printf "Call to function '%s' with type parameters '%s' does not fulfill constraints '%s'." (ppshow f) (ppshow t) (ppshow c)

-- Lookup
errorEnumLookup l e n         = mkErr l $ printf "Cannot find member '%s' in enumeration '%s'" (ppshow n) (ppshow e)
errorPrimLookup l e n         = mkErr l $ printf "Cannot find member '%s' in primitive type '%s'" (ppshow n) (ppshow e)
errorMemLookup l t m          = mkErr l $ printf "Cannot find MEmber '%s' in type '%s'" (ppshow m) (ppshow t)
errorGenericLookup l t f      = mkErr l $ printf "Cannot find member '%s' in type '%s'" (ppshow f) (ppshow t)
errorAmbientLookup l t f      = mkErr l $ printf "Cannot find member '%s' in ambient element '%s'" (ppshow f) (ppshow t)
errorUnionLookup l t f        = mkErr l $ printf "Cannot find member '%s' in any part of the union '%s'" (ppshow f) (ppshow t)

---------------------------------------------------------------------------
-- | LIQUID
---------------------------------------------------------------------------
errorCyclicDefs l x stk       = mkErr l $ printf "Cyclic definitions: %s in %s" (ppshow x) (ppshow stk)
errorBadTAlias l t nt ne a m  = mkErr l $ printf "Invalid type alias application: %s \nExpected %d type, %d value arguments, but got %d and %d" (ppshow t) a m nt ne
errorTAliasNumArgs l t a x n  = mkErr l $ printf "Invalid type alias application on type '%s'. Expected %d type, %d value arguments, but %d found." (ppshow t) a x n
errorTAliasMismatch l t a     = mkErr l $ printf "Invalid type alias application %s : Cannot convert %s into value argument" (ppshow t) (ppshow a)

errorBadPAlias l p nx ne      = mkErr l $ printf "Invalid predicate alias application: %s \nExpected %d arguments, but got %d." (ppshow p) nx ne

errorNoMatchCallee l fn ts ft = mkErr l $ show $ text "No matching callee type for:" <+> ticks (pp fn)      $+$
                                                 text "Argument Types:" <+> intersperse comma (map (pp . toType) ts) $+$
                                                 text "Function Type:"  <+> pp (toType ft)

errorMultipleCasts l cs       = mkErr l $ printf "Multiple Casts: %s" (ppshow cs)
errorUnsafeExtends l          = mkErr l $ printf "Unsafe Extends"
errorWellFormed l             = mkErr l $ printf "Well-formedness Error"
errorForbiddenSyms l t xs     = mkErr l $ show $ text "Symbol(s):" <+>
                                                 intersperse comma (map (ticks . pp) xs) <+>
                                                 text "is (are) not readonly, local, or an immutable field," <+>
                                                 text "so should not be appearing in the refinement of type" $+$
                                                 pp t

errorUnboundSyms l x t s m    = mkErr l $ printf "Symbol '%s', appearing in type '%s' of '%s' is unbound [ERROR_CODE: %s]." (ppshow s) (ppshow t) (ppshow x) (ppshow m)
unimplementedReservedSyms l   = mkErr l $ printf "Please avoid using 'func' and 'obj' as symbols in refinements."
errorAsgnInRef l x t a        = mkErr l $ printf "Only readonly variables can be used in refinements. In type '%s' symbol '%s' is %s." (ppshow t) (ppshow x) (ppshow a)
errorContextual l e t         = mkErr l $ printf "Contextual error occured when checking expression '%s' under type '%s'." (ppshow e) (ppshow t)
bugDeadCast l                 = mkErr l $ "Dead-cast with no error associated."
unimplSplitUnion l t1 t2      = mkErr l $ printf "Cannot generate constraints for the subtyping of '%s' and '%s'." (ppshow t1) (ppshow t2)

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
errorCallSuperOnNonClass l x  = mkErr l $ printf "Cannot call 'super' on non class type '%s'." (ppshow x)
errorAssignsFields l x t      = mkErr l $ printf "Variable '%s' with type '%s' can only be assigned fields or returned." (ppshow x) (ppshow t)
errorFinalField l f t         = mkErr l $ printf "Cannot assign final field '%s' of type '%s'." (ppshow f) (ppshow t)
errorImmutableRefAsgn l f t   = mkErr l $ printf "Cannot assign field '%s' through non-mutable reference of type '%s' [Hint: try making field '%s' assignable]." (ppshow f) (ppshow t) (ppshow f)
errorCallOptional l m t       = mkErr l $ printf "Cannot call optional field '%s' of type '%s'." (ppshow m) (ppshow t)

errorUnionMergePrims l t a b  = mkErr l $ printf "In type '%s', cannot merge primitive types '%s' and '%s'." (ppshow t) (ppshow a) (ppshow b)
errorUnionMergeVars l t a b   = mkErr l $ printf "In type '%s', cannot merge type variables '%s' and '%s'." (ppshow t) (ppshow a) (ppshow b)
errorUnionMergeAnds l t a b   = mkErr l $ printf "In type '%s', cannot merge intersection types '%s' and '%s'." (ppshow t) (ppshow a) (ppshow b)
errorUnionMergeObjs l t a b   = mkErr l $ printf "In type '%s', cannot merge object types '%s' and '%s', as they correspond to structurally equivalent types." (ppshow t) (ppshow a) (ppshow b)
errorUnionMergeAlls l t       = mkErr l $ printf "In type '%s', cannot have type abstraction as part of a union." (ppshow t)
errorUnionMergeTys l t a b    = mkErr l $ printf "In type '%s', cannot merge named types '%s' and '%s'." (ppshow t) (ppshow a) (ppshow b)
errorUnionMergeMods l t a b   = mkErr l $ printf "In type '%s', cannot merge module types '%s' and '%s'." (ppshow t) (ppshow a) (ppshow b)
errorUnionMergeFuns l t       = mkErr l $ printf "In type '%s', cannot merge multiple function types." (ppshow t)

errorArrayLitCtxType l e      = mkErr l $ printf "Cannot type array literal '%s' without a contextual type." (ppshow e)
errorArrayLitType l e t       = mkErr l $ printf "Cannot cast array '%s' with non array type '%s'." (ppshow e) (ppshow t)
bugArrayBIType l f t          = mkErr l $ printf "[BUG] Inconsistent built-in arrray literal ('%s') type '%s'." (ppshow f) (ppshow t)
errorBoundSubt l v t          = mkErr l $ printf "Could not find a valid instantiation to satisfy the bound of '%s': '%s'" (ppshow v) (ppshow t)
errorVarDecl l x t e s        = mkErr l $ printf "Expression '%s' of type '%s' cannot be assigned to variable '%s' with expected type '%s'" (ppshow e) (ppshow s) (ppshow x) (ppshow t)

