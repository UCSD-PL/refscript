{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Rsc.Errors where

import           Language.Fixpoint.Misc
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Typecheck.Types
import           Text.PrettyPrint.HughesPJ
import           Text.Printf



type FError = FixResult Error

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
errorReadOnlyUninit l x       = mkErr l $ printf "Cannot declare uninitialized readonly variable `%s`." (ppshow x)
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
unimplSSAMulVarDecl l v       = mkErr l $ show $ text "[Unimplemented] Only support a single variable declaration per" <+>
                                                 text "declaration statement. Declaration:" $+$ nest 2 (pp v) $+$
                                                 text "has mutliple."


---------------------------------------------------------------------------
-- | Types
---------------------------------------------------------------------------

errorTypeMembers l t t'       = mkErr l $ show $ text "Cannot combine type members:" $+$
                                                 nest 2 (pp t) $+$ text "and" $+$ nest 2 (pp t')

errorTypeMembersNidx l        = mkErr l $ show $ text "A type can only have a single numeric indexer."
errorTypeMembersSidx l        = mkErr l $ show $ text "A type can only have a single string indexer."

---------------------------------------------------------------------------
-- | TC
---------------------------------------------------------------------------

-- Unification
errorRigidUnify l a t         = mkErr l $ show $ text "Cannot unify rigid variable" <+> ticks (pp a) <+> text "with" <+> ticks (pp t)
errorOccursCheck l a t        = mkErr l $ printf "Occurs check fails: %s in %s" (ppshow a) (ppshow t)
errorFreeTyVar l t            = mkErr l $ printf "Type not fully instantiated: %s" (ppshow t)
errorUnification l t t'       = mkErr l $ printf "Cannot unify types: %s and %s" (ppshow t) (ppshow t')
errorMergeSubst l t t'        = mkErr l $ printf "At merging substitutions cannot unify types: %s and %s" (ppshow t) (ppshow t')
errorUniqueTypeParams l       = mkErr l $ printf "Only unique type paramteres are allowed"
errorListMismatch l ts ts'    = mkErr l $ show $ text "Cannot unify types:" $+$ cat (map (nest 2 . pp) ts) $+$
                                                 text "against" $+$ cat (map (nest 2 . pp) ts') $+$
                                                 text "because of mismatch in count."

-- Subtyping
errorDownCast l t             = mkErr l $ printf "Downcast to '%s'" (ppshow t)
errorClassExtends l x y t1 t2 = mkErr l $ printf "Type '%s' cannot extend type '%s'. Type for '%s': '%s'. Type for '%s': '%s'" (ppshow x) (ppshow y)
                                                  (ppshow x) (ppshow t1) (ppshow y) (ppshow t2)

errorAssignability l a b      = mkErr l $ show $ text "Type:" $+$ nest 2 (pp a) $+$ text "is not assignable to type:" $+$ nest 2 (pp b)
errorSubtype       l a b      = mkErr l $ show $ text "Type:" $+$ nest 2 (pp a) $+$ text "is not a subtype of type:"  $+$ nest 2 (pp b)

errorNonObjectType l t        = mkErr l $ printf "Type '%s' cannot be treated as an object type." (ppshow t)

errorIncompMutTy l m m' t t' fo = mkErr l $ show $ text "Immutability modifier" <+> ticks (pp m ) <+>
                                                 maybe empty (\f -> text "of field" <+> ticks (pp f)) fo <+>
                                                 text "of type:" $+$ nest 2 (pp t) $+$
                                                 text "is not compatible with immutability modifier" <+> ticks (pp m') <+>
                                                 maybe empty (\f -> text "of field" <+> ticks (pp f)) fo <+>
                                                 text "of type:" $+$ nest 2 (pp t')

errorUniqueRef l x            = mkErr l $ show $ text "Cannot have a unique reference in this context." $+$
                                                 maybe empty (\x' -> ticks (pp x') <+> text "seems to be unique.") x $+$
                                                 text "Hint: Try casting subexpressions to non-unique permissions."
errorUniqueAsgn l x           = mkErr l $ show $ text "Cannot re-assign unique reference:" <+> ticks (pp x) <> pp "." $+$
                                                 text "Try casting" <+> ticks (pp x) <+> text "to a non-unique type."
errorMutUnknown l t           = mkErr l $ show $ sep [text "Unknown mutability modifier:", nest 2 (pp t)]
bugMutPartInvalid l t t'      = mkErr l $ show $ text "[BUG] Invalid mutability part of types:" $+$
                                                 nest 2 (pp t) $+$ text "and" $+$
                                                 nest 2 (pp t')
errorMutInvalid l t t'        = mkErr l $ show $ sep [text "Invalid mutability type(s):", nest 2 (pp t), text "or", nest 2 (pp t')]

errorIncompCallSigs l t t'    = mkErr l $ printf "Types '%s' and '%s' have incompatible call signatures." (ppshow t) (ppshow t')
errorIncompCtorSigs l t t'    = mkErr l $ printf "Types '%s' and '%s' have incompatible constructor signatures." (ppshow t) (ppshow t')
errorIncompNIdxSigs l t t'    = mkErr l $ printf "Types '%s' and '%s' have incompatible numeric index signatures." (ppshow t) (ppshow t')
errorIncompSIdxSigs l t t'    = mkErr l $ printf "Types '%s' and '%s' have incompatible string index signatures." (ppshow t) (ppshow t')
errorObjectType l t t'        = mkErr l $ printf "Cannot treat object type '%s' nominally as type '%s'." (ppshow t) (ppshow t')
errorOptionalElt l p t t'     = mkErr l $ printf "Unmatching optionality values for property '%s' in types '%s' and '%s'." (ppshow p) (ppshow t) (ppshow t')
errorIncompatOptional l f     = mkErr l $ printf "Property '%s' has incompatible optionality modifiers." (ppshow f)
errorConstrMissing l t        = mkErr l $ printf "Could not find constructor for type '%s'." (ppshow t)

errorObjSubtype l t t' fs     = mkErr l $ show $ text "Object type:"         $+$
                                                 nest 2 (pp t)               $+$
                                                 text "is not a subtype of:" $+$
                                                 nest 2 (pp t')              $+$
                                                 text "The former is missing field(s):" <+>
                                                 intersperse comma (map (ticks . pp) fs)

errorBoundsInvalid l vs ts cs  = mkErr l $ show $ text "The inferred types:" $+$
                                                 nest 2 (intersperse comma (map pp ts))  $+$
                                                 text "for the type variables:" $+$
                                                 nest 2 (intersperse comma (map pp vs))  $+$
                                                 text "are not subtypes of the bounds:" $+$
                                                 nest 2 (intersperse comma (map pp cs))

-- Typechecking
ppBind (a,b) = pp a <+> dcolon <+> pp b

errorCallNotSup l fn ft es ts = mkErr l $ show $ text "Cannot call"       <+> ticks (pp fn)                 <+>
                                                 text "with signature"    $+$ nest 2 (pp ft)                $+$
                                                 text "with argument(s):" $+$
                                                 nest 2 (vcat (map ppBind (safeZip "errorCallNotSup" es ts)))

errorOptFunUnsup l f e        = mkErr l $ show $ text "Cannot call optional member" <+> ticks (pp f) <+> text "of expression" $+$ nest 2 (pp e)

errorMethMutIncomp l fn mts m = mkErr l $ show $ text "Cannot call"       <+> ticks (pp fn)                 <+>
                                                 text "with signature:"   $+$ nest 2 (vcat (map pp mts))    $+$
                                                 text "on" <+> pp m <+> text "receiver."

errorNoMutAvailable l e t     = mkErr l $ show $ text "Cannot retrieve mutability annotation from" <+> ticks (pp e) <+> text "of type" $+$ nest 2 (pp t)

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
errorUnboundId l x            = mkErr l $ show $ text "Identifier" <+> ticks (pp x) <+> text "is unbound."
errorEnvJoin l x t1 t2        = mkErr l $ printf "Variable '%s' has different types ('%s' and '%s') when joining environments." (ppshow x) (ppshow t1) (ppshow t2)
errorEnvJoinUnif l x t1 t2    = mkErr l $ show $ text "Error in unifying types:" $+$
                                                 nest 2 (pp t1) $+$ text "and" $+$ nest 2 (pp t2) $+$
                                                 text "for variable" <+> ticks (pp x) <+> text "when joining environments."

errorArgMismatch l x t n m    = mkErr l $ show $ text "Mismatch in number of arguments passed to function" <+>
                                                 ticks (pp x) <> text ". Signature:" $+$ pp t $+$
                                                 text "expects" <+> pp n <+> text "arguments, but" <+> pp m <+> text "were given."

errorFunArgMismatch l t t'    = mkErr l $ show $ text "Mismatch in number of arguments when subtyping:" $+$
                                                 nest 2 (pp t) $+$ text "and" $+$ nest 2 (pp t')

errorArgName l x y            = mkErr l $ printf "Wrong Parameter Name at %s: Saw %s but Expected %s" (ppshow l) (ppshow x) (ppshow y)
errorExtractNonFld l f x t    = mkErr l $ printf "Cannot extract non-field '%s' from object '%s' of type '%s'." (ppshow f) (ppshow x) (ppshow t)
errorExtractFldMut l f t      = mkErr l $ show $ text "Cannot extract mutability from field" <+> ticks (pp f) <+>
                                                 text "of type" $+$ nest 2 (pp t)

errorExtractMut l t e         = mkErr l $ show $ text "Cannot extract mutability of type" $+$ nest 2 (pp t) $+$
                                                 text "of expression" <+> ticks (pp e)

errorMissingFld l f t         = mkErr l $ printf "Field '%s' is missing from type '%s'." (ppshow f) (ppshow t)
errorNoFuncAnn l              = mkErr l $ printf "No type annotation or contextual type for anonymous function."
errorUnfoldType l t           = mkErr l $ show $ text "Could not unfold type:" $+$ nest 2 (pp t)
errorUnresolvedType l t       = mkErr l $ printf "Could not resolve type '%s'." (ppshow t)
errorUnresolvedTypes l t1 t2  = mkErr l $ printf "Could not resolve types '%s' and '%s'." (ppshow t1) (ppshow t2)
errorConsSigMissing l t       = mkErr l $ printf "Constructor signature for '%s' is missing." (ppshow t)
errorModuleExport l m x       = mkErr l $ printf "Module '%s' does not export '%s'." (ppshow m) (ppshow x)
errorDeadCast l               = mkErr l $ printf "Deadcast inserted."
errorUqMutSubtyping l e t rt  = mkErr l $ printf "No subtyping allowed at unique mutability when returning expression '%s' of type '%s' to type '%s'." (ppshow e) (ppshow t) (ppshow rt)
errorTypeParamConstr l f t c  = mkErr l $ printf "Call to function '%s' with type parameters '%s' does not fulfill constraints '%s'." (ppshow f) (ppshow t) (ppshow c)

-- Lookup
errorEnumLookup l e n         = mkErr l $ printf "Cannot find member '%s' in enumeration '%s'" (ppshow n) (ppshow e)
errorPrimLookup l e n         = mkErr l $ printf "Cannot find member '%s' in primitive type '%s'" (ppshow n) (ppshow e)
errorMemLookup l m t          = mkErr l $ printf "Cannot find member '%s' in type '%s'" (ppshow m) (ppshow t)
errorGenericLookup l f t      = mkErr l $ printf "Cannot find member '%s' in type '%s'" (ppshow f) (ppshow t)
errorAmbientLookup l t f      = mkErr l $ printf "Cannot find member '%s' in ambient element '%s'" (ppshow f) (ppshow t)
errorUnionLookup l t f        = mkErr l $ printf "Cannot find member '%s' in any part of the union '%s'" (ppshow f) (ppshow t)

errorExprTyping l e t         = mkErr l $ show $ text "Could not type expression" <+> ticks (pp e) <+> text "at type:" $+$ nest 2 (pp t)
errorLoopWiden l _ x' t t'    = mkErr l $ show $ text "The type bound to variable" <+> ticks (pp x') <> pp ":" $+$
                                                 nest 2 (pp t') $+$
                                                 text "needs to be a subtype of the type" $+$
                                                 nest 2 (pp t) $+$
                                                 text "bound to the same variable before the loop."

---------------------------------------------------------------------------
-- | LIQUID
---------------------------------------------------------------------------
errorCyclicDefs l x stk       = mkErr l $ printf "Cyclic definitions: %s in %s" (ppshow x) (ppshow stk)
errorBadTAlias l t nt ne a m  = mkErr l $ printf "Invalid type alias application: %s \nExpected %d type, %d value arguments, but got %d and %d" (ppshow t) a m nt ne
errorTAliasNumArgs l t a x n  = mkErr l $ printf "Invalid type alias application on type '%s'. Expected %d type, %d value arguments, but %d found." (ppshow t) a x n
errorTAliasMismatch l t a     = mkErr l $ printf "Invalid type alias application %s : Cannot convert %s into value argument" (ppshow t) (ppshow a)

errorBadPAlias l p nx ne      = mkErr l $ show $ text "Invalid predicate alias application:" $+$
                                                 nest 2 (pp p) $+$
                                                 text "Expected" <+> pp nx <+> text "arguments, but got" <+> pp ne <> text "."

errorNoMatchCallee l fn ts ft = mkErr l $ show $ text "No matching callee type for:" <+> ticks (pp fn)      $+$
                                                 text "Argument Types:" $+$
                                                 nest 2 (intersperse comma (map (pp . toType) ts)) $+$
                                                 text "Function Type:"  $+$
                                                 nest 2 (pp (toType ft))

errorMultipleCasts l cs       = mkErr l $ printf "Multiple Casts: %s" (ppshow cs)
errorUnsafeExtends l          = mkErr l $ printf "Unsafe Extends"
errorWellFormed l             = mkErr l $ printf "Well-formedness Error"
errorForbiddenSyms l t xs     = mkErr l $ show $ text "Symbol(s):" <+>
                                                 intersperse comma (map (ticks . pp) xs) <+>
                                                 text "is (are) not readonly, local, or an immutable field," <+>
                                                 text "so should not be appearing in the refinement of type" $+$
                                                 pp t

errorUnboundSyms l x t s m    = mkErr l $ show $ text "Symbol" <+> ticks (pp s) <+> text "appearing in type:" $+$
                                                 nest 2 (pp t) $+$ text "of" <+> ticks (pp x) <+>
                                                 text "is unbound [ERROR_CODE:" <+> pp m <+> text "]."

unimplementedReservedSyms l   = mkErr l $ printf "Please avoid using 'func' and 'obj' as symbols in refinements."
errorAsgnInRef l x t a        = mkErr l $ show $ text "Only readonly variables can be used in refinements." $+$
                                                 text "In type:" $+$ nest 2 (pp t) $+$
                                                 text "symbol" <+> ticks (pp x) <+> text "is" <+> pp a <> text "."
errorContextual l e t         = mkErr l $ printf "Contextual error occured when checking expression '%s' under type '%s'." (ppshow e) (ppshow t)
bugDeadCast l                 = mkErr l $ "Dead-cast with no error associated."
unimplSplitC l t1 t2          = mkErr l $ show $ text "[Unimplemented] Cannot generate constraints for the subtyping of" $+$
                                                 nest 2 (pp t1) $+$ text "and" $+$ nest 2 (pp t2)
bugSplitC l t1 t2             = mkErr l $ show $ text "[BUG] Cannot generate constraints for the subtyping of" $+$
                                                 nest 2 (pp t1) $+$ text "and" $+$ nest 2 (pp t2)

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
errorImmutableRefAsgn l f e t = mkErr l $ show $ text "Cannot assign field" <+> ticks (pp f) <+>
                                                 text "of expression" <+> ticks (pp e) <+>
                                                 text "through non-mutable reference of type:" $+$
                                                 nest 2 (pp t) $+$
                                                 text "[Hint: try making field" <+> ticks (pp f) <+> text "assignable]."
errorMethAsgn l m             = mkErr l $ show $ text "Cannot re-assign method" <+> ticks (pp m)

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
errorNewExprCtxType l e       = mkErr l $ printf "Cannot type new expression '%s' without a contextual type." (ppshow e)
unimpCondExpCtxType l e       = mkErr l $ show $ text "[Unimplemented] Cannot type conditional expression" $+$
                                                 nest 2 (pp e) $+$ text "without a contextual type."

bugStaticField l f c          = mkErr l $ show $ text "[BUG] Invalid annotation for static field" <+> ticks (pp f) <+> text "of class" <+> ticks (pp c) <> text "."
bugStaticMethod l f c         = mkErr l $ show $ text "[BUG] Invalid annotation for static method" <+> ticks (pp f) <+> text "of class" <+> ticks (pp c) <> text "."
errorArrayLitType l e t       = mkErr l $ printf "Cannot cast array '%s' with non array type '%s'." (ppshow e) (ppshow t)
bugArrayBIType l f t          = mkErr l $ printf "[BUG] Inconsistent built-in arrray literal ('%s') type '%s'." (ppshow f) (ppshow t)
errorBoundSubt l v t          = mkErr l $ printf "Could not find a valid instantiation to satisfy the bound of '%s': '%s'" (ppshow v) (ppshow t)
errorVarDecl l x t e s        = mkErr l $ printf "Expression '%s' of type '%s' cannot be assigned to variable '%s' with expected type '%s'" (ppshow e) (ppshow s) (ppshow x) (ppshow t)

