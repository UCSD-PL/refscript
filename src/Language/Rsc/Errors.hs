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
unimplemented l s x           = mkErr l $ text $ printf "Unimplemented %s: %s" (ppshow s) (ppshow x)
unsupportedNonSingleConsTy l  = mkErr l $ text $ printf "Only a single constructor signature is supported."
unsupportedConvFun l t1 t2    = mkErr l $ text $ printf "Unsupported case in convertFun: '%s' and '%s'" (ppshow t1) (ppshow t2)
unsupportedSplitElt l t1 t2   = mkErr l $ text $ printf "Unsupported case in splitTM: '%s' and '%s'" (ppshow t1) (ppshow t2)
unsupportedStaticNoInit l x   = mkErr l $ text $ printf "Unsupported uninitialized static field '%s'." (ppshow x)
unsupportedUnionTVar l t      = mkErr l $ text $ printf "Unsupported multiple type variables in union '%s'." (ppshow t)
unsupportedMethodComp l m m'  = mkErr l $ text $ printf "Unsupported method comparison between '%s' and '%s'." (ppshow m) (ppshow m')

bug' l s                      = err   l $ text $ printf "BUG: %s" s
bug l s                       = mkErr l $ text $ printf "BUG: %s" s
impossible l s                = mkErr l $ text $ printf "IMPOSSIBLE: %s" s
bugBadSubtypes l t1 t2        = mkErr l $ text $ printf "BUG: Unexpected Subtyping Constraint\n%s <: %s" (ppshow t1) (ppshow t2)
bugUnknownAlias l x           = mkErr l $ text $ printf "BUG: Unknown definition for alias %s" (ppshow x)
bugMissingTypeArgs l x        = mkErr l $ text "BUG: Missing type arguments at call to" <+> pp x
bugUnknown l thing x          = mkErr l $ text $ printf "BUG: Cannot find '%s' in '%s'" thing (ppshow x)
bugNoCasts l e                = mkErr l $ text $ printf "BUG: No casts found for expression '%s'" (ppshow e)
bugClassInstVarInit l x       = mkErr l $ text $ printf "BUG: Instance variable '%s' initialization should have been moved to constructor." (ppshow x)
bugNestedCasts l e            = mkErr l $ text $ printf "BUG: Nested casts on expression '%s'." (ppshow e)
bugClassDefNotFound l x       = mkErr l $ text $ printf "BUG: Class definition for '%s' not found." (ppshow x)
bugEnvFindTy l x              = mkErr l $ text $ printf "BUG: envFindTy failed to find binding '%s'" (ppshow x)


---------------------------------------------------------------------------
-- | Rsc
---------------------------------------------------------------------------
errorInvalidTopStmt l x       = mkErr l $ text $ printf "Invalid top-level statement: %s" (ppshow x)
errorDuplicateKey l x         = mkErr l $ text $ printf "Duplicate key '%s' when merging creating environment" (ppshow x)
errorClassAnnot l c           = mkErr l $ text "Invalid annotation for class:" <+> ticks (pp c)
errorInterfaceAnnot l c       = mkErr l $ text "Invalid annotation for interface:" <+> ticks (pp c)
errorStatementeAnnot l s      = mkErr l $ text "Statement" $+$ nest 2 (pp s) $+$ text "cannot have a type annotation"


---------------------------------------------------------------------------
-- | SSA
---------------------------------------------------------------------------
errorReadOnlyUninit l x       = mkErr l $ text $ printf "Cannot declare uninitialized readonly variable `%s`." (ppshow x)
errorWriteImmutable l m x     = mkErr l $ text $ printf "Cannot assign to %s-variable '%s'. Add a type annotation to indicate it is globally writable." (ppshow m) (ppshow x)
errorSSAUnboundId l x         = mkErr l $ text $ printf "SSA: Identifier '%s' unbound" (ppshow x)
errorUpdateInExpr l e         = mkErr l $ text $ printf "Unsupported: assignment in If-then-else expression %s" (ppshow e)
errorEffectInFieldDef l       = mkErr l $ text $ printf "Cannot have effects in field initialization."
errorUninitStatFld l x        = mkErr l $ text $ printf "Uninitialized static member '%s' is not allowed." (ppshow x)
errorForeignLocal l x         = mkErr l $ text $ printf "Cannot reference local (out-of-scope) variable '%s'" (ppshow x)
bugSuperNotHandled l e        = mkErr l $ text $ printf "BUG: Expression '%s' should have been taken care of." (ppshow e)
bugSuperWithNoParent l        = mkErr l $ text $ printf "BUG: Calling 'super()' in constructor of class with no parent."
unimplementedInfix l e        = mkErr l $ text $ printf "UNIMPLEMENTED: Infix expression '%s' as standalone expression." (ppshow e)
unimplSSAMulVarDecl l v       = mkErr l $ text "[Unimplemented] Only support a single variable declaration per" <+>
                                          text "declaration statement. Declaration:" $+$ nest 2 (pp v) $+$
                                          text "has mutliple."


---------------------------------------------------------------------------
-- | Types
---------------------------------------------------------------------------

errorTypeMembers l t t'       = mkErr l $ text "Cannot combine type members:" $+$
                                                 nest 2 (pp t) $+$ text "and" $+$ nest 2 (pp t')
errorTypeMembersNidx l        = mkErr l $ text "A type can only have a single numeric indexer."
errorTypeMembersSidx l        = mkErr l $ text "A type can only have a single string indexer."

---------------------------------------------------------------------------
-- | TC
---------------------------------------------------------------------------

-- Unification
errorRigidUnify l a t         = mkErr l $ text "Cannot unify rigid variable" <+> ticks (pp a) <+> text "with" <+> ticks (pp t)
errorOccursCheck l a t        = mkErr l $ text $ printf "Occurs check fails: %s in %s" (ppshow a) (ppshow t)
errorFreeTyVar l t            = mkErr l $ text $ printf "Type not fully instantiated: %s" (ppshow t)
errorUnification l t t'       = mkErr l $ text $ printf "Cannot unify types: %s and %s" (ppshow t) (ppshow t')
errorMergeSubst l t t'        = mkErr l $ text $ printf "At merging substitutions cannot unify types: %s and %s" (ppshow t) (ppshow t')
errorUniqueTypeParams l       = mkErr l $ text $ printf "Only unique type paramteres are allowed"
errorListMismatch l ts ts'    = mkErr l $ text "Cannot unify types:" $+$ cat (map (nest 2 . pp) ts) $+$
                                                 text "against" $+$ cat (map (nest 2 . pp) ts') $+$
                                                 text "because of mismatch in count."

-- Subtyping
errorDownCast l t             = mkErr l $ text "Downcast to:" $+$ nest 2 (pp t)
errorSubtype       l a b      = mkErr l $ text "Type:" $+$ nest 2 (pp a) $+$ text "is not a subtype of type:"  $+$ nest 2 (pp b)
errorNonObjectType l t        = mkErr l $ text $ printf "Type '%s' cannot be treated as an object type." (ppshow t)

errorIncompMutTy l m m' t t' fo = mkErr l $ text "Immutability modifier" <+> ticks (pp m ) <+>
                                                 maybe empty (\f -> text "of field" <+> ticks (pp f)) fo <+>
                                                 text "of type:" $+$ nest 2 (pp t) $+$
                                                 text "is not compatible with immutability modifier" <+> ticks (pp m') <+>
                                                 maybe empty (\f -> text "of field" <+> ticks (pp f)) fo <+>
                                                 text "of type:" $+$ nest 2 (pp t')

errorUniqueRef l x            = mkErr l $ text "Cannot have a unique reference in this context." $+$
                                                 maybe empty (\x' -> ticks (pp x') <+> text "seems to be unique.") x $+$
                                                 text "Hint: Try casting subexpressions to non-unique permissions."
errorUniqueAsgn l x           = mkErr l $ text "Cannot re-assign unique reference:" <+> ticks (pp x) <> pp "." $+$
                                                 text "Try casting" <+> ticks (pp x) <+> text "to a non-unique type."
errorMutUnknown l t           = mkErr l $ sep [text "Unknown mutability modifier:", nest 2 (pp t)]
bugMutPartInvalid l t t'      = mkErr l $ text "[BUG] Invalid mutability part of types:" $+$
                                                 nest 2 (pp t) $+$ text "and" $+$
                                                 nest 2 (pp t')
errorMutInvalid l t t'        = mkErr l $ sep [text "Invalid mutability type(s):", nest 2 (pp t), text "or", nest 2 (pp t')]

errorIncompCallSigs l t t'    = mkErr l $ text $ printf "Types '%s' and '%s' have incompatible call signatures." (ppshow t) (ppshow t')
errorIncompCtorSigs l t t'    = mkErr l $ text $ printf "Types '%s' and '%s' have incompatible constructor signatures." (ppshow t) (ppshow t')
errorIncompNIdxSigs l t t'    = mkErr l $ text $ printf "Types '%s' and '%s' have incompatible numeric index signatures." (ppshow t) (ppshow t')
errorIncompSIdxSigs l t t'    = mkErr l $ text $ printf "Types '%s' and '%s' have incompatible string index signatures." (ppshow t) (ppshow t')
errorObjectType l t t'        = mkErr l $ text $ printf "Cannot treat object type '%s' nominally as type '%s'." (ppshow t) (ppshow t')
errorOptionalElt l p t t'     = mkErr l $ text $ printf "Unmatching optionality values for property '%s' in types '%s' and '%s'." (ppshow p) (ppshow t) (ppshow t')
errorIncompatOptional l f     = mkErr l $ text $ printf "Property '%s' has incompatible optionality modifiers." (ppshow f)
errorConstrMissing l t        = mkErr l $ text $ printf "Could not find constructor for type '%s'." (ppshow t)

errorObjSubtype l t t' fs     = mkErr l $ text "Object type:"         $+$
                                                 nest 2 (pp t)               $+$
                                                 text "is not a subtype of:" $+$
                                                 nest 2 (pp t')              $+$
                                                 text "The former is missing field(s):" <+>
                                                 intersperse comma (map (ticks . pp) fs)

errorBoundsInvalid l vs ts cs  = mkErr l $ text "The inferred types:" $+$
                                                 nest 2 (intersperse comma (map pp ts))  $+$
                                                  text "for the type variables:" $+$
                                                 nest 2 (intersperse comma (map pp vs))  $+$
                                                 text "are not subtypes of the bounds:" $+$
                                                 nest 2 (intersperse comma (map pp cs))

-- Typechecking
ppBind (a,b) = pp a <+> dcolon <+> pp b

errorCallNotSup l fn ft es ts = mkErr l $ text "Cannot call"       <+> ticks (pp fn)                 <+>
                                                 text "with signature"    $+$ nest 2 (pp ft)                $+$
                                                 text "with argument(s):" $+$
                                                 nest 2 (vcat (map ppBind (safeZip "errorCallNotSup" es ts)))

errorOptFunUnsup l f e        = mkErr l $ text "Cannot call optional member" <+> ticks (pp f) <+> text "of expression" $+$ nest 2 (pp e)

errorMethMutIncomp l fn mts m = mkErr l $ text "Cannot call"       <+> ticks (pp fn)                 <+>
                                                 text "with signature:"   $+$ nest 2 (vcat (map pp mts))    $+$
                                                 text "on" <+> pp m <+> text "receiver."

errorNoMutAvailable l e t     = mkErr l $ text "Cannot retrieve mutability annotation from" <+> ticks (pp e) <+> text "of type" $+$ nest 2 (pp t)

errorCallNotFound l e f       = mkErr l $ text $ printf "Cannot find callable property '%s' of type '%s'." (ppshow f) (ppshow e)
errorClassEltAnnot l c s      = mkErr l $ text $ printf "Class '%s' needs to have a single annotation for element '%s'." (ppshow c) (ppshow s)
errorUnboundIdEnv l x t       = mkErr l $ text $ printf "ZOGBERT Identifier '%s' unbound in %s" (ppshow x) (ppshow t)
errorUnboundName l x          = mkErr l $ text $ printf "Name '%s' is unbound." (ppshow x)
errorUnboundPath l x          = mkErr l $ text $ printf "Path '%s' is unbound." (ppshow x)
errorUnboundId l x            = mkErr l $ text "Identifier" <+> ticks (pp x) <+> text "is unbound."
errorArgMismatch l x t n m    = mkErr l $ text "Mismatch in number of arguments passed to function" <+>
                                                 ticks (pp x) <> text ". Signature:" $+$ pp t $+$
                                                 text "expects" <+> pp n <+> text "arguments, but" <+> pp m <+> text "were given."
errorFunArgMismatch l t t'    = mkErr l $ text "Mismatch in number of arguments when subtyping:" $+$
                                                 nest 2 (pp t) $+$ text "and" $+$ nest 2 (pp t')
errorExtractFldMut l f t      = mkErr l $ text "Cannot extract mutability from field" <+> ticks (pp f) <+>
                                                 text "of type" $+$ nest 2 (pp t)

errorExtractMut l t e         = mkErr l $ text "Cannot extract mutability of type" $+$ nest 2 (pp t) $+$
                                                 text "of expression" <+> ticks (pp e)

errorNoFuncAnn l              = mkErr l $ text "No type annotation or contextual type for anonymous function."
errorUnfoldType l t           = mkErr l $ text "Could not unfold type:" $+$ nest 2 (pp t)
errorUnresolvedTypes l t1 t2  = mkErr l $ text "Could not resolve types:" $+$ nest 2 (pp t1) $+$ text "and" $+$ nest 2 (pp t2)
errorDeadCast l               = mkErr l $ text "Deadcast inserted."

-- Lookup
errorEnumLookup l e n         = mkErr l $ text $ printf "Cannot find member '%s' in enumeration '%s'" (ppshow n) (ppshow e)
errorPrimLookup l f t         = mkErr l $ text "Cannot find member" <+> ticks (pp f) <+> text "in primitive type" <+> pp t
errorMemLookup l m t          = mkErr l $ text "Cannot find member" <+> ticks (pp m) <+> text "in type: " $+$ nest 2 (pp t)
errorGenericLookup l f t      = mkErr l $ text "Cannot find member" <+> ticks (pp f) <+> text "in type: " $+$ nest 2 (pp t)
errorAmbientLookup l t f      = mkErr l $ text $ printf "Cannot find member '%s' in ambient element '%s'" (ppshow f) (ppshow t)
errorUnionLookup l t f        = mkErr l $ text $ printf "Cannot find member '%s' in any part of the union '%s'" (ppshow f) (ppshow t)

errorLoopWiden l _ x' t t'    = mkErr l $ text "The type bound to variable" <+> ticks (pp x') <> pp ":" $+$
                                                 nest 2 (pp t') $+$
                                                 text "needs to be a subtype of the type" $+$
                                                 nest 2 (pp t) $+$
                                                 text "bound to the same variable before the loop."

---------------------------------------------------------------------------
-- | LIQUID
---------------------------------------------------------------------------
errorCyclicDefs l x stk       = mkErr l $ text $ printf "Cyclic definitions: %s in %s" (ppshow x) (ppshow stk)
errorBadTAlias l t nt ne a m  = mkErr l $ text $ printf "Invalid type alias application: %s \nExpected %d type, %d value arguments, but got %d and %d" (ppshow t) a m nt ne
errorTAliasNumArgs l t a x n  = mkErr l $ text $ printf "Invalid type alias application on type '%s'. Expected %d type, %d value arguments, but %d found." (ppshow t) a x n
errorTAliasMismatch l t a     = mkErr l $ text $ printf "Invalid type alias application %s : Cannot convert %s into value argument" (ppshow t) (ppshow a)

errorBadPAlias l p nx ne      = mkErr l $ text "Invalid predicate alias application:" $+$
                                                 nest 2 (pp p) $+$
                                                 text "Expected" <+> pp nx <+> text "arguments, but got" <+> pp ne <> text "."

errorNoMatchCallee l fn ts ft = mkErr l $ text "No matching callee type for:" <+> ticks (pp fn)      $+$
                                                 text "Argument Types:" $+$
                                                 nest 2 (intersperse comma (map (pp . toType) ts)) $+$
                                                 text "Function Type:"  $+$
                                                 nest 2 (pp (toType ft))

errorMultipleCasts l cs       = mkErr l $ text $ printf "Multiple Casts: %s" (ppshow cs)
errorUnsafeExtends l          = mkErr l $ text $ printf "Unsafe Extends"
errorWellFormed l             = mkErr l $ text $ printf "Well-formedness Error"
errorForbiddenSyms l t xs     = mkErr l $ text "Symbol(s):" <+>
                                                 intersperse comma (map (ticks . pp) xs) <+>
                                                 text "is (are) not readonly, local, or an immutable field," <+>
                                                 text "so should not be appearing in the refinement of type" $+$
                                                 pp t

errorUnboundSyms l t s m      = mkErr l $ text "Symbol" <+> ticks (pp s) <+> text "appearing in type:" $+$
                                                 nest 2 (pp t) $+$
                                                 text "is unbound" <+> brackets (text "ERROR_CODE:" <+> pp m)

unimplementedReservedSyms l   = mkErr l $ text $ printf "Please avoid using 'func' and 'obj' as symbols in refinements."
errorAsgnInRef l x t a        = mkErr l $ text "Only readonly variables can be used in refinements." $+$
                                                 text "In type:" $+$ nest 2 (pp t) $+$
                                                 text "symbol" <+> ticks (pp x) <+> text "is" <+> pp a <> text "."
errorContextual l e t         = mkErr l $ text $ printf "Contextual error occured when checking expression '%s' under type '%s'." (ppshow e) (ppshow t)
unimplSplitC l t1 t2          = mkErr l $ text "[Unimplemented] Cannot generate constraints for the subtyping of" $+$
                                                 nest 2 (pp t1) $+$ text "and" $+$ nest 2 (pp t2)
bugSplitC l t1 t2             = mkErr l $ text "[BUG] Cannot generate constraints for the subtyping of" $+$
                                                 nest 2 (pp t1) $+$ text "and" $+$ nest 2 (pp t2)

---------------------------------------------------------------------------
-- | Pervasive (typechecking TC and Liquid)
---------------------------------------------------------------------------
errorSuper l                  = mkErr l $ text $ printf "Cannot resolve reference to super."
errorMissingFields l t1 t2 x  = mkErr l $ text $ printf "Cannot convert %s to %s. Type %s is missing fields %s." (ppshow t1) (ppshow t2) (ppshow t1) (ppshow x)
errorVarDeclAnnot l x         = mkErr l $ text $ printf "Variable definition of '%s' can have at most one type annotation." (ppshow x)
errorNonFunction l f t        = mkErr l $ text $ printf "Non-function type: %s :: %s." (ppshow f) (ppshow t)
errorMissingReturn l          = mkErr l $ text $ printf "Missing Return statement."
errorMissingSpec l f          = mkErr l $ text $ printf "Missing signature for '%s'." (ppshow f)
errorVariadic l f             = mkErr l $ text $ printf "Cannot call variadic on type '%s'." (ppshow f)
errorVariadicNoArgs l f       = mkErr l $ text $ printf "Cannot make variadic call '%s' without arguments." (ppshow f)
errorConflateTypeMembers l es = mkErr l $ text $ printf "Cannot conflate type members '%s'." (ppshow es)
errorCallSuperOnNonClass l x  = mkErr l $ text $ printf "Cannot call 'super' on non class type '%s'." (ppshow x)
errorAssignsFields l x t      = mkErr l $ text $ printf "Variable '%s' with type '%s' can only be assigned fields or returned." (ppshow x) (ppshow t)
errorImmutableRefAsgn l f e t = mkErr l $ text "Cannot assign field" <+> ticks (pp f) <+>
                                                 text "of expression" <+> ticks (pp e) <+>
                                                 text "through non-mutable reference of type:" $+$
                                                 nest 2 (pp t) $+$
                                                 text "[Hint: try making field" <+> ticks (pp f) <+> text "assignable]."
errorMethAsgn l m             = mkErr l $ text "Cannot re-assign method" <+> ticks (pp m)
errorCallOptional l m t       = mkErr l $ text $ printf "Cannot call optional field '%s' of type '%s'." (ppshow m) (ppshow t)
errorUnionMergePrims l t a b  = mkErr l $ text $ printf "In type '%s', cannot merge primitive types '%s' and '%s'." (ppshow t) (ppshow a) (ppshow b)
errorUnionMergeVars l t a b   = mkErr l $ text $ printf "In type '%s', cannot merge type variables '%s' and '%s'." (ppshow t) (ppshow a) (ppshow b)
errorUnionMergeAnds l t a b   = mkErr l $ text $ printf "In type '%s', cannot merge intersection types '%s' and '%s'." (ppshow t) (ppshow a) (ppshow b)
errorUnionMergeObjs l t a b   = mkErr l $ text $ printf "In type '%s', cannot merge object types '%s' and '%s', as they correspond to structurally equivalent types." (ppshow t) (ppshow a) (ppshow b)
errorUnionMergeAlls l t       = mkErr l $ text $ printf "In type '%s', cannot have type abstraction as part of a union." (ppshow t)
errorUnionMergeTys l t a b    = mkErr l $ text $ printf "In type '%s', cannot merge named types '%s' and '%s'." (ppshow t) (ppshow a) (ppshow b)
errorUnionMergeMods l t a b   = mkErr l $ text $ printf "In type '%s', cannot merge module types '%s' and '%s'." (ppshow t) (ppshow a) (ppshow b)
errorUnionMergeFuns l t       = mkErr l $ text $ printf "In type '%s', cannot merge multiple function types." (ppshow t)

errorArrayLitCtxType l e      = mkErr l $ text $ printf "Cannot type array literal '%s' without a contextual type." (ppshow e)
errorNewExprCtxType l e       = mkErr l $ text $ printf "Cannot type new expression '%s' without a contextual type." (ppshow e)
unimpCondExpCtxType l e       = mkErr l $ text "[Unimplemented] Cannot type conditional expression" $+$
                                                 nest 2 (pp e) $+$ text "without a contextual type."

bugStaticField l f c          = mkErr l $ text "[BUG] Invalid annotation for static field" <+> ticks (pp f) <+> text "of class" <+> ticks (pp c) <> text "."
bugStaticMethod l f c         = mkErr l $ text "[BUG] Invalid annotation for static method" <+> ticks (pp f) <+> text "of class" <+> ticks (pp c) <> text "."
errorArrayLitType l e t       = mkErr l $ text $ printf "Cannot cast array '%s' with non array type '%s'." (ppshow e) (ppshow t)
bugArrayBIType l f t          = mkErr l $ text $ printf "[BUG] Inconsistent built-in arrray literal ('%s') type '%s'." (ppshow f) (ppshow t)
errorMultipleVarDeclAnns l x  = mkErr l $ text "Cannot have multple variable declaration annotations for variable" <+> ticks (pp x) <> text "."
errorJoinSymInfo l s1 s2      = mkErr l $ text "Cannot combine the following annotations:" $+$
                                                 nest 2 (pp s1) $+$ text "and" $+$ nest 2 (pp s2)
errorDupModule l ls m         = mkErr l $ text "Cannot handle multiply defined modules. Module" <+> ticks (pp m) <+>
                                          text "is also defined in the following locations:" $+$
                                          nest 2 (vcat $ map (pp . sourceSpanSrcSpan) ls)

