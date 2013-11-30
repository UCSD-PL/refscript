{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveDataTypeable        #-}

module Language.Nano.Errors where

-- import Data.Typeable                      (Typeable)
-- import Control.Exception.Base
-- import qualified Control.Monad.Error as E
import Debug.Trace
import Text.Printf 
import Text.PrettyPrint.HughesPJ
import Language.ECMAScript3.PrettyPrint
-- import Text.Parsec.Pos                   
import Language.ECMAScript3.Parser        (SourceSpan (..))
-- import qualified Language.Fixpoint.Types as F
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


bug l s                   = mkErr l $ printf "BUG: %s" s 
bugBadPhi l t1s t2s       = mkErr l $ printf "BUG: Unbalanced Phi at %s \n %s \n %s" (ppshow l) (ppshow t1s) (ppshow t2s)
bugBadSubtypes l x        = mkErr l $ printf "BUG: Unexpected Subtyping Constraint \n %s" (ppshow x)
bugUnboundPhiVar l x      = mkErr l $ printf "BUG: Phi Variable %s is unbound" (ppshow x)
bugUnboundVariable l x    = mkErr l $ printf "BUG: Variable %s is unbound in environment at %s" (ppshow x) (ppshow l)
bugMissingTypeArgs l      = mkErr l $ printf "BUG: Missing Type Arguments at %s" (ppshow l)
bugTBodiesOccur l s       = mkErr l $ printf "BUG: There should be no TBodies herie %s" s
bugBadUnions l s          = mkErr l $ printf "BUG: No unions should be found here (%s)" s
bugBadFunction l          = mkErr l $ printf "BUG: No function expression was found"
errorArgName l x y        = mkErr l $ printf "Wrong Parameter Name at %s: Saw %s but Expected %s" (ppshow l) (ppshow x) (ppshow y)  
errorMissingSpec l f      = mkErr l $ printf "Missing Signature For %s defined at %s" (ppshow f) (ppshow l)
errorDuplicate i l l'     = mkErr l $ printf "Duplicate Specification for %s:\n  %s \n  %s" (ppshow i) (ppshow l) (ppshow l')
errorArgMismatch l        = mkErr l $ printf "Mismatch in Number of Args in Call" 
errorNonFunction l f t    = mkErr l $ printf "Non-function type for %s :: %s" (ppshow f) (ppshow t)  
errorUnboundId l x        = mkErr l $ printf "Identifier %s unbound" (ppshow x) 
errorUnboundType l x      = mkErr l $ printf "Type identifier \'%s\' unbound" (ppshow x)
errorUnboundIdEnv l x t   = mkErr l $ printf "ZOGBERT Identifier %s unbound in %s" (ppshow x) (ppshow t)
errorWrongType l m e t t' = mkErr l $ printf "%s -- unexpected type for %s :: %s expected %s" m (ppshow e) (ppshow t) (ppshow t')
errorJoin l x t t'        = mkErr l $ printf "Conflicting join for %s \n   %s\n   %s" (ppshow x) (ppshow t) (ppshow t') 
errorJoinSubsts l θ θ'    = mkErr l $ printf "Cannot join substs: %s\nand\n%s" (ppshow θ) (ppshow θ')
errorUnification l t t'   = mkErr l $ printf "Cannot unify types: %s and %s" (ppshow t) (ppshow t')
errorBoundTyVar l a t     = mkErr l $ printf "Cannot unify bound type parameter %s with %s" (ppshow a) (ppshow t)
errorFreeTyVar l t        = mkErr l $ printf "Type not fully instantiated: %s" (ppshow t)
errorWriteImmutable l x   = mkErr l $ printf "Cannot write immutable: %s" (ppshow x)
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
errorLiquid l             = mkErr l $ printf "Liquid Type Error at %s" (ppshow l)
errorESC l                = mkErr l $ printf "ESC Error at %s" (ppshow l)
errorMultipleTypeArgs l   = mkErr l $ printf "Multiple Type Args"
errorDownCast l t         = mkErr l $ printf "Downcast: %s" (ppshow t) 
errorDeadCast l           = mkErr l $ printf "Deadcast"
errorTypeAssign l t1 t2   = mkErr l $ printf "Cannot assign type %s to %s" (ppshow t1) (ppshow t2)


