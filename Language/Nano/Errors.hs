{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveDataTypeable        #-}

module Language.Nano.Errors where

import Data.Typeable                      (Typeable)
import Control.Exception.Base
import Debug.Trace
import Text.Printf 
import Text.PrettyPrint.HughesPJ
import Language.ECMAScript3.PrettyPrint
import Text.Parsec.Pos                   
import Language.ECMAScript3.Parser        (SourceSpan (..))
import qualified Control.Monad.Error as E


--------------------------------------------------------------------------------
-- | SourceSpans ---------------------------------------------------------------
--------------------------------------------------------------------------------

instance PP SourceSpan where 
  pp    = ppSourceSpan


ppSourceSpan z  = parens 
                $ text (printf "file %s: (%d, %d) - (%d, %d)" f l c l' c')  
  where 
    (f,l ,c )   = sourcePosElts $ sp_begin z
    (_,l',c')   = sourcePosElts $ sp_end   z

sourcePosElts s = (src, line, col)
  where 
    src         = sourceName   s 
    line        = sourceLine   s
    col         = sourceColumn s 

---------------------------------------------------------------------------
-- | Errors ---------------------------------------------------------------
---------------------------------------------------------------------------

data Error = Error { errLoc :: SourceSpan, errMsg :: String }
               deriving (Show, Typeable)

instance Exception Error

instance E.Error Error where
  strMsg = Error dummySpan 

dummySpan :: SourceSpan
dummySpan = Span l l 
  where l = initialPos ""


---------------------------------------------------------------------------
-- | Constructing Errors --------------------------------------------------
---------------------------------------------------------------------------





bugBadPhi l t1s t2s       = Error l $ printf "BUG: Unbalanced Phi at %s \n %s \n %s" (ppshow l) (ppshow t1s) (ppshow t2s)
bugBadSubtypes l x        = Error l $ printf "BUG: Unexpected Subtyping Constraint \n %s" (ppshow x)
bugUnboundPhiVar l x      = Error l $ printf "BUG: Phi Variable %s is unbound" (ppshow x)
bugUnboundVariable l x    = Error l $ printf "BUG: Variable %s is unbound in environment at %s" (ppshow x) (ppshow l)
bugMissingTypeArgs l      = Error l $ printf "BUG: Missing Type Arguments at %s" (ppshow l)
bugTBodiesOccur l s       = Error l $ printf "BUG: There should be no TBodies herie %s" s
bugBadUnions l s          = Error l $ printf "BUG: No unions should be found here (%s)" s
bugBadFunction l          = Error l $ printf "BUG: No function expression was found"
errorArgName l x y        = Error l $ printf "Wrong Parameter Name at %s: Saw %s but Expected %s" (ppshow l) (ppshow x) (ppshow y)  
errorMissingSpec l f      = Error l $ printf "Missing Signature For %s defined at %s" (ppshow f) (ppshow l)
errorDuplicate i l l'     = Error l $ printf "Duplicate Specification for %s:\n  %s \n  %s" (ppshow i) (ppshow l) (ppshow l')
errorArgMismatch l        = Error l $ printf "Mismatch in Number of Args in Call" 
errorNonFunction l f t    = Error l $ printf "Non-function type for %s :: %s" (ppshow f) (ppshow t)  
errorUnboundId l x        = Error l $ printf "Identifier %s unbound" (ppshow x) 
errorUnboundType l x      = Error l $ printf "Type identifier \'%s\' unbound" (ppshow x)
errorUnboundIdEnv l x t   = Error l $ printf "ZOGBERT Identifier %s unbound in %s" (ppshow x) (ppshow t)
errorWrongType l m e t t' = Error l $ printf "%s -- unexpected type for %s :: %s expected %s" m (ppshow e) (ppshow t) (ppshow t')
errorJoin l x t t'        = Error l $ printf "Conflicting join for %s \n   %s\n   %s" (ppshow x) (ppshow t) (ppshow t') 
errorJoinSubsts l θ θ'    = Error l $ printf "Cannot join substs: %s\nand\n%s" (ppshow θ) (ppshow θ')
errorUnification l t t'   = Error l $ printf "Cannot unify types: %s and %s" (ppshow t) (ppshow t')
errorBoundTyVar l a t     = Error l $ printf "Cannot unify bound type parameter %s with %s" (ppshow a) (ppshow t)
errorFreeTyVar l t        = Error l $ printf "Type not fully instantiated: %s" (ppshow t)
errorWriteImmutable l x   = Error l $ printf "Cannot write immutable: %s" (ppshow x)
errorInvalidTopStmt l x   = Error l $ printf "Invalid top-level statement: %s" (ppshow x) 
errorOccursCheck l a t    = Error l $ printf "Occurs check fails: %s in %s" (ppshow a) (ppshow t)
errorRigidUnify l a t     = Error l $ printf "Cannot unify rigid variable %s with %s" (ppshow a) (ppshow t) 
errorSubType l m t t'     = Error l $ printf "%s -- Type %s is not a subtype of %s" m (ppshow t) (ppshow t')
errorCast l m e t         = Error l $ printf "%s -- Cannot cast non-variable expression: %s to %s" m (ppshow e) (ppshow t)
errorObjSubtyping l t t'  = Error l $ printf "Object type: %s is not a subtype of %s" (ppshow t) (ppshow t')
errorObjectAccess l e t   = Error l $ printf "Dot notation on non object expression %s :: %s" (ppshow e) (ppshow t)
errorObjectTAccess l t    = Error l $ printf "Dot notation not permitted on expressions of type %s" (ppshow t)
errorObjectBinding l      = Error l $ printf "Field does not exist in object" 
errorNullUndefined l      = Error l $ printf "Null type is not a subtype of undefined"
errorUniqueTypeParams l   = Error l $ printf "Only unique type paramteres are allowed"
errorBracketAccess l t f  = Error l $ printf "Cannot access field \"%s\" of type: %s" (ppshow f) (ppshow t)
errorAnnotation l e t ta  = Error l $ printf "Type %s does not satisfy annotation %s at expression %s." (ppshow t) (ppshow ta) (ppshow e)
 
ppshow                  = render . pp

tracePP     ::  (PP a) => String -> a -> a
tracePP s x = trace (printf "\nTrace: [%s]: %s" s (ppshow x)) x

instance PP a => PP (Either String a) where 
  pp (Left s)  = text $ "ERROR!" ++ s
  pp (Right x) = pp x 

instance PP String where
  pp = text
