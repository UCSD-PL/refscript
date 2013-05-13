module Language.Nano.Errors where

import Text.Printf 
import Text.PrettyPrint.HughesPJ
import Language.ECMAScript3.PrettyPrint

errorDuplicate i l l' = printf "Duplicate Specification for %s:\n  %s \n  %s" (ppshow i) (ppshow l) (ppshow l')
errorArgMismatch      = printf "Mismatch in Number of Args in Call" 
errorNonFunction f    = printf "Non-function type for %s" (ppshow f)  
errorUnboundId x      = printf "Identifier %s unbound" (ppshow x) 
errorWrongType e t t' = printf "Unexpected type for %s :: %s expected %s" (ppshow e) (ppshow t) (ppshow t')
errorJoin x t t'      = printf "Cannot join %s :: %s expected %s" (ppshow x) (ppshow t) (ppshow t') 
ppshow                = render . pp


