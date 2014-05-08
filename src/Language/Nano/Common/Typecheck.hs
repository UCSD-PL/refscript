-- | Common functionality between raw and liquid typechecking

{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE ConstraintKinds        #-}

module Language.Nano.Common.Typecheck (safeExtends) where 

import           Control.Applicative                ((<$>), (<*>))

import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Annots
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Lookup

import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types            as F
import           Language.ECMAScript3.PrettyPrint
-- import           Debug.Trace                        hiding (traceShow)

type PPR r = (PP r, F.Reftable r)

-------------------------------------------------------------------------------
safeExtends :: (Monad m, PPR r) =>
  (SourceSpan -> RType r -> RType r -> m Bool)      -- Subtyping relation
  -> (Error -> m ())                                -- Error function
  -> SourceSpan                                     -- Source
  -> TDefEnv (RType r)                              -- Type definition env
  -> TDef (RType r)                                 -- Type definition 
  -> m ()
-------------------------------------------------------------------------------
safeExtends subtype err l δ (TD _ c vs (Just (p, ts)) es) =
  mapM_ sub [ (eltSym ee, eltType ee, eltType pe) | pe <- flatten δ (findSymOrDie p δ,ts)
                                                  , ee <- es, sameBinder pe ee ]
  where
    sub (s,t1,t2) = subtype l t1 t2 >>= \case 
      True  -> return ()
      False -> err $ errorClassExtends l c p s t1 t2

safeExtends _ _ l δ (TD _ _ _ Nothing _) = return ()
