{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Language.Rsc.Symbols (

  -- * Symbols
    SymInfoQ (..), SymInfo

  -- * Symbol list
  , SymList (..), symbols, symbols'

  -- * Symbol environment
  , SymEnv, symEnv, symEnv'

  -- * Special symbols
  , mkArgumentsSI

) where

import           Control.Exception            (throw)
import           Data.Generics
import           Data.List                    (find, findIndex, partition)
import qualified Data.Map.Strict              as M
import           Data.Maybe                   (fromMaybe)
import           Language.Fixpoint.Misc       (safeZip)
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ



--------------------------------------------------------------------------------
-- | Symbol information
--------------------------------------------------------------------------------

data SymInfoQ q r = SI { v_loc  :: Locality
                       , v_asgn :: Assignability
                       , v_init :: Initialization
                       , v_type :: RTypeQ q r
                       }
                       deriving (Data, Typeable, Functor)

type SymInfo r    = SymInfoQ AK r


-- | @argBind@ returns a dummy type binding `arguments :: T `
--   where T is an object literal containing the non-undefined `ts`.
--------------------------------------------------------------------------------
mkArgumentsSI :: (F.Reftable r, IsLocated l) => l -> [RType r] -> SymInfo r
--------------------------------------------------------------------------------
mkArgumentsSI l ts = SI Local RdOnly Initialized
                   $ immObjectLitTy [pLen] [tLen]
  where
    ts'            = take k ts
    ps'            = PropNum l . toInteger <$> [0 .. k-1]
    pLen           = PropId l $ lenId l
    tLen           = tNum `strengthen` rLen
    rLen           = F.ofReft $ F.uexprReft k
    k              = fromMaybe (length ts) $ findIndex isTUndef ts

--------------------------------------------------------------------------------
immObjectLitTy :: F.Reftable r => [Prop l] -> [RType r] -> RType r
--------------------------------------------------------------------------------
immObjectLitTy ps ts | length ps == length ts
                     = TObj tIM elts fTop
                     | otherwise
                     = error "Mismatched args for immObjectLit"
  where
    elts = typeMembersFromList [ ( F.symbol p, FI Req Final t )
                                 | (p,t) <- safeZip "immObjectLitTy" ps ts ]

-- | Instances

instance F.Reftable r => SubstitutableQ q r (SymInfoQ q r) where
  apply θ (SI l a i t)      = SI l a i $ apply θ t


--------------------------------------------------------------------------------
-- | List of Symbols in block
--------------------------------------------------------------------------------

newtype SymList r = SL { s_list :: [(Id SrcSpan, SyntaxKind, SymInfo r)] }

-- TODO: Add modules as well?
--------------------------------------------------------------------------------
symbols :: [Statement (AnnR r)] -> SymList r
--------------------------------------------------------------------------------
symbols s = SL [ (fSrc <$> n, k, SI loc a i t) | (n,l,k,a,i) <- hoistBindings s
                                               , fact        <- fFact l
                                               , (loc, t)    <- annToType fact ]
  where
    annToType (ClassAnn   l (TS _ b _)) = [(l, TClass b)]       -- Class
    annToType (SigAnn     l t         ) = [(l, t)]              -- Function
    annToType (VarAnn     l _ (Just t)) = [(l, t)]              -- Variables
    annToType (ModuleAnn  l q         ) = [(l, TMod q)]         -- Modules
    annToType _                         = [ ]

--------------------------------------------------------------------------------
symbols' :: F.Reftable r => [Statement (AnnR r)] -> [(Id SrcSpan, Assignability)]
--------------------------------------------------------------------------------
symbols' s = [ (fSrc <$> n, a) | (n,l,k,a,i) <- hoistBindings s ]


--------------------------------------------------------------------------------
-- | Symbol environment
--------------------------------------------------------------------------------

type SymEnv r = Env (SymInfo r)

--------------------------------------------------------------------------------
symEnv :: F.Reftable r => [Statement (AnnR r)] -> SymEnv r
--------------------------------------------------------------------------------
symEnv = symEnv' . symbols

--------------------------------------------------------------------------------
symEnv' :: F.Reftable r => SymList r -> SymEnv r
--------------------------------------------------------------------------------
symEnv' = envMap snd
        . envFromListWithKey mergeSymInfo
        . concatMap f
        . M.toList
        . foldl merge M.empty
        . s_list
  where
    merge ms (x, k, v) = M.insertWith (++) (F.symbol x) [(k,v)] ms

    f (s, vs) = [(s, (k, v)) | (k@FuncDeclKind  , v) <- vs] ++
                [(s, (k, v)) | (k@VarDeclKind   , v) <- vs] ++
                [(s, (k, v)) | (k@ClassDeclKind , v) <- vs] ++
                [(s, (k, v)) | (k@ModuleDeclKind, v) <- vs] ++
                [(s, (k, v)) | (k@EnumDeclKind  , v) <- vs]

--------------------------------------------------------------------------------
mergeSymInfo :: F.Reftable r => F.Symbol -> (SyntaxKind, SymInfo r)
                                         -> (SyntaxKind, SymInfo r)
                                         -> (SyntaxKind, SymInfo r)
--------------------------------------------------------------------------------
mergeSymInfo x (k1, SI l1 a1 i1 t1) (k2, SI l2 a2 i2 t2)
  | l1 == l2, k1 == k2, a1 == a2, i1 == i2
  = (k1, SI l1 a1 i1 (t1 `mappend` t2))
mergeSymInfo x _ _ = throw $ errorDuplicateKey (srcPos x) x

