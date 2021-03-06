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

  -- * Conversions
  , symToField
  , setSiType

  -- * Symbol list
  , SymList (..), symbols

  -- * Symbol environment
  , SymEnv, symEnv, symEnv'

  -- * Special symbols
  , mkArgumentsSI

  -- * Extraction
  , varDeclSymbol

) where

import           Data.Generics
import           Data.List                      (findIndex)
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (catMaybes, fromMaybe)
import           Language.Fixpoint.Misc         (safeZip)
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Misc              (foldM1)
import           Language.Rsc.Names
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types



--------------------------------------------------------------------------------
-- | Symbol information
--------------------------------------------------------------------------------

data SymInfoQ q r = SI { v_name :: F.Symbol
                       , v_loc  :: Locality
                       , v_asgn :: Assignability
                       , v_type :: RTypeQ q r
                       }
                       deriving (Data, Typeable, Functor)

type SymInfo r    = SymInfoQ AK r

instance F.Symbolic (SymInfoQ q r) where
  symbol = v_name


symToField (SI n _ WriteGlobal t) = FI n Req tMU t
symToField (SI n _ _           t) = FI n Req tIM t

setSiType s t = s { v_type = t }

-- | @argBind@ returns a dummy type binding `arguments :: T `
--   where T is an object literal containing the non-undefined `ts`.
--------------------------------------------------------------------------------
mkArgumentsSI :: (F.Reftable r, IsLocated l) => l -> [RType r] -> SymInfo r
--------------------------------------------------------------------------------
mkArgumentsSI l ts = SI getArgSym Local RdOnly ty
  where
    ty             = TFun [] (immObjectLitTy [pLen] [tLen]) fTop
    pLen           = PropId l (lenId l)
    tLen           = tNum `strengthen` rLen
    rLen           = F.ofReft (F.uexprReft k)
    k              = fromMaybe (length ts) (findIndex isTUndef ts)

--------------------------------------------------------------------------------
immObjectLitTy :: F.Reftable r => [Prop l] -> [RType r] -> RType r
--------------------------------------------------------------------------------
immObjectLitTy ps ts | length ps == length ts
                     = TObj tIM elts fTop
                     | otherwise
                     = error "Mismatched args for immObjectLit"
  where
    elts = tmsFromList [ FI (sym p) Req tIM t | (p,t) <- pts ]
    pts  = safeZip "immObjectLitTy" ps ts
    sym  = F.symbol

-- | Instances

instance F.Reftable r => SubstitutableQ q r (SymInfoQ q r) where
  apply θ (SI n l a t) = SI n l a $ apply θ t


--------------------------------------------------------------------------------
-- | List of Symbols in block
--------------------------------------------------------------------------------

newtype SymList r = SL { s_list :: [(Id SrcSpan, SyntaxKind, SymInfo r)] }

-- TODO: Add modules as well?
--------------------------------------------------------------------------------
symbols :: [Statement (AnnR r)] -> SymList r
--------------------------------------------------------------------------------
symbols s = SL [ (fSrc <$> n, k, SI (F.symbol n) loc a t)
                 | (n,l,k,a) <- hoistBindings s
                 , fact      <- fFact l
                 , (loc, t)  <- annToType fact ]
  where
    annToType (ClassAnn   l (TS _ b _)) = [(l, TClass b)]       -- Class
    annToType (SigAnn   _ l t         ) = [(l, t)]              -- Function
    annToType (VarAnn   _ l _ (Just t)) = [(l, t)]              -- Variables
    annToType (ModuleAnn  l q         ) = [(l, TMod q)]         -- Modules
    annToType _                         = [ ]


--------------------------------------------------------------------------------
varDeclSymbol :: (F.Reftable r, PP (SymInfo r))
              => VarDecl (AnnR r) -> Either Error (Maybe (SymInfo r))
--------------------------------------------------------------------------------
varDeclSymbol (VarDecl l x _) =
    case nmErrs of
      []   -> catSymInfo sis
      errs -> Left (catErrors errs)
  where
    xSym    = F.symbol x
    sis     = [SI y loc a t | VarAnn y loc a (Just t) <- fFact l]
    nmErrs  = catMaybes [toDerr a y xSym | SI y _ a _ <- sis, y /= xSym]

    catSymInfo [ ] = Right Nothing
    catSymInfo [s] = Right (Just s)
    catSymInfo ss  | all (isTFun . v_type) ss
                   = Just <$> foldM1 (concatSymInfo l) ss
                   | otherwise
                   = Left (errorMultipleVarDeclAnns l x)

    toDerr _          y x | y == x              = Nothing
    toDerr WriteLocal y x | y `F.isPrefixOfSym` x = Nothing
    toDerr _          y x = Just (errorDeclMismatch l y x)



--------------------------------------------------------------------------------
-- | Symbol environment
--------------------------------------------------------------------------------

type SymEnv r = Env (SymInfo r)

--------------------------------------------------------------------------------
symEnv :: F.Reftable r => [Statement (AnnR r)] -> Either Error (SymEnv r)
--------------------------------------------------------------------------------
symEnv = symEnv' . symbols

--------------------------------------------------------------------------------
symEnv' :: F.Reftable r => SymList r -> Either Error (SymEnv r)
--------------------------------------------------------------------------------
symEnv' sl = envMap snd <$> envFromListWithKeyM mergeSymInfo vs
  where
    vs = concatMap f $ M.toList $ foldl merge M.empty $ s_list sl

    merge ms (x, k, v) = M.insertWith (++) (F.symbol x) [(k,v)] ms

    f (s, vs) = [(s, (k, v)) | (k@FuncDeclKind  , v) <- vs] ++
                [(s, (k, v)) | (k@VarDeclKind   , v) <- vs] ++
                [(s, (k, v)) | (k@ClassDeclKind , v) <- vs] ++
                [(s, (k, v)) | (k@ModuleDeclKind, v) <- vs] ++
                [(s, (k, v)) | (k@EnumDeclKind  , v) <- vs]

--------------------------------------------------------------------------------
mergeSymInfo :: F.Reftable r
  => F.Symbol -> (SyntaxKind, SymInfo r) -> (SyntaxKind, SymInfo r)
  -> Either Error (SyntaxKind, SymInfo r)
--------------------------------------------------------------------------------
mergeSymInfo _ (k1, SI m1 l1 a1 t1) (k2, SI m2 l2 a2 t2)
  | m1 == m2, l1 == l2, k1 == k2, a1 == a2
  = Right (k1, SI m1 l1 a1 (t1 `mappend` t2))

mergeSymInfo x _ _ = Left (errorDuplicateKey (srcPos x) x)


concatSymInfo l s1@(SI m1 l1 a1 t1) s2@(SI m2 l2 a2 t2)
  | (m1, l1, a1) == (m2, l2, a2), isTFun t1, isTFun t2
  = Right (SI m1 l1 a1 (t1 `mappend` t2))
  | otherwise
  = Left (errorJoinSymInfo l s1 s2)




