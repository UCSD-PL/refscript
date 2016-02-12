{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Language.Rsc.Module (

  -- * Modules
    ModuleDefQ (..), ModuleDef
  , moduleEnv

) where

import           Control.Monad                (liftM, void)
import           Data.Default
import           Data.Generics
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Misc            (concatMapM, single)
import           Language.Rsc.Names
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Program
import           Language.Rsc.Symbols
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types

--------------------------------------------------------------------------------
-- | Modules
--------------------------------------------------------------------------------
--
--  As per TypeScript spec par. 10.2:
--
--  Each module body has a declaration space for local variables (including
--  functions, modules, class constructor functions, and enum objects), a
--  declaration space for local named types (classes, interfaces, and enums),
--  and a declaration space for local namespaces (containers of named types).
--  Every declaration (whether local or exported) in a module contributes to
--  one or more of these declaration spaces.
--
--  PV: the last case has not been included
--
data ModuleDefQ q r = ModuleDef {
  --
  -- | Variables (Interfaces excluded, as they don't appear as language bindings)
  --
    m_variables :: Env (SymInfoQ q r)
  --
  -- | Types definitions
  --
  , m_types     :: Env (TypeDeclQ q r)
  --
  -- | Enumerations definitions
  --
  , m_enums     :: Env EnumDef
  --
  -- | Absolute path of module
  --
  , m_path      :: AbsPath
  }
  deriving (Data, Typeable, Functor)

type ModuleDef = ModuleDefQ AK

instance Monoid (ModuleDefQ q r) where
  mempty = ModuleDef mempty mempty mempty def
  ModuleDef v t e p `mappend` ModuleDef v' t' e' _ = ModuleDef (v `mappend` v')
                                                               (t `mappend` t')
                                                               (e `mappend` e')
                                                               p

--------------------------------------------------------------------------------
-- | Modules environment
--------------------------------------------------------------------------------

type ModuleEnv r = QEnv (ModuleDef r)

-- | `moduleEnv ss` creates a module store from the statements in @ss@
--   For every module we populate:
--
--    * m_variables with: functions, variables, class constructors, modules
--
--    * m_types with: classes and interfaces
--
--------------------------------------------------------------------------------
moduleEnv :: (PPR r, Typeable r, Data r) => BareRsc r -> Either FError (ModuleEnv r)
--------------------------------------------------------------------------------
moduleEnv (Rsc { code = Src stmts }) =
    (qenvFromList . map toQEnvList) `liftM` mapM mkMod (accumModuleStmts stmts)
  where
    toQEnvList p  = (m_path p, p)
    mkMod (p,s)   = ModuleDef <$> varEnv p s
                              <*> typeEnv s
                              <*> enumEnv s
                              <*> return p

    -- | Variables
    varEnv p =  return . symEnv' . SL . vStmts p
    vStmts p s = concatMap (vStmt p) s

    vStmt _ (VarDeclStmt _ vds)
      = [ ( ss x
          , VarDeclKind
          , SI (sym x) loc a Uninitialized t
          )
          | VarDecl l x _ <- vds
          , VarAnn loc a (Just t) <- fFact l
        ]
    vStmt _ (FunctionStmt l x _ _)
      = [ ( ss x
          , FuncDeclKind
          , SI (sym x) loc Ambient Initialized t
          )
          | SigAnn loc t <- fFact l
        ]
    vStmt _ (ClassStmt l x _)
      = [ ( ss x
          , ClassDeclKind
          , SI (sym x) loc Ambient Initialized (TClass b)
          )
          | ClassAnn loc (TS _ b _) <- fFact l
        ]
    -- TODO: Fix the Locality in the following two
    vStmt p (ModuleStmt l x _)
      = [ ( ss x
          , ModuleDeclKind
          , SI (sym x) Local Ambient Initialized (TMod (pathInPath l p x)))
        ]
    vStmt p (EnumStmt _ x _)
      = [ ( ss x
          , EnumDeclKind
          , SI (sym x) Local Ambient Initialized (TRef (Gen (QN p $ F.symbol x) []) fTop))
        ]
    vStmt _ _ = []

    -- | Type Definitions
    typeEnv = liftM envFromList . tStmts
    tStmts  = concatMapM tStmt

    tStmt c@ClassStmt{}     = single <$> toDeclaration c
    tStmt c@InterfaceStmt{} = single <$> toDeclaration c
    tStmt _                 = return [ ]

    -- | Enumerations
    enumEnv = return . envFromList . eStmts
    eStmts  = concatMap eStmt

    eStmt (EnumStmt _ n es)  = [(fmap srcPos n, EnumDef (F.symbol n) (envFromList $ sEnumElt <$> es))]
    eStmt _                  = []
    sEnumElt (EnumElt _ s e) = (F.symbol s, void e)

    ss  = fmap fSrc
    sym = F.symbol

--------------------------------------------------------------------------------
toDeclaration :: PPR r => Statement (AnnR r) -> Either FError (Id SrcSpan, TypeDecl r)
--------------------------------------------------------------------------------
toDeclaration (ClassStmt l c cs)
  | [ts] <- cas = Right (cc, TD ts (extractTypeMembers cs))
  | otherwise   = Left $ F.Unsafe [F.err (sourceSpanSrcSpan l) errMsg ]
  where
    cc     = fmap fSrc c
    cas    = [ ts | ClassAnn _ ts <- fFact l ]
    errMsg = "[TODO fix error msg] Invalid class annotation for class: " ++ ppshow c

toDeclaration (InterfaceStmt l c)
  | [t] <- ifaceAnns  = Right (fmap fSrc c,t)
  | otherwise         = Left $ F.Unsafe [F.err (sourceSpanSrcSpan l) errMsg ]
  where
    ifaceAnns = [ t | InterfaceAnn t <- fFact l ]
    errMsg    = "[TODO fix error msg] Invalid interface annotation for interface: " ++ ppshow c

toDeclaration s       = Left $ F.Unsafe $ single
                      $ F.err (sourceSpanSrcSpan $ getAnnotation s)
                      $ "Statement\n" ++ ppshow s ++ "\ncannot have a type annotation."


-- | Given a list of class elements, returns a @TypeMembers@ structure
--------------------------------------------------------------------------------
extractTypeMembers :: PPR r => [ClassElt (AnnR r)] -> TypeMembers r
--------------------------------------------------------------------------------
extractTypeMembers cs  = mkTypeMembers ms sms call ctor sidx nidx
  where
    ms   = [(sym x, m) | MemberVarDecl  l False x _   <- cs, MemberAnn m <- fFact l]
        ++ [(sym x, m) | MemberMethDecl l False x _ _ <- cs, MemberAnn m <- fFact l]
    sms  = [(sym x, m) | MemberVarDecl  l True  x _   <- cs, MemberAnn m <- fFact l]
        ++ [(sym x, m) | MemberMethDecl l True  x _ _ <- cs, MemberAnn m <- fFact l]
    call = []
    ctor = [t | Constructor l _ _ <- cs, CtorAnn t <- fFact l]
    sidx = []
    nidx = []
    sym  = F.symbol
