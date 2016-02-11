{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}

-------------------------------------------------------------------------------------
-- | SSA Monad
-------------------------------------------------------------------------------------


module Language.Rsc.SSA.SSAMonad (

   -- * SSA Information
     Var

   -- * SSA Monad
   , SSAM
   , ssaError
   , execute
   , tryAction
   , tick

   -- * SSA Environment
   , SsaEnv (..)
   , initGlobSsaEnv
   , initCallableSsaEnv
   , initModuleSsaEnv
   , initClassSsaEnv
   , initSsaVar
   , updSsaEnv
   , varDeclToAsgn
   , freshenAnn
   , freshenIdSSA
   , findSsaEnv

   , getCounter
   , ssaEnvIds
   , envToFgn

   -- * Access Annotations
   , addAnn, getAnns
   , setMeas, getMeas
   , getCHA

   -- * Queries
   , isBIArgumentsVar

   -- * Tracking Assignability
   , getAssignability

   ) where

import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.HashSet                   as S
import qualified Data.IntMap.Strict             as IM
import           Data.Maybe                     (fromMaybe)
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Misc
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.Symbols
import           Language.Rsc.Traversals
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ      (($+$))

-- import           Debug.Trace                        (trace)



type SSAM r     = ExceptT Error (State (SsaState r))


-- | SSA Monad state
--
data SsaState r = SsaST {

  -- XXX: really the only thing that needs to be in the monad
  --
    cnt  :: !Int                      -- ^ Counter for fresh ints

  , anns :: !(AnnInfo r)              -- ^ Map of annotation

  , meas :: S.HashSet F.Symbol        -- ^ Measures

  }

-- | SSA Environment
--
data SsaEnv r = SsaEnv {

    mAsgn    :: Env Assignability     -- ^ All vars

  , mSSA     :: Env (Var r)           -- ^ Source var |-> active SSA name
                                      --   (only SSA'ed vars)
  , ssaCHA   :: ClassHierarchy r

  , curClass :: Maybe AbsName

  , curPath  :: AbsPath

  }

-- data SEntry r = SE {
--     s_asgn  :: Assignability          -- ^ A variable's assignability
--   , ssa_var :: Var r                  -- ^ Active corresponding SSA Var
-- }

instance PP (SsaEnv r) where
  pp (SsaEnv mA mS _ _ _) = pp "Assignability" $+$ pp mA $+$
                            pp "SSA Vars"      $+$ pp mS

resolveConflict _ a a' | a == a' = a
                       | otherwise = error "SSAMonad-initGlobSsaEnv"


--------------------------------------------------------------------------------
visibleSyms :: F.Reftable r => [Statement (AnnR r)] -> [(Id SrcSpan, Assignability)]
--------------------------------------------------------------------------------
visibleSyms s = [ (fSrc <$> n, a) | (n,_,_,a,_) <- hoistBindings s ]

--------------------------------------------------------------------------------
initGlobSsaEnv
  :: F.Reftable r => [Statement (AnnR r)] -> ClassHierarchy r -> SsaEnv r
--------------------------------------------------------------------------------
initGlobSsaEnv fs cha =
    SsaEnv mA mS cha Nothing emptyPath
  where
    mA = envFromListWithKey resolveConflict (visibleSyms fs)
    mS = mempty

--------------------------------------------------------------------------------
initCallableSsaEnv
  :: (F.Reftable r, F.Symbolic x, IsLocated x)
  => SsaEnv r -> [x] -> [Statement (AnnR r)] -> SsaEnv r
--------------------------------------------------------------------------------
initCallableSsaEnv g xs bd = g { mAsgn = mA, mSSA = mS }
  where
    mA    = envAdd  arg RdOnly                -- `arguments` var
          $ envAdd  ret ReturnVar             -- "return" var
          $ envAdds (map (,WriteLocal) xs)    -- function arguments
          $ envAdds (visibleSyms bd)          -- lifted visible vars
          $ envMap toFgn (mAsgn g)            -- outer scope vars (as foreign)
    mS    = mempty
    arg   = argId dummySpan
    ret   = returnId dummySpan

toFgn WriteLocal = ForeignLocal
toFgn a   = a

envToFgn  = envMap toFgn

initModuleSsaEnv l g m bd = SsaEnv mA mS cha cls path
  where
    mA    = envAdds (visibleSyms bd)          -- lifted visible vars
          $ envMap toFgn (mAsgn g)            -- outer scope vars (as foreign)
    mS    = mempty
    cha   = ssaCHA g
    cls   = curClass g
    path  = pathInPath l (curPath g) m


initClassSsaEnv l g n = SsaEnv mA mS cha cls path
  where
    mA    = envMap toFgn (mAsgn g)
    mS    = mempty
    cha   = ssaCHA g
    cls   = Just (nameInPath l (curPath g) n)
    path  = curPath g


getCounter = cnt <$> get

ssaEnvIds = envKeys

-- Get a variables assignability with a default value
-------------------------------------------------------------------------------------
getAssignability :: F.Symbolic x => SsaEnv r -> x -> Assignability -> Assignability
-------------------------------------------------------------------------------------
getAssignability (mAsgn -> m) x a = fromMaybe a (envFindTy x m)

-------------------------------------------------------------------------------------
initSsaVar   :: SsaEnv r -> AnnSSA r -> Var r -> SSAM r (Var r, SsaEnv r)
-------------------------------------------------------------------------------------
initSsaVar g l x  = case varDeclAnnToAsgn l of
    a@WriteLocal -> do (x', g') <- updSsaEnv g l x
                       return (x', updAsgn a g')
    a            -> return (x, updAsgn a g)
  where
    updAsgn a g = g { mAsgn = envAdd x a (mAsgn g) }

-- Assignability from a VarDecl annotation
--
varDeclToAsgn (VarDecl l _ _) = varDeclAnnToAsgn l

varDeclAnnToAsgn l
  | (a:_) <- [ a_ | VarAnn _ a_ _ <- fFact l ] = a
  | otherwise = WriteLocal

-------------------------------------------------------------------------------------
updSsaEnv :: IsLocated l => SsaEnv r -> l -> Var r -> SSAM r (Var r, SsaEnv r)
-------------------------------------------------------------------------------------
updSsaEnv g l x =
    go (getAssignability g x WriteLocal)
  where
    go   WriteLocal   = updSsaEnvLocal g l x
    go   WriteGlobal  = return (x, g)
    go m@Ambient      = ssaError $ errorWriteImmutable l m x
    go m@RdOnly       = ssaError $ errorWriteImmutable l m x
    go m@ForeignLocal = ssaError $ errorWriteImmutable l m x
    go m@ReturnVar    = ssaError $ errorWriteImmutable l m x

-------------------------------------------------------------------------------------
updSsaEnvLocal :: IsLocated l => SsaEnv r -> l -> Var r -> SSAM r (Var r, SsaEnv r)
-------------------------------------------------------------------------------------
updSsaEnvLocal g l x = do
    x'    <- mkSSAId <$> freshenAnn l <*> pure x <*> tick
    let g' = insertSsaEnv g x x'
    return (x', g')

-------------------------------------------------------------------------------------
tick :: SSAM r Int
-------------------------------------------------------------------------------------
tick = do n     <- cnt <$> get
          modify $ \st -> st { cnt = 1 + n }
          return n

-------------------------------------------------------------------------------------
freshenAnn :: IsLocated l => l -> SSAM r (AnnSSA r)
-------------------------------------------------------------------------------------
freshenAnn l = FA <$> tick <**> srcPos l <**> []

-------------------------------------------------------------------------------------
freshenIdSSA         :: IsLocated l => Id l -> SSAM r (Var r)
-------------------------------------------------------------------------------------
freshenIdSSA (Id l x) = Id <$> freshenAnn l <*> return x

-------------------------------------------------------------------------------------
findSsaEnv   :: F.Symbolic x => SsaEnv r -> x -> Maybe (Var r)
-------------------------------------------------------------------------------------
findSsaEnv g x = envFindTy x (mSSA g)

-------------------------------------------------------------------------------------
insertSsaEnv :: SsaEnv r -> Var r -> Var r -> SsaEnv r
-------------------------------------------------------------------------------------
insertSsaEnv g x xSSA = g { mSSA = envAdd x xSSA (mSSA g) }


-------------------------------------------------------------------------------------
isBIArgumentsVar :: F.Symbolic a => SsaEnv r -> a -> Bool
-------------------------------------------------------------------------------------
isBIArgumentsVar g x =
  case envFindTy x (mAsgn g) of
    Just RdOnly -> F.symbol x == argSym
    _           -> False

addAnn l f = modify $ \st -> st { anns = IM.insertWith (++) (fId l) [f] (anns st) }

setMeas m  = modify $ \st -> st { meas= m }
getMeas    = meas   <$> get

getAnns    = anns   <$> get

getCHA     = ssaCHA <$> get

-------------------------------------------------------------------------------------
ssaError :: Error -> SSAM r a
-------------------------------------------------------------------------------------
ssaError = throwE

-------------------------------------------------------------------------------------
execute :: BareRsc r -> SSAM r a -> Either (F.FixResult Error) a
-------------------------------------------------------------------------------------
execute p act
  = case runState (runExceptT act) (initState p) of
      (Left err, _) -> Left $ F.Unsafe [err]
      (Right x, _)  -> Right x

-- Try the action @act@ in the current state.
-- The state will be intact in the end. Just the result will be returned
-------------------------------------------------------------------------------------
-- tryAction :: SSAM r a -> SSAM r (Either e a, s)
-------------------------------------------------------------------------------------
tryAction act = get >>= return . runState (runExceptT act)

-------------------------------------------------------------------------------------
initState :: BareRsc r -> SsaState r
-------------------------------------------------------------------------------------
initState p = SsaST (maxId p) IM.empty S.empty

