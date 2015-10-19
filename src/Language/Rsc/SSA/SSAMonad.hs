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

   -- * SSA Environment
   , SsaEnv (..)
   , initGlobSsaEnv
   , initCallableSsaEnv
   , initModuleSsaEnv
   , initClassSsaEnv
   , initSsaVar
   , updSsaEnv
   , freshenAnn
   , freshenIdSSA
   , findSsaEnv

   -- , extSsaEnv
   , setSsaVars
   , getSsaVars
   -- , setSsaEnvGlob
   , ssaVars
   -- , getSsaEnvGlob
   , getAstCount
   , ssaEnvIds

   -- * Access Annotations
   , addAnn, getAnns
   -- , setGlobs, getGlobs
   -- , getAsgn
   , setMeas, getMeas
   , getCHA

   -- Classes / Modules
   -- , withinClass
   -- , getCurrentClass
   -- , withinModule

   -- * Tracking Assignability
   , getAssignability
   -- , withAssignabilities
   -- , withAsgnEnv

   ) where

import           Control.Applicative         ((<$>), (<*>))
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.HashSet                as S
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as I
import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types     as F
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
import           Language.Rsc.Traversals
import           Language.Rsc.Types

-- import           Debug.Trace                        (trace)



type SSAM r     = ExceptT Error (State (SsaState r))


-- | SSA Monad state
--
data SsaState r = SsaST {
    cnt     :: !Int                      -- ^ Fresh index for SSA vars
  , ssaVars :: Env (Var r)               -- ^ Program var -> latest SSA name
  , anns    :: !(AnnInfo r)              -- ^ Map of annotation
  , meas    :: S.HashSet F.Symbol        -- ^ Measures
  , ast_cnt :: !NodeId                   -- ^ Fresh AST index
  }

-- | SSA Environment
--
data SsaEnv r = SsaEnv {
    asgn     :: Env Assignability
  , ssaCHA   :: ClassHierarchy r
  , curClass :: Maybe AbsName
  , curPath  :: AbsPath
  }

instance PP (SsaEnv r) where
  pp (SsaEnv asgn _ _ _) = pp asgn

-------------------------------------------------------------------------------------
initGlobSsaEnv :: F.Reftable r => [Statement (AnnR r)] -> ClassHierarchy r -> SsaEnv r
-------------------------------------------------------------------------------------
initGlobSsaEnv fs cha
  = SsaEnv (envFromListConcat (accumVars' fs)) cha Nothing emptyPath

initCallableSsaEnv l g arg ret xs bd = SsaEnv env cha cls path
  where
    env  = envMap toFgn (asgn g)
         & envUnion (envFromListConcat (accumVars' bd))
         & envAdd ret ReturnVar
         & envAdd arg RdOnly
         & envAdds (xs `zip` repeat WriteLocal)
    cha  = ssaCHA g
    cls  = curClass g
    path = curPath g

toFgn WriteLocal = ForeignLocal
toFgn a          = a

initModuleSsaEnv l g m ss = SsaEnv env cha cls path
  where
    env  = envFromListConcat (accumVars' ss) `envUnion` envMap toFgn (asgn g)
    cha  = ssaCHA g
    cls  = curClass g
    path = pathInPath l (curPath g) m


initClassSsaEnv l g n = SsaEnv env cha cls path
  where
    env  = envMap toFgn (asgn g)
    cha  = ssaCHA g
    cls  = Just (nameInPath l (curPath g) n)
    path = curPath g


getAstCount = ast_cnt <$> get

ssaEnvIds = envKeys

setSsaVars  :: Env (Var r) -> SSAM r ()
setSsaVars θ = modify $ \st -> st { ssaVars = θ }

getSsaVars :: SSAM r (Env (Var r))
getSsaVars  = ssaVars <$> get

-------------------------------------------------------------------------------------
getAssignability :: IsLocated l => SsaEnv r -> Id l -> SSAM r Assignability
-------------------------------------------------------------------------------------
getAssignability g@(asgn -> asgn) x
  | Just a <- envFindTy x asgn = return a
  | otherwise                  = ssaError $ errorUnboundId x x

-------------------------------------------------------------------------------------
initSsaVar   :: SsaEnv r -> AnnSSA r -> Var r -> SSAM r (Var r)
-------------------------------------------------------------------------------------
initSsaVar g l x
  = do  a <- getAssignability g x
        go a
  where
    go Ambient     = return x
    go RdOnly      = return x
    go WriteGlobal = return x
    go _           = updSsaEnv g l x

-------------------------------------------------------------------------------------
updSsaEnv   :: SsaEnv r -> AnnSSA r -> Var r -> SSAM r (Var r)
-------------------------------------------------------------------------------------
updSsaEnv g a@(srcPos -> l) x
  = do  a <- getAssignability g x
        go a
  where
    go   WriteLocal   = updSsaEnvLocal g a x
    go   WriteGlobal  = return x
    go m@Ambient      = ssaError $ errorWriteImmutable l m x
    go m@RdOnly       = ssaError $ errorWriteImmutable l m x
    go m@ForeignLocal = ssaError $ errorWriteImmutable l m x
    go m@ReturnVar    = ssaError $ errorWriteImmutable l m x

-------------------------------------------------------------------------------------
updSsaEnvLocal :: SsaEnv r -> AnnSSA r -> Var r -> SSAM r (Var r)
-------------------------------------------------------------------------------------
updSsaEnvLocal g a x
  = do n     <- cnt <$> get
       let x' = mkSSAId a x n
       modify $ \st -> st { ssaVars = envAdds [(x, x')] (ssaVars st) }
                          { cnt     = 1 + n }
       return x'

-------------------------------------------------------------------------------------
freshenAnn :: IsLocated l => l -> SSAM r (AnnSSA r)
-------------------------------------------------------------------------------------
freshenAnn l
  = do n     <- ast_cnt <$> get
       modify $ \st -> st { ast_cnt = 1 + n }
       return $ FA n (srcPos l) []

-------------------------------------------------------------------------------------
freshenIdSSA         :: IsLocated l => Id l -> SSAM r (Var r)
-------------------------------------------------------------------------------------
freshenIdSSA (Id l x) = Id <$> freshenAnn l <*> return x

-------------------------------------------------------------------------------------
findSsaEnv   :: Var r -> SSAM r (Maybe (Var r))
-------------------------------------------------------------------------------------
findSsaEnv x
  = do  θ <- ssaVars <$> get
        case envFindTy x θ of
          Just ssa_v -> return $ Just ssa_v
          Nothing    -> return $ Nothing


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
execute :: BareRsc r -> ClassHierarchy r -> SSAM r a -> Either (F.FixResult Error) a
-------------------------------------------------------------------------------------
execute p cha act
  = case runState (runExceptT act) (initState p cha) of
      (Left err, _) -> Left $ F.Unsafe [err]
      (Right x, _)  -> Right x

-- Try the action @act@ in the current state.
-- The state will be intact in the end. Just the result will be returned
tryAction act = get >>= return . runState (runExceptT act)

-------------------------------------------------------------------------------------
initState :: BareRsc r -> ClassHierarchy r -> SsaState r
-------------------------------------------------------------------------------------
initState p cha = SsaST 0 envEmpty IM.empty S.empty (maxId p)

