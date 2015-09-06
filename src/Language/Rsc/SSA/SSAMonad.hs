{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}

-------------------------------------------------------------------------------------
-- | SSA Monad
-------------------------------------------------------------------------------------


module Language.Rsc.SSA.SSAMonad (

   -- * SSA Information
     Var, SsaInfo (..)

   -- * SSA Monad
   , SSAM
   , ssaError
   , execute
   , tryAction

   -- * SSA Environment
   , SsaEnv
   , names
   , initSsaEnv, updSsaEnv, updSsaEnv'
   , freshenAnn
   , freshenIdSSA
   , findSsaEnv
   , extSsaEnv
   , setSsaEnv
   , setSsaEnvGlob
   , getSsaEnv
   , getSsaEnvGlob
   , getAstCount
   , ssaEnvIds

   -- * Access Annotations
   , addAnn, getAnns
   , setGlobs, getGlobs
   , getAsgn
   , setMeas, getMeas
   , getProgram
   , getCHA

   -- Classes / Modules
   , withinClass
   , getCurrentClass
   , withinModule

   -- * Tracking Assignability
   , getAssignability
   , withAssignabilities
   , withAsgnEnv

   ) where

import           Control.Applicative          ((<$>), (<*>))
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.HashSet                 as S
import qualified Data.IntMap.Strict           as IM
import qualified Data.IntSet                  as I
import           Data.Maybe                   (fromMaybe)
import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.Annots
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.Types

-- import           Debug.Trace                        (trace)

type SSAM r     = ExceptT Error (State (SsaState r))

data SsaState r = SsaST {
  --
  -- ^ Program
  --
    ssa_pgm     :: SsaRsc r
  --
  -- ^ Class hierarchy
  --
  , ssa_cha     :: ClassHierarchy r
  --
  -- ^ Assignability status
  --
  , assign      :: Env Assignability
  --
  -- ^ Current SSA names
  --
  , names       :: SsaEnv r
  --
  -- ^ Like above but for globs
  --
  , glob_names  :: SsaEnv r
  --
  -- ^ Fresh index for SSA vars
  --
  , ssa_cnt     :: !Int
  --
  -- ^ Map of annotation
  --
  , anns        :: !(AnnInfo r)
  --
  -- ^ Global definition ids
  --
  , ssa_globs   :: I.IntSet
  --
  -- ^ Measures
  --
  , ssa_meas    :: S.HashSet F.Symbol
  --
  -- ^ Fresh AST index
  --
  , ssa_ast_cnt :: !NodeId
  --
  -- ^ Class
  --
  , ssa_class   :: Maybe AbsName
  --
  -- ^ Module path
  --
  , ssa_path    :: AbsPath
  }

type SsaEnv r     = Env (SsaInfo r)


-------------------------------------------------------------------------------------
extSsaEnv    :: [Var r] -> SsaEnv r -> SsaEnv r
-------------------------------------------------------------------------------------
extSsaEnv xs = envAdds [(x, SI x) | x <- xs]

-------------------------------------------------------------------------------------
getSsaEnv   :: SSAM r (SsaEnv r)
-------------------------------------------------------------------------------------
getSsaEnv   = names <$> get

-------------------------------------------------------------------------------------
getSsaEnvGlob   :: SSAM r (SsaEnv r)
-------------------------------------------------------------------------------------
getSsaEnvGlob   = glob_names <$> get


getAstCount = ssa_ast_cnt <$> get

ssaEnvIds = envKeys

-------------------------------------------------------------------------------------
setSsaEnv    :: SsaEnv r -> SSAM r ()
------------------------------------------------------------------------------------
setSsaEnv θ = modify $ \st -> st { names = θ }

-------------------------------------------------------------------------------------
setSsaEnvGlob    :: SsaEnv r -> SSAM r ()
------------------------------------------------------------------------------------
setSsaEnvGlob θ = modify $ \st -> st { glob_names = θ }

------------------------------------------------------------------------------------
withAsgnEnv :: Env Assignability -> SSAM r a -> SSAM r a
------------------------------------------------------------------------------------
withAsgnEnv γ act
  = do zOld  <- assign <$> get
       modify $ \st -> st { assign = γ `envUnion` zOld }
       ret   <- act
       modify $ \st -> st { assign = zOld }
       return $ ret


------------------------------------------------------------------------------------
withAssignabilities :: IsLocated l => [(Id l, Assignability)] -> SSAM r a -> SSAM r a
------------------------------------------------------------------------------------
withAssignabilities l act
  = do zOld  <- assign <$> get
       modify $ \st -> st { assign = envFromList' l `envUnion` zOld }
       ret   <- act
       modify $ \st -> st { assign = zOld }
       return $ ret

-------------------------------------------------------------------------------------
withinClass :: F.Symbolic x => x -> SSAM r a -> SSAM r a
-------------------------------------------------------------------------------------
withinClass c act
  = do  cOld    <- ssa_class <$> get
        path    <- ssa_path  <$> get
        modify   $ \st -> st { ssa_class = Just $ nameInPath (srcPos dummySpan) path c }
        ret     <- act
        modify   $ \st -> st { ssa_class = cOld }
        return   $ ret

getCurrentClass
  = ssa_class <$> get

-------------------------------------------------------------------------------------
withinModule :: F.Symbolic x => x -> SSAM r a -> SSAM r a
-------------------------------------------------------------------------------------
withinModule m act
  = do  pOld    <- ssa_path <$> get
        modify   $ \st -> st { ssa_path = extendAbsPath pOld m }
        ret     <- act
        modify   $ \st -> st { ssa_path = pOld }
        return   $ ret

-------------------------------------------------------------------------------------
getAssignability :: Var r -> SSAM r Assignability
-------------------------------------------------------------------------------------
getAssignability x = fromMaybe WriteLocal . envFindTy x . assign <$> get

-------------------------------------------------------------------------------------
initSsaEnv   :: AnnSSA r -> Var r -> SSAM r (Var r)
-------------------------------------------------------------------------------------
initSsaEnv ll x       = getAssignability x >>= go
  where
    go Ambient        = return x
    go _              = updSsaEnv ll x

-------------------------------------------------------------------------------------
updSsaEnv   :: AnnSSA r -> Var r -> SSAM r (Var r)
-------------------------------------------------------------------------------------
updSsaEnv ll x        = getAssignability x >>= go
  where
    go   WriteLocal   = updSsaEnvLocal ll x
    go   WriteGlobal  = updSsaEnvGlobal ll x
    go m@Ambient      = ssaError $ errorWriteImmutable l m x
    go m@ForeignLocal = ssaError $ errorWriteImmutable l m x
    go m@ReturnVar    = ssaError $ errorWriteImmutable l m x
    l                 = srcPos ll

updSsaEnv' l x = (,) <$> getAssignability x <*> updSsaEnv l x

-------------------------------------------------------------------------------------
updSsaEnvLocal :: AnnSSA r -> Var r -> SSAM r (Var r)
-------------------------------------------------------------------------------------
updSsaEnvLocal l x
  = do n     <- ssa_cnt <$> get
       let x' = mkSSAId l x n
       modify $ \st -> st {names = envAdds [(x, SI x')] (names st)} {ssa_cnt = 1 + n}
       return x'

-------------------------------------------------------------------------------------
updSsaEnvGlobal :: AnnSSA r -> Var r -> SSAM r (Var r)
-------------------------------------------------------------------------------------
updSsaEnvGlobal _ x
  = do modify $ \st -> st {glob_names = envAdds [(x, SI x)] (glob_names st)}
       return x

-------------------------------------------------------------------------------------
freshenAnn :: IsLocated l => l -> SSAM r (AnnSSA r)
-------------------------------------------------------------------------------------
freshenAnn l
  = do n     <- ssa_ast_cnt <$> get
       modify $ \st -> st {ssa_ast_cnt = 1 + n}
       return $ FA n (srcPos l) []

-------------------------------------------------------------------------------------
freshenIdSSA         :: IsLocated l => Id l -> SSAM r (Var r)
-------------------------------------------------------------------------------------
freshenIdSSA (Id l x) = Id <$> freshenAnn l <*> return x

-------------------------------------------------------------------------------------
findSsaEnv   :: Var r -> SSAM r (Maybe (Var r))
-------------------------------------------------------------------------------------
findSsaEnv x
  = do θ  <- names <$> get
       case envFindTy x θ of
         Just (SI i) -> return $ Just i
         Nothing     -> return $ Nothing


addAnn l f = modify $ \st -> st { anns = IM.insertWith (++) (fId l) [f] (anns st) }
setGlobs g = modify $ \st -> st { ssa_globs = g }
setMeas m  = modify $ \st -> st { ssa_meas= m }

getMeas    = ssa_meas  <$> get
getAnns    = anns      <$> get
getGlobs   = ssa_globs <$> get
getProgram = ssa_pgm   <$> get
getCHA     = ssa_cha   <$> get
getAsgn    = assign    <$> get

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
initState p cha = SsaST p cha envEmpty envEmpty envEmpty 0 IM.empty
                        I.empty S.empty (maxId p) Nothing (mkAbsPath [])
