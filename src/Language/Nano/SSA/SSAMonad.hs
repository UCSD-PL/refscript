{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- | SSA Monad 
-------------------------------------------------------------------------------------


module Language.Nano.SSA.SSAMonad (
   
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
   , setMeas, getMeas
   , getProgram

   -- Classes / Modules
   , withinClass
   , getCurrentClass
   , withinModule

   -- * Tracking Assignability
   , getAssignability
   , withAssignabilities, withAssignability

   ) where 

import           Control.Applicative                ((<$>),(<*>))
import           Control.Monad.State                
import           Control.Monad.Trans.Except

import           Data.Maybe                         (fromMaybe) 
import qualified Data.HashSet                       as S
import qualified Data.IntSet                        as I
import qualified Data.IntMap.Strict                 as IM
import qualified Language.Fixpoint.Types            as F
import           Language.Nano.Annots
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Names
import           Language.Nano.Locations
import           Language.Nano.Program
import           Language.Nano.Types
import           Language.Nano.Syntax

import           Language.Fixpoint.Errors

-- import           Debug.Trace                        (trace)

type SSAM r     = ExceptT Error (State (SsaState r))

data SsaState r = SsaST { 
  -- 
  -- ^ Program
  --
    ssa_pgm       :: NanoBareR r
  -- 
  -- ^ Assignability status 
  --
  , assign        :: Env Assignability
  -- 
  -- ^ Current SSA names 
  --
  , names         :: SsaEnv r
  -- 
  -- ^ Like above but for globs
  --
  , glob_names    :: SsaEnv r
  -- 
  -- ^ Fresh index for SSA vars
  --
  , ssa_cnt       :: !Int                     
  -- 
  -- ^ Map of annotation
  --
  , anns          :: !(AnnInfo r)             
  -- 
  -- ^ Global definition ids
  --
  , ssa_globs     :: I.IntSet                 
  -- 
  -- ^ Measures
  --
  , ssa_meas      :: S.HashSet F.Symbol       
  --
  -- ^ Fresh AST index
  --
  , ssa_ast_cnt   :: !NodeId
  --
  -- ^ Class
  --
  , ssa_class     :: Maybe AbsName
  -- 
  -- ^ Module path
  --
  , ssa_path      :: AbsPath
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
withAssignabilities :: IsLocated l => [(Assignability, [Id l])] -> SSAM r a -> SSAM r a 
------------------------------------------------------------------------------------
withAssignabilities l act
  = do zOld  <- assign <$> get
       modify $ \st -> st { assign = zNew `envUnion` zOld } 
       ret   <- act
       modify $ \st -> st { assign = zOld }
       return $ ret
    where 
       zNew   = envFromList' [ (x, m) | (m, xs) <- l, x <- xs ]

-------------------------------------------------------------------------------------
withAssignability :: IsLocated l => Assignability -> [Id l] -> SSAM r a -> SSAM r a 
-------------------------------------------------------------------------------------
withAssignability m xs = withAssignabilities [(m,xs)]

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
    go ReadOnly       = return x
    go _              = updSsaEnv ll x

-------------------------------------------------------------------------------------
updSsaEnv   :: AnnSSA r -> Var r -> SSAM r (Var r)
-------------------------------------------------------------------------------------
updSsaEnv ll x        = getAssignability x >>= go
  where 
    go   WriteLocal   = updSsaEnvLocal ll x
    go   WriteGlobal  = updSsaEnvGlobal ll x
    go m@ForeignLocal = ssaError $ errorWriteImmutable l m x 
    go m@ReadOnly     = ssaError $ errorWriteImmutable l m x 
    go m@ReturnVar    = ssaError $ errorWriteImmutable l m x 
    go m@ImportDecl   = ssaError $ errorWriteImmutable l m x 
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
       return $ Ann n (srcPos l) []

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


addAnn l f = modify $ \st -> st { anns = IM.insertWith (++) (ann_id l) [f] (anns st) }
getAnns    = anns <$> get

setGlobs g =  modify $ \st -> st { ssa_globs = g } 
getGlobs   = ssa_globs <$> get

setMeas m =  modify $ \st -> st { ssa_meas= m } 
getMeas   = ssa_meas <$> get

getProgram = ssa_pgm <$> get


-------------------------------------------------------------------------------------
ssaError :: Error -> SSAM r a
-------------------------------------------------------------------------------------
ssaError = throwE


-------------------------------------------------------------------------------------
execute         :: NanoBareR r -> SSAM r a -> Either (F.FixResult Error) a 
-------------------------------------------------------------------------------------
execute p act 
  = case runState (runExceptT act) (initState p) of 
      (Left err, _) -> Left $ F.Unsafe [err]
      (Right x, _)  -> Right x

-- Try the action @act@ in the current state. 
-- The state will be intact in the end. Just the result will be returned
tryAction act = get >>= return . runState (runExceptT act)

initState :: NanoBareR r -> SsaState r
initState p = SsaST p 
                    envEmpty 
                    envEmpty 
                    envEmpty 
                    0
                    IM.empty 
                    I.empty 
                    S.empty 
                    (max_id p) 
                    Nothing
                    (mkAbsPath [])


