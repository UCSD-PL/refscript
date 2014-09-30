{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- | SSA Monad 
-------------------------------------------------------------------------------------


module Language.Nano.SSA.SSAMonad (
   
   -- * SSA Information
     Var(..)
   , SsaInfo (..)
   
   -- * SSA Monad
   , SSAM
   , ssaError
   , execute
   , tryAction
 
   -- * SSA Environment
   , SsaEnv
   , names
   , updSsaEnv 
   , freshenAnnSSA
   , freshenIdSSA
   , findSsaEnv
   , extSsaEnv
   , setSsaEnv
   , getSsaEnv
   , getAstCount
   , ssaEnvIds
 
   -- * Access Annotations
   , addAnn, getAnns
   , setGlobs, getGlobs
   , setMeas, getMeas

   -- * Tracking Assignability
   , getAssignability
   , withAssignability

   ) where 

import           Control.Applicative                ((<$>))
import           Control.Monad.State                
import           Control.Monad.Trans.Except

import           Data.Maybe                         (fromMaybe) 
import qualified Data.HashMap.Strict                as M 
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
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax

import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc             

-- import           Debug.Trace                        (trace)

type SSAM r     = ExceptT Error (State (SsaState r))

data SsaState r = SsaST { 
  -- 
  -- ^ Assignability status 
  --
    assign        :: Env Assignability        
  -- 
  -- ^ Current SSA names 
  --
  , names         :: SsaEnv r                 
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
  }

type SsaEnv r     = Env (SsaInfo r)


-- -------------------------------------------------------------------------------------
-- withExtSsaEnv    :: [Var] -> SSAM r a -> SSAM r a
-- -------------------------------------------------------------------------------------
-- withExtSsaEnv xs act 
--   = do θ      <- getSsaEnv 
--        let θ'  = envAdds [(x, SI x) | x <- xs] θ
--        modify  $ \st -> st { names = θ' } 
--        r      <- act
--        modify  $ \st -> st { names = θ }
--        return  r

-------------------------------------------------------------------------------------
extSsaEnv    :: [Var r] -> SsaEnv r -> SsaEnv r
-------------------------------------------------------------------------------------
extSsaEnv xs = envAdds [(x, SI x) | x <- xs]

-------------------------------------------------------------------------------------
getSsaEnv   :: SSAM r (SsaEnv r)
-------------------------------------------------------------------------------------
getSsaEnv   = names <$> get 


getAstCount = ssa_ast_cnt <$> get  


ssaEnvIds = (unpack . snd <$>) . envToList
  where
    unpack (SI v) = v

-------------------------------------------------------------------------------------
setSsaEnv    :: SsaEnv r -> SSAM r () 
------------------------------------------------------------------------------------
setSsaEnv θ = modify $ \st -> st { names = θ } 

-------------------------------------------------------------------------------------
withAssignability :: Assignability -> [Var r] -> SSAM r a -> SSAM r a 
-------------------------------------------------------------------------------------
withAssignability m xs act
  = do zOld  <- assign <$> get
       modify $ \st -> st { assign = zNew `envUnion` zOld } 
       ret   <- act
       modify $ \st -> st { assign = zOld }
       return $ ret
    where 
       zNew   = envFromList $ (, m) <$> xs 

-------------------------------------------------------------------------------------
getAssignability :: Var r -> SSAM r Assignability 
-------------------------------------------------------------------------------------
getAssignability x = fromMaybe WriteLocal . envFindTy x . assign <$> get


-------------------------------------------------------------------------------------
updSsaEnv   :: AnnSSA r -> Var r -> SSAM r (Var r)
-------------------------------------------------------------------------------------
updSsaEnv ll x 
  = do mut <- getAssignability x
       case mut of
         WriteLocal  -> updSsaEnvLocal ll x
         WriteGlobal -> return x
         ReadOnly    -> ssaError $ errorWriteImmutable l x 
         ReturnVar   -> ssaError $ errorWriteImmutable l x 
         ThisVar     -> ssaError $ errorWriteImmutable l x 
         ImportDecl  -> ssaError $ errorWriteImmutable l x 
  where
    l = srcPos ll

updSsaEnvLocal :: AnnSSA r -> Var r -> SSAM r (Var r)
updSsaEnvLocal l x 
  = do n     <- ssa_cnt <$> get
       let x' = mkSSAId l x n
       modify $ \st -> st {names = envAdds [(x, SI x')] (names st)} {ssa_cnt = 1 + n}
       return x'

freshenAnnSSA :: AnnSSA r -> SSAM r (AnnSSA r)
freshenAnnSSA (Ann _ l a)
  = do n     <- ssa_ast_cnt <$> get 
       modify $ \st -> st {ssa_ast_cnt = 1 + n}
       return $ Ann n l a

freshenIdSSA :: IsLocated l => Id l -> SSAM r (Var r)
freshenIdSSA (Id l x) 
  = do n     <- ssa_ast_cnt <$> get 
       modify $ \st -> st {ssa_ast_cnt = 1 + n}
       return $ Id (Ann n (srcPos l) []) x


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
initState p = SsaST envEmpty envEmpty 0 IM.empty I.empty S.empty (max_id p)

