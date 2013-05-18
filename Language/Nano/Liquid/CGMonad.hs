-- | Operations pertaining to Constraint Generation

module Language.Nano.Liquid.CGMonad (
    
  -- * Constraint Generation Monad
    CGM (..)

  -- * Execute Action and Get FInfo
  , getFInfo 

  -- * Throw Errors
  , cgError       -- :: (IsLocated l) => l -> String -> CGM a 

  -- * Instantiate Fresh Type Args (at Call-Site)
  , freshTyArgs   -- :: (IsLocated l) => l -> CGEnv -> ([TVar], RefType) -> CGM RefType 
  
  -- * Instantiate Fresh Type (at Phi-site) 
  , freshTy       -- :: (IsLocated l) => l -> CGEnv -> [(Id l, Type)] -> CGM (CGEnv, [RefType])  

  -- * Environment API
  , envAddFresh   -- :: (IsLocated l) => l -> RefType -> CGEnv -> CGM (Id AnnType, CGEnv) 
  , envAdds       -- :: (F.Symbolic x, IsLocated x) => [(x, RefType)] -> CGEnv -> CGM CGEnv
  , envAddReturn  -- :: (IsLocated f)  => f -> RefType -> CGEnv -> CGM CGEnv 
  , envAddGuard   -- :: (F.Symbolic x, IsLocated x) => [(x, Bool)] -> CGEnv -> CGM CGEnv  
  , envFindTy     -- :: (F.Symbolic x) => x -> CGEnv -> RefType 
  , envFindReturn -- :: CGEnv -> RefType 

  -- * Add Subtyping Constraints
  , subTypes       :: (IsLocated l) => l -> CGEnv -> [RefType] -> [RefType] -> CGM () 
  ) where


subTypes       = error "TOBD"

-------------------------------------------------------------------------------
getFInfo :: NanoRefType -> TCM a -> F.FInfo Cinfo  
-------------------------------------------------------------------------------
getFInfo = error "TOBD"

-- getFInfo pgm act 
--   = case runState (runErrorT act) (initState pgm) of 
--       (Left err, _) -> errorstar err
--       (Right x, st) -> 
--       applyNonNull (Right x) Left (reverse $ tc_errs st)
--     
-- 
-- cgStateFInfo ((cs', ws'), cg)  
--   = F.FI { F.cm    = M.fromList $ F.addIds cs'  
--          , F.ws    = ws'
--          , F.bs    = binds cg
--          , F.gs    = builtinMeasureSEnv
--          , F.lits  = []
--          , F.kuts  = F.ksEmpty
--          , F.quals = [] }
-- 
-- getFixCs 
--   = do cs'   <- cs    <$> get
--        γbs   <- bbγs  <$> get
--        em    <- edgem <$> get 
--        ccs'  <- closeSubC em γbs cs'
--        fcs'  <- concatMapM splitC ccs' 
--        return $ fcs'
-- 
-- getFixWs
--   = do ws'   <- ws <$> get
--        fws'  <- concatMapM splitW ws'
--        return $ fws'
-- 
-- 
-- finState act  = do act
--                    fcs'  <- getFixCs
--                    fws'  <- getFixWs
--                    return $ (fcs', fws') 
-- 
-- initState pgm = error "TOBD"
-- 
-- cgStateFInfo ((cs', ws'), cg)  
--   = F.FI { F.cm    = M.fromList $ F.addIds cs'  
--          , F.ws    = ws'
--          , F.bs    = binds cg
--          , F.gs    = builtinMeasureSEnv
--          , F.lits  = []
--          , F.kuts  = F.ksEmpty
--          , F.quals = [] }

freshTy :: AnnType -> 
freshTyArgs = error "TOBD"

getTypInst :: AnnType -> [Type] 
getTypInst (Ann l fs) 
  = case [ts | TypInst ts <- fs] of 
      [ts] -> return ts
      _    -> cgError l $ errorMissingTypeArgs


