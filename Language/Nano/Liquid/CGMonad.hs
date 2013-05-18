-- | Operations pertaining to Constraint Generation

module Language.Nano.Liquid.CGMonad (
    
  -- * Constraint Generation Monad
    CGM (..)

  -- * Execute Action and Get FInfo
  , getFInfo 

  -- * Throw Errors
  , cgError    -- :: SrcPos -> String -> CGM a 

  -- * Fresh Id
  , getFreshId -- :: SrcPos -> CGM (Id AnnType) 
  ) where



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


