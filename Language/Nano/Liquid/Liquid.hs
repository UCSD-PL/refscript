{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}

-- | Top Level for Refinement Type checker

module Language.Nano.Liquid.Liquid (verifyFile) where

import           Text.Printf                        (printf)
-- import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Control.Monad
import           Control.Monad.State
import           Control.Applicative                ((<$>))

import qualified Data.ByteString.Lazy               as B
import qualified Data.HashMap.Strict                as M
import qualified Data.List                          as L

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser        (SourceSpan (..))

import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Files
import           Language.Fixpoint.Interface        (solve)

import           Language.Nano.CmdLine              (getOpts)
import           Language.Nano.Errors
import qualified Language.Nano.Env                  as E
import           Language.Nano.Misc
import           Language.Nano.Types
import qualified Language.Nano.Annots               as A
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Parse
import           Language.Nano.Typecheck.Typecheck  (typeCheck) 
import           Language.Nano.Typecheck.Subst      (fromList)
import           Language.Nano.Typecheck.Compare
import           Language.Nano.SSA.SSA
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.CGMonad

import           System.Console.CmdArgs.Default

import           Debug.Trace                        as T

--------------------------------------------------------------------------------
verifyFile       :: FilePath -> IO (F.FixResult (SourceSpan, String))
--------------------------------------------------------------------------------
verifyFile f =   
  do  p   <- parseNanoFromFile f
      -- Liquid { kVarInst = kv } <- getOpts
      cfg <- getOpts 
      fmap (, "") <$> reftypeCheck cfg f (typeCheck (ssaTransform p))

-- DEBUG VERSION 
-- ssaTransform' x = tracePP "SSATX" $ ssaTransform x 

reftypeCheck       :: Config -> FilePath -> Nano AnnType RefType -> IO (F.FixResult SourceSpan)
reftypeCheck cfg f = solveConstraints f . generateConstraints cfg

--------------------------------------------------------------------------------
solveConstraints :: FilePath -> CGInfo -> IO (F.FixResult SourceSpan) 
--------------------------------------------------------------------------------
solveConstraints f cgi 
  = do (r, sol) <- solve def f [] $ cgi_finfo cgi
       let r'    = fmap (srcPos . F.sinfo) r
       renderAnnotations f sol r' $ cgi_annot cgi
       donePhase (F.colorResult r) (F.showFix r) 
       return r'

renderAnnotations srcFile sol res ann  
  = do writeFile   annFile  $ wrapStars "Constraint Templates" ++ "\n" 
       appendFile  annFile  $ ppshow ann
       appendFile  annFile  $ wrapStars "Inferred Types"       ++ "\n" 
       appendFile  annFile  $ ppshow ann'
       B.writeFile jsonFile $ A.annotByteString res ann' 
       donePhase Loud "Written Inferred Types"
    where 
       jsonFile = extFileName Json  srcFile
       annFile  = extFileName Annot srcFile
       ann'     = tidy $ applySolution sol ann

applySolution :: F.FixSolution -> A.AnnInfo RefType -> A.AnnInfo RefType 
applySolution = fmap . fmap . tx
  where
    tx s (F.Reft (x, zs))   = F.Reft (x, F.squishRefas (appSol s <$> zs))
    appSol _ ra@(F.RConc _) = ra 
    appSol s (F.RKvar k su) = F.RConc $ F.subst su $ M.lookupDefault F.PTop k s  

tidy = id

--------------------------------------------------------------------------------
generateConstraints     :: Config -> NanoRefType -> CGInfo 
--------------------------------------------------------------------------------
generateConstraints cfg pgm = getCGInfo cfg pgm $ consNano pgm

--------------------------------------------------------------------------------
consNano     :: NanoRefType -> CGM ()
--------------------------------------------------------------------------------
consNano pgm@(Nano {code = Src fs}) 
  = consStmts (initCGEnv pgm) fs >> return ()

  -- = forM_ fs . consFun =<< initCGEnv pgm
initCGEnv pgm = CGE (specs pgm) F.emptyIBindEnv []

--------------------------------------------------------------------------------
consFun :: CGEnv -> FunctionStatement AnnType -> CGM CGEnv  
--------------------------------------------------------------------------------
consFun g (FunctionStmt l f xs body) 
  = do ft             <- tracePP msg <$> (freshTyFun g l f =<< getDefType f)
       g'             <- envAdds [(f, ft)] g 
       g''            <- envAddFun l g' f xs ft
       gm             <- consStmts g'' body
       maybe (return ()) (\g -> subType l g tVoid (envFindReturn g'')) gm
       return g'
    where 
       msg = printf "freshTyFun f = %s" (ppshow f)

consFun _ _ = error "consFun called not with FunctionStmt"

-----------------------------------------------------------------------------------
envAddFun :: AnnType -> CGEnv -> Id AnnType -> [Id AnnType] -> RefType -> CGM CGEnv
-----------------------------------------------------------------------------------
envAddFun l g f xs ft = envAdds tyBinds =<< envAdds (varBinds xs ts') =<< (return $ envAddReturn f t' g) 
  where  
    (αs, yts, t)      = mfromJust "envAddFun" $ bkFun ft
    tyBinds           = [(Loc (srcPos l) α, tVar α) | α <- αs]
    varBinds          = safeZip "envAddFun"
    (su, ts')         = renameBinds yts xs 
    t'                = F.subst su t

renameBinds yts xs   = (su, [F.subst su ty | B _ ty <- yts])
  where 
    su               = F.mkSubst $ safeZipWith "renameBinds" fSub yts xs 
    fSub yt x        = (b_sym yt, F.eVar x)
    
-- checkFormal x t 
--   | xsym == tsym = (x, t)
--   | otherwise    = errorstar $ errorArgName (srcPos x) xsym tsym
--   where 
--     xsym         = F.symbol x
--     tsym         = F.symbol t

--------------------------------------------------------------------------------
consStmts :: CGEnv -> [Statement AnnType]  -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------
consStmts = consSeq consStmt

--------------------------------------------------------------------------------
consStmt :: CGEnv -> Statement AnnType -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------

-- | @consStmt g s@ returns the environment extended with binders that are
-- due to the execution of statement s. @Nothing@ is returned if the
-- statement has (definitely) hit a `return` along the way.

-- skip
consStmt g (EmptyStmt _) 
  = return $ Just g

-- x = e
consStmt g (ExprStmt _ (AssignExpr _ OpAssign (LVar lx x) e))   
  = consAsgn g (Id lx x) e

-- e
consStmt g (ExprStmt _ e)   
  = consExpr g e >> return (Just g) 

-- s1;s2;...;sn
consStmt g (BlockStmt _ stmts) 
  = consStmts g stmts 

-- if b { s1 }
consStmt g (IfSingleStmt l b s)
  = consStmt g (IfStmt l b s (EmptyStmt l))

-- HINT: 1. Use @envAddGuard True@ and @envAddGuard False@ to add the binder 
--          from the condition expression @e@ into @g@ to obtain the @CGEnv@ 
--          for the "then" and "else" statements @s1@ and @s2 respectively. 
--       2. Recursively constrain @s1@ and @s2@ under the respective environments.
--       3. Combine the resulting environments with @envJoin@ 

-- if e { s1 } else { s2 }
consStmt g (IfStmt l e s1 s2)
  = do (xe, ge) <- consExpr g e
       g1'      <- (`consStmt` s1) $ envAddGuard xe True  ge 
       g2'      <- (`consStmt` s2) $ envAddGuard xe False ge 
       envJoin l g g1' g2'

-- var x1 [ = e1 ]; ... ; var xn [= en];
consStmt g (VarDeclStmt _ ds)
  = consSeq consVarDecl g ds

-- return e 
consStmt g (ReturnStmt l (Just e))
  = do (xe, g') <- consExpr g e 
       subType l g' (envFindTy xe g') (envFindReturn g')
       return Nothing

-- return
consStmt _ (ReturnStmt _ Nothing)
  = return Nothing 

-- function f(x1...xn){ s }
consStmt g s@(FunctionStmt _ _ _ _)
  = Just <$> consFun g s

-- OTHER (Not handled)
consStmt _ s 
  = errorstar $ "consStmt: not handled " ++ ppshow s


------------------------------------------------------------------------------------
consVarDecl :: CGEnv -> VarDecl AnnType -> CGM (Maybe CGEnv) 
------------------------------------------------------------------------------------

consVarDecl g (VarDecl _ x (Just e)) 
  = consAsgn g x e  

consVarDecl g (VarDecl _ _ Nothing)  
  = return $ Just g

------------------------------------------------------------------------------------
consAsgn :: CGEnv -> Id AnnType -> Expression AnnType -> CGM (Maybe CGEnv) 
------------------------------------------------------------------------------------
consAsgn g x e 
  = do (x', g') <- consExpr g e
       Just <$> envAdds [(x, envFindTy x' g')] g'

------------------------------------------------------------------------------------
consExpr :: CGEnv -> Expression AnnType -> CGM (Id AnnType, CGEnv) 
------------------------------------------------------------------------------------

-- | @consExpr g e@ returns a pair (g', x') where
--   x' is a fresh, temporary (A-Normalized) variable holding the value of `e`,
--   g' is g extended with a binding for x' (and other temps required for `e`)

consExpr g (DownCast a e) 
  = do  (x, g') <- consExpr g e
        consDownCast g' x a e 

consExpr g (UpCast a e)
  = do  (x, g') <- consExpr g e
        consUpCast g' x a e

consExpr g (DeadCast a e)
  = consDeadCast g a e

consExpr g (IntLit l i)               
  = envAddFresh l (eSingleton tInt i) g

consExpr g (BoolLit l b)
  = envAddFresh l (pSingleton tBool b) g 

consExpr g (StringLit l s)
  = envAddFresh l (eSingleton tString s) g

consExpr g (VarRef i x)
  = do addAnnot l x' $ envFindTy x g
       return (x', g) 
    where 
       x'  =  {- tracePP msg   -} x 
       {-msg = printf "consExpr x = %s at %s" (ppshow x') (ppshow l)-}
       l   = srcPos i

consExpr g (PrefixExpr l o e)
  = consCall g l o [e] (prefixOpTy o $ renv g)

consExpr g (InfixExpr l o e1 e2)        
  = consCall g l o [e1, e2] (infixOpTy o $ renv g)

consExpr g (CallExpr l e es)
  = do (x, g') <- consExpr g e 
       consCall g' l e es $ envFindTy x g'

consExpr  g (DotRef l e i)
  = do  (x, g') <- consExpr g e
        case getBinding i $ envFindTy x g' of 
          Left  s -> errorstar s
          Right t -> envAddFresh l t g'
       
consExpr g (ObjectLit l ps) 
  = do  (x, g') <- consObj l g ps
        envAddFresh l (envFindTy x g') g'

consExpr _ e 
  = error $ (printf "consExpr: not handled %s" (ppshow e))



---------------------------------------------------------------------------------------------
consUpCast :: CGEnv -> Id AnnType -> AnnType -> Expression AnnType -> CGM (Id AnnType, CGEnv)
---------------------------------------------------------------------------------------------
consUpCast g x a e 
  = do  γ         <- getTDefs
        let tE'    = thd4 $ compareTs γ tE tU -- the compatible version for tE
        (x',g')   <- envAddFresh l tE' g 
        return     $ (x', g')
  where tE         = envFindTy x g 
        tU         = rType $ head [ t | Assume t <- ann_fact a]
        eq a b     = toType a == toType b
        l          = getAnnotation e
      

---------------------------------------------------------------------------------------------
consDownCast :: CGEnv -> Id AnnType -> AnnType -> Expression AnnType -> CGM (Id AnnType, CGEnv)
---------------------------------------------------------------------------------------------
consDownCast g x a e 
  = do  
        γ        <- getTDefs
        let (_,ts,_)  = unionParts γ tE tC
        -- TODO: Also need the toplevel constraint here
        forM_ ts  $ castSubM g x l
        (x', g') <- envAddFresh l tC g
        return    $ (x', g')
    where 
      tE               = envFindTy x g
      tC               = rType $ head [ t | Assume t <- ann_fact a]
      l                = getAnnotation e


---------------------------------------------------------------------------------------------
castSubM :: CGEnv -> Id AnnType -> AnnType -> (RefType, RefType) -> CGM () 
---------------------------------------------------------------------------------------------
castSubM g x l (t1, t2) 
  = do (g', t1', t2') <- fixBase g x (t1, t2)
       let msg         = printf "Adding cast Sub: %s\n<:\n%s" (ppshow t1') (ppshow t2')
       -- subType can be called directly at this point
       tracePP msg   <$> subType l g' t1' t2'


-- | fixBase converts:                                                  
--                         -----tE-----              -----tC-----       
-- g, x :: { v: U | r } |- { v: B | p }           <: { v: B | q }       
--              ^                                                       
--              |______Union                                            
--                                                                      
-- into:                                                                
--                                                                      
-- g, x :: { v: B | r } |- { v: B | p ∧ (v = x) } <: { v: B | q }       
-- --------g'----------    ----------tE'---------    ---- tC-----       
--                                                                      
---------------------------------------------------------------------------------------------
fixBase :: CGEnv-> Id AnnType -> (RefType, RefType) -> CGM (CGEnv, RefType, RefType)
---------------------------------------------------------------------------------------------
fixBase g x (tE, tC) =
  do ttE     <- true tE
     let rX'  = ttE `strengthen` rTypeReft (envFindTy x g)
     g'      <- envAdds [(x, rX')] g
     let ttE' = eSingleton ttE x 
     return (g', ttE', tC)


---------------------------------------------------------------------------------------------
consDeadCast :: CGEnv -> AnnType -> Expression AnnType -> CGM (Id AnnType, CGEnv)
---------------------------------------------------------------------------------------------
consDeadCast g a e =
  do  subType l g tru fls
      envAddFresh l tC g
  where
    tC  = rType $ head [ t | Assume t <- ann_fact a]      -- the cast type
    l   = getAnnotation e
    tru = tTop
    fls = tTop `strengthen` F.predReft F.PFalse


---------------------------------------------------------------------------------------------
consCall :: (PP a) 
         => CGEnv -> AnnType -> a -> [Expression AnnType] -> RefType -> CGM (Id AnnType, CGEnv)
---------------------------------------------------------------------------------------------

--   1. Fill in @instantiate@ to get a monomorphic instance of @ft@ 
--      i.e. the callee's RefType, at this call-site (You may want 
--      to use @freshTyInst@)
--   2. Use @consExpr@ to determine types for arguments @es@
--   3. Use @subTypes@ to add constraints between the types from (step 2) and (step 1)
--   4. Use the @F.subst@ returned in 3. to substitute formals with actuals in output type of callee.

consCall g l _ es ft 
  = do (_,its,ot)   <- mfromJust "consCall" . bkFun <$> instantiate l g ft
       (xes, g')    <- consScan consExpr g es
       let (su, ts') = renameBinds its xes
       subTypes l g' xes ts'
       envAddFresh l (F.subst ({- F.traceFix msg -} su) ot) g'
    -- where 
    --   msg xes its = printf "consCall-SUBST %s %s" (ppshow xes) (ppshow its)

instantiate :: AnnType -> CGEnv -> RefType -> CGM RefType
instantiate l g t =  {- tracePP msg  <$>  -} freshTyInst l g αs τs tbody 
  where 
    (αs, tbody)   = bkAll t
    τs            = getTypArgs l αs 
    {-msg           = printf "instantiate [%s] %s %s" (ppshow $ ann l) (ppshow αs) (ppshow tbody)-}


getTypArgs :: AnnType -> [TVar] -> [Type] 
getTypArgs l αs
  = case [i | TypInst i <- ann_fact l] of 
      [i] | length i == length αs -> i 
      _                           -> errorstar $ bugMissingTypeArgs $ srcPos l

---------------------------------------------------------------------------------
consScan :: (CGEnv -> a -> CGM (b, CGEnv)) -> CGEnv -> [a] -> CGM ([b], CGEnv)
---------------------------------------------------------------------------------
consScan step g xs  = go g [] xs 
  where 
    go g acc []     = return (reverse acc, g)
    go g acc (x:xs) = do (y, g') <- step g x
                         go g' (y:acc) xs

---------------------------------------------------------------------------------
consSeq  :: (CGEnv -> a -> CGM (Maybe CGEnv)) -> CGEnv -> [a] -> CGM (Maybe CGEnv) 
---------------------------------------------------------------------------------
consSeq f           = foldM step . Just 
  where 
    step Nothing _  = return Nothing
    step (Just g) x = f g x


---------------------------------------------------------------------------------
consObj :: AnnType -> CGEnv -> [(Prop AnnType, Expression AnnType)] -> CGM (Id AnnType, CGEnv)
---------------------------------------------------------------------------------
consObj l g pe = 
  do
    let (ps, es) = unzip pe
    (xes, g')    <- consScan consExpr g es
    let pxs = zipWith B (map F.symbol ps) $ map (\x -> envFindTy x g') xes
    envAddFresh  l (TObj pxs F.top) g'
    -- XXX What kind of refinements could we get here?
  
