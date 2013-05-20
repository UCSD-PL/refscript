{-# LANGUAGE TupleSections #-}
module Language.Nano.Typecheck.SSA (ssaTransform) where 

import           Control.Applicative                ((<$>), (<*>))
import           Control.Monad                
import           Control.Monad.State                
import           Control.Monad.Error

import qualified Data.HashMap.Strict as M 
import qualified Data.HashSet as S 
import qualified Data.List as L
import           Data.Monoid
import           Data.Maybe                         (isJust, fromMaybe, maybeToList)

import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
import           Language.Fixpoint.Misc             
import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Text.Printf                        (printf)
import qualified Data.Traversable as T
import           Text.Parsec.Pos              

----------------------------------------------------------------------------------
ssaTransform :: (PP t) => Nano SourcePos t -> Nano AnnSSA t
----------------------------------------------------------------------------------
ssaTransform = either (errorstar . snd) id . execute . ssaNano 


----------------------------------------------------------------------------------
ssaNano :: (PP t) => Nano SourcePos t -> SSAM (Nano AnnSSA t) 
----------------------------------------------------------------------------------
ssaNano p@(Nano {code = Src fs}) 
  = do -- fs'    <- forM fs $ T.mapM stripAnn
       addImmutables $ envMap (\_ -> ()) (env    p) 
       addImmutables $ envMap (\_ -> ()) (consts p) 
       fs''   <- mapM ssaFun fs
       anns   <- getAnns
       return $ p {code = Src $ (patchAnn anns <$>) <$> fs''}

stripAnn :: AnnBare -> SSAM SourcePos
stripAnn (Ann l fs) = forM_ fs (addAnn l) >> return l   

patchAnn     :: AnnInfo -> SourcePos -> AnnSSA
patchAnn m l = Ann l $ M.lookupDefault [] l m

-------------------------------------------------------------------------------------
ssaFun :: FunctionStatement SourcePos -> SSAM (FunctionStatement SourcePos)
-------------------------------------------------------------------------------------
ssaFun (FunctionStmt l f xs body) 
  = do setSsaEnv   $ initSsaEnv $ (returnId l) : xs 
       (_, body') <- ssaStmts body 
       return      $ FunctionStmt l f xs body'

-------------------------------------------------------------------------------------
ssaSeq :: (a -> SSAM (Bool, a)) -> [a] -> SSAM (Bool, [a])  
-------------------------------------------------------------------------------------
ssaSeq f            = go True 
  where 
    go False zs     = return (False, zs)
    go b     []     = return (b    , [])
    go True  (x:xs) = do (b , y)  <- f x
                         (b', ys) <- go b xs
                         return      (b', y:ys)

-------------------------------------------------------------------------------------
ssaStmts   :: [Statement SourcePos] -> SSAM (Bool, [Statement SourcePos])
-------------------------------------------------------------------------------------
ssaStmts = ssaSeq ssaStmt

-------------------------------------------------------------------------------------
ssaStmt    :: Statement SourcePos -> SSAM (Bool, Statement SourcePos)
-------------------------------------------------------------------------------------

-- skip
ssaStmt s@(EmptyStmt _) 
  = return (True, s)

-- x = e
ssaStmt (ExprStmt l1 (AssignExpr l2 OpAssign (LVar l3 x) e))   
  = do (x', e') <- ssaAsgn l2 (Id l3 x) e
       return (True, VarDeclStmt l1 [VarDecl l2 x' (Just e')])

-- e
ssaStmt (ExprStmt l e)   
  = do e' <- ssaExpr e
       return (True, ExprStmt l e')

-- s1;s2;...;sn
ssaStmt (BlockStmt l stmts) 
  = do (b, stmts') <- ssaStmts stmts
       return (b, BlockStmt l stmts')

-- if b { s1 }
ssaStmt (IfSingleStmt l b s)
  = ssaStmt (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
ssaStmt (IfStmt l e s1 s2)
  = do e'           <- ssaExpr e
       θ            <- getSsaEnv
       (θ1, s1')    <- ssaWith θ ssaStmt s1
       (θ2, s2')    <- ssaWith θ ssaStmt s2
       (θ', φ1, φ2) <- envJoin l θ1 θ2        
       let stmt'     = IfStmt l e' (splice s1' φ1) (splice s2' φ2)
       case θ' of
         Just θ''   -> setSsaEnv θ'' >> return (True,  stmt') 
         Nothing    ->                  return (False, stmt')

-- var x1 [ = e1 ]; ... ; var xn [= en];
ssaStmt (VarDeclStmt l ds)
  = do (_, ds') <- ssaSeq ssaVarDecl ds
       return (True, VarDeclStmt l ds')

-- return e 
ssaStmt s@(ReturnStmt l Nothing) 
  = return (False, s)

-- return e 
ssaStmt (ReturnStmt l (Just e)) 
  = do e' <- ssaExpr e
       return (False, ReturnStmt l (Just e'))

-- OTHER (Not handled)
ssaStmt s 
  = convertError "ssaStmt" s

-------------------------------------------------------------------------------------
splice :: Statement SourcePos -> Maybe (Statement SourcePos) -> Statement SourcePos
-------------------------------------------------------------------------------------
splice s Nothing   = s
splice s (Just s') = seqStmt (getAnnotation s) s s' 

seqStmt _ (BlockStmt l s) (BlockStmt _ s') = BlockStmt l (s ++ s')
seqStmt l s s'                             = BlockStmt l [s, s']

-------------------------------------------------------------------------------------
ssaWith :: SsaEnv -> (a -> SSAM (Bool, a)) -> a -> SSAM (Maybe SsaEnv, a)
-------------------------------------------------------------------------------------
ssaWith θ f x 
  = do setSsaEnv θ
       (b, x') <- f x
       (, x')  <$> (if b then Just <$> getSsaEnv else return Nothing)

-------------------------------------------------------------------------------------
ssaExpr    :: Expression SourcePos -> SSAM (Expression SourcePos) 
-------------------------------------------------------------------------------------

ssaExpr e@(IntLit _ _)               
  = return e 

ssaExpr e@(BoolLit _ _)
  = return e 

ssaExpr e@(VarRef l x)
  = do imm <- isImmutable x
       if imm then return e else (VarRef l <$> findSsaEnv x)

ssaExpr (PrefixExpr l o e)
  = PrefixExpr l o <$> ssaExpr e

ssaExpr (InfixExpr l o e1 e2)        
  = InfixExpr l o <$> ssaExpr e1 <*> ssaExpr e2

ssaExpr (CallExpr l e es)
  = CallExpr l <$> ssaExpr e <*> mapM ssaExpr es

ssaExpr e 
  = convertError "ssaExpr" e

-------------------------------------------------------------------------------------
ssaVarDecl :: VarDecl SourcePos -> SSAM (Bool, VarDecl SourcePos)
-------------------------------------------------------------------------------------

ssaVarDecl (VarDecl l x (Just e)) 
  = do (x', e') <- ssaAsgn l x e
       return    (True, VarDecl l x' (Just e'))

ssaVarDecl z@(VarDecl l x Nothing)  
  = convertError "ssaVarDECL x" x 

------------------------------------------------------------------------------------
ssaAsgn :: SourcePos -> Id SourcePos -> Expression SourcePos -> SSAM (Id SourcePos, Expression SourcePos) 
------------------------------------------------------------------------------------
ssaAsgn l x e 
  = do e' <- ssaExpr e 
       x' <- updSsaEnv l x
       return (x', e')


-------------------------------------------------------------------------------------
envJoin :: SourcePos -> Maybe SsaEnv -> Maybe SsaEnv 
           -> SSAM ( Maybe SsaEnv
                   , Maybe (Statement SourcePos)
                   , Maybe (Statement SourcePos) )
-------------------------------------------------------------------------------------
envJoin _ Nothing Nothing     = return (Nothing, Nothing, Nothing)
envJoin l Nothing (Just θ)    = return (Just θ , Nothing, Nothing) 
envJoin l (Just θ) Nothing    = return (Just θ , Nothing, Nothing) 
envJoin l (Just θ1) (Just θ2) = envJoin' l θ1 θ2

envJoin' l θ1 θ2
  = do setSsaEnv θ'                          -- Keep Common binders 
       stmts      <- forM phis $ phiAsgn l   -- Adds Phi-Binders, Phi Annots, Return Stmts
       θ''        <- getSsaEnv 
       let (s1,s2) = unzip stmts
       return (Just θ'', Just $ BlockStmt l s1, Just $ BlockStmt l s2) 
    where 
       θ           = envIntersectWith meet θ1 θ2
       θ'          = envRights θ
       phis        = envToList $ envLefts θ 
       meet        = \x1 x2 -> if x1 == x2 then Right x1 else Left (x1, x2)

phiAsgn l (x, (SI x1, SI x2))
  = do x' <- updSsaEnv l x          -- Generate FRESH phi name
       addAnn l (PhiVar x')         -- RECORD x' as PHI-Var at l 
       let s1 = mkPhiAsgn l x' x1   -- Create Phi-Assignments
       let s2 = mkPhiAsgn l x' x2   -- for both branches
       return $ (s1, s2) 
  where 
       mkPhiAsgn l x y = VarDeclStmt l [VarDecl l x (Just $ VarRef l y)]


-------------------------------------------------------------------------------------
-- | SSA Monad ----------------------------------------------------------------------
-------------------------------------------------------------------------------------

type SSAM     = ErrorT String (State SsaState)

data SsaState = SsaST { immutables :: Env ()   -- ^ globals
                      , names      :: SsaEnv   -- ^ current SSA names 
                      , count      :: !Int     -- ^ fresh index
                      , anns       :: !AnnInfo -- ^ built up map of annots 
                      }

type SsaEnv     = Env SsaInfo 
newtype SsaInfo = SI (Id SourcePos) deriving (Eq)

-------------------------------------------------------------------------------------
initSsaEnv    :: [Id SourcePos] -> SsaEnv 
-------------------------------------------------------------------------------------
initSsaEnv xs = envFromList [(x, SI x) | x <- xs] 


-------------------------------------------------------------------------------------
getSsaEnv   :: SSAM SsaEnv 
-------------------------------------------------------------------------------------
getSsaEnv   = names <$> get 

-------------------------------------------------------------------------------------
addImmutables   :: Env () -> SSAM () 
-------------------------------------------------------------------------------------
addImmutables z = modify $ \st -> st { immutables = envExt z (immutables st) } 
  where
    envExt x y  = envFromList (envToList x ++ envToList y)


-------------------------------------------------------------------------------------
setSsaEnv    :: SsaEnv -> SSAM () 
-------------------------------------------------------------------------------------
setSsaEnv θ = modify $ \st -> st { names = θ } 


-------------------------------------------------------------------------------------
updSsaEnv   :: SourcePos -> Id SourcePos -> SSAM (Id SourcePos) 
-------------------------------------------------------------------------------------
updSsaEnv l x 
  = do imm   <- isImmutable x 
       when imm $ ssaError l $ errorWriteImmutable x
       n     <- count <$> get
       let x' = newId l x n
       modify $ \st -> st {names = envAdds [(x, SI x')] (names st)} {count = 1 + n}
       return x'

---------------------------------------------------------------------------------
isImmutable   :: Id SourcePos -> SSAM Bool 
---------------------------------------------------------------------------------
isImmutable x = envMem x . immutables <$> get

newId :: SourcePos -> Id SourcePos -> Int -> Id SourcePos 
newId l (Id _ x) n = Id l (x ++ "_" ++ show n)  

-------------------------------------------------------------------------------
findSsaEnv   :: Id SourcePos -> SSAM (Id SourcePos) 
-------------------------------------------------------------------------------
findSsaEnv x 
  = do θ  <- names <$> get 
       -- ns <- allNames  
       case envFindTy x θ of 
         Just (SI i) -> return i 
         Nothing     -> ssaError (srcPos x) $ errorUnboundId x -- errorUnboundIdEnv x ns 

allNames = do xs <- map fst . envToList . names      <$> get
              ys <- map fst . envToList . immutables <$> get
              return $ xs ++ ys

-------------------------------------------------------------------------------
addAnn     :: SourcePos -> Fact -> SSAM ()
-------------------------------------------------------------------------------
addAnn l f = modify $ \st -> st { anns = inserts l f (anns st) }


-------------------------------------------------------------------------------
getAnns    :: SSAM AnnInfo 
-------------------------------------------------------------------------------
getAnns    = anns <$> get


-------------------------------------------------------------------------------
ssaError       :: SourcePos -> String -> SSAM a
-------------------------------------------------------------------------------
ssaError l msg = throwError $ printf "ERROR at %s : %s" (ppshow l) msg


-- inserts l xs m = M.insert l (xs ++ M.lookupDefault [] l m) m

-------------------------------------------------------------------------------
execute         :: SSAM a -> Either (SourcePos, String) a 
-------------------------------------------------------------------------------
execute act 
  = case runState (runErrorT act) initState of 
      (Left err, _) -> Left  (initialPos "" ,  err)
      (Right x, st) -> Right x

initState :: SsaState
initState = SsaST envEmpty envEmpty 0 M.empty


