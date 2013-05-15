module Language.Nano.Liquid.SSA (
  -- * SSA Transform
  ssaTransform
  ) where 

import           Control.Applicative                ((<$>), (<*>))
import           Control.Monad                
import           Control.Monad.State                
import qualified Data.HashSet as S 
import qualified Data.List as L
import           Data.Monoid
import           Data.Maybe                         (isJust, fromMaybe, maybeToList)

import           Language.Nano.Types
import           Language.Nano.Liquid.Types
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.Fixpoint.Misc             
import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Text.Printf                        (printf)


ssaTransform :: NanoBare -> NanoSSA
ssaTransform pgm = error "TBD" -- execute $ ssaNano pgm
-- 1. builds TABLE inside SSAM
-- 2. execute
-- 3. extract table
-- 4. use Functor instance/table to decorate :: SourcePos -> (SourcePos, Maybe Annot) 

-------------------------------------------------------------------------------------
ssaNano :: Nano SourcePos () -> SSAM (Nano SourcePos ())
-------------------------------------------------------------------------------------
ssaNano p@(Nano {code = Src fs}) 
  = do fs'   <- mapM ssaFun fs 
       return $ p {code = Src fs'}

-------------------------------------------------------------------------------------
ssaFun :: FunctionStatement SourcePos -> SSAM (FunctionStatement SourcePos)
-------------------------------------------------------------------------------------
ssaFun (FunctionStmt l f xs body) 
  = do setSsaEnv   $ initSsaEnv xs 
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
       (b', φ1, φ2) <- joinSsaEnv l θ1 θ2        -- MERGE & RECORD SIDE CONDITIONS!!!!
       return        $ (b', IfStmt l e' (splice s1' φ1) (splice s2' φ2))

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
splice s (Just s') = BlockStmt (getAnnotation s) [s, s'] 


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

ssaExpr (VarRef l x)
  = VarRef l <$> findSsaEnv x

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
-- | SSA Monad ----------------------------------------------------------------------
-------------------------------------------------------------------------------------

type SSAM = State SsaState

type SsaState = ()

-------------------------------------------------------------------------------------
-- | SSA Environment ----------------------------------------------------------------
-------------------------------------------------------------------------------------


-- type Annm   = M.HashMap SourcePos Annot
type SsaEnv = Env (Id SourcePos) 

-------------------------------------------------------------------------------------
initSsaEnv  = error "TBD"

-------------------------------------------------------------------------------------
getSsaEnv   :: SSAM SsaEnv 
-------------------------------------------------------------------------------------
getSsaEnv   = error "TBD"


-------------------------------------------------------------------------------------
setSsaEnv   :: SsaEnv -> SSAM () 
-------------------------------------------------------------------------------------
setSsaEnv   = error "TBD"

-------------------------------------------------------------------------------------
updSsaEnv   :: SourcePos -> Id SourcePos -> SSAM (Id SourcePos) 
-------------------------------------------------------------------------------------
updSsaEnv   = error "TBD"

-------------------------------------------------------------------------------------
findSsaEnv  :: Id SourcePos -> SSAM (Id SourcePos) 
-------------------------------------------------------------------------------------
findSsaEnv  = error "TBD" 
        -- OLD: maybe (tcError l $ errorUnboundId x) return $ envFindTy x γ
        -- WARNING: make sure to NOT mess with global vars -- e.g. FUNCTION NAMES!
        -- MARK THOSE AS UNASSIGNABLE?

-------------------------------------------------------------------------------------
joinSsaEnv :: SourcePos -> Maybe SsaEnv -> Maybe SsaEnv 
           -> SSAM (Bool, Maybe (Statement SourcePos), Maybe (Statement SourcePos))
-------------------------------------------------------------------------------------
joinSsaEnv = error "TBD"


