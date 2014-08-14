-- Keeping these here to avoid warnings in SSA.hs
--
{-# OPTIONS_GHC -fno-warn-incomplete-patterns     #-}
{-# LANGUAGE LambdaCase                           #-}

-------------------------------------------------------------------------------------
-- | SSA Types
-------------------------------------------------------------------------------------

module Language.Nano.SSA.Types (

  -- * Assignability 
    Assignability (..), writeGlobalVars -- , readOnlyVars  

  , hoistReadOnly, hoistVarDecls

  -- , SSAEnv (..), SSAEnvO
  -- * Auxiliary 
  , case1, case2, case3

  ) where


import           Data.Generics                   
import           Language.Nano.Typecheck.Types
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax 
import           Text.PrettyPrint.HughesPJ 
import           Language.Nano.Types


------------------------------------------------------------------------------------------
-- | Assignability 
------------------------------------------------------------------------------------------

data Assignability 
  = ReadOnly    -- ^ import,  cannot be modified, can appear in refinements
                -- ^ contain: FunctionStmts, Measures, Classes, Modules.
  | WriteLocal  -- ^ written in local-scope, can be SSA-ed, can appear in refinements
  | WriteGlobal -- ^ written in non-local-scope, cannot do SSA, cannot appear in refinements


instance PP Assignability where
  pp ReadOnly    = text "ReadOnly"
  pp WriteLocal  = text "WriteLocal"
  pp WriteGlobal = text "WriteGlobal"


-- | `writeGlobalVars p` returns symbols that have `WriteMany` status, i.e. may be 
--    re-assigned multiply in non-local scope, and hence
--    * cannot be SSA-ed
--    * cannot appear in refinements
--    * can only use a single monolithic type (declared or inferred)
-------------------------------------------------------------------------------
writeGlobalVars           :: Data r => [Statement (AnnType r)] -> [Id (AnnType r)]
-------------------------------------------------------------------------------
writeGlobalVars stmts      = everything (++) ([] `mkQ` fromVD) stmts
  where 
    fromVD (VarDecl l x _) = [ x | VarAnn _ <- ann_fact l ]


-- | `hoistReadOnly` returns all the ReadOnly variables that are visible in the 
--    scope of the input statements.
-------------------------------------------------------------------------------
hoistReadOnly :: Data a => [Statement a] -> [Id a]
-------------------------------------------------------------------------------
hoistReadOnly = everythingBut (++) myQ
  where
    myQ a     = case cast a :: (Data a => Maybe (Statement a)) of
                  Just  s -> fSt s
                  Nothing -> 
                      case cast a :: (Data a => Maybe (Expression a)) of
                        Just  s -> fExp s
                        Nothing -> ([], False)

    fSt                       :: Data a => (Statement a) -> ([Id a],Bool)
    fSt (FunctionStmt _ x _ _) = ([x], True)
    fSt (FunctionDecl _ x _  ) = ([x], True)
    fSt (ClassStmt _ x _ _ _ ) = ([x], True)
    fSt (ModuleStmt _ x _    ) = ([x], True)
    fSt _                      = ([], False)
    fExp                      :: Data a => Expression a -> ([Id a],Bool)
    fExp (FuncExpr _ _ _ _)    = ([ ], True)
    fExp _                     = ([ ], False)


-- | `hoistVarDecls` returns all the declared variables 
--   
--   XXX: using extQ does not seem to work - doing the unfloding manually here.
--
-------------------------------------------------------------------------------
hoistVarDecls :: Data a => [Statement a] -> [Id a]
-------------------------------------------------------------------------------
hoistVarDecls = everythingBut (++) myQ
  where
    myQ a     = case cast a :: (Data a => Maybe (Statement a)) of
                  Just  s -> fSt s
                  Nothing -> 
                      case cast a :: (Data a => Maybe (Expression a)) of
                        Just  s -> fExp s
                        Nothing -> 
                           case cast a :: (Data a => Maybe (VarDecl a)) of
                             Just  s -> fVD s
                             Nothing -> ([], False)
              
    fSt                       :: Data a => (Statement a) -> ([Id a],Bool)
    fSt (FunctionStmt _ _ _ _) = ([ ], True)
    fSt (FunctionDecl _ _ _  ) = ([ ], True)
    fSt (ClassStmt _ _ _ _ _ ) = ([ ], True)
    fSt (ModuleStmt _ _ _    ) = ([ ], True)
    fSt _                      = ([ ], False)
    fExp                      :: Data a => Expression a -> ([Id a],Bool)
    fExp (FuncExpr _ _ _ _)    = ([ ], True)
    fExp _                     = ([ ], False)
    fVD (VarDecl _ x _)        = ([x], False)



-- | Auxiliary functions
case1 g f e = g (\case [e'] -> f e') [e]
case2 g f e1 e2 = g (\case [e1', e2'] -> f e1' e2') [e1, e2]
case3 g f e1 e2 e3 = g (\case [e1', e2', e3'] -> f e1' e2' e3') [e1, e2, e3]

