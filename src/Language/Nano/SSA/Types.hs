-- Keeping these here to avoid warnings in SSA.hs
--
{-# OPTIONS_GHC -fno-warn-incomplete-patterns   #-}
{-# LANGUAGE LambdaCase                         #-}
{-# LANGUAGE DeriveDataTypeable                 #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}

-------------------------------------------------------------------------------------
-- | SSA Types
-------------------------------------------------------------------------------------

module Language.Nano.SSA.Types (

    hoistReadOnly, hoistVarDecls

  -- , SSAEnv (..), SSAEnvO
  -- * Auxiliary 
  , case1, case2, case3

  ) where

import           Data.Generics                   
import           Data.Either (partitionEithers)

import           Language.Nano.Annots

import           Language.Nano.Syntax 


-- | `hoistReadOnly` returns all the ReadOnly variables that are visible in the 
--    scope of the input statements.
-------------------------------------------------------------------------------
hoistReadOnly :: Data a => [Statement a] -> [Id a]
-------------------------------------------------------------------------------
hoistReadOnly = everythingBut (++) myQ
  where
    myQ a | Just s <- cast a :: (Data a => Maybe (Statement a))  = fSt  s
          | Just s <- cast a :: (Data a => Maybe (Expression a)) = fExp s
          | otherwise                                            = ([], False)

    fSt :: Data a => (Statement a) -> ([Id a],Bool)
    fSt (FunctionStmt _ x _ _  ) = ([x], True)
    fSt (FuncOverload _ x _    ) = ([x], True)
    fSt (FuncAmbDecl  _ x _    ) = ([x], True)
    fSt (ClassStmt    _ x _ _ _) = ([x], True)
    fSt (ModuleStmt   _ x _    ) = ([x], True)
    fSt _                        = ([], False)

    fExp  :: Data a => Expression a -> ([Id a],Bool)
    fExp (FuncExpr _ _ _ _)      = ([ ], True)
    fExp _                       = ([ ], False)


-- | `hoistVarDecls` returns all the declared variables 

hoistVarDecls = partitionEithers . everythingBut (++) myQ
  where
    myQ a | Just s <- cast a :: (Data a => Maybe (Statement a))  = fSt  s
          | Just s <- cast a :: (Data a => Maybe (Expression a)) = fExp s
          | Just s <- cast a :: (Data a => Maybe (VarDecl a))    = fVD  s
          | otherwise                                            = ([], False)
              
    fSt                :: Data a => (Statement a) -> ([Either (Id a) (Id a)],Bool)
    fSt FunctionStmt{}  = ([ ], True)
    fSt FuncOverload{}  = ([ ], True)
    fSt FuncAmbDecl {}  = ([ ], True)
    fSt ClassStmt   {}  = ([ ], True)
    fSt ModuleStmt  {}  = ([ ], True)
    fSt _               = ([ ], False)
 
    fExp               :: Data a => Expression a -> ([Either (Id a) (Id a)],Bool)
    fExp FuncExpr{}     = ([ ], True)
    fExp _              = ([ ], False)
    fVD (VarDecl l x _) | ReadOnlyVar `elem` ann_fact l = ([Left  x], False)
                        | otherwise                     = ([Right x], False)


-- | Auxiliary functions
case1 g f e        = g (\case [e']            -> f e'         ) [e]
case2 g f e1 e2    = g (\case [e1', e2']      -> f e1' e2'    ) [e1, e2]
case3 g f e1 e2 e3 = g (\case [e1', e2', e3'] -> f e1' e2' e3') [e1, e2, e3]

