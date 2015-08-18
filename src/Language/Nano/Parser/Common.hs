{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}


module Language.Nano.Parser.Common (
    xyP, axyP, withSpan, postP
  , convertTVars, convertTVar
  , dot, plus, question
  , withinSpacesP

) where

import           Control.Applicative           ((<$>), (<*>))
import qualified Data.HashSet                  as HS
import           Data.Maybe                    (listToMaybe)
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Parse
import qualified Language.Fixpoint.Types       as F
import           Language.Nano.Annots
import           Language.Nano.AST
import           Language.Nano.Locations       hiding (val)
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types
import           Language.Nano.Visitor
import           Text.Parsec                   hiding (State, parse)
import qualified Text.Parsec.Token             as T

----------------------------------------------------------------------------------
dot :: Parser String
plus :: Parser String
question :: Parser String
----------------------------------------------------------------------------------
dot        = T.dot        lexer
plus       = T.symbol     lexer "+"
question   = T.symbol     lexer "?"

----------------------------------------------------------------------------------
withinSpacesP :: Parser a -> Parser a
----------------------------------------------------------------------------------
withinSpacesP p = do { spaces; a <- p; spaces; return a }


xyP lP sepP rP
  = (\x _ y -> (x, y)) <$> lP <*> (spaces >> sepP) <*> rP


assignabilityP
  =  try (withinSpacesP (reserved "global"  ) >> return WriteGlobal)
 <|> try (withinSpacesP (reserved "local"   ) >> return WriteLocal )
 <|> try (withinSpacesP (reserved "readonly") >> return Ambient    )
 <|>     (return WriteGlobal)

axyP lP sepP rP
  = do  a <- assignabilityP
        i <- withinSpacesP lP
        spaces >> sepP
        r <- rP
        return (i,a,r)

withSpan f p = do pos   <- getPosition
                  x     <- p
                  pos'  <- getPosition
                  return $ f (SS pos pos') x

postP p post = const <$> p <*> post


-- | @convertTvars@ converts @RCon@s corresponding to _bound_ type-variables to @TVar@
convertTVars = visitNano convertTvarVisitor []

----------------------------------------------------------------------------------
convertTVar    :: (F.Reftable r, Transformable t, Show q) => [TVar] -> t q r -> t q r
----------------------------------------------------------------------------------
convertTVar as = trans tx as []
  where
    tx αs _ (TRef (Gen c []) r) | Just α <- mkTvar αs c = TVar α r
    tx _  _ t = t

mkTvar αs r = listToMaybe [ α { tv_loc = srcPos r }  | α <- αs, F.symbol α == F.symbol r]

----------------------------------------------------------------------------------
convertTvarVisitor :: (F.Reftable r) => Visitor () [TVar] (AnnRel r)
----------------------------------------------------------------------------------
convertTvarVisitor = defaultVisitor {
    ctxStmt = ctxStmtTvar
  , ctxCElt = ctxCEltTvar
  , txStmt  = transFmap (const . convertTVar)
  , txExpr  = transFmap (const . convertTVar)
  , txCElt  = transFmap (const . convertTVar)
  }

ctxStmtTvar as s = go s ++ as
  where
    go :: Statement (AnnRel r)  -> [TVar]
    go s@(FunctionStmt {}) = grab s
    go s@(FuncAmbDecl {})  = grab s
    go s@(FuncOverload {}) = grab s
    go s@(IfaceStmt {})    = grab s
    go s@(ClassStmt {})    = grab s
    go s@(ModuleStmt {})   = grab s
    go _                   = []

    grab :: Statement (AnnQ q r) -> [TVar]
    grab = concatMap factTVars . ann_fact . getAnnotation

ctxCEltTvar as s = go s ++ as
  where
    go :: ClassElt (AnnRel r)  -> [TVar]
    go s@Constructor{}     = grab s
    go s@MemberMethDef{}   = grab s
    go _                   = []

    grab :: ClassElt (AnnQ q r) -> [TVar]
    grab = concatMap factTVars . ann_fact . getAnnotation

----------------------------------------------------------------------------------
factTVars :: FactQ q r -> [TVar]
----------------------------------------------------------------------------------
factTVars = go
  where
    tvars t | Just ts <- bkFuns t
            = HS.toList $ foldUnions
            $ map HS.fromList [ btvToTV <$> t | (t, _, _) <- ts ]
            | otherwise
            = []

    foldUnions (α:αs) = foldl HS.intersection α αs
    foldUnions _      = HS.empty

    go (VarAnn _ (Just t))    = tvars t
    go (FuncAnn t)            = tvars t
    go (FieldAnn _ t)         = tvars t
    go (MethAnn _ t)          = tvars t
    go (ConsAnn t)            = tvars t
    go (ClassAnn (TS _ bs _)) = btvToTV <$> b_args bs
    go (InterfaceAnn (TD (TS _ bs _) _))
                              = btvToTV <$> b_args bs
    go _                      = []

