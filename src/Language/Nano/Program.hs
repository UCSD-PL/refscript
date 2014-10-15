
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE OverlappingInstances      #-}

module Language.Nano.Program (

  -- * Programs
    Nano (..)
  , NanoBare, NanoSSA, NanoBareR, NanoSSAR, NanoRefType
  , NanoTypeR, NanoType, ExprSSAR, StmtSSAR
  , Source (..)

  -- * Nano Definition
  , IsNano (..), checkTopStmt

  -- * Nano Transformations
  , flattenStmt

  -- * SSA Ids 
  , mkNextId, isNextId, mkSSAId , mkKeysId, mkKeysIdxId, mkCtorStr, mkCtorId


  -- * Traversals / folds
  , hoistTypes
  , hoistGlobals
  , visibleNames
  , scrapeModules
  , writeGlobalVars
  , scrapeVarDecl

  -- * Types
  , mkTypeMembers
  , mkVarEnv
  , SyntaxKind(..)
  , MemberKind(..)
  , VarInfo 

  ) where

import           Control.Applicative     hiding (empty)
import           Control.Exception              (throw)
import           Data.Monoid             hiding ((<>))            
import           Data.Maybe                     (maybeToList, listToMaybe, catMaybes)
import           Data.List                      (stripPrefix, partition)
import           Data.Tuple                     (swap)
import           Data.Generics                   
import qualified Data.Map.Strict                as M
import qualified Data.IntMap                    as I
import           Text.PrettyPrint.HughesPJ 

import           Language.Nano.Annots
import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types

import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.PrettyPrint

import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types        as F

-- import           Debug.Trace                        hiding (traceShow)


---------------------------------------------------------------------------------
-- | Nano Program 
---------------------------------------------------------------------------------

data Nano a r = Nano { 
  -- 
  -- ^ Code to check
  --
    code      :: !(Source a)               
  -- 
  -- ^ Measure Signatures
  --
  , consts    :: !(Env (RType r))          
  -- 
  -- ^ Type aliases
  --
  , tAlias    :: !(TAliasEnv (RType r))    
  -- 
  -- ^ Predicate aliases
  --
  , pAlias    :: !(PAliasEnv)              
  -- 
  -- ^ Qualifiers
  --
  , pQuals    :: ![F.Qualifier]            
  -- 
  -- ^ Type Invariants
  --
  , invts     :: ![Located (RType r)]      
  -- 
  -- ^ Maximum id 
  --
  , max_id    :: NodeId

  } deriving (Functor, Data, Typeable)

type NanoBareR r   = Nano (AnnBare r) r                    -- ^ After Parse
type NanoSSAR r    = Nano (AnnSSA  r) r                    -- ^ After SSA  
type NanoTypeR r   = Nano (AnnType r) r                    -- ^ After TC
type NanoRefType   = NanoTypeR F.Reft                      -- ^ After Liquid

type ExprSSAR r    = Expression (AnnSSA r)
type StmtSSAR r    = Statement  (AnnSSA r)

type NanoBare      = NanoBareR ()
type NanoSSA       = NanoSSAR ()
type NanoType      = NanoTypeR ()


newtype Source a = Src [Statement a]
  deriving (Data, Typeable)

instance Monoid (Source a) where
  mempty                    = Src []
  mappend (Src s1) (Src s2) = Src $ s1 ++ s2

instance Functor Source where 
  fmap f (Src zs) = Src (map (fmap f) zs)

instance (PP r, F.Reftable r) => PP (Nano a r) where
  pp pgm@(Nano {code = (Src s) }) 
    =   text "\n******************* Code **********************"
    $+$ pp s
    $+$ text "\n******************* Constants *****************"
    $+$ pp (consts pgm) 
    $+$ text "\n******************* Predicate Aliases *********"
    $+$ pp (pAlias pgm)
    $+$ text "\n******************* Type Aliases **************"
    $+$ pp (tAlias pgm)
    $+$ text "\n******************* Qualifiers ****************"
    $+$ vcat (F.toFix <$> (take 3 $ pQuals pgm))
    $+$ text "..."
    $+$ text "\n******************* Invariants ****************"
    $+$ vcat (pp <$> (invts pgm))
    $+$ text "\n***********************************************\n"

instance PP t => PP (I.IntMap t) where
  pp m = vcat (pp <$> I.toList m)

instance PP t => PP (F.SEnv t) where
  pp m = vcat $ pp <$> F.toListSEnv m


---------------------------------------------------------------------
-- | Wrappers around `Language.ECMAScript3.Syntax` ------------------
---------------------------------------------------------------------

-- | `IsNano` is a predicate that describes the **syntactic subset** 
--   of ECMAScript3 that comprises `Nano`.

class IsNano a where 
  isNano :: a -> Bool


instance IsNano InfixOp where
  isNano OpLT         = True --  @<@
  isNano OpLEq        = True --  @<=@
  isNano OpGT         = True --  @>@
  isNano OpGEq        = True --  @>=@
  -- isNano OpEq         = True --  @==@
  isNano OpStrictEq   = True --  @===@
  isNano OpNEq        = True --  @!=@
  isNano OpStrictNEq  = True --  @!==@

  isNano OpLAnd       = True --  @&&@
  isNano OpLOr        = True --  @||@

  isNano OpSub        = True --  @-@
  isNano OpAdd        = True --  @+@
  isNano OpMul        = True --  @*@
  isNano OpDiv        = True --  @/@
  isNano OpMod        = True --  @%@
  isNano OpInstanceof = True --  @instanceof@
  isNano OpIn         = True --  @in@
  isNano OpBOr        = True --  @|@
  isNano e            = errortext (text "Not Nano InfixOp!" <+> pp e)

instance IsNano (LValue a) where 
  isNano (LVar _ _)        = True
  isNano (LDot _ e _)      = isNano e
  isNano (LBracket _ e e') = isNano e && isNano e'

instance IsNano (VarDecl a) where
  isNano (VarDecl _ _ (Just e)) = isNano e
  isNano (VarDecl _ _ Nothing)  = True

instance IsNano (Expression a) where 
  isNano (BoolLit _ _)           = True
  isNano (IntLit _ _)            = True
  isNano (NullLit _ )            = True
  isNano (ArrayLit _ es)         = all isNano es
  isNano (StringLit _ _)         = True
  isNano (CondExpr _ e1 e2 e3)   = all isNano [e1,e2,e3]
  isNano (VarRef _ _)            = True
  isNano (InfixExpr _ o e1 e2)   = isNano o && isNano e1 && isNano e2
  isNano (PrefixExpr _ o e)      = isNano o && isNano e
  isNano (CallExpr _ e es)       = all isNano (e:es)
  isNano (ObjectLit _ bs)        = all isNano $ snd <$> bs
  isNano (DotRef _ e _)          = isNano e
  isNano (BracketRef _ e1 e2)    = isNano e1 && isNano e2
  isNano (AssignExpr _ _ l e)    = isNano e && isNano l && isNano e
  isNano (UnaryAssignExpr _ _ l) = isNano l
  isNano (ThisRef _)             = True 
  isNano (SuperRef _)            = True 
  isNano (FuncExpr _ _ _ s)      = isNano s
  isNano (NewExpr _ e es)        = isNano e && all isNano es
  isNano (Cast _ e)              = isNano e
  isNano e                       = errortext (text "Not Nano Expression!" <+> pp e)
  -- isNano _                     = False

instance IsNano AssignOp where
  isNano OpAssign     = True
  isNano OpAssignAdd  = True
  isNano x            = errortext (text "Not Nano AssignOp!" <+> pp x) 
  -- isNano _        = False

instance IsNano PrefixOp where
  isNano PrefixLNot   = True
  isNano PrefixMinus  = True 
  isNano PrefixPlus   = True 
  isNano PrefixTypeof = True 
  isNano PrefixBNot   = True 
  isNano e            = errortext (text "Not Nano PrefixOp!" <+> pp e)
  -- isNano _            = False

instance IsNano (Statement a) where
  isNano (EmptyStmt _)            = True                   --  skip
  isNano (ExprStmt _ e)           = isNanoExprStatement e  --  x = e
  isNano (BlockStmt _ ss)         = isNano ss              --  sequence
  isNano (IfSingleStmt _ b s)     = isNano b && isNano s
  isNano (IfStmt _ b s1 s2)       = isNano b && isNano s1 && isNano s2
  isNano (WhileStmt _ b s)        = isNano b && isNano s
  isNano (ForStmt _ i t inc b)    = isNano i && isNano t && isNano inc && isNano b
  isNano (ForInStmt _ init e s)   = isNano init && isNano e && isNano s
  isNano (VarDeclStmt _ ds)       = all isNano ds
  isNano (ReturnStmt _ e)         = isNano e
  isNano (FunctionStmt _ _ _ b)   = isNano b
  isNano (SwitchStmt _ e cs)      = isNano e && not (null cs) && isNano cs
  isNano (ClassStmt _ _ _ _  bd)  = all isNano bd
  isNano (ThrowStmt _ e)          = isNano e
  isNano (FuncAmbDecl _ _ _) = True
  isNano (FuncOverload _ _ _) = True
  isNano (IfaceStmt _)            = True
  isNano (ModuleStmt _ _ s)       = all isNano s
  isNano (EnumStmt _ _ _)         = True
  isNano e                        = errortext (text "Not Nano Statement:" $$ pp e)

instance IsNano (ClassElt a) where
  isNano (Constructor _ _ ss)       = all isNano ss
  isNano (MemberMethDecl _ _ _ _ )  = True
  isNano (MemberMethDef _ _ _ _ ss) = all isNano ss
  isNano (MemberVarDecl _ _ _ eo)   = isNano eo

instance IsNano a => IsNano (Maybe a) where 
  isNano (Just x) = isNano x
  isNano Nothing  = True

instance IsNano [(Statement a)] where 
  isNano = all isNano 

instance IsNano (ForInit a) where 
  isNano NoInit        = True
  isNano (VarInit vds) = all isNano vds
  isNano (ExprInit e)  = isNano e

instance IsNano (ForInInit a) where 
  isNano (ForInVar _)  = True
  isNano e             = errortext (text "Not Nano ForInInit:" $$ pp e)


-- | Holds for `Expression` that is a valid side-effecting `Statement` 

isNanoExprStatement :: Expression a -> Bool
isNanoExprStatement (UnaryAssignExpr _ _ lv) = isNano lv
isNanoExprStatement (AssignExpr _ o lv e)    = isNano o && isNano lv && isNano e
isNanoExprStatement (CallExpr _ e es)        = all isNano (e:es)
isNanoExprStatement (Cast _ e)               = isNanoExprStatement e
isNanoExprStatement e@(FuncExpr _ _ _ _ )    = errortext (text "Unannotated function expression" <+> pp e)
isNanoExprStatement e                        = errortext (text "Not Nano ExprStmtZ!" <+> pp e)

-- | Switch Statement

-- Is Nano-js code if each clause ends with a break statement

instance IsNano (CaseClause a) where
  isNano (CaseClause _ e st) = isNano e && holdsInit isNano st' && endsWithBreak st'
    where st' = concatMap flattenStmt st
  isNano (CaseDefault _  st) =             holdsInit isNano st' && endsWithBreak st'
    where st' = concatMap flattenStmt st

class EndsWithBreak a where
  endsWithBreak :: a -> Bool

instance EndsWithBreak (Statement a) where
  endsWithBreak (BlockStmt _ xs)      = endsWithBreak xs
  endsWithBreak (BreakStmt _ Nothing) = True
  endsWithBreak _                     = False

instance EndsWithBreak ([Statement a]) where
  endsWithBreak [] = False
  endsWithBreak xs = endsWithBreak $ last xs

instance IsNano [(CaseClause a)] where 
  isNano [] = False
  isNano xs = all isNano xs && holdsInit (not . defaultC) xs
    where
      defaultC (CaseClause _ _ _) = False
      defaultC (CaseDefault _ _ ) = True

-- | Check if `p` hold for all xs but the last one.  
holdsInit :: (a -> Bool) -> [a] -> Bool
holdsInit _ [] = True
holdsInit p xs = all p $ init xs

-- | Trivial Syntax Checking 
-- TODO: Add check for top-level classes here.

checkTopStmt :: (IsLocated a) => Statement a -> Statement a
checkTopStmt s | checkBody [s] = s
checkTopStmt s | otherwise     = throw $ errorInvalidTopStmt (srcPos s) s

checkBody :: [Statement a] -> Bool
-- Adding support for loops so removing the while check
checkBody stmts = all isNano stmts -- && null (getWhiles stmts) 

flattenStmt (BlockStmt _ ss) = concatMap flattenStmt ss
flattenStmt s                = [s]


--------------------------------------------------------------------------------
-- | Manipulating SSA Ids
--------------------------------------------------------------------------------

mkSSAId :: (F.Symbolic x, IsLocated a) => a -> x -> Int -> Id a
mkSSAId l x n = Id l (F.symbolString (F.symbol x) ++ ssaStr ++ show n)  

mkNextId :: Id a -> Id a
mkNextId (Id a x) =  Id a $ nextStr ++ x

isNextId :: Id a -> Maybe (Id a)
isNextId (Id a s) = Id a <$> stripPrefix nextStr s

mkKeysId :: Id a -> Id a
mkKeysId (Id a x) =  Id a $ keysStr ++ x

mkKeysIdxId :: Id a -> Id a
mkKeysIdxId (Id a x) =  Id a $ keysIdxStr ++ x

mkCtorId l (Id _ x) = Id l $ mkCtorStr x
mkCtorStr x         = x ++ ctorStr

nextStr    = "_NEXT_"
ssaStr     = "_SSA_"
keysIdxStr = "_KEYS_IDX_"
keysStr    = "_KEYS_"
ctorStr    = "_CTOR_"



---------------------------------------------------------------------------
-- | AST Traversals
---------------------------------------------------------------------------


-- | Find all language level bindings whose scope reaches the current scope. 
--   This includes: 
--    * function definitions/declarations, 
--    * classes, 
--    * modules,
--    * global (annotated) variables
--
--   E.g. declarations in the If-branch of a conditional expression. Note how 
--   declarations do not escape module or function blocks.
--
-------------------------------------------------------------------------------
hoistBindings :: Data r 
              => [Statement (AnnType r)] 
              -> [(Id (AnnType r), AnnType r, SyntaxKind, Assignability, Initialization)]
-------------------------------------------------------------------------------
hoistBindings = everythingBut (++) myQ
  where
    myQ a = case cast a :: (Data r => Maybe (Statement (AnnType r))) of
              Just  s -> fSt s
              Nothing -> 
                  case cast a :: (Data r => Maybe (Expression (AnnType r))) of
                    Just  s -> fExp s
                    Nothing -> 
                        case cast a :: (Data r => Maybe (VarDecl (AnnType r))) of
                          Just  s -> fVd s
                          Nothing -> ([], False)

    fSt :: Statement (AnnType r) -> ([(Id (AnnType r), AnnType r, SyntaxKind, Assignability, Initialization)],Bool)
    fSt (FunctionStmt l n _ _)  = ([(n, l, FuncDefKind, ReadOnly, Initialized)], True)
    fSt (FuncAmbDecl l n _)     = ([(n, l, FuncAmbientKind, ImportDecl, Initialized)], True)
    fSt (FuncOverload l n _  )  = ([(n, l, FuncOverloadKind, ImportDecl, Initialized)], True)
    fSt (ClassStmt l n _ _ _ )  = ([(n, l, ClassDefKind   , ReadOnly, Initialized)], True)
    fSt (ModuleStmt l n _)      = ([(n, l { ann_fact = ModuleAnn (F.symbol n) : ann_fact l}, ModuleDefKind, ReadOnly, Initialized)], True)
    fSt (EnumStmt l n _)        = ([(n, l { ann_fact = EnumAnn (F.symbol n)   : ann_fact l}, EnumDefKind  , ReadOnly, Initialized)], True)
    fSt _                       = ([], False)

    fExp :: Expression (AnnType r) -> ([(Id (AnnType r), AnnType r, SyntaxKind, Assignability, Initialization)], Bool)
    fExp _                     = ([], True)

    fVd :: VarDecl (AnnType r) -> ([(Id (AnnType r), AnnType r, SyntaxKind, Assignability, Initialization)], Bool)
    fVd (VarDecl l n eo)       = ([(n, l, VarDeclKind, WriteGlobal, Uninitialized) | VarAnn _    <- ann_fact l], True)
    fVd (VarDecl l n eo)       = ([(n, l, VarDeclKind, WriteGlobal, Initialized)   | AmbVarAnn _ <- ann_fact l], True)


initStatus (Just _ ) = Initialized
initStatus _         = Uninitialized



-- | Find classes / interfaces in scope
-------------------------------------------------------------------------------
hoistTypes :: Data a => [Statement a] -> [Statement a]
-------------------------------------------------------------------------------
hoistTypes = everythingBut (++) myQ
  where
    myQ a  = case cast a :: (Data a => Maybe (Statement a)) of
               Just  s -> fSt s
               Nothing -> 
                   case cast a :: (Data a => Maybe (Expression a)) of
                     Just  s -> fExp s
                     Nothing -> ([], False)

    fSt (FunctionStmt _ _ _ _) = ([ ], True)
    fSt FuncAmbDecl{}  = ([ ], True)
    fSt FuncOverload{}     = ([ ], True)
    fSt s@(ClassStmt {})       = ([s], True)
    fSt s@(IfaceStmt {})       = ([s], True)
    fSt (ModuleStmt {})        = ([ ], True)
    fSt _                      = ([ ], False)
    fExp :: Expression a -> ([Statement a], Bool)
    fExp _                     = ([ ], True)



-------------------------------------------------------------------------------
hoistGlobals :: Data r => [Statement (AnnType r)] -> [Id (AnnType r)]
-------------------------------------------------------------------------------
hoistGlobals = everythingBut (++) myQ
  where
    myQ a  = case cast a :: (Data r => Maybe (Statement (AnnType r))) of
               Just  s -> fSt s
               Nothing -> 
                   case cast a :: (Data r => Maybe (Expression (AnnType r))) of
                     Just  s -> fExp s
                     Nothing -> case cast a :: (Data r => Maybe (VarDecl (AnnType r))) of
                                  Just  s -> fVd s
                                  Nothing -> ([], False)

    fSt                 :: Statement (AnnType r) -> ([Id (AnnType r)], Bool)
    fSt (FunctionStmt{})      = ([ ], True)
    fSt FuncAmbDecl{} = ([ ], True)
    fSt FuncOverload{}    = ([ ], True)
    fSt (ClassStmt{})         = ([ ], True)
    fSt (ModuleStmt{})        = ([ ], True)
    fSt _                     = ([ ], False)
    fExp                :: Expression (AnnType r) -> ([Id (AnnType r)], Bool)
    fExp _               = ([ ], True)
    fVd                 :: VarDecl (AnnType r) -> ([Id (AnnType r)], Bool)
    fVd (VarDecl l x _)  = ([ x | VarAnn _ <- ann_fact l ], True)
    fVd (VarDecl l x _)  = ([ x | AmbVarAnn _ <- ann_fact l ], True)




-- | Summarise all nodes in top-down, left-to-right order, carrying some state
--   down the tree during the computation, but not left-to-right to siblings,
--   and also stop when a condition is true.
---------------------------------------------------------------------------
everythingButWithContext :: s -> (r -> r -> r) -> GenericQ (s -> (r, s, Bool)) -> GenericQ r
---------------------------------------------------------------------------
everythingButWithContext s0 f q x
  | stop      = r
  | otherwise = foldl f r (gmapQ (everythingButWithContext s' f q) x)
    where (r, s', stop) = q x s0



type VarInfo r = (SyntaxKind, Visibility, Assignability, RType r, Initialization)

---------------------------------------------------------------------------
-- | AST Folds
---------------------------------------------------------------------------

-- Only descend down modules 
-------------------------------------------------------------------------------
collectModules :: (IsLocated a, Data a) => [Statement a] -> [(AbsPath, [Statement a])]
-------------------------------------------------------------------------------
collectModules ss = topLevel : rest ss
  where
    rest                      = everythingButWithContext [] (++) $ ([],,False) `mkQ` f
    f e@(ModuleStmt _ x ms) s = let p = s ++ [F.symbol x] in
                                ([(AP $ QPath (srcPos e) p, ms)], p, False) 
    f _                    s  = ([], s, True)
    topLevel                  = (AP $ QPath (srcPos dummySpan) [], ss)


---------------------------------------------------------------------------------------
visibleNames :: Data r => [Statement (AnnSSA r)] -> [(Id SourceSpan, VarInfo r)]
---------------------------------------------------------------------------------------
visibleNames s = [ (ann <$> n,(k,v,a,t,i)) | (n,l,k,a,i) <- hoistBindings s
                                           , f           <- ann_fact l
                                           , t           <- annToType (ann l) n a f
                                           , let v        = visibility l ]
  where
    annToType _ _ ReadOnly   (VarAnn t)    = [t] -- Hoist ReadOnly vars (i.e. function defs)
    annToType _ _ ImportDecl (VarAnn t)    = [t] -- Hoist ImportDecl (i.e. function decls)
    annToType _ _ ReadOnly   (AmbVarAnn t) = [t] -- Hoist ReadOnly vars (i.e. function defs)
    annToType _ _ ImportDecl (AmbVarAnn t) = [t] -- Hoist ImportDecl (i.e. function decls)
    annToType l n _          (ClassAnn {}) = [TClass  $ RN $ QName l [] (F.symbol n)]
    annToType l _ _          (ModuleAnn n) = [TModule $ RP $ QPath l [n]]
    annToType l _ _          (EnumAnn n)   = [TEnum   $ RN $ QName l [] (F.symbol n)]
    annToType _ _ _          _             = []


-- | `scrapeModules ss` creates a module store from the statements in @ss@
--   For every module we populate:
--
--    * m_variables with: functions, variables, class constructors, modules
--
--    * m_types with: classes and interfaces
--
---------------------------------------------------------------------------------------
scrapeModules               :: PPR r => [Statement (AnnSSA r)] -> QEnv (ModuleDef r)
---------------------------------------------------------------------------------------
scrapeModules                    = qenvFromList . map mkMod . collectModules
  where
    mkMod (ap, m)                = (ap, {- trace (ppshow (envKeys $ varEnv m)
                                           ++ "\n\n" ++ ppshow (envKeys $ typeEnv m)) $ -}
                                         ModuleDef (varEnv m) (typeEnv m) (enumEnv m) ap)
    drop1 (_,b,c,d,e)            = (b,c,d,e)
    varEnv                       = envMap drop1 . mkVarEnv . vStmts
    typeEnv                      = envFromList  . tStmts
    enumEnv                      = envFromList  . eStmts

    vStmts                       = concatMap vStmt
    vStmt :: PPR r => Statement (AnnSSA r) -> [(Id SourceSpan, VarInfo r)]
    vStmt (VarDeclStmt _ vds)    = [ (ss x, (VarDeclKind, visibility l, WriteGlobal, t, Uninitialized))
                                       | VarDecl l x eo <- vds
                                       , VarAnn t <- ann_fact l ]
    vStmt (VarDeclStmt _ vds)    = [ (ss x, (VarDeclKind, visibility l, WriteGlobal, t, Initialized))
                                       | VarDecl l x eo <- vds
                                       , AmbVarAnn t <- ann_fact l ]
    vStmt (FunctionStmt l x _ _) = [ (ss x, (FuncDefKind, visibility l, ReadOnly, t, Initialized))
                                       | VarAnn t <- ann_fact l ]
    vStmt (FuncAmbDecl l x _)    = [ (ss x, (FuncAmbientKind, visibility l, ImportDecl, t, Initialized))
                                       | VarAnn t <- ann_fact l ]
    vStmt (FuncOverload l x _)   = [ (ss x, (FuncOverloadKind, visibility l, ImportDecl, t, Initialized))
                                       | VarAnn t <- ann_fact l ]
    vStmt (ClassStmt l x _ _ _)  = [ (ss x, (ClassDefKind, visibility l, ReadOnly, TClass   $ RN $ QName (ann l) [] $  F.symbol x , Initialized)) ]
    vStmt (ModuleStmt l x _)     = [ (ss x, (ModuleDefKind, visibility l, ReadOnly, TModule $ RP $ QPath (ann l)    $ [F.symbol x], Initialized)) ]
    vStmt (EnumStmt l x _)       = [ (ss x, (ModuleDefKind, visibility l, ReadOnly, TEnum   $ RN $ QName (ann l) [] $  F.symbol x , Initialized)) ]
    vStmt _                      = [ ] 

    tStmts                       = concatMap tStmt
    tStmt :: PPR r => Statement (AnnSSA r) -> [(Id SourceSpan, IfaceDef r)]
    tStmt c@(ClassStmt{})        = maybeToList $ resolveType c
    tStmt c@(IfaceStmt _)        = maybeToList $ resolveType c
    tStmt _                      = [ ]

    eStmts                       = concatMap eStmt
    eStmt :: PPR r => Statement (AnnSSA r) -> [(Id SourceSpan, EnumDef)]
    syms                         = (F.symbol <$>)
    eStmt (EnumStmt _ n es)      = [(fmap srcPos n, EnumDef (F.symbol n) (I.fromList  $ catMaybes $ enumElt  <$> es)
                                                                         (envFromList $ catMaybes $ sEnumElt <$> es))]
    eStmt _                      = []
    enumElt (EnumElt _ s i)      = (,F.symbol s) <$> i
    sEnumElt (EnumElt _ s i)     = (F.symbol s,) <$> i
    ss = fmap ann


visibility :: Annot (Fact r) a -> Visibility
visibility l | ExporedModElt `elem` ann_fact l = Exported
             | otherwise                       = Local


data SyntaxKind = 
    FuncDefKind 
  | FuncAmbientKind
  | FuncOverloadKind
  | MethDefKind 
  | MethDeclKind
  | FieldDefKind
  | CtorDefKind
  | VarDeclKind
  | ClassDefKind
  | ModuleDefKind
  | EnumDefKind
  deriving ( Eq )

instance PP SyntaxKind where
  pp FuncDefKind      = text "FuncDefKind"
  pp FuncOverloadKind = text "FuncOverloadKind"
  pp FuncAmbientKind  = text "FuncAmbientKind"
  pp MethDefKind      = text "MethDefKind"
  pp MethDeclKind     = text "MethDeclKind"
  pp FieldDefKind     = text "FieldDefKind"
  pp CtorDefKind      = text "CtorDefKind"
  pp VarDeclKind      = text "VarDeclKind"
  pp ClassDefKind     = text "ClassDefKind"
  pp ModuleDefKind    = text "ModuleDefKind"
  pp EnumDefKind      = text "EnumDefKind"

---------------------------------------------------------------------------------------
mkVarEnv :: PPR r => F.Symbolic s => [(s, VarInfo r)] -> Env (VarInfo r)
---------------------------------------------------------------------------------------
mkVarEnv                     = envFromList . concatMap f . M.toList . foldl merge M.empty
  where
    merge ms (x,(s,v,a,t,i)) = M.insertWith (++) (F.symbol x) [(s,v,a,t,i)] ms
    f (s, vs)                = [ (s,(k,v,w, g t [ t' | (FuncOverloadKind, _, _, t', _) <- vs ], i))
                                               | (k@FuncDefKind    , v, w, t, i) <- vs ] ++
                         amb [ (s,(k,v,w,t,i)) | (k@FuncAmbientKind, v, w, t, i) <- vs ] ++ 
                             [ (s,(k,v,w,t,i)) | (k@VarDeclKind    , v, w, t, i) <- vs ] ++ 
                             [ (s,(k,v,w,t,i)) | (k@ClassDefKind   , v, w, t, i) <- vs ] ++
                             [ (s,(k,v,w,t,i)) | (k@ModuleDefKind  , v, w, t, i) <- vs ] ++
                             [ (s,(k,v,w,t,i)) | (k@EnumDefKind    , v, w, t, i) <- vs ]
    g t []                   = t
    g _ ts                   = mkAnd ts
    amb [ ]                  = [ ] 
    amb [a]                  = [a]
    amb ((s,(k,v,w,t,i)):xs) = [(s,(k,v,w, mkAnd (t : map tyOf xs),i))]    
    tyOf (_,(_,_,_,t,_))     = t


-- FIXME (?): Does not take into account classes with missing annotations.
--            Ts -> rsc translation should add annotations everywhere.
-- TODO: Use safeExtends to check inheritance
---------------------------------------------------------------------------------------
resolveType :: PPR r => Statement (AnnSSA r) -> Maybe (Id SourceSpan, IfaceDef r)
---------------------------------------------------------------------------------------
resolveType  (ClassStmt l c _ _ cs)
  = case [ t | ClassAnn t <- ann_fact l ] of
      [(vs, h)] -> Just (cc, ID ClassKind cc vs h $ typeMembers cs)
      _         -> Nothing
  where
    cc        = fmap ann c

resolveType (IfaceStmt l)
  = listToMaybe [ (n, t) | IfaceAnn t@(ID _ n _ _ _) <- ann_fact l ]

resolveType _ = Nothing 

data MemberKind = MemDefinition | MemDeclaration deriving ( Eq )

---------------------------------------------------------------------------------------
typeMembers                      :: [ClassElt (AnnSSA r)] -> TypeMembers r
---------------------------------------------------------------------------------------
typeMembers                       =  mkTypeMembers . concatMap go
  where
    go (MemberVarDecl l s _ _)    = [(sk s    , MemDefinition , f) | FieldAnn f  <- ann_fact l]
    go (MemberMethDef l s _ _ _ ) = [(sk s    , MemDefinition , f) | MethAnn  f  <- ann_fact l]
    go (MemberMethDecl l s _ _ )  = [(sk s    , MemDeclaration, f) | MethAnn  f  <- ann_fact l]
    go (Constructor l _ _)        = [(sk False, MemDefinition , a) | ConsAnn  a  <- ann_fact l]
    sk True                       = StaticMember 
    sk False                      = InstanceMember 

---------------------------------------------------------------------------------------
mkTypeMembers       :: [(StaticKind, MemberKind, TypeMember r)] -> TypeMembers r
---------------------------------------------------------------------------------------
mkTypeMembers        = M.map (g . f) . foldl merge M.empty
  where
    merge ms (s,m,t) = M.insertWith (++) (F.symbol t,s) [(m,t)] ms
    f                = mapPair (map snd) . partition ((== MemDefinition) . fst) 
    g ([t],[])       = t
    g ( _ ,ts)       = foldl1 joinElts ts

joinElts (CallSig t1)        (CallSig t2)       = CallSig         $ joinTys t1 t2 
joinElts (ConsSig t1)        (ConsSig t2)       = ConsSig         $ joinTys t1 t2 
joinElts (IndexSig x1 s1 t1) (IndexSig _ _ t2)  = IndexSig x1 s1  $ joinTys t1 t2
joinElts (FieldSig x1 m1 t1) (FieldSig _ m2 t2) | m1 == m2 
                                                = FieldSig x1 m1  $ joinTys t1 t2 
joinElts (MethSig x1 m1 t1)  (MethSig _ m2 t2)  | m1 == m2 
                                                = MethSig  x1 m1  $ joinTys t1 t2 
joinElts t                   _                  = t

joinTys t1 t2 = mkAnd $ bkAnd t1 ++ bkAnd t2 


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
    fromVD (VarDecl l x _) = [ x | VarAnn _ <- ann_fact l ] ++ [ x | AmbVarAnn _ <- ann_fact l ]


-- | scrapeVarDecl: Scrape a variable declaration for annotations
----------------------------------------------------------------------------------
scrapeVarDecl :: VarDecl (AnnSSA r) -> [RType r]
----------------------------------------------------------------------------------
scrapeVarDecl (VarDecl l _ _) = [ t | VarAnn                 t  <- ann_fact l ] 
                             ++ [ t | AmbVarAnn              t  <- ann_fact l ] 
                             ++ [ t | FieldAnn (FieldSig _ _ t) <- ann_fact l ]

