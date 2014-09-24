
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TupleSections             #-}
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
  , mkNextId, isNextId, mkSSAId , mkKeysId, mkKeysIdxId -- , stripSSAId


  -- * Traversals / folds
  , hoistTypes, hoistGlobals
  , visibleNames, scrapeModules, writeGlobalVars

  , scrapeVarDecl

  ) where

import           Control.Applicative     hiding (empty)
import           Control.Exception              (throw)
import           Data.Monoid             hiding ((<>))            
import           Data.Maybe                     (maybeToList, listToMaybe)
import           Data.List                      (stripPrefix)            
import           Data.Generics                   
import qualified Data.IntMap                 as I
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


type PPR  r = (PP r, F.Reftable r, Data r)


---------------------------------------------------------------------------------
-- | Nano Program 
---------------------------------------------------------------------------------

data Nano a r = Nano { 
  -- 
  -- ^ Code to check
  --
    code      :: !(Source a)               
  -- 
  -- ^ Annotations (keeping this to scrape qualifiers later)
  -- ^ XXX: The names are bogus - made unique to avoid overwrites
  --
  , qualPool  :: !(Env (RType r))
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
  isNano (FunctionDecl _ _ _)     = True
  isNano (IfaceStmt _)            = True
  isNano (ModuleStmt _ _ s)       = all isNano s
  isNano e                        = errortext (text "Not Nano Statement:" $$ pp e)

instance IsNano (ClassElt a) where
  isNano (Constructor _ _ ss)        = all isNano ss
  isNano (MemberMethDecl _ _ _ _ ss) = all isNano ss
  isNano (MemberVarDecl _ _ vd)      = isNano vd

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
    
{-    
getWhiles :: [Statement SourceSpan] -> [Statement SourceSpan]
getWhiles stmts = everything (++) ([] `mkQ` fromWhile) stmts
  where 
    fromWhile s@(WhileStmt {}) = [s]
    fromWhile _                = [] 
-}

flattenStmt (BlockStmt _ ss) = concatMap flattenStmt ss
flattenStmt s                = [s]



--------------------------------------------------------------------------------
-- | Manipulating SSA Ids
--------------------------------------------------------------------------------

mkSSAId :: IsLocated a => a -> Id a -> Int -> Id a
mkSSAId l (Id _ x) n = Id l (x ++ ssaStr ++ show n)  

-- Returns the identifier as is if this is not an SSAed name.
-- stripSSAId :: Id a -> Id a
-- stripSSAId (Id l x) = Id l (unpack $ head $ splitOn (pack ssaStr) (pack x))

mkNextId :: Id a -> Id a
mkNextId (Id a x) =  Id a $ nextStr ++ x

isNextId :: Id a -> Maybe (Id a)
isNextId (Id a s) = Id a <$> stripPrefix nextStr s

mkKeysId :: Id a -> Id a
mkKeysId (Id a x) =  Id a $ keysStr ++ x

mkKeysIdxId :: Id a -> Id a
mkKeysIdxId (Id a x) =  Id a $ keysIdxStr ++ x

nextStr    = "_NEXT_"
ssaStr     = "_SSA_"
keysIdxStr = "_KEYS_IDX_"
keysStr    = "_KEYS_"



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
hoistBindings :: Data r => [Statement (AnnType r)] -> [(Id (AnnType r), AnnType r, Assignability)]
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

    fSt :: Statement (AnnType r) -> ([(Id (AnnType r), AnnType r, Assignability)],Bool)
    fSt (FunctionStmt l n _ _) = ([(n, l, ReadOnly)], True)
    fSt (FunctionDecl l n _  ) = ([(n, l, ImportDecl)], True)    
    fSt (ClassStmt l n _ _ _ ) = ([(n, l, ReadOnly)], True)
    fSt (ModuleStmt l n _)     = ([(n, Ann (srcPos l) [ModuleAnn $ F.symbol n], ReadOnly)], True)
    fSt _                      = ([], False)

    fExp :: Expression (AnnType r) -> ([(Id (AnnType r), AnnType r, Assignability)], Bool)
    fExp _                     = ([], True)

    fVd :: VarDecl (AnnType r) -> ([(Id (AnnType r), AnnType r, Assignability)], Bool)
    fVd (VarDecl l n _)        = ([(n, l, WriteGlobal) | VarAnn _ <- ann_fact l], True)



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
    fSt (FunctionDecl _ _ _  ) = ([ ], True)
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
    fSt (FunctionStmt{}) = ([ ], True)
    fSt (FunctionDecl{}) = ([ ], True)
    fSt (ClassStmt{})    = ([ ], True)
    fSt (ModuleStmt{})   = ([ ], True)
    fSt _                = ([ ], False)
    fExp                :: Expression (AnnType r) -> ([Id (AnnType r)], Bool)
    fExp _               = ([ ], True)
    fVd                 :: VarDecl (AnnType r) -> ([Id (AnnType r)], Bool)
    fVd (VarDecl l x _)  = ([ x | VarAnn _ <- ann_fact l ], True)




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
visibleNames :: Data r => [Statement (AnnSSA r)] -> [(Id SourceSpan, (RType r, Assignability))]
---------------------------------------------------------------------------------------
visibleNames s = [ (ann <$> n,(t,a)) | (n,Ann l ff,a) <- hoistBindings s
                                     , f              <- ff
                                     , t              <- annToType l n a f ]
  where
    annToType _ _ ReadOnly   (VarAnn t)      = [t] -- Hoist ReadOnly vars (i.e. function defs)
    annToType _ _ ImportDecl (VarAnn t)      = [t] -- Hoist ImportDecl (i.e. function decls)
    annToType l n _          (ClassAnn {})   = [TClass $ RN $ QName l [] (F.symbol n)]
    annToType l _ _          (ModuleAnn n)   = [TModule $ RP $ QPath l [n]]
    annToType _ _ _          _               = []



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
scrapeModules                = qenvFromList . map mkMod . collectModules
  where
    visibility l                  | ExporedModElt `elem` ann_fact l = Exported
                                  | otherwise                       = Local

    mkMod (ap, m)                 = (ap, {- trace (ppshow (envKeys $ varEnv m)
                                            ++ "\n\n" ++ ppshow (envKeys $ typeEnv m)) $ -}
                                          ModuleDef (varEnv m) (typeEnv m) ap)
    varEnv                        = envFromList . vStmts
    typeEnv                       = envFromList . tStmts

    vStmts                        = concatMap vStmt

    vStmt :: PPR r => Statement (AnnSSA r) -> [(Id SourceSpan, (Visibility, Assignability, RType r))]
    vStmt (VarDeclStmt _ vds)     = [ (ss x, (visibility l, WriteGlobal, t)) 
                                    | VarDecl l x _ <- vds
                                    , VarAnn t <- ann_fact l 
                                    ]
    vStmt (FunctionStmt l x _ _)  = [ (ss x, (visibility l, ReadOnly, t)) 
                                    | VarAnn t <- ann_fact l 
                                    ]
    vStmt (FunctionDecl l x _  )  = [ (ss x, (visibility l, ImportDecl, t)) 
                                    | VarAnn t <- ann_fact l 
                                    ]
    vStmt (ClassStmt l x _ _ _)   = [ (ss x, (visibility l, ReadOnly, TClass $ RN $ QName (ann l) [] $ F.symbol x)) ]
    vStmt (ModuleStmt l x _)      = [ (ss x, (visibility l, ReadOnly, TModule $ RP $ QPath (ann l) [F.symbol x])) ]
    vStmt _                       = [ ] 

    tStmts                        = concatMap tStmt

    tStmt :: PPR r => Statement (AnnSSA r) -> [(Id SourceSpan, IfaceDef r)]
    tStmt c@(ClassStmt _ _ _ _ _) = maybeToList $ resolveType c
    tStmt c@(IfaceStmt _)         = maybeToList $ resolveType c
    tStmt _                       = [ ]

    ss = fmap ann


-- FIXME (?): Does not take into account classes with missing annotations.
--            Ts -> rsc translation should add annotations everywhere.
-- TODO: Use safeExtends to check inheritance
---------------------------------------------------------------------------------------
resolveType :: PPR r => Statement (AnnSSA r) -> Maybe (Id SourceSpan, IfaceDef r)
---------------------------------------------------------------------------------------
resolveType  (ClassStmt l c _ _ cs)
  = case [ t | ClassAnn t <- ann_fact l ] of
      [(vs, h)] -> Just (cc, ID True cc vs h (rMem (tc vs) cs))
      _         -> Nothing
  where
    cc        = fmap ann c
    x         = RN $ QName (srcPos l) [] (F.symbol c)
    tc vs     = TApp (TRef x) ((`TVar` fTop) <$> vs) fTop
    rMem      = concatMap . typeMembers

resolveType (IfaceStmt l)
  = listToMaybe [ (n, t) | IfaceAnn t@(ID _ n _ _ _) <- ann_fact l ]

resolveType _ = Nothing 


-- | `typeMembers` returns all the TypeMember elements associated with a class 
--    element -- XXX: No constructor is added if missing
--
---------------------------------------------------------------------------------------
typeMembers :: PPR r => RType r -> ClassElt (AnnSSA r) -> [TypeMember r]
---------------------------------------------------------------------------------------
typeMembers _ (Constructor l _ _ ) = [ c | ConsAnn c   <- ann_fact l ]

typeMembers _ (MemberVarDecl _ static (VarDecl l _ _)) 
    | static    = [ s | StatAnn  s@(StatSig _ _ _)  <- ann_fact l ]
    | otherwise = [ f | FieldAnn f@(FieldSig _ _ _) <- ann_fact l ]

typeMembers t (MemberMethDecl l static _ _ _ )
    | static    = [ s                  | StatAnn s@(StatSig _ _ _)  <- ann_fact l ]
    | otherwise = [ setThisBinding m t | MethAnn m@(MethSig _ _ _)  <- ann_fact l ]


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


-- | scrapeVarDecl: Scrape a variable declaration for annotations
----------------------------------------------------------------------------------
scrapeVarDecl :: VarDecl (AnnSSA r) -> [RType r]
----------------------------------------------------------------------------------
scrapeVarDecl (VarDecl l _ _) = [ t | VarAnn                 t  <- ann_fact l ] 
                             ++ [ t | FieldAnn (FieldSig _ _ t) <- ann_fact l ]

