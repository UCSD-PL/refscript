-- | Global type definitions for Refinement Type Checker

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE NoMonomorphismRestriction   #-}

module Language.Nano.Typecheck.Types (

  -- * Programs
    Nano (..)
  , NanoBare
  , NanoSSA, NanoSSAR, NanoTSSAR, NanoTypeR 
  , NanoType
  , ExprSSAR, StmtSSAR
  , Source (..)
  , FunctionStatement
  , mapCode
  -- , sourceNano
  -- , sigsNano

  -- * (Refinement) Types
  , RType (..)
  , Bind (..)
  , toType
  , ofType
  , strengthen 

  -- * Predicates on Types 
  , isTop
  , isNull
  , isUndefined
  , isObj
  , isUnion

  -- * Constructing Types
  , mkUnion, mkUnionR

  -- * Deconstructing Types
  , bkFun
  , bkFuns
  , bkAll
  , bkAnd
  , bkUnion, rUnion
  , rTypeR, setRTypeR
  , noUnion
  , unionCheck
  , funTys
  , renameBinds 
  , calleeType

  -- * Regular Types
  , Type
  , TBody (..)
  , TVar (..)
  , TCon (..)

  -- * Primitive Types
  , tInt
  , tBool
  , tString
  , tTop
  , tVoid
  , tErr
  , tFunErr
  , tVar
  , tArr
  , tUndef
  , tNull
  , tAnd

  , isTVar
  , isArr

  -- * Operator Types
  , infixOpTy
  , prefixOpTy 
  , builtinOpTy
  , arrayLitTy

  -- * Annotations
  , Annot (..)
  , UFact
  , Fact (..)
  , Cast(..)
  , AnnToken(..)
  , varDeclAnnot

  -- * Aliases for annotated Source 
  , AnnBare, UAnnBare
  , AnnSSA , UAnnSSA
  , AnnType, UAnnType
  , AnnInfo, UAnnInfo
--  , SST

  -- * Contexts
  , CallSite (..)
  , IContext
  , emptyContext
  , pushContext

  -- * Mutability 
  , Mutability (..)
  , writeGlobalVars  
  , readOnlyVars  

  -- * Aliases
  , Alias (..)
  , TAlias (..)
  , PAlias (..)
  , PAliasEnv
  , TAliasEnv
  ) where 

import           Text.Printf
import           Data.Hashable
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Data.Monoid                    hiding ((<>))            
import qualified Data.List                      as L
import qualified Data.HashMap.Strict            as M
import           Data.Generics                   
import           Data.Typeable                  ()
import           Language.ECMAScript3.Syntax    hiding (Cast)
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser.Type  (SourceSpan (..))
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Env

import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Errors
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ 

import           Control.Applicative            hiding (empty)
import           Control.Monad.Error            ()

-- import           Debug.Trace (trace)

-- | Type Variables
data TVar = TV { tv_sym :: F.Symbol
               , tv_loc :: SourceSpan 
               }
            deriving (Show, Ord, Data, Typeable)

instance Eq TVar where 
  a == b = tv_sym a == tv_sym b

instance IsLocated TVar where 
  srcPos = tv_loc

instance Hashable TVar where 
  hashWithSalt i α = hashWithSalt i $ tv_sym α 

instance F.Symbolic TVar where
  symbol = tv_sym 

instance PP TVar where 
  pp     = pprint . F.symbol

instance F.Symbolic a => F.Symbolic (Located a) where 
  symbol = F.symbol . val

-- | Constructed Type Bodies
data TBody r 
   = TD { td_con  :: !TCon          -- TDef name ...
        , td_args :: ![TVar]        -- Type variables
        , td_body :: !(RType r)     -- int or bool or fun or object ...
        , td_pos  :: !SourceSpan    -- Source position
        } deriving (Eq, Ord, Show, Functor, Data, Typeable)

-- | Type Constructors
data TCon 
  = TInt                   
  | TBool                
  | TString
  | TVoid              
  | TTop
  | TDef  (Id SourceSpan)
  | TUn
  | TNull
  | TUndef
    deriving (Ord, Show, Data, Typeable)

-- | (Raw) Refined Types 
data RType r  
  = TApp TCon [RType r]     r   -- ^ C T1,...,Tn
  | TVar TVar               r   -- ^ A
  | TFun [Bind r] (RType r) r   -- ^ (x1:T1,...,xn:Tn) => T
  | TObj [Bind r]           r   -- ^ {f1:T1,...,fn:Tn} 
  | TArr (RType r)          r   -- ^ [T] 
  | TBd  (TBody r)              -- ^ ???
  | TAll TVar (RType r)         -- ^ forall A. T
  | TAnd [RType r]              -- ^ (T1..) => T1' /\ ... /\ (Tn..) => Tn' 
  | TExp F.Expr                 -- ^ "Expression" parameters for type-aliases: never appear in real/expanded RType
    deriving (Ord, Show, Functor, Data, Typeable)

data Bind r
  = B { b_sym  :: F.Symbol
      , b_type :: !(RType r)
      } 
    deriving (Eq, Ord, Show, Functor, Data, Typeable)


-- | Standard Types
type Type    = RType ()

-- | Stripping out Refinements 
toType :: RType a -> Type
toType = fmap (const ())
  
-- | Adding in Refinements
ofType :: (F.Reftable r) => Type -> RType r
ofType = fmap (const F.top)

-- | `calleeType` uses the types at the callsite to extract the appropriate
--   conjunct from an intersection.

calleeType l ts ft@(TAnd fts) = L.find (argsMatch ts) fts
calleeType _ _ ft             = Just ft

-- | `argsMatch ts ft` holds iff the arg-types in `ft` are identical to `ts` ... 
argsMatch :: [RType a] -> RType b -> Bool
argsMatch ts ft = case bkFun ft of 
                    Nothing        -> False
                    Just (_,xts,_) -> (toType <$> ts) == ((toType . b_type) <$> xts)

funTys l f xs ft 
  = case bkFuns ft of
      Nothing -> die $ errorNonFunction (srcPos l) f ft 
      Just ts -> zip ([0..] :: [Int]) [funTy l f xs t | t <- ts]

funTy l f xs (αs, yts, t) 
  | eqLen xs yts = let (su, ts') = renameBinds yts xs 
                   in  (αs, ts', F.subst su t)    
  | otherwise    = die $ errorArgMismatch (srcPos l)

eqLen xs ys       = length xs == length ys 

renameBinds yts xs    = (su, [F.subst su ty | B _ ty <- yts])
  where 
    su                = F.mkSubst $ safeZipWith "renameBinds" fSub yts xs 
    fSub yt x         = (b_sym yt, F.eVar x)


bkFuns :: RType r -> Maybe [([TVar], [Bind r], RType r)]
bkFuns = sequence . fmap bkFun . bkAnd 

bkFun :: RType r -> Maybe ([TVar], [Bind r], RType r)
bkFun t = do let (αs, t') = bkAll t
             (xts, t'')  <- bkArr t'
             return        (αs, xts, t'')
         
bkArr (TFun xts t _) = Just (xts, t)
bkArr _              = Nothing

bkAll                :: RType a -> ([TVar], RType a)
bkAll t              = go [] t
  where 
    go αs (TAll α t) = go (α : αs) t
    go αs t          = (reverse αs, t)

bkAnd                :: RType r -> [RType r]
bkAnd (TAnd ts)      = ts
bkAnd t              = [t]

---------------------------------------------------------------------------------
mkUnion :: (Ord r, Eq r, F.Reftable r) => [RType r] -> RType r
---------------------------------------------------------------------------------
mkUnion = mkUnionR F.top


---------------------------------------------------------------------------------
mkUnionR :: (Ord r, Eq r, F.Reftable r) => r -> [RType r] -> RType r
---------------------------------------------------------------------------------
mkUnionR _ [ ] = tErr
mkUnionR r [t] = strengthen t r
mkUnionR r ts  | length ts' > 1 = TApp TUn ts' r
               | otherwise      = strengthen (head ts') r
                where ts' = L.sort $ L.nub ts


---------------------------------------------------------------------------------
bkUnion :: RType r -> [RType r]
---------------------------------------------------------------------------------
bkUnion (TApp TUn xs _) = xs
bkUnion t               = [t]


-- | Strengthen the top-level refinement

---------------------------------------------------------------------------------
strengthen                   :: F.Reftable r => RType r -> r -> RType r
---------------------------------------------------------------------------------
strengthen (TApp c ts r) r'  = TApp c ts $ r' `F.meet` r 
strengthen (TVar α r)    r'  = TVar α    $ r' `F.meet` r 
strengthen (TArr t r)    r'  = TArr t    $ r' `F.meet` r
strengthen t _               = t                         

-- NOTE: r' is the OLD refinement. 
--       We want to preserve its VV binder as it "escapes", 
--       e.g. function types. Sigh. Should have used a separate function binder.


-- | Strengthen the refinement of a type @t2@ deeply, using the 
-- refinements of an equivalent (having the same raw version) 
-- type @t1@.
-- TODO: Add checks for equivalence in union and objects

---------------------------------------------------------------------------------
-- | Predicates on Types 
---------------------------------------------------------------------------------

-- | Top-level Top (any) check
isTop :: RType r -> Bool
isTop (TApp TTop _ _)   = True 
isTop (TApp TUn  ts _ ) = any isTop ts
isTop _                 = False

isUndefined :: RType r -> Bool
isUndefined (TApp TUndef _ _)   = True 
isUndefined _                   = False

isNull :: RType r -> Bool
isNull (TApp TNull _ _)   = True 
isNull _                  = False

isObj :: RType r -> Bool
isObj (TObj _ _)        = True
isObj _                 = False

isUnion :: RType r -> Bool
isUnion (TApp TUn _ _) = True           -- top-level union
isUnion _              = False

-- Get the top-level refinement for unions - use Top (True) otherwise
rUnion               :: F.Reftable r => RType r -> r
rUnion (TApp TUn _ r) = r
rUnion _              = F.top
 
-- Get the top-level refinement 
rTypeR               :: RType r -> r
rTypeR (TApp _ _ r ) = r
rTypeR (TVar _ r   ) = r
rTypeR (TFun _ _ r ) = r
rTypeR (TObj _ r   ) = r
rTypeR (TArr _ r   ) = r
rTypeR (TBd  _     ) = errorstar "Unimplemented: rTypeR - TBd"
rTypeR (TAll _ _   ) = errorstar "Unimplemented: rTypeR - TAll"
rTypeR (TAnd _ )     = errorstar "Unimplemented: rTypeR - TAnd"
rTypeR (TExp _)      = errorstar "Unimplemented: rTypeR - TExp"

setRTypeR :: RType r -> r -> RType r
setRTypeR (TApp c ts _   ) r' = TApp c ts r'
setRTypeR (TVar v _      ) r' = TVar v r'
setRTypeR (TFun xts ot _ ) r' = TFun xts ot r'
setRTypeR (TObj xts _    ) r' = TObj xts r'
setRTypeR (TArr t _      ) r  = TArr t r
setRTypeR (TBd  _        ) _  = errorstar "Unimplemented: setRTypeR - TBd"
setRTypeR (TAll _ _      ) _  = errorstar "Unimplemented: setRTypeR - TAll"
setRTypeR (TAnd _        ) _  = errorstar "Unimplemented: setRTypeR - TAnd"
setRTypeR (TExp _        ) _  = errorstar "Unimplemented: setRTypeR - TExp"


---------------------------------------------------------------------------------------
noUnion :: (F.Reftable r) => RType r -> Bool
---------------------------------------------------------------------------------------
noUnion (TApp TUn _ _)  = False
noUnion (TApp _  rs _)  = and $ map noUnion rs
noUnion (TFun bs rt _)  = and $ map noUnion $ rt : (map b_type bs)
noUnion (TObj bs    _)  = and $ map noUnion $ map b_type bs
noUnion (TArr t     _)  = noUnion t
noUnion (TBd  _      )  = error "noUnion: cannot have TBodies here"
noUnion (TAll _ t    )  = noUnion t
noUnion _               = True

unionCheck t | noUnion t = t 
unionCheck t | otherwise = error $ printf "%s found. Cannot have unions." $ ppshow t


instance Eq TCon where
  TInt    == TInt    = True   
  TBool   == TBool   = True           
  TString == TString = True
  TVoid   == TVoid   = True         
  TTop    == TTop    = True
  TDef i1 == TDef i2 = F.symbol i1 == F.symbol i2
  TUn     == TUn     = True
  TNull   == TNull   = True
  TUndef  == TUndef  = True
  _       == _       = False
 
instance (Eq r, Ord r, F.Reftable r) => Eq (RType r) where
  TApp TUn t1 _       == TApp TUn t2 _       = (null $ t1 L.\\ t2) && (null $ t2 L.\\ t1)
  TApp c1 t1s r1      == TApp c2 t2s r2      = (c1, t1s, r1)  == (c2, t2s, r2)
  TVar v1 r1          == TVar v2 r2          = (v1, r1)       == (v2, r2)
  TFun b1 t1 r1       == TFun b2 t2 r2       = (b1, t1, r1)   == (b2, t2, r2)
  TObj b1 r1          == TObj b2 r2          = (null $ b1 L.\\ b2) && (null $ b2 L.\\ b1) && r1 == r2
  TArr t1 r1          == TArr t2 r2          = t1 == t2 && r1 == r2
  TBd (TD c1 a1 b1 _) == TBd (TD c2 a2 b2 _) = (c1, a1, b1)   == (c2, a2, b2)
  TAll _ _            == TAll _ _            = errorstar "Unimplemented: Eq (RType r)" -- TODO
  _                   == _                   = False



---------------------------------------------------------------------------------
-- | Nano Program = Code + Types for all function binders
---------------------------------------------------------------------------------

data Nano a t = Nano { code   :: !(Source a)        -- ^ Code to check
                     , specs  :: !(Env t)           -- ^ Imported Specifications
                     , sigs   :: !(Env t)           -- ^ Signatures for Code
                     , consts :: !(Env t)           -- ^ Measure Signatures 
                     , defs   :: !(Env t)           -- ^ Type definitions
							       , tAlias :: !(TAliasEnv t)     -- ^ Type aliases
                     , pAlias :: !(PAliasEnv)       -- ^ Predicate aliases
                     , tAnns  :: !(Env t)           -- ^ Type annotations
                     , quals  :: ![F.Qualifier]     -- ^ Qualifiers
                     , invts  :: ![Located t]       -- ^ Type Invariants
                     } deriving (Functor, Data, Typeable)

type NanoBareR r   = Nano (AnnBare r) (RType r)
type NanoSSAR r    = Nano (AnnSSA  r) (RType r)
type NanoTSSAR r   = Nano (AnnTSSA  r) (RType r)
type NanoTypeR r   = Nano (AnnType r) (RType r)

type ExprSSAR r    = Expression (AnnSSA r)
type StmtSSAR r    = Statement  (AnnSSA r)

type NanoBare      = NanoBareR ()
type NanoSSA       = NanoSSAR ()
type NanoType      = NanoTypeR ()

{-@ measure isFunctionStatement :: (Statement SourceSpan) -> Prop 
    isFunctionStatement (FunctionStmt {}) = true
    isFunctionStatement (_)               = false
  @-}

{-@ type FunctionStatement = {v:(Statement SourceSpan) | (isFunctionStatement v)} @-}
type FunctionStatement a = Statement a 

{-@ newtype Source a = Src [FunctionStatement a] @-}
-- newtype Source a = Src [FunctionStatement a]
newtype Source a = Src [Statement a]
  deriving (Data, Typeable)

instance Monoid (Source a) where
  mempty                    = Src []
  mappend (Src s1) (Src s2) = Src $ s1 ++ s2

instance Functor Source where 
  fmap f (Src zs) = Src (map (fmap f) zs)

instance (PP t, PP F.Reft) => PP (Nano a t) where
  pp pgm@(Nano {code = (Src s) }) 
    =   text "******************* Code **********************"
    $+$ pp s
    $+$ text "******************* Specifications ************"
    $+$ pp (specs pgm)
    $+$ text "******************* Definitions ***************"
    $+$ pp (sigs  pgm)
    $+$ text "******************* Constants *****************"
    $+$ pp (consts pgm) 
    $+$ text "******************* Type Annotations **********"
    $+$ pp (tAnns pgm) 
    $+$ text "******************* Type Definitions **********"
    $+$ pp (defs  pgm)
    $+$ text "******************* Predicate Aliases *********"
    $+$ pp (pAlias pgm)
    $+$ text "******************* Type Aliases **************"
    $+$ pp (tAlias pgm)
    $+$ text "******************* Qualifiers ****************"
    $+$ F.toFix (quals  pgm) 
    $+$ text "******************* Invariants ****************"
    $+$ pp (invts pgm) 
    $+$ text "***********************************************"
    
instance Monoid (Nano a t) where 
  mempty        = Nano mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty 
  mappend p1 p2 = Nano { code   = (code   p1) `mappend` (code   p2)
                       , specs  = (specs  p1) `mappend` (specs  p2)
                       , sigs   = (defs   p1) `mappend` (defs   p2)
                       , consts = (consts p1) `mappend` (consts p2)
                       , defs   = (tDefs  p1) `mappend` (tDefs  p2)
                       , tAlias = (tAlias p1) `mappend` (tAlias p2)
                       , pAlias = (pAlias p1) `mappend` (pAlias p2)
                       , tAnns  = (tAnns  p1) `M.union` (tAnns  p2)
                       , quals  = (quals  p1) `mappend` (quals  p2)
                       , invts  = (invts  p1) `mappend` (invts  p2)
                       } 

mapCode :: (a -> b) -> Nano a t -> Nano b t
mapCode f n = n { code = fmap f (code n) }


---------------------------------------------------------------------------------
-- | Specifications (moved from Parser - MOVE BACK!!!)
---------------------------------------------------------------------------------


instance (PP l, PP t) => PP (PSpec l t) where
  pp (Meas (i, t))   = text "measure: " <+> pp i
  pp (Bind (i, t))   = text "bind: " <+>  pp i <+> text " :: " <+> pp t
  pp (Extern (i, t)) = text "extern: " <+>  pp i <+> text " :: " <+> pp t
  pp (Qual q)        = text "qualifier..."
  pp (Type (i, t))   = text "type def"
  pp (Invt l t)      = text "invariant"


------------------------------------------------------------------------------------------
-- | Mutability 
------------------------------------------------------------------------------------------

data Mutability 
  = ReadOnly    -- ^ import,  cannot be modified, can appear in refinements
  | WriteLocal  -- ^ written in local-scope, can be SSA-ed, can appear in refinements
  | WriteGlobal -- ^ written in non-local-scope, cannot do SSA, cannot appear in refinements


-- | `writeGlobalVars p` returns symbols that have `WriteMany` status, i.e. may be 
--    re-assigned multiply in non-local scope, and hence
--    * cannot be SSA-ed
--    * cannot appear in refinements
--    * can only use a single monolithic type (declared or inferred)
 
writeGlobalVars   :: PP t => Nano a t -> [Id SourceSpan] 
writeGlobalVars p = envIds mGnty 
  where
    mGnty         = foldl1 envUnion [specs p, tAnns p, sigs p]  -- guarantees

-- | `immutableVars p` returns symbols that must-not be re-assigned and hence
--    * can appear in refinements

readOnlyVars   :: Nano a t -> [Id SourceSpan] 
readOnlyVars p = envIds $ mAssm `envUnion` mMeas
  where 
    mAssm      = specs p   -- assumes
    mMeas      = consts p  -- measures



---------------------------------------------------------------------------
-- | Pretty Printer Instances ---------------------------------------------
---------------------------------------------------------------------------

instance PP () where 
  pp _ = text ""

instance PP a => PP [a] where 
  pp = ppArgs brackets comma 

instance PP a => PP (Maybe a) where 
  pp = maybe (text "Nothing") pp 

instance F.Reftable r => PP (RType r) where
  pp (TVar α r)                 = F.ppTy r $ pp α 
  pp (TFun xts t _)             = ppArgs parens comma xts <+> text "=>" <+> pp t 
  pp t@(TAll _ _)               = text "forall" <+> ppArgs id space αs <> text "." <+> pp t' where (αs, t') = bkAll t
  pp (TAnd ts)                  = vcat [text "/\\" <+> pp t | t <- ts]
  pp (TExp e)                   = pprint e 
  pp (TApp TUn ts r)            = F.ppTy r $ ppArgs id (text "+") ts 
  pp (TApp d@(TDef _)ts r)      = F.ppTy r $ ppTC d <+> ppArgs brackets comma ts 
  pp (TApp c [] r)              = F.ppTy r $ ppTC c 
  pp (TApp c ts r)              = F.ppTy r $ parens (ppTC c <+> ppArgs id space ts)  
  pp (TArr t r)                 = F.ppTy r $ brackets (pp t)  
  pp (TObj bs r )               = F.ppTy r $ ppArgs braces comma bs
  pp (TBd (TD (TDef id) v r _)) = pp (F.symbol id) <+> ppArgs brackets comma v <+> pp r
  pp (TBd _)                    = error "This is not an acceptable form for TBody" 

instance PP TCon where
  pp TInt             = text "Int"
  pp TBool            = text "Boolean"
  pp TString          = text "String"
  pp TVoid            = text "Void"
  pp TTop             = text "Top"
  pp TUn              = text "Union:"
  pp (TDef x)         = pprint (F.symbol x)
  pp TNull            = text "Null"
  pp TUndef           = text "Undefined"

instance Hashable TCon where
  hashWithSalt s TInt        = hashWithSalt s (0 :: Int)
  hashWithSalt s TBool       = hashWithSalt s (1 :: Int)
  hashWithSalt s TString     = hashWithSalt s (2 :: Int)
  hashWithSalt s TVoid       = hashWithSalt s (3:: Int)
  hashWithSalt s TTop        = hashWithSalt s (4 :: Int)
  hashWithSalt s TUn         = hashWithSalt s (5 :: Int)
  hashWithSalt s TNull       = hashWithSalt s (6 :: Int)
  hashWithSalt s TUndef      = hashWithSalt s (7 :: Int)
  hashWithSalt s (TDef z)    = hashWithSalt s (8 :: Int) + hashWithSalt s z

instance F.Reftable r => PP (Bind r) where 
  pp (B x t)        = pp x <> colon <> pp t 

ppArgs p sep          = p . intersperse sep . map pp
ppTC TInt             = text "Int"
ppTC TBool            = text "Boolean"
ppTC TString          = text "String"
ppTC TVoid            = text "Void"
ppTC TTop             = text "Top"
ppTC TUn              = text "Union:"
ppTC (TDef x)         = pprint (F.symbol x)
ppTC TNull            = text "Null"
ppTC TUndef           = text "Undefined"


instance (PP s, PP t) => PP (M.HashMap s t) where
  pp m = vcat $ pp <$> M.toList m

-- MOVE TO PARSER
instance (PP r, F.Reftable r) => PP (AnnToken r) where
  pp (TBind (id,t)) = pp id <+> text " :: " <+> pp t
  pp (TType t)      = pp t
  pp (TSpec s)      = pp s
  pp EmptyToken     = text "<empyt>"

-----------------------------------------------------------------------------
-- | IContext keeps track of context of intersection-type cases -------------
-----------------------------------------------------------------------------

-- | Keeps track of intersection-type context, to allow casts to be guarded by
--   context. Otherwise, the "dead-casts" for one case must be proven in another
--   case which is impossible. See tests/liquid/pos/misc/negate-05.js
--   A context IC [i_1,...,i_n] denotes the case where we use the conjunct i_k
--   from the kth function in lexical scope order (ignoring functions that have
--   a single conjunct.)

class CallSite a where
  siteIndex :: a -> Int

instance CallSite Int where
  siteIndex i = i

newtype IContext = IC [Int] 
                   deriving (Eq, Ord, Show, Data, Typeable)

instance PP Int where
  pp        = int

instance PP IContext where
  pp (IC x) = text "Context: " <+> pp x

emptyContext         :: IContext
emptyContext         = IC []

pushContext          :: (CallSite a) => a -> IContext -> IContext
pushContext s (IC c) = IC ((siteIndex s) : c) 

-----------------------------------------------------------------------------
-- | Casts ------------------------------------------------------------------
-----------------------------------------------------------------------------

data Cast t  = UCST {castTarget :: t } -- ^ up-cast
             | DCST {castTarget :: t } -- ^ down-cast 
             | DC   {castTarget :: t } -- ^ dead-cast
             deriving (Eq, Ord, Show, Data, Typeable)

instance (PP a) => PP (Cast a) where
  pp (UCST t) = text "Upcast  : " <+> pp t
  pp (DCST t) = text "Downcast: " <+> pp t
  pp (DC   t) = text "Deadcast: " <+> pp t

-----------------------------------------------------------------------------
-- | Annotations ------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Annotations: Extra-code decorations needed for Refinement Type Checking
--   Ideally, we'd have "room" for these inside the @Statement@ and
--   @Expression@ type, but are tucking them in using the @a@ parameter.

data Fact r
  = PhiVar      ![(Id SourceSpan)]
  | TypInst     !IContext ![RType r]
  | TCast       !IContext !(Cast (RType r))
  | TAnnot      !(RType r)
    deriving (Eq, Ord, Show, Data, Typeable)

type UFact = Fact ()

data Annot b a = Ann { ann :: a, ann_fact :: [b] } deriving (Show, Data, Typeable)
type AnnBare r = Annot (Fact r) SourceSpan -- NO facts
type AnnSSA  r = Annot (Fact r) SourceSpan -- Phi facts
type AnnTSSA r = Annot (Fact r) SourceSpan -- Phi + t. annot. facts
type AnnType r = Annot (Fact r) SourceSpan -- Phi + t. annot. + Cast facts
type AnnInfo r = M.HashMap SourceSpan [Fact r] 

type UAnnBare = AnnBare () 
type UAnnSSA  = AnnSSA  ()
type UAnnType = AnnType ()
type UAnnInfo = AnnInfo ()

-- MOVE TO PARSER !!!
-- | `AnnToken`: Elements that can are parsed along the source as annotations.

data AnnToken r 
  = TBind (Id SourceSpan, RType r)          -- ^ Function signature binding
  | TType (RType r)                         -- ^ Variable declaration binding
  | TSpec (PSpec SourceSpan (RType r))      -- ^ Specs: qualifiers, measures, type defs, etc.
  | EmptyToken                              -- ^ Dummy empty token
  deriving (Eq, Ord, Show, Data, Typeable)



instance HasAnnotation (Annot b) where 
  getAnnotation = ann 

instance Ord (AnnSSA  r) where 
  compare (Ann s1 _) (Ann s2 _) = compare s1 s2

instance Eq (Annot a SourceSpan) where 
  (Ann s1 _) == (Ann s2 _) = s1 == s2

instance IsLocated (Annot a SourceSpan) where 
  srcPos = ann

instance IsLocated TCon where
  srcPos (TDef z) = srcPos z
  srcPos _        = srcPos dummySpan

instance (F.Reftable r, PP r) => PP (Fact r) where
  pp (PhiVar x)       = text "phi"  <+> pp x
  pp (TypInst ξ ts)   = text "inst" <+> pp ξ <+> pp ts 
  pp (TCast  ξ c)     = text "cast" <+> pp ξ <+> pp c
  pp (TAnnot t)       = text "annotation" <+> pp t

instance (F.Reftable r, PP r) => PP (AnnInfo r) where
  pp             = vcat . (ppB <$>) . M.toList 
    where 
      ppB (x, t) = pp x <+> dcolon <+> pp t

instance (PP a, PP b) => PP (Annot b a) where
  pp (Ann x ys) = text "Annot: " <+> pp x <+> pp ys

varDeclAnnot v = listToMaybe [ t | TAnnot t <- ann_fact $ getAnnotation v]
--type SST r     = (SourceSpan, Maybe (RType r))

-----------------------------------------------------------------------
-- | Primitive / Base Types -------------------------------------------
-----------------------------------------------------------------------

tVar   :: (F.Reftable r) => TVar -> RType r
tVar   = (`TVar` F.top) 

isTVar (TVar _ _) = True
isTVar _          = False

tInt, tBool, tUndef, tNull, tString, tVoid, tErr :: (F.Reftable r) => RType r
tInt     = TApp TInt     [] F.top 
tBool    = TApp TBool    [] F.top
tString  = TApp TString  [] F.top
tTop     = TApp TTop     [] F.top
tVoid    = TApp TVoid    [] F.top
tUndef   = TApp TUndef   [] F.top
tNull    = TApp TNull    [] F.top
tErr     = tVoid
tFunErr  = ([],[],tErr)
tAnd ts  = case ts of 
             [ ] -> errorstar "BUG: empty intersection"
             [t] -> t
             _   -> TAnd ts

tArr    = (`TArr` F.top)

isArr (TArr _ _ ) = True
isArr _           = False


-----------------------------------------------------------------------
-- | Operator Types ---------------------------------------------------
-----------------------------------------------------------------------

-----------------------------------------------------------------------
builtinOpTy       :: (IsLocated l) => l -> BuiltinOp -> Env t -> t
-----------------------------------------------------------------------
builtinOpTy l o g = fromMaybe err $ envFindTy ox g
  where 
    err           = die $ bugUnknown (srcPos l) "builtinOp" o
    ox            = builtinOpId o
 
builtinOpId BIUndefined     = builtinId "BIUndefined"
builtinOpId BIBracketRef    = builtinId "BIBracketRef"
builtinOpId BIBracketAssign = builtinId "BIBracketAssign"
builtinOpId BIArrayLit      = builtinId "BIArrayLit"
builtinOpId BINumArgs       = builtinId "BINumArgs"

-- arrayLitTy :: (IsLocated l) => l -> Int -> Env t -> t
arrayLitTy l n g 
  = case ty of 
      TAll α (TFun [xt] t r) -> TAll α $ TFun (arrayLitBinds n xt) (arrayLitOut n t) r
      _                      -> err 
    where
      ty                     = builtinOpTy l BIArrayLit g
      err                    = die $ bug (srcPos l) $ "Bad Type for ArrayLit Constructor"
      
arrayLitBinds n (B x t) = [B (x_ i) t | i <- [1..n]] 
  where
    xs                  = F.symbolString x
    x_                  = F.symbol . (xs ++) . show 

arrayLitOut n t         = F.subst1 t (F.symbol $ builtinOpId BINumArgs, F.expr (n::Int))

-----------------------------------------------------------------------
infixOpTy :: InfixOp -> Env t -> t 
-----------------------------------------------------------------------
infixOpTy o g = fromMaybe err $ envFindTy ox g
  where 
    err       = errorstar $ printf "Cannot find infixOpTy %s" (ppshow ox) -- (ppshow g)
    ox        = infixOpId o

infixOpId OpLT       = builtinId "OpLT"
infixOpId OpLEq      = builtinId "OpLEq"
infixOpId OpGT       = builtinId "OpGT"
infixOpId OpGEq      = builtinId "OpGEq"
infixOpId OpEq       = builtinId "OpEq"
infixOpId OpStrictEq = builtinId "OpSEq"
infixOpId OpNEq      = builtinId "OpNEq"
infixOpId OpLAnd     = builtinId "OpLAnd"
infixOpId OpLOr      = builtinId "OpLOr"
infixOpId OpSub      = builtinId "OpSub"
infixOpId OpAdd      = builtinId "OpAdd"
infixOpId OpMul      = builtinId "OpMul"
infixOpId OpDiv      = builtinId "OpDiv"
infixOpId OpMod      = builtinId "OpMod"
infixOpId o          = errorstar $ "Cannot handle: infixOpId " ++ ppshow o

-----------------------------------------------------------------------
prefixOpTy :: PrefixOp -> Env t -> t 
-----------------------------------------------------------------------
prefixOpTy o g = fromMaybe err $ envFindTy (prefixOpId o) g
  where 
    err       = convertError "prefixOpTy" o

prefixOpId PrefixMinus  = builtinId "PrefixMinus"
prefixOpId PrefixLNot   = builtinId "PrefixLNot"
prefixOpId PrefixTypeof = builtinId "PrefixTypeof"
prefixOpId o            = errorstar $ "Cannot handle: prefixOpId " ++ ppshow o


builtinId       = mkId . ("builtin_" ++)


-----------------------------------------------------------------------
-- Type and Predicate Aliases -----------------------------------------
-----------------------------------------------------------------------

data Alias a s t = Alias {
    al_name   :: Id SourceSpan  -- ^ alias name
  , al_tyvars :: ![a]           -- ^ type  parameters  
  , al_syvars :: ![s]           -- ^ value parameters 
  , al_body   :: !t             -- ^ alias body
  } deriving (Show, Functor, Data, Typeable)

type TAlias t    = Alias TVar F.Symbol t
type PAlias      = Alias ()   F.Symbol F.Pred 
type TAliasEnv t = Env (TAlias t)
type PAliasEnv   = Env PAlias

instance IsLocated (Alias a s t) where
  srcPos = srcPos . al_name

instance (PP a, PP s, PP t) => PP (Alias a s t) where
  pp (Alias n αs πs body) = text "alias" <+> pp n <+> text "=" <+> pp body 
