{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}


module Language.Nano.Parser.Parser
    (
      parseNanoFromFiles
    , parseScriptFromJSON
    , parseIdFromJSON
    , RawSpec(..)
    ) where

import           Control.Applicative               ((*>), (<$>), (<*), (<*>))
import           Control.Exception                 (throw)
import           Control.Monad
import           Control.Monad.Trans               (MonadIO, liftIO)
import           Data.Aeson                        (eitherDecode)
import           Data.Aeson.Types                  hiding (Error, Parser, parse)
import qualified Data.Aeson.Types                  as AI
import qualified Data.ByteString.Lazy.Char8        as B
import           Data.Char                         (isLower)
import           Data.Default
import           Data.Either                       (partitionEithers)
import qualified Data.Foldable                     as FO
import           Data.Generics                     hiding (Generic)
import           Data.Graph.Inductive.Graph
import qualified Data.HashMap.Strict               as HM
import qualified Data.HashSet                      as HS
import           Data.Interned.Internal.Text
import qualified Data.IntMap.Strict                as I
import qualified Data.List                         as L
import           Data.Maybe                        (catMaybes, fromMaybe, listToMaybe, maybeToList)
import           Data.Monoid                       (mappend, mconcat, mempty)
import           Data.Text                         (Text)
import qualified Data.Text                         as DT
import           Data.Traversable                  (mapAccumL)
import           Data.Tuple
import           Data.Vector                       ((!))
import           GHC.Generics
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Names
import           Language.Fixpoint.Parse
import qualified Language.Fixpoint.Types           as F
import qualified Language.Fixpoint.Visitor         as FV
import           Language.Nano.Annots              hiding (err)
import           Language.Nano.AST
import qualified Language.Nano.ClassHierarchy      as CHA
import           Language.Nano.Core.Env
import           Language.Nano.Errors
import           Language.Nano.Liquid.Alias
import           Language.Nano.Liquid.Qualifiers
import           Language.Nano.Liquid.Types
import           Language.Nano.Locations           hiding (val)
import           Language.Nano.Misc                (fst4, (<###>), (<##>))
import           Language.Nano.Names
import           Language.Nano.Parser.Annotations
import           Language.Nano.Parser.Common
import           Language.Nano.Parser.Declarations
import           Language.Nano.Parser.Types
import           Language.Nano.Pretty
import           Language.Nano.Program
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types               hiding (Exported)
import           Language.Nano.Visitor
import           Prelude                           hiding (mapM)
import           Text.Parsec                       hiding (State, parse)
import           Text.Parsec.Error                 (errorMessages, showErrorMessages)
import           Text.Parsec.Language              (emptyDef)
import           Text.Parsec.Pos                   (SourcePos, newPos)
import           Text.Parsec.Token                 (identLetter, identStart)
import qualified Text.Parsec.Token                 as T
import           Text.PrettyPrint.HughesPJ         (text, ($$), (<+>))
import qualified Text.PrettyPrint.HughesPJ         as P

-- import           Debug.Trace                             ( trace, traceShow)



--------------------------------------------------------------------------------------
-- | Parse File and Type Signatures
--------------------------------------------------------------------------------------

-- Parse the contents of a FilePath list into a program structure with relative
-- qualified names.
--------------------------------------------------------------------------------------
parseNanoFromFiles :: [FilePath] -> IO (Either (F.FixResult Error) (NanoBareR F.Reft))
--------------------------------------------------------------------------------------
parseNanoFromFiles fs =
  partitionEithers <$> mapM parseScriptFromJSON fs >>= \case
    ([],ps) -> return $ either Left mkNano $ parseAnnots $ concat ps
    (es,_ ) -> return $ Left $ mconcat es

--------------------------------------------------------------------------------------
getJSON :: MonadIO m => FilePath -> m B.ByteString
--------------------------------------------------------------------------------------
getJSON = liftIO . B.readFile

--------------------------------------------------------------------------------------
parseScriptFromJSON :: FilePath -> IO (Either (F.FixResult a) [Statement (SrcSpan, [RawSpec])])
--------------------------------------------------------------------------------------
parseScriptFromJSON filename = decodeOrDie <$> getJSON filename
  where
    decodeOrDie s =
      case eitherDecode s :: Either String [Statement (SrcSpan, [RawSpec])] of
        Left msg -> Left  $ F.Crash [] $ "JSON decode error:\n" ++ msg
        Right p  -> Right $ p


--------------------------------------------------------------------------------------
parseIdFromJSON :: FilePath -> IO (Either (F.FixResult a) [Id (SrcSpan, [RawSpec])])
--------------------------------------------------------------------------------------
parseIdFromJSON filename = decodeOrDie <$> getJSON filename
  where
    decodeOrDie s =
      case eitherDecode s :: Either String [Id (SrcSpan, [RawSpec])] of
        Left msg -> Left  $ F.Crash [] $ "JSON decode error:\n" ++ msg
        Right p  -> Right $ p


---------------------------------------------------------------------------------
mkNano :: [Statement (SrcSpan, [Spec])] -> Either (F.FixResult Error) (NanoBareR F.Reft)
---------------------------------------------------------------------------------
mkNano ss = undefined
--             return   (mkNano' ss)
--         >>= return . convertTVars
--         >>= return . expandAliases
--         >>= return . replaceAbsolute
--         >>= return . replaceDotRef
--         >>= return . scrapeQuals
--         >>=          scrapeModules
--         >>= return . fixEnums
--         >>= return . fixFunBinders
--         >>= return . buildCHA
--
---------------------------------------------------------------------------------
mkNano' :: [Statement (SrcSpan, [Spec])] -> NanoBareRelR F.Reft
---------------------------------------------------------------------------------
mkNano' ss = Nano {
        code          = Src (checkTopStmt <$> ss')
      , consts        = envFromList [ mapSnd (ntrans f g) t | Meas t <- anns ]
      , tAlias        = envFromList [ t | TAlias t <- anns ]
      , pAlias        = envFromList [ t | PAlias t <- anns ]
      , pQuals        =             [ t | Qual   t <- anns ]
      , pOptions      =             [ t | Option t <- anns ]
      , invts         = [Loc (srcPos l) (ntrans f g t) | Invt l t <- anns ]
      , maxId         = endingId
    }
  where
    toBare           :: Int -> (SrcSpan, [Spec]) -> AnnRel F.Reft
    toBare n (l,αs)   = Ann n l $ catMaybes $ extractFact <$> αs
    f (QN p s)        = QN (g p) s
    g (QP RK_ l ss)   = QP AK_ l ss
    starting_id       = 0
    (endingId, ss')   = mapAccumL (mapAccumL (\n -> (n+1,) . toBare n)) starting_id ss
    anns              = concatMap (FO.foldMap snd) ss

---------------------------------------------------------------------------------
extractFact :: PSpec t r -> Maybe (FactQ RK r)
---------------------------------------------------------------------------------
extractFact = go
  where
    go (Bind    (_,a,t))  = Just $ VarAnn a t
    go (AmbBind (_,t)  )  = Just $ AmbVarAnn t

    go (Constr  t)        = Just $ ConsAnn t
    go (Field (s, f))     = Just $ FieldAnn s f
    go (Method  (s, m))   = Just $ MethAnn s m

    go (Class t)          = Just $ ClassAnn t
    go (Iface t)          = Just $ InterfaceAnn t
    go (CastSp _ t)       = Just $ UserCast t
    go (Exported  _)      = Just $ ExportedElt
    go (RdOnly _)         = Just $ ReadOnlyVar
    go (AnFunc t)         = Just $ FuncAnn t
    go _                  = Nothing

---------------------------------------------------------------------------------
scrapeQuals     :: NanoBareR F.Reft -> NanoBareR F.Reft
---------------------------------------------------------------------------------
scrapeQuals p = p { pQuals = qs ++ pQuals p}
  where
    qs        = qualifiers $ mkUq $ foldNano tbv [] [] p
    tbv       = defaultVisitor { accStmt = stmtTypeBindings
                               , accCElt = celtTypeBindings }

mkUq                  = zipWith tx ([0..] :: [Int])
  where
    tx i (Id l s, t)  = (Id l $ s ++ "_" ++ show i, t)


stmtTypeBindings _                = go
  where
    go (FunctionStmt l f _ _)     = [(f, t) | FuncAnn t <- ann_fact l ] ++
                                    [(f, t) | VarAnn  _ (Just t) <- ann_fact l ]
    go (VarDeclStmt _ vds)        = [(x, t) | VarDecl l x _ <- vds
                                            , VarAnn  _ (Just t) <- ann_fact l ]
    go _                          = []

celtTypeBindings _                = undefined -- TODO
-- celtTypeBindings _                = (mapSnd eltType <$>) . go
--   where
--     go (Constructor l _ _)        = [(x, e) | ConsAnn  e <- ann_fact l
--                                             , let x       = Id l "ctor" ]
--     go (MemberVarDecl l _ x _)    = [(x, e) | FieldAnn e <- ann_fact l  ]
--                                  ++ [(x, e) | StatAnn  e <- ann_fact l  ]
--     go (MemberMethDef l _ x _ _)  = [(x, e) | MethAnn  e <- ann_fact l  ]
--     go _                          = []

-- debugTyBinds p@(Nano {code = Src ss}) = trace msg p
--   where
--     xts = [(x, t) | (x, (t, _)) <- visibleNames ss ]
--     msg = unlines $ "debugTyBinds:" : (ppshow <$> xts)


type PState = Integer

--------------------------------------------------------------------------------------
parseAnnots :: [Statement (SrcSpan, [RawSpec])]
             -> Either (F.FixResult Error) [Statement (SrcSpan, [Spec])]
--------------------------------------------------------------------------------------
parseAnnots ss =
  case mapAccumL (mapAccumL f) (0,[]) ss of
    ((_,[]),b) -> Right $ b
    ((_,es),_) -> Left  $ F.Unsafe es
  where
    f st (ss,sp) = mapSnd ((ss),) $ L.mapAccumL (parse ss) st sp

--------------------------------------------------------------------------------------
parse :: SrcSpan -> (PState, [Error]) -> RawSpec -> ((PState, [Error]), Spec)
--------------------------------------------------------------------------------------
parse _ (st,errs) c = failLeft $ runParser (parser c) st f (getSpecString c)
  where
    parser s = do a     <- parseAnnot s
                  state <- getState
                  it    <- getInput
                  case it of
                    ""  -> return $ (state, a)
                    _   -> unexpected $ "trailing input: " ++ it

    failLeft (Left err)      = ((st, (fromError err): errs), ErrorSpec)
    failLeft (Right (s, r))  = ((s, errs), r)

    -- Slight change from this one:
    --
    -- http://hackage.haskell.org/package/parsec-3.1.5/docs/src/Text-Parsec-Error.html#ParseError
    --
    showErr = showErrorMessages "or" "unknown parse error" "expecting"
                "unexpected" "end of input" . errorMessages
    fromError err = mkErr ss   $ showErr err
                              ++ "\n\nWhile parsing: "
                              ++ show (getSpecString c)
    ss = srcPos c
    f = sourceName $ sp_start ss


instance PP (RawSpec) where
  pp = text . getSpecString


-- | Replace `TRef x _ _` where `x` is a name for an enumeration with `number`
---------------------------------------------------------------------------------------
fixEnums :: PPR r => QEnv (ModuleDef r) -> NanoBareR r -> (QEnv (ModuleDef r), NanoBareR r)
---------------------------------------------------------------------------------------
fixEnums m p@(Nano { code = Src ss }) = (m',p')
  where
    p'    = p { code = Src $ (tr <$>) <$> ss }
    m'    = fixEnumsInModule m `qenvMap` m
    tr    = transAnnR f []
    f _ _ = fixEnumInType m

---------------------------------------------------------------------------------------
fixEnumInType :: F.Reftable r => QEnv (ModuleDef r) -> RType r -> RType r
---------------------------------------------------------------------------------------
fixEnumInType ms (TRef (Gen (QN p x) []) r)
  | Just m <- qenvFindTy p ms
  , Just e <- envFindTy x $ m_enums m
  = if isBvEnum e then tBV32 `strengthen` r
                  else tNum  `strengthen` r
fixEnumInType _ t = t

---------------------------------------------------------------------------------------
fixEnumsInModule :: F.Reftable r => QEnv (ModuleDef r) -> ModuleDef r -> ModuleDef r
---------------------------------------------------------------------------------------
fixEnumsInModule m = trans (const $ const $ fixEnumInType m) [] []


-- | Replace all relative qualified names and paths in a program with full ones.
---------------------------------------------------------------------------------------
replaceAbsolute :: PPR r => NanoBareRelR r -> NanoBareR r
---------------------------------------------------------------------------------------
replaceAbsolute = undefined
-- replaceAbsolute pgm@(Nano { code = Src ss }) = pgm { code = Src $ (tr <$>) <$> ss }
--   where
--     (ns, ps)        = extractQualifiedNames ss
--     tr l            = ntransAnnR (safeAbsName l) (safeAbsPath l) l
--     safeAbsName l a = case absAct (absoluteName ns) l a of
--                         Just a' -> a'
--                         -- If it's a type alias, don't throw error
--                         Nothing | isAlias a -> toAbsoluteName a
--                                 | otherwise -> throw $ errorUnboundName (srcPos l) a
--     safeAbsPath l a = case absAct (absolutePath ps) l a of
--                         Just a' -> a'
--                         Nothing -> throw $ errorUnboundPath (srcPos l) a
--
--     isAlias (QN RK_ _ [] s) = envMem s $ tAlias pgm
--     isAlias (QN _   _ _  _) = False
--
--     absAct f l a    = I.lookup (ann_id l) mm >>= (`f` a)
--     mm              = snd $ visitStmts vs (QP AK_ def []) ss
--     vs              = defaultVisitor { ctxStmt = cStmt }
--                                      { accStmt = acc   }
--                                      { accExpr = acc   }
--                                      { accCElt = acc   }
--                                      { accVDec = acc   }
--     cStmt (QP AK_ l p) (ModuleStmt _ x _)
--                     = QP AK_ l $ p ++ [symbol x]
--     cStmt q _       = q
--     acc c s         = I.singleton (ann_id a) c where a = getAnnotation s
--

-- | Replace `a.b.c...z` with `offset(offset(...(offset(a),"b"),"c"),...,"z")`
---------------------------------------------------------------------------------------
replaceDotRef :: NanoBareR F.Reft -> NanoBareR F.Reft
---------------------------------------------------------------------------------------
replaceDotRef p@(Nano{ code = Src fs, tAlias = ta, pAlias = pa, invts = is })
    = p { code         = Src $      tf       <##>  fs
        , tAlias       = transRType tt [] [] <###> ta
        , pAlias       =            tt [] [] <##>  pa
        , invts        = transRType tt [] [] <##>  is
        }
  where
    tf (Ann l a facts) = Ann l a $ trans tt [] [] <$> facts
    tt _ _             = fmap $ FV.trans vs () ()

    vs                 = FV.defaultVisitor { FV.txExpr = tx }
    -- tx _ (F.EVar s)      | (x:y:zs) <- DT.pack "." `DT.splitOn` DT.pack (symbolString s)
    --                    = foldl offset (F.eVar x) (y:zs)
    tx _ e             = e
    offset k v         = F.EApp offsetLocSym [F.expr k, F.expr v]


-- | Add a '#' at the end of every function binder (to avoid capture)
--
---------------------------------------------------------------------------------------
fixFunBinders :: PPR r => QEnv (ModuleDef r) -> NanoBareR r -> (QEnv (ModuleDef r), NanoBareR r)
---------------------------------------------------------------------------------------
fixFunBinders m p@(Nano { code = Src ss }) = (m', p')
  where
    p'    = p { code = Src $ (tr <$>) <$> ss }
    m'    = qenvMap fixFunBindersInModule m
    tr    = transAnnR f []
    f _ _ = fixFunBindersInType

fixFunBindersInType t | Just is <- bkFuns t = mkAnd $ map (mkFun . f) is
                      | otherwise           = t
  where
    f (vs, yts, t)    = (vs, ssb yts, ss t)
      where
        ks            = [ y | B y _ <- yts ]
        ks'           = (F.eVar . (`mappend` symbol [symSepName])) <$> ks
        su            = F.mkSubst $ zip ks ks'
        ss            = F.subst su
        ssb bs        = [ B (ss s) (ss t) | B s t <- bs ]


fixFunBindersInModule m@(ModuleDef { m_variables = mv, m_types = mt })
                = m { m_variables = mv', m_types = mt' }
  where
   mv'          = envMap f mv
   f (VI a i t) = VI a i $ fixFunBindersInType t
   mt'          = envMap (trans g [] []) mt
   g _ _        = fixFunBindersInType


-- | `scrapeModules ss` creates a module store from the statements in @ss@
--   For every module we populate:
--
--    * m_variables with: functions, variables, class constructors, modules
--
--    * m_types with: classes and interfaces
--
---------------------------------------------------------------------------------------
-- scrapeModules :: PPR r => NanoBareR r -> Either (F.FixResult Error) (QEnv (ModuleDef r))
---------------------------------------------------------------------------------------
scrapeModules pgm@(Nano { code = Src stmts })
  = do  mods  <- return $ accumModules stmts
        mods' <- mapM mkMod mods
        return $ qenvFromList mods'
  where

    mkMod :: PPR r => (AbsPath, [Statement (AnnR r)]) -> Either (F.FixResult Error) (AbsPath, ModuleDefQ AK r)
    mkMod (p,ss)      = do  ve <- return $ varEnv p ss
                            te <- typeEnv p ss
                            ee <- return $ enumEnv ss
                            return (p, ModuleDef ve te ee p)

    -- drop1 (_,b,c,d,e) = (b,c,d,e)

    varEnv p          = mkVarEnv . vStmts p

    mkVarEnv          = undefined -- TODO

    typeEnv p ss      = tStmts p ss >>= return . envFromList

    enumEnv           = envFromList  . eStmts

    vStmts                         = concatMap . vStmt

    vStmt _ (VarDeclStmt _ vds)    = [(ss x, VarDeclKind     , VI a ui t ) | VarDecl l x _ <- vds
                                                                           , VarAnn a (Just t) <- ann_fact l ]
                                  ++ [(ss x, VarDeclKind     , VI wg ii t) | VarDecl l x _ <- vds
                                                                           , AmbVarAnn t <- ann_fact l ]
    -- The Assignabilities below are overwitten to default values
    vStmt _ (FunctionStmt l x _ _) = [(ss x, FuncDefKind     , VI am ii t) | VarAnn _ (Just t) <- ann_fact l ]
    vStmt _ (FuncAmbDecl l x _)    = [(ss x, FuncAmbientKind , VI am ii t) | VarAnn _ (Just t) <- ann_fact l ]
    vStmt _ (FuncOverload l x _)   = [(ss x, FuncOverloadKind, VI am ii t) | VarAnn _ (Just t) <- ann_fact l ]

    vStmt p (ClassStmt l x _ _ _)  = [(ss x, ClassDefKind    , VI am ii $ TType ClassK b)
                                                                           | ClassAnn (TS _ b _) <- ann_fact l ]

    vStmt p (ModuleStmt l x _)     = [(ss x, ModuleDefKind   , VI am ii $ TMod $ pathInPath l p x)]
    vStmt p (EnumStmt l x _)       = [(ss x, EnumDefKind     , VI am ii $ TType EnumK $ BGen (QN p $ symbol x) []) ]
    vStmt _ _                      = []

    wg   = WriteGlobal
    am   = Ambient
    ui   = Uninitialized
    ii   = Initialized

    tStmts                   = concatMapM . tStmt

    tStmt ap c@ClassStmt{}   = single <$> resolveType ap c
    tStmt ap c@IfaceStmt{}   = single <$> resolveType ap c
    tStmt _ _                = return $ [ ]

    eStmts                   = concatMap eStmt

    eStmt (EnumStmt _ n es)  = [(fmap srcPos n, EnumDef (symbol n) (envFromList $ sEnumElt <$> es))]
    eStmt _                  = []
    sEnumElt (EnumElt _ s e) = (symbol s, fmap (const ()) e)
    ss                       = fmap ann

---------------------------------------------------------------------------------------
resolveType :: PPR r => AbsPath
                     -> Statement (AnnR r)
                     -> Either (F.FixResult Error) (Id SrcSpan, TypeDecl r)
---------------------------------------------------------------------------------------
resolveType (QP AK_ _ ss) (ClassStmt l c _ _ cs)
  | [ts] <- cas
  = typeMembers cs >>= return . (cc,) . TD ts
  | otherwise
  = Left $ F.Unsafe [err (sourceSpanSrcSpan l) errMsg ]
  where
    cc     = fmap ann c
    cas    = [ ts | ClassAnn ts <- ann_fact l ]
    errMsg = "Invalid class annotation: " ++ show (intersperse P.comma (map pp cas))

resolveType _ (IfaceStmt l c)
  | [t] <- ifaceAnns  = Right (fmap ann c,t)
  | otherwise         = Left $ F.Unsafe [err (sourceSpanSrcSpan l) errMsg ]
  where
    ifaceAnns         = [ t | InterfaceAnn t <- ann_fact l ]
    errMsg            = "Invalid interface annotation: "
                     ++ show (intersperse P.comma (map pp ifaceAnns))

resolveType _ s       = Left $ F.Unsafe $ single
                      $ err (sourceSpanSrcSpan $ getAnnotation s)
                      $ "Statement\n" ++ ppshow s ++ "\ncannot have a type annotation."

