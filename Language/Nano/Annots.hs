-- | Code for Saving and Rendering Annotations

module Language.Nano.Annots (
   
    -- * Type for Annotation Map
    AnnotMap
    
    -- * Adding new Annotations
  , addAnnot

    -- * Rendering Annotations
  , annotate 
  ) where


import qualified Data.HashMap.Strict as M

data AnnInfo a = A (M.HashMap SourcePos a)

instance Functor AnnInfo where 
  fmap f (A m) = A (fmap f m)

------------------------------------------------------------------------------
-- | Adding New Annotations --------------------------------------------------
------------------------------------------------------------------------------

addAnnot     :: SourcePos -> a -> AnnInfo a -> AnnInfo a
addAnnot l x (A m) = A (M.add l x m)

------------------------------------------------------------------------------
-- | Dumping Annotations To Disk ---------------------------------------------
------------------------------------------------------------------------------

-- annotate :: ( ) => FilePath -> FixResult SourcePos -> AnnInfo a -> IO ()
annotate src res a = B.writeFile jsonFile annJson 
  where 
    jsonFile       = extFileName Json src
    annJson        = encode $ mkAnnMap res a


mkAnnMap :: FixResult SourcePos -> AnnInfo a -> AnnMap 


data AnnMap  = Ann { 
    types  :: M.HashMap SourcePos (String, String)  -- ^ SourcePos -> (Var, Type)
  , errors :: [(SourcePos, SourcePos)]              -- ^ List of error intervals
  } 
