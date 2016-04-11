import           Control.Monad
import           Data.List                          (find)
import           Data.Maybe
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           System.Environment                 (getEnvironment)
import           System.Exit
import           System.FilePath.Posix              ((</>))
import           System.Posix.Env                   hiding (getEnvironment)
import           System.Process

main  = defaultMainWithHooks $ simpleUserHooks {
          preBuild  = writePrelude
        }


-- * Build prelude.d.ts
--
preludeDir :: FilePath
preludeDir = "./include"

preludeFiles :: [FilePath]
preludeFiles = [
    "./rsc/prims.d.ts",
    "./rsc/mutability.d.ts",
    "./rsc/aliases.d.ts",
    "./rsc/qualifiers.d.ts",
    "./rsc/measures.d.ts",
    "./ambient/undefined.d.ts",
    "./ambient/object.d.ts",
    "./ambient/array.d.ts",
    "./ambient/list.d.ts",
    "./ambient/boolean.d.ts",
    "./ambient/function.d.ts",
    "./ambient/string.d.ts",
    "./ambient/number.d.ts",
    "./ambient/iarguments.d.ts",
    "./ambient/regexp.d.ts",
    "./ambient/error.d.ts",
    "./ambient/console.d.ts",
    "./ambient/math.d.ts"
  ]

preludeName = "prelude.d.ts"

writePrelude _ _ = do
    txts <- forM preludeFiles (readFile . (preludeDir </>))
    _    <- writeFile (preludeDir </> preludeName) (mconcat txts)
    return (Nothing, [])

