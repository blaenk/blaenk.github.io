import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription as PD

import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.List (find, isPrefixOf)
import System.Directory (copyFile)
import System.FilePath ((</>))

main :: IO ()
main = do
  defaultMainWithHooks $ simpleUserHooks { postBuild = copyBinary }

copyBinary :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyBinary args buildFlags pkgDesc buildInfo = do
  let exe = find ("site" `isPrefixOf`) $ map exeName (executables pkgDesc)

  when (isJust exe) $ do
    let binary = fromJust exe
        dir = buildDir buildInfo

    putStrLn $ "Copying executable '" ++ binary ++ "' to current directory..."
    copyFile (dir </> "site" </> binary) binary
