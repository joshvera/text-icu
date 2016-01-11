import Data.Maybe
import qualified Distribution.PackageDescription as P
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory
import System.Process

main = defaultMainWithHooks simpleUserHooks { preConf = pre, confHook = conf }

pre :: Args -> ConfigFlags -> IO P.HookedBuildInfo
pre args configFlags = do
  info <- preConf simpleUserHooks args configFlags
  dir <- getCurrentDirectory
  (_, _, _, handle) <- createProcess . shell $ "echo Setting up... \\\n"
    ++ "&& git submodule sync\\\n"
    ++ "&& git submodule update --recursive\\\n"
    ++ "&& script/build.sh\\\n"
  _ <- waitForProcess handle
  return info

conf :: (P.GenericPackageDescription, P.HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
conf x flags = do
  localBuildInfo <- confHook simpleUserHooks x flags
  let packageDescription = localPkgDescr localBuildInfo
      library = fromJust $ P.library packageDescription
      libraryBuildInfo = P.libBuildInfo library
      relativeIncludeDirs = [ "common", "i18n" ] in do
      dir <- getCurrentDirectory
      return localBuildInfo {
        localPkgDescr = packageDescription {
          P.library = Just $ library {
            P.libBuildInfo = libraryBuildInfo {
              P.extraLibDirs = (dir ++ "/vendor/libicu/lib") : P.extraLibDirs libraryBuildInfo,
              P.includeDirs = (((dir ++ "/vendor/libicu/source/") ++) <$> relativeIncludeDirs) ++ P.includeDirs libraryBuildInfo
            }
          }
        }
      }
