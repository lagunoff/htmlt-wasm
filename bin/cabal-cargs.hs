#!/usr/bin/env runghc

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

-- TODO: Make sure existence of Wno-ambiguous-fields always correlates
-- with following condition
#if MIN_VERSION_base(4,16,0)
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
#endif

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Data.Maybe
import Data.Typeable
import Debug.Trace
import Distribution.Compiler (CompilerFlavor(..), buildCompilerFlavor)
import Distribution.Package (Dependency, PackageName)
import Distribution.Package (PackageIdentifier(..) , Dependency(..))
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec.Warning (PWarning)
import Distribution.System (OS(..), Arch(..))
import Distribution.Types.Flag (PackageFlag(..), FlagName)
import Distribution.Types.UnqualComponentName
import Distribution.Version (Version, withinRange)
import GHC.Generics (Generic)
import System.Directory
import System.Environment
import System.Exit (exitFailure, exitSuccess)
import System.FilePath.Posix
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map as HM
import qualified Distribution.Compiler as C
import qualified Distribution.PackageDescription as PD
import qualified Distribution.System as S
import qualified Language.Haskell.Extension as EXT

#if MIN_VERSION_Cabal(3,6,0)
import Distribution.Utils.Path
#endif


buildInfoSourceDirs :: PD.BuildInfo -> [FilePath]
#if MIN_VERSION_Cabal(3,6,0)
buildInfoSourceDirs = fmap getSymbolicPath . PD.hsSourceDirs
#else
buildInfoSourceDirs = PD.hsSourceDirs
#endif

main = error $ unlines
  [ "Usage: ghc ./cabal-cargs.hs -e \"printGhcArgs "
  , "(FromCabalFile \"./mylibrary.cabal\" Nothing)"
  ]

data GhcArgsOptions
  = FromCabalFile
    { fcbf_cabal_file :: FilePath
    , fcbf_sections :: Maybe [Section]
    }
  deriving (Show)

printGhcArgs :: GhcArgsOptions -> IO ()
printGhcArgs = \case
  FromCabalFile {fcbf_cabal_file, fcbf_sections} -> do
    espec <- runExceptT $ fromCabalFile fcbf_cabal_file
    spec <- case espec of
      Right spec -> return $ maybe spec (\s -> spec {sections = s}) fcbf_sections
      Left e -> fail e
    putStr . L.intercalate " " . (defaultFlags<>) . format . fromSpec $ spec
  where
    defaultFlags = ["-hide-all-packages"]

data Spec = Spec
   { sections      :: [Section]              -- ^ the sections used for collecting the compiler args
   , fields        :: [Field]                  -- ^ for these fields compiler args are collected
   , condVars      :: CondVars               -- ^ used for the evaluation of the conditional fields in the cabal file
   , pkgDescrp     :: GenericPackageDescription -- ^ the package description of the read in cabal file
   , cabalFile     :: FilePath                  -- ^ the cabal file read from
   , distDir       :: Maybe FilePath            -- ^ the dist directory of the cabal build
   , packageDB     :: Maybe FilePath            -- ^ the directory of package database of the cabal sandbox
   , relativePaths :: Bool                      -- ^ if all returned paths are relative to the directory of the cabal file, otherwise all paths are absolute
   } deriving (Show)

fromCabalFile :: FilePath -> ExceptT String IO Spec
fromCabalFile file = do
   pkgDescrp <- packageDescription file
   absFile   <- liftIO (canonicalizePath file)
   return $ Spec
      { sections      = allSections pkgDescrp
      , fields        = allFields
      , condVars      = fromDefaults pkgDescrp
      , pkgDescrp     = pkgDescrp
      , cabalFile     = absFile
      , distDir       = Nothing
      , packageDB     = Nothing
      , relativePaths = False
      }

data CondVars = CondVars
   { flags           :: FlagMap          -- ^ the enable state of the flags, initialized with the default flag values in the cabal file
   , os              :: OS               -- ^ the used OS, by default the one cabal was build on
   , arch            :: Arch             -- ^ the used ARCH, by default the one cabal was build on
   , compilerFlavor  :: CompilerFlavor   -- ^ the used CompilerFlavor, by default the one cabal was build on
   , compilerVersion :: Maybe Version    -- ^ the user specified compiler version
   } deriving (Show)

fromDefaults :: GenericPackageDescription -> CondVars
fromDefaults pkgDescrp = CondVars
  { flags           = flags
  , os              = S.buildOS
  , arch            = S.buildArch
  , compilerFlavor  = buildCompilerFlavor
  , compilerVersion = Nothing
  }
   where
      flags = HM.fromList $ map nameWithDflt (PD.genPackageFlags pkgDescrp)

      nameWithDflt MkPackageFlag { PD.flagName = name, PD.flagDefault = dflt } =
         (PD.unFlagName name, dflt)

type FlagMap  = HM.Map String Bool

-- | A section of the cabal file.
data Section
  = Library
  | Executable String
  | TestSuite String
  | Benchmark String
  deriving (Show, Eq)

allSections :: GenericPackageDescription -> [Section]
allSections pkgDescr = concat
  [ maybe [] (const [Library]) (PD.condLibrary pkgDescr)
  , map (Executable . unUnqualComponentName . fst) (PD.condExecutables pkgDescr)
  , map (TestSuite . unUnqualComponentName . fst) (PD.condTestSuites pkgDescr)
  , map (Benchmark . unUnqualComponentName . fst) (PD.condBenchmarks pkgDescr)
  ]

data Field = Hs_Source_Dirs
           | Ghc_Options
           | Default_Extensions
           | Default_Language

           | Cpp_Options
           | C_Sources
           | Cc_Options

           | Extra_Lib_Dirs
           | Extra_Libraries
           | Ld_Options

           | Include_Dirs
           | Includes

           | Build_Depends

           | Package_Db             -- ^ the package database of a cabal sandbox
           | Root_Dir               -- ^ the root directory of the cabal package
           | Autogen_Hs_Source_Dirs -- ^ dirs of automatically generated haskell source files by cabal (e.g. Paths_*)
           | Autogen_Include_Dirs   -- ^ dirs of automatically generated include files by cabal
           | Autogen_Includes       -- ^ automatically generated include files by cabal (e.g. cabal_macros.h)

           | Hdevtools_Socket       -- ^ the socket file for hdevtools
           deriving (Typeable, Show, Eq, Enum, Bounded)

-- | Get all known fields.
allFields :: [Field]
allFields = [ minBound .. maxBound ]

packageDescription :: FilePath -> ExceptT String IO GenericPackageDescription
packageDescription file = do
  contents <- liftIO $ BS.readFile file
  let (warnings, result) = runParseResult $ parseGenericPackageDescription contents
  liftIO $ showWarnings warnings
  case result of
    Left (_, errors) -> throwE $ show errors
    Right pkgDescrp  -> return pkgDescrp
  where
    showWarnings :: [PWarning] -> IO ()
    showWarnings [] = return ()
    showWarnings ws = putStrLn $ "cabal-cargs: " ++ (L.intercalate ", " $ map show ws)

data CompilerArgs = CompilerArgs
   { hsSourceDirs        :: [FilePath]
   , ghcOptions          :: [String]
   , defaultExtensions   :: [String]
   , defaultLanguage     :: [String]
   , cppOptions          :: [String]
   , cSources            :: [FilePath]
   , ccOptions           :: [String]
   , extraLibDirs        :: [FilePath]
   , extraLibraries      :: [String]
   , ldOptions           :: [String]
   , includeDirs         :: [FilePath]
   , includes            :: [String]
   , buildDepends        :: [String]
   , packageDB           :: Maybe FilePath -- ^ the path to the package database of the cabal sandbox
   , rootDir             :: Maybe FilePath -- ^ the root directory of the cabal package
   , autogenHsSourceDirs :: [FilePath]     -- ^ dirs of automatically generated haskell source files by cabal (e.g. Paths_*)
   , autogenIncludeDirs  :: [FilePath]     -- ^ dirs of automatically generated include files by cabal
   , autogenIncludes     :: [String]       -- ^ automatically generated include files by cabal (e.g. cabal_macros.h)
   , hdevtoolsSocket     :: Maybe FilePath -- ^ the path to the hdevtools socket file
   }
   deriving (Show, Eq, Generic)

defaultCompilerArgs :: CompilerArgs
defaultCompilerArgs = CompilerArgs
   { hsSourceDirs        = []
   , ghcOptions          = []
   , defaultExtensions   = []
   , defaultLanguage     = []
   , cppOptions          = []
   , cSources            = []
   , ccOptions           = []
   , extraLibDirs        = []
   , extraLibraries      = []
   , ldOptions           = []
   , includeDirs         = []
   , includes            = []
   , buildDepends        = []
   , packageDB           = Nothing
   , autogenHsSourceDirs = []
   , autogenIncludeDirs  = []
   , autogenIncludes     = []
   , hdevtoolsSocket     = Nothing
   , rootDir             = Nothing
   }

fromSpec :: Spec -> CompilerArgs
fromSpec spec =
  L.foldl' collectFromSection defaultCompilerArgs (sections spec)

  -- changePaths $ L.foldl' collectFromSection defaultCompilerArgs (sections spec)
   -- where
   --    changePaths cargs
   --       | relativePaths spec
   --       = cargs -- & hsSourceDirsL            %~ map stripCabalDir
   --               -- & cSourcesL                %~ map stripCabalDir
   --               -- & extraLibDirsL            %~ map stripCabalDir
   --               -- & includeDirsL             %~ map stripCabalDir
   --               -- & autogenHsSourceDirsL     %~ map stripCabalDir
   --               -- & autogenIncludeDirsL      %~ map stripCabalDir
   --               -- & packageDBL . _Just       %~ stripCabalDir
   --               -- & hdevtoolsSocketL . _Just %~ stripCabalDir


  where
    collectFromSection cargs section =
      L.foldl' addCarg cargs (fields spec)
      where
        addCarg :: CompilerArgs -> Field -> CompilerArgs
        addCarg cargs = \case
          Package_Db ->
            cargs {packageDB = (\Spec{packageDB} -> packageDB) spec}
          Root_Dir ->
            cargs {rootDir = Just (takeDirectory $ cabalFile spec)}
          Autogen_Hs_Source_Dirs
            | Just distDir <- distDir spec -> cargs {autogenHsSourceDirs = [distDir ++ "/build/autogen"]}
            | otherwise -> cargs
          Autogen_Include_Dirs
            | Just distDir <- distDir spec -> cargs {autogenIncludeDirs = [distDir ++ "/build/autogen"]}
            | otherwise -> cargs
          Autogen_Includes
            | Just _ <- distDir spec -> cargs {autogenIncludes = ["cabal_macros.h"]}
            | otherwise -> cargs
          Hdevtools_Socket ->
            cargs {hdevtoolsSocket = Just ".hdevtools.sock"}
          Build_Depends ->
            cargs {buildDepends = L.nub (buildDepends cargs) ++ dependencies }
          otherField ->
            overCompilerArgsField otherField (L.nub . (++ buildInfoFields)) cargs
            where
              buildInfoFields = concatMap (viewField otherField) buildInfos
          where
            dependencies = getPkgName <$> dependencyIfToList (condVars spec) section (pkgDescrp spec)
            buildInfos = collectBuildInfoIf (condVars spec) section (pkgDescrp spec)
            getPkgName (Dependency pkgName _ _) = PD.unPackageName pkgName

-- | A traversal for the 'Dependency' of 'Section' that match 'CondVars'.
dependencyIfToList :: CondVars -> Section -> GenericPackageDescription -> [Dependency]
dependencyIfToList condVars section = case section of
  Library ->
    maybe [] (traverseDependencyIf condVars) . PD.condLibrary
  Executable name ->
    maybe [] (traverseDependencyIf condVars) . findExecutableByName name . PD.condExecutables
  TestSuite name ->
    maybe [] (traverseDependencyIf condVars) . findExecutableByName name . PD.condTestSuites
  Benchmark name ->
    maybe [] (traverseDependencyIf condVars) . findExecutableByName name . PD.condBenchmarks
  where

    traverseDependencyIf :: CondVars -> CondTree' dat -> [Dependency]
    traverseDependencyIf condVars (PD.CondNode dat constr comps) =
      constr <> join (fmap traverseCompIf comps)
       where
         traverseCompIf (PD.CondBranch cond ifComp elseComp) =
           if condMatches then traverseDependencyIf condVars ifComp
             else maybe [] (traverseDependencyIf condVars) elseComp
           where
             condMatches = evalCondVar condVars cond

type CondTree' a = PD.CondTree PD.ConfVar [Dependency] a

-- | A traversal for the 'BuildInfo' of 'Section' that match 'CondVars'.
collectBuildInfoIf :: CondVars -> Section -> GenericPackageDescription -> [PD.BuildInfo]
collectBuildInfoIf condVars section = case section of
  Library ->
    fmap PD.libBuildInfo . maybe [] (traverseDataIf condVars) . PD.condLibrary
  Executable name ->
    fmap PD.buildInfo . maybe [] (traverseDataIf condVars) . findExecutableByName name . PD.condExecutables
  TestSuite name ->
    fmap PD.testBuildInfo . maybe [] (traverseDataIf condVars) . findExecutableByName name . PD.condTestSuites
  Benchmark name ->
    fmap PD.benchmarkBuildInfo . maybe [] (traverseDataIf condVars) . findExecutableByName name . PD.condBenchmarks
  where
    traverseDataIf :: CondVars -> CondTree' dat -> [dat]
    traverseDataIf condVars (PD.CondNode dat constr comps) =
      dat : join (fmap traverseCompIf comps)
      where
        traverseCompIf (PD.CondBranch cond ifComp elseComp) =
          if condMatches then traverseDataIf condVars ifComp
            else maybe [] (traverseDataIf condVars) elseComp
          where
            condMatches = evalCondVar condVars cond

findExecutableByName :: String -> [(PD.UnqualComponentName, b)] -> Maybe b
findExecutableByName name =
  fmap snd . L.find ((== name) . unUnqualComponentName . fst)

viewField :: Field -> PD.BuildInfo -> [String]
viewField = \case
  Hs_Source_Dirs -> buildInfoSourceDirs
  Ghc_Options -> getGHCOptions . PD.options
  Default_Extensions -> fmap extToString . oldAndDefaultExtensions
  Default_Language -> maybe [] (pure . langToString) . PD.defaultLanguage
  Cpp_Options -> PD.cppOptions
  C_Sources -> PD.cSources
  Cc_Options -> PD.ccOptions
  Extra_Lib_Dirs -> PD.extraLibDirs
  Extra_Libraries -> PD.extraLibs
  Ld_Options -> PD.ldOptions
  Include_Dirs -> PD.includeDirs
  Includes -> PD.includes
  _ -> const []
  where
    getGHCOptions (C.PerCompilerFlavor ghc _ghcjs) = ghc
    extToString :: EXT.Extension -> String
    extToString = \case
      EXT.EnableExtension knownExt    -> show knownExt
      EXT.DisableExtension knownExt   -> "No" ++ show knownExt
      EXT.UnknownExtension unknownExt -> unknownExt
    oldAndDefaultExtensions :: PD.BuildInfo -> [EXT.Extension]
    oldAndDefaultExtensions buildInfo
      = PD.oldExtensions buildInfo ++ PD.defaultExtensions buildInfo
    langToString :: EXT.Language -> String
    langToString (EXT.UnknownLanguage l) = l
    langToString lang = show lang

overCompilerArgsField :: Field -> ([String] -> [String]) -> CompilerArgs -> CompilerArgs
overCompilerArgsField field f = case field of
  Hs_Source_Dirs         -> \ca@CompilerArgs{hsSourceDirs} -> ca {hsSourceDirs = f hsSourceDirs}
  Ghc_Options            -> \ca@CompilerArgs{ghcOptions} -> ca {ghcOptions = f ghcOptions}
  Default_Extensions     -> \ca@CompilerArgs{defaultExtensions} -> ca {defaultExtensions = f defaultExtensions}
  Default_Language       -> \ca@CompilerArgs{defaultLanguage} -> ca {defaultLanguage = f defaultLanguage}
  Cpp_Options            -> \ca@CompilerArgs{cppOptions} -> ca {cppOptions = f cppOptions}
  C_Sources              -> \ca@CompilerArgs{cSources} -> ca {cSources = f cSources}
  Cc_Options             -> \ca@CompilerArgs{ccOptions} -> ca {ccOptions = f ccOptions}
  Extra_Lib_Dirs         -> \ca@CompilerArgs{extraLibDirs} -> ca {extraLibDirs = f extraLibDirs}
  Extra_Libraries        -> \ca@CompilerArgs{extraLibraries} -> ca {extraLibraries = f extraLibraries}
  Ld_Options             -> \ca@CompilerArgs{ldOptions} -> ca {ldOptions = f ldOptions}
  Include_Dirs           -> \ca@CompilerArgs{includeDirs} -> ca {includeDirs = f includeDirs}
  Includes               -> \ca@CompilerArgs{includes} -> ca {includes = f includes}
  Build_Depends          -> \ca@CompilerArgs{buildDepends} -> ca {buildDepends = f buildDepends}
  Package_Db             -> \ca@CompilerArgs{packageDB} -> ca {packageDB = listToMaybe $ f $ maybeToList packageDB}
  Root_Dir               -> \ca@CompilerArgs{rootDir} -> ca {rootDir = listToMaybe $ f $ maybeToList rootDir}
  Autogen_Hs_Source_Dirs -> \ca@CompilerArgs{autogenHsSourceDirs} -> ca {autogenHsSourceDirs = f autogenHsSourceDirs}
  Autogen_Include_Dirs   -> \ca@CompilerArgs{autogenIncludeDirs} -> ca {autogenIncludeDirs = f autogenIncludeDirs}
  Autogen_Includes       -> \ca@CompilerArgs{autogenIncludes} -> ca {autogenIncludes = f autogenIncludes}
  Hdevtools_Socket       -> \ca@CompilerArgs{hdevtoolsSocket} -> ca {hdevtoolsSocket = listToMaybe $ f $ maybeToList hdevtoolsSocket}

-- | Evaluate the 'Condition' using the 'CondVars'.
evalCondVar :: CondVars -> PD.Condition PD.ConfVar -> Bool
evalCondVar condVars = eval'
   where
      eval' (PD.Var var)    = hasVar var
      eval' (PD.Lit val)    = val
      eval' (PD.CNot c)     = not $ eval' c
      eval' (PD.COr c1 c2)  = eval' c1 || eval' c2
      eval' (PD.CAnd c1 c2) = eval' c1 && eval' c2

      hasVar (PD.OS osVar)     = osVar == os condVars
      hasVar (PD.Arch archVar) = archVar == arch condVars
      hasVar (PD.Impl cflavor vrange)
         | Just version <- compilerVersion condVars
         = cflavor == compilerFlavor condVars && version `withinRange` vrange

         | otherwise
         = cflavor == compilerFlavor condVars

      hasVar (PD.PackageFlag name)
         | Just v <- HM.lookup (PD.unFlagName name) (flags condVars)
         = v

         | otherwise
         = False

format :: CompilerArgs -> [String]
format cargs = concat [ formatHsSourceDirs $ hsSourceDirs cargs
                          , ghcOptions cargs
                          , map ("-X" ++) (defaultExtensions cargs)
                          , map ("-X" ++) (defaultLanguage cargs)
                          , map ("-optP" ++) (cppOptions cargs)
                          , map ("-optc" ++) (ccOptions cargs)
                          , map ("-L" ++) (extraLibDirs cargs)
                          , map ("-l" ++) (extraLibraries cargs)
                          , formatIncludeDirs $ includeDirs cargs
                          , formatIncludes $ includes cargs
                          , formatBuildDepends $ buildDepends cargs
                          , maybe []
                                  (\db -> ["-clear-package-db", "-global-package-db", "-package-db=" ++ db])
                                  ((\CompilerArgs{packageDB} -> packageDB) cargs)
                          , formatHsSourceDirs $ autogenHsSourceDirs cargs
                          , formatIncludeDirs $ autogenIncludeDirs cargs
                          , formatIncludes $ autogenIncludes cargs
                          , cSources cargs
                          ]
   where
      formatBuildDepends []   = []
      formatBuildDepends deps = map ("-package=" ++) deps

      formatHsSourceDirs = map ("-i" ++)
      formatIncludeDirs  = map ("-I" ++)

      formatIncludes incs = reverse $ L.foldl' addInclude [] incs
         where
            addInclude incs inc = ("-optP" ++ inc) : ("-optP-include") : incs
