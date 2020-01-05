#!/usr/bin/env stack
-- stack --resolver lts-14.19 script
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}

import Control.Concurrent.STM (STM)
import Control.Lens ((&), (.~))
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Foldable (for_)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath ((</>))
import System.Process.Typed (ProcessConfig)
import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson.Lens as Yaml
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.List as List
import qualified Data.Maybe as List (mapMaybe)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Yaml as Yaml
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.Directory as FilePath
import qualified System.Process.Typed as Process
import qualified TH.RelativePaths as TH


main
  :: IO ()
main = do
  -- prevent "Progress 1/2: typelevel-rewrite-rulesmalformed-argument test passed" issue
  putStrLn "\n"

  -- "/.../stack.yaml"
  let stackYamlPath :: FilePath
      stackYamlPath = $(TH.lift =<< TH.pathRelativeToCabalPackage "stack.yaml")

  -- {
  --   resolver: ...,
  --   packages: ["."],
  --   extra-deps: [...],
  -- }
  stackYaml <- either (error . Yaml.prettyPrintParseException) id
           <$> Yaml.decodeFileEither stackYamlPath

  -- "/.../test/error-messages-cases"
  let casesDir :: FilePath
      casesDir = $(TH.lift =<< TH.pathRelativeToCabalPackage "test/error-messages-cases")

  -- ["malformed-argument", ...]
  caseSubdirs <- FilePath.listDirectory casesDir

  for_ caseSubdirs $ \caseSubdir -> do
    -- "malformed-argument"
    let caseName :: Text
        caseName = Text.pack caseSubdir

    -- "/.../test/error-messages-cases/malformed-argument"
    let workingDir :: FilePath
        workingDir = casesDir </> caseSubdir

    -- "/.../test/error-messages-cases/malformed-argument/stack.yaml"
    let stackYamlPath' :: FilePath
        stackYamlPath' = workingDir </> "stack.yaml"

    -- {
    --   resolver: ...,
    --   packages: ["../../..", "."],
    --   extra-deps: [...],
    -- }
    let stackYaml' :: Yaml.Value
        stackYaml' = stackYaml & Yaml.key "packages" .~ Yaml.toJSON ["../../..", "." :: String]
    Yaml.encodeFile stackYamlPath' stackYaml'

    -- stack --stack-yaml "/.../test/error-messages-cases/malformed-argument/stack.yaml" build
    let processConfig :: ProcessConfig () () ()
        processConfig = Process.proc "stack" ["--stack-yaml", stackYamlPath', "build"]
                      & Process.setWorkingDir workingDir

    expectedOutput <- Text.readFile (workingDir </> "expected")
    (exitCode, actualOutputBS) <- Process.readProcessStderr processConfig
    case exitCode of
      ExitSuccess -> do
        error $ Text.unpack
              $ "expected an error message, but " <> caseName <> " unexpectedly succeeded!"
      ExitFailure _ -> do
        let actualOutput :: Text
            actualOutput = Text.decodeUtf8 actualOutputBS

            -- "malformed-argument    > build (lib)" -> Just "build (lib)"
            stripCasePrefix :: Text -> Maybe Text
            stripCasePrefix t = do
              t' <- Text.stripPrefix caseName t
              let t'' = Text.dropWhile (== ' ') t'
              Text.stripPrefix "> " t''

            -- "Progress\b\b\b\b\b\b\b\bfoo" -> "foo"
            cleanLine :: Text -> Text
            cleanLine t = if Text.any (== '\b') t
                          then cleanLine
                             $ Text.dropWhile (== '\b')
                             $ Text.dropWhile (/= '\b')
                             $ t
                          else t

            actualLines :: [Text]
            actualLines = List.mapMaybe stripCasePrefix
                        . fmap cleanLine
                        . Text.lines
                        $ actualOutput

            expectedLines :: [Text]
            expectedLines = filter ((== Nothing) . Text.stripPrefix "#")
                          . Text.lines
                          $ expectedOutput

        if expectedLines `List.isInfixOf` actualLines
        then do
          -- malformed-argument test passed
          Text.putStrLn $ caseName <> " test passed"
        else do
          error $ Text.unpack
                $ caseName <> " test failed\n"
               <> "expected:\n"
               <> Text.unlines (map ("  " <>) expectedLines)
               <> "got:\n"
               <> Text.unlines (map ("  " <>) $ if List.null actualLines
                                                then Text.lines actualOutput
                                                else actualLines)
