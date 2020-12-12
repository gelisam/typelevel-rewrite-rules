#!/usr/bin/env stack
-- stack --resolver lts-16.25 script
{-# LANGUAGE OverloadedStrings, PackageImports, ScopedTypeVariables, TemplateHaskell #-}

import Control.Concurrent.STM (STM)
import Control.Lens ((&), (.~))
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Foldable (for_)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath ((</>))
import System.Process.Typed (ProcessConfig)
import qualified Control.Concurrent.STM as STM
import qualified "lens-aeson" Data.Aeson.Lens as Yaml
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

main :: IO ()
main = putStrLn "typechecks."
