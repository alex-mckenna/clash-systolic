module Main (main) where

import Prelude

import Build_doctests     (flags, pkgs, module_sources)
import System.Environment (lookupEnv)
import System.Process     (readProcess)
import Test.DocTest       (doctest)


getGlobalPackageDb :: IO String
getGlobalPackageDb = readProcess "ghc" ["--print-global-package-db"] ""

main :: IO ()
main = do
  inNixShell <- lookupEnv "IN_NIX_SHELL"
  extraFlags <-
    case inNixShell of
      Nothing -> pure []
      Just _ -> pure . ("-package-db=" <>) <$> getGlobalPackageDb

  let args = flags <> extraFlags <> pkgs <> module_sources
  doctest args

