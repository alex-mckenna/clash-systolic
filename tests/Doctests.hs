module Main (main) where

import Prelude

import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)

main :: IO ()
main =
  getArgs >>= mainFromCabal "clash-systolic"

