module Main (main) where

import Prelude

import Test.Tasty


main :: IO ()
main =
  defaultMain $
    testGroup "." []

