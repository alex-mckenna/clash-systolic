{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Clash.Systolic.Network.Tree
  ( Tree(..)
  , BinaryTree
  ) where

import Clash.Prelude

import Clash.Systolic.Cell
import Clash.Systolic.Network

data Tree k h cell where
  Leaf
    :: ( SystolicCell cell
       , CellInput cell  ~ a
       , CellOutput cell ~ Vec k a
       , KnownNat k, 2 <= k
       , NFDataX a
       )
    => cell
    -> Tree k 0 cell

  Node
    :: ( SystolicCell cell
       , CellInput cell  ~ a
       , CellOutput cell ~ Vec k a
       , KnownNat k, 2 <= k
       , KnownNat h
       , NFDataX a
       )
    => cell
    -> Vec k (Tree k h cell)
    -> Tree k (h + 1) cell

type BinaryTree = Tree 2

instance SystolicNetwork (Tree k h cell) where
  type NetworkInput  (Tree k h cell) = CellInput cell
  type NetworkOutput (Tree k h cell) = Vec (k ^ (h + 1)) (CellInput cell)

  systolicNetwork config input =
    case config of
      Leaf x ->
        register undefined (systolicCell x input)

      Node x children ->
        let root   = register undefined (systolicCell x input)
            leaves = liftA2 systolicNetwork children (unbundle root)
         in fmap concat (bundle leaves)

