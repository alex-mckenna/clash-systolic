{-# LANGUAGE MagicHash #-}

-- |
-- Module       : Clash.Systolic.Cell
-- Description  : Systolic Cell API
-- Copyright    : (C) 2021 Alex McKenna
-- License      : BSD-3-Clause
-- Maintainer   : alexmckenna@qaylogic.com
-- Stability    : Experimental
--
module Clash.Systolic.Cell
  ( -- * Systolic Cells
    -- $preamble
    SystolicCell(..)
  , systolicCellB
  ) where

import Clash.Prelude
import Clash.Signal.Internal (joinSignal#)

-- $preamble
--
-- A systolic cell is the basic processing element of a systolic network,
-- and provides a basic behaviour that can, when multiple cells are connected
-- together, provide a complex functionality. Cells are defined by
--
--   1. Defining a type which allows the inputs and outputs of the cell to be
--   determined, and provides any static configuration for the cell. This
--   can be as simple as
--
--       @
--       data MacCell = MacCell
--       @
--
--   if the inputs and outputs are monomorphic, and there is no
--   configuration. More generally, this type may look something like
--
--       @
--       data GeneralCell a
--         = ModeA
--         | ModeB { configB :: a }
--         | ...
--       @
--
--   2. Defining an instance of this class for the cell. Instances specify the
--      input and output for the cell, and define the behaviour of the cell
--      from a given configuration.
--
--        @
--        instance SystolicCell MacCell where
--          type CellInput  MacCell = (Int, Int, Int)
--          type CellOutput MacCell = Int
--
--          systolicCell MacCell input =
--            fmap (\(acc, x, y) -> acc + x * y) input
--        @
--

class SystolicCell cell where
  type CellInput cell
  type CellOutput cell

  -- TODO Document this function.
  systolicCell
    :: forall dom
     . cell
    -- ^ Static configuration for the cell
    -> Signal dom (CellInput cell)
    -- ^ Bundled input to the cell
    -> Signal dom (CellOutput cell)
    -- ^ Bundled output from the cell

-- | A function that produces a systolic cell configuration can itself be used
-- as a systolic cell. The input of this cell must contain both the input for
-- the function that generates the cell and the input for the cell itself.
--
-- __WARNING:__ As the semantics of this instance correspond to dynamically
-- creating a cell then passing an input signal to it, this is a somewhat
-- dubious instance. /It is not recommended for use in production designs./
--
instance (SystolicCell cell) => SystolicCell (a -> cell) where
  type CellInput  (a -> cell) = (a, CellInput cell)
  type CellOutput (a -> cell) = CellOutput cell

  systolicCell genCell genInput =
    let cell  = fmap (genCell . fst) genInput
        input = fmap snd genInput
     in joinSignal# (liftA2 systolicCell cell (pure input))

-- | A variant of 'systolicCell' which automatically bundles the input and
-- unbundles the output using the 'Clash.Signal.Bundle.Bundle' class.
--
{-# INLINE systolicCellB #-}
systolicCellB
  :: forall cell dom
   . ( SystolicCell cell
     , Bundle (CellInput cell)
     , Bundle (CellOutput cell)
     )
  => cell
  -- ^ Static configuration for the cell
  -> Unbundled dom (CellInput cell)
  -- ^ Unbundled input to the cell
  -> Unbundled dom (CellOutput cell)
  -- ^ Unbundled output from the cell
systolicCellB cell =
  unbundle . systolicCell cell . bundle

