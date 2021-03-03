-- |
-- Module       : Clash.Systolic.Network
-- Description  : Systolic network API
-- Copyright    : (C) 2021 Alex McKenna
-- License      : BSD-3-Clause
-- Maintainer   : alex@qbaylogic.com
-- Stability    : Experimental
--
module Clash.Systolic.Network
  ( -- * Systolic Networks
    -- $preamble
    SystolicNetwork(..)
  , systolicNetworkB
  ) where

import Clash.Prelude

-- $preamble
--
-- A systolic network contains interconnected cells, which consume input data
-- and produce output data every clock cycle.

class SystolicNetwork network where
  type NetworkInput network
  type NetworkOutput network

  systolicNetwork
    :: forall dom
     . (HiddenClockResetEnable dom)
    => network
    -- ^ Static configuration for the network
    -> Signal dom (NetworkInput network)
    -- ^ Bundled input to the network
    -> Signal dom (NetworkOutput network)
    -- ^ Unbundled output from the network

-- | A variant of 'systolicNetwork' which automatically bundles the input and
-- unbundles the output using the 'Clash.Signal.Bundle.Bundle' class.
--
{-# INLINE systolicNetworkB #-}
systolicNetworkB
  :: forall network dom
   . ( SystolicNetwork network
     , Bundle (NetworkInput network)
     , Bundle (NetworkOutput network)
     , HiddenClockResetEnable dom
     )
  => network
  -- ^ Static configuration for the network
  -> Unbundled dom (NetworkInput network)
  -- ^ Unbundled input to the network
  -> Unbundled dom (NetworkOutput network)
  -- ^ Unbundled output from the network
systolicNetworkB network =
  unbundle . systolicNetwork network . bundle

