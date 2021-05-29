{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module       : Clash.Systolic.Network.Pipeline
-- Description  : Unidirectional systolic piplines
-- Copyright    : (C) 2021 Alex McKenna
-- License      : BSD-2-Clause
-- Maintainer   : alexmckenna@qaylogic.com
-- Stability    : Experimental
--
module Clash.Systolic.Network.Pipeline
  ( Pipeline(..)
  ) where

import Clash.Prelude

import Clash.Systolic.Cell
import Clash.Systolic.Network


type family Head xs where
  Head (x ': _) = x

type family Last xs where
  Last '[x]       = x
  Last (_ ': xs)  = Last xs

infixr 8 `Then`
infix  9 `Finally`

data Pipeline :: [Type] -> Type where
  Finally
    :: forall a b
     . ( SystolicCell a
       , SystolicCell b
       , CellOutput a ~ CellInput b
       , NFDataX (CellOutput a)
       , NFDataX (CellOutput b)
       )
    => a
    -- ^ Configuration for the penultimate pipeline stage
    -> b
    -- ^ Configuration for the final pipeline stage
    -> Pipeline '[a, b]

  Then
    :: forall a b cells
     . ( SystolicCell a
       , SystolicCell b
       , CellOutput a ~ CellInput b
       , NFDataX (CellOutput a)
       )
    => a
    -- ^ Configuration for the additional pipeline stage
    -> Pipeline (b ': cells)
    -- ^ The pipeline to be extended
    -> Pipeline (a ': b ': cells)
    -- ^ The extended pipeline

instance SystolicNetwork (Pipeline cells) where
  type NetworkInput  (Pipeline cells) = CellInput (Head cells)
  type NetworkOutput (Pipeline cells) = CellOutput (Last cells)

  systolicNetwork config input =
    case config of
      Finally x y ->
        let outX = register undefined (systolicCell x input)
            outY = register undefined (systolicCell y outX)
         in outY

      Then x xs ->
        let outX  = register undefined (systolicCell x input)
            outXs = systolicNetwork xs outX
         in outXs

