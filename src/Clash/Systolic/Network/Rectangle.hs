{-# LANGUAGE RecordWildCards #-}

module Clash.Systolic.Network.Rectangle
  ( Rect(..)
  , Delay(..)
  ) where

import Clash.Prelude
import Data.Bifunctor
import Data.Kind

import Clash.Systolic.Cell
import Clash.Systolic.Network

-- | For some use cases, it can be desirable to successively delay each input
-- or output along one or both dimensions of the network. This helps to ensure
--
--   * that cells receive the correct input each cycle (when delaying input)
--   * that the network output is dense (when delaying output)
--
data Delay
  = NoDelay
  | DelayHorizontal
  | DelayVertical
  | DelayBoth

data Rect cell (n :: Nat) (m :: Nat) :: Type -> Type -> Type where
  Rect
    :: ( SystolicCell cell
       , CellInput cell  ~ (a, b)
       , CellOutput cell ~ (a, b)
       , KnownNat n, 1 <= n
       , KnownNat m, 1 <= m
       , NFDataX a
       , NFDataX b
       )
    => { genCell  :: Index n -> Index m -> cell
         -- ^ Function to generate configuration for cells from their index
       , delayIn  :: Delay
         -- ^ Delay to apply to network input
       , delayOut :: Delay
         -- ^ Delay to apply to network output
       }
    -> Rect cell n m a b

{-# INLINE lastRow #-}
lastRow :: forall n m a. (1 <= n) => Vec n (Vec m a) -> Vec m a
lastRow = leToPlus @1 @n last

{-# INLINE lastColumn #-}
lastColumn :: forall n m a. (1 <= m) => Vec n (Vec m a) -> Vec n a
lastColumn = fmap (leToPlus @1 @m last)

{-# INLINE bundleRect #-}
bundleRect
  :: forall dom n m a b
   . (KnownNat n, KnownNat m)
  => (Vec n (Signal dom a), Vec m (Signal dom b))
  -> Signal dom (Vec n a, Vec m b)
bundleRect = bundle . bimap bundle bundle

{-# INLINE unbundleRect #-}
unbundleRect
  :: forall dom n m a b
   . (KnownNat n, KnownNat m)
  => Signal dom (Vec n a, Vec m b)
  -> (Vec n (Signal dom a), Vec m (Signal dom b))
unbundleRect = bimap unbundle unbundle . unbundle

delayInputs
  :: forall dom n m a b
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , KnownNat m
     , NFDataX a
     , NFDataX b
     )
  => Delay
  -> (Vec n (Signal dom a), Vec m (Signal dom b))
  -> (Vec n (Signal dom a), Vec m (Signal dom b))
delayInputs = \case
  NoDelay         -> id
  DelayHorizontal -> first  (smap delayBy)
  DelayVertical   -> second (smap delayBy)
  DelayBoth       -> bimap  (smap delayBy) (smap delayBy)
 where
  delayBy i signal =
    foldl (\s _ -> register undefined s) signal (replicate i ())

delayOutputs
  :: forall dom n m a b
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , KnownNat m
     , NFDataX a
     , NFDataX b
     )
  => Delay
  -> (Vec n (Signal dom a), Vec m (Signal dom b))
  -> (Vec n (Signal dom a), Vec m (Signal dom b))
delayOutputs d =
  -- A bit of a hack, but delaying each index by n-i / m-i is fiddly
  bimap reverse reverse . delayInputs d . bimap reverse reverse

instance SystolicNetwork (Rect cell n m a b) where
  type NetworkInput  (Rect cell n m a b) = (Vec n a, Vec m b)
  type NetworkOutput (Rect cell n m a b) = (Vec n a, Vec m b)

  systolicNetwork Rect{..} inputs =
    bundleRect $ delayOutputs delayOut
      (fmap fst <$> lastColumn cells, fmap snd <$> lastRow cells)
   where
    (inH, inV)  = delayInputs delayIn (unbundleRect inputs)
    cells       = imap (map . build) (repeat @n (indicesI @m))

    build n m   =
      let
          inA | m == 0    = inH !! n
              | otherwise = fmap fst (cells !! n !! pred m)

          inB | n == 0    = inV !! m
              | otherwise = fmap snd (cells !! pred n !! m)

       in
          register undefined $ systolicCell (genCell n m) (bundle (inA, inB))

