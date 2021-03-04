module Clash.Systolic.Network.Rectangle
  ( Rect(..)
  , InputDelay(..)
  ) where

import Clash.Prelude

import Clash.Systolic.Cell
import Clash.Systolic.Network
import Data.Foldable as Foldable
import Data.Kind


{-# INLINE lastLE #-}
lastLE :: forall n a. (1 <= n) => Vec n a -> a
lastLE = leToPlus @1 @n last

delayInput
  :: forall n dom a
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , NFDataX a
     )
  => Index n
  -> Signal dom a
  -> Signal dom a
delayInput n signal =
  Foldable.foldr (const $ register undefined) signal [1..n]

-- | For some use cases, it can be desirable to successively delay each input
-- along one or both dimensions of the network, in order to ensure that each
-- cell gets the corrent horizontal and vertical data at the same time.
--
data InputDelay
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
    => (Index n -> Index m -> cell)
    -- ^ Function to generate configuration for cells from their index
    -> InputDelay
    -- ^ Input delay to the network
    -> Rect cell n m a b

instance SystolicNetwork (Rect cell n m a b) where
  type NetworkInput  (Rect _ n m a b) = (Vec n a, Vec m b)
  type NetworkOutput (Rect _ n m a b) = (Vec n a, Vec m b)

  systolicNetwork (Rect gen inputDelay) inputs =
    liftA2 (,) outA outB
   where
     outA   = bundle (fmap fst . lastLE <$> cells)
     outB   = bundle (fmap snd <$> lastLE cells)
     cells  = imap (map . build) (repeat @n (indicesI @m))

     build n m =
       let
           inA | m == 0    = delayA n (fmap ((!!n) . fst) inputs)
               | otherwise = fmap fst (cells !! n !! pred m)

           inB | n == 0    = delayB m (fmap ((!!m) . snd) inputs)
               | otherwise = fmap snd (cells !! pred n !! m)
        in
           register undefined $ systolicCell (gen n m) (liftA2 (,) inA inB)

     (delayA, delayB) =
       case inputDelay of
         NoDelay          -> (const id,   const id)
         DelayHorizontal  -> (delayInput, const id)
         DelayVertical    -> (const id,   delayInput)
         DelayBoth        -> (delayInput, delayInput)

