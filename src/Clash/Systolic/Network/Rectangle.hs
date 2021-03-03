module Clash.Systolic.Network.Rectangle
  ( Rect(..)
  ) where

import Clash.Prelude

import Clash.Systolic.Cell
import Clash.Systolic.Network
import Data.Kind


{-# INLINE lastLE #-}
lastLE :: forall n a. (1 <= n) => Vec n a -> a
lastLE = leToPlus @1 @n last

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
    -> Rect cell n m a b

instance SystolicNetwork (Rect cell n m a b) where
  type NetworkInput  (Rect _ n m a b) = (Vec n a, Vec m b)
  type NetworkOutput (Rect _ n m a b) = (Vec n a, Vec m b)

  systolicNetwork (Rect gen) inputs =
    liftA2 (,) outA outB
   where
     outA   = bundle (fmap fst . lastLE <$> cells)
     outB   = bundle (fmap snd <$> lastLE cells)
     cells  = imap (map . build) (repeat @n (indicesI @m))

     build n m =
       let
           inA | m == 0    = fmap ((!!n) . fst) inputs
               | otherwise = fmap fst (cells !! n !! pred m)

           inB | n == 0    = fmap ((!!m) . snd) inputs
               | otherwise = fmap snd (cells !! pred n !! m)
        in
           register undefined $ systolicCell (gen n m) (liftA2 (,) inA inB)

