# Clash Systolic - Generalised Systolic Architectures

[![CI](https://github.com/alex-mckenna/clash-systolic/actions/workflows/ci.yml/badge.svg)](https://github.com/alex-mckenna/clash-systolic/actions/workflows/ci.yml)

> **systole (noun)**
>
> the phase of the heartbeat when the heart muscle contracts and pumps blood
> from the chambers into the arteries

A batteries-included library for writing designs which use systolic networks.
Systolic *networks* are taken as a generalisation of the classic systolic
array architecture, and can be used to implement any manner of structure
in a systolic manner including

  * pipelines
  * systolic arrays
  * systolic trees
  * systolic lattices
  * systolic neutral networks

The API for `clash-systolic` aims to be as lightweight as possible. Once a
systolic cell or network is configured, it is simply a function from a signal
of input to a signal of output. This makes it easy to integrate with existing
clash designs, as there is no separate API to learn.

# Overview
## Systolic Cells

Systolic cells describe the behaviour of an individual processing element in a
network.

## Systolic Networks

Systolic networks describe the interconnection of processing elements.

# Example - Ripple Carry Adder

A ripple carry adder is a simple adder circuit constructed by chaining together
adders, propagating the carry from one adder to the next. Using `clash-systolic`
we can implement this as a systolic network where

  * each cell is an n-bit adder, taking two n-bit numbers and a carry bit
    and outputting an n-bit number and a carry bit

  * the network takes in an initial carry and two n * m bit numbers, and
    outputs the final carry and an n * m bit number. The network handles
    splitting the input into chunks and merging the outputs, as well as
    delaying the input and output chunks by the correct amount

Assuming the project is based off the
[clash starter project](https://github.com/clash-lang/clash-starters), the
only preamble needed is

```haskell
import Clash.Prelude
import Clash.Systolic
```

The first thing to be defined is the type of cells, containing the static
configuration for the adder. For this example, the only configuration is the
number of bits that a single adder cell can add.

```haskell
data Adder n where
  Adder :: SNat n -> Adder n
```

The instance for `SystolicCell` specifies the interface and behaviour for the
`Adder` cell. For convenience, we use `BitVector` to represent values, as this
allows any type which implements `BitPack` to be used (provided the signal is
packed and unpacked outside the final network).

```haskell
instance SystolicCell (Adder n) where
  type CellInput (Adder n) =
    (BitVector n, BitVector n, Bit)

  type CellOutput (Adder n) =
    (BitVector n, Bit)

  systolicCell (Adder SNat) =
    fmap $ \(x, y, cIn) ->
      let (c, z) = split (add x y + resize (pack cIn))
       in (z, unpack c)
```

With the behaviour of an individual cell specified, a network can now be
defined which instantiates and connects the cells together. This is configured
by the number of stages and the width of each cell's input. The constraints
for the network are also given in this configuration type, which are

  * there must be at least one adder
  * each adder must add at least one bit

```haskell
data RippleAdder stages width where
  RippleAdder
    :: ( KnownNat stages, stages <= 1
       , KnownNat width,  width <= 1
       )
    => SNat (stages * width)
    -> RippleAdder stages width
```

The implementation of `SystolicNetwork` for `RippleAdder` is where most of the
complexity is introduced. Here we have to

  * split each input `BitVector` into the input for each cell
  * delay each input `BitVector` so input and carry meet cells in sync
  * instantiate and connect the adders for the network
  * delay the output of each adder so results are produced in sync
  * extract the final carry and outputs from adders, and merge the outputs

Firstly, an auxiliary function is defined to delay signals by their position
in the network. This delays each signal in a `Vec` by it's 0-based index:

```haskell
delaySignals
  :: forall dom n a
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , NFDataX a
     )
  => Vec n (Signal dom a)
  -> Vec n (Signal dom a)
delaySignals =
  smap delayByIndex
 where
  delayByIndex i signal =
    foldl (\x _ -> register undefined x) signal (replicate i ())
```

Another useful auxiliary function is to extract the first adder from the cells
in the network (as carry flows from LSB to MSB, the first adder will have the
final carry value). As `Clash.Sized.Vector.head` uses `(+)` in it's type
signature instead of a `(<=)` constraint, `leToPlus` is used to change the type
to use a `(<=)` constraint instead.

```haskell
firstAdder :: forall n a. (1 <= n) => Vec n a -> a
firstAdder = leToPlus @1 @n head
```

With these definitions, `SystolicNetwork` can be implemented:

```haskell
instance SystolicNetwork (RippleAdder stages width) where
  type NetworkInput (RippleAdder stages width) =
    (BitVector (stages * width), BitVector (stages * width), Bit)

  type NetworkOutput (RippleAdder stages width) =
    (BitVector (stages * width), Bit)

  systolicNetwork (RippleAdder SNat) inputs =
    bundle (zBv, cOut)
   where
    -- Network Inputs
    (xBv, yBv, cIn) = unbundle inputs
    xIn             = delayInputs (traverse unpack xBv)
    yIn             = delayInputs (traverse unpack yBv)

    -- Network Outputs
    cOut            = fmap snd (firstAdder adders)
    zOut            = delayOutputs (fmap fst <$> adders)
    zBv             = fmap pack (sequenceA zOut)

    -- Network Cells
    adders          = fmap build (indicesI @stages)

    build i =
      let carry | i == maxBound = cIn
                | otherwise     = fmap snd (adders !! succ i)
       in register undefined $
            systolicCell (Adder SNat) (bundle (xIn !! i, yIn !! i, carry))

    -- Delaying
    delayInputs     = reverse . delaySignals . reverse
    delayOutputs    = delaySignals
```

# License

`clash-systolic` is licensed under the BSD 2 Clause license, see
[LICENSE](LICENSE).

