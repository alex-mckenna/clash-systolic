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

- [Overview](#overview)
  * [Systolic Cells](#systolic-cells)
  * [Systolic Networks](#systolic-networks)
- [Example - Ripple Carry Adder](#example---ripple-carry-adder)
  * [Setup](#setup)
  * [Adder Cell](#adder-cell)
  * [Ripple Adder Network](#ripple-adder-network)
  * [Simulating in `clashi`](#simulating-in-clashi)
- [Frequently Asked Questions](#frequently-asked-questions)
- [License](#license)

# Overview
## Systolic Cells

Systolic cells describe the behaviour of an individual processing element in a
network. These provide the basic functionality for a network and should be
designed in such a way that each cell acts as a black box - consuming some
input and producing some output every cycle.

> :warning: **Warning: Using registers in cells**
>
> Cells are intended to only specify behaviour, as cells are not expected to
> know if their input / output is an external signal to / from a network. This
> means that cells should *not* delay their input or output signals, as this
> can lead to incorrect behaviour when cells are connected to form a network.

The type of a cell should contain all the information needed to implement the
`SystolicCell` class. This means user defined cell types should allow the
inputs and outputs of the cell to be determined, as well as any static
configuration for the cell.

## Systolic Networks

Systolic networks describe the interconnection of processing elements. These
provide templates for how to instantiate and connect individual cells to build
the desired circuit. Similarly to cells, networks are also black boxes which
consume some input and produce some output each cycle. Note that for many types
of network, there will be a latency before the first output is produced.

The type of a network should contain all the information needed to implement
the `SystolicNetwork` class. This means user defined network types should allow
the inputs and outputs of a network to be determined, as well as any static
configuration for the network.

# Example - Ripple Carry Adder
## Setup
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

## Adder Cell

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

## Ripple Adder Network

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

The `SystolicNetwork` instance for `RippleAdder` contains most of the
complexity in the implementation, as it has to

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

## Simulating in `clashi`

From `clashi` we can test that the ripple adder network performs as expected:

```
> import Data.List as List
> let xs = [62, 75, 54, 86, 54, 76, 36, 67] :: [BitVector 8]
> let ys = [2, 4, 8, 16, 32, 64, 128, 255] :: [BitVector 8]
> let cs = [0, 0, 0, 0, 0, 0, 0, 0] :: [Bit]
> let netIn = List.zip3 xs ys cs
> let net2x4 = systolicNetwork @_ @System (PipelinedAdder @2 @4 SNat)
> showX (simulateN 10 net2x4 netIn)
"[(...._....,X),(...._....,.),(0100_0000,0),(0100_1111,0),(0011_1110,0),(0110_0110,0),(0101_0110,0),(1000_1100,0),(1010_0100,0),(0100_0010,1)]"
```

If the number of stages and width of each stage are changed, the latency of the
network changes accordingly:

```
> let net1x8 = systolicNetwork @_ @System (PipelinedAdder @1 @8 SNat)
> let net4x2 = systolicNetwork @_ @System (PipelinedAdder @4 @2 SNat)
> showX (simulateN 9 net1x8 netIn)
"[(...._....,X),(0100_0000,0),(0100_1111,0),(0011_1110,0),(0110_0110,0),(0101_0110,0),(1000_1100,0),(1010_0100,0),(0100_0010,1)]"
> showX (simulateN 12 net4x2 netIn)
"[(...._....,X),(...._....,.),(...._....,.),(...._....,.),(0100_0000,0),(0100_1111,0),(0011_1110,0),(0110_0110,0),(0101_0110,0),(1000_1100,0),(1010_0100,0),(0100_0010,1)]"
```

# Frequently Asked Questions

<dl>
  <dt>Why is cell / network X not defined in the library?</dt>
  <dd>

As `clash-systolic` aims to be as lightweight as possible, the only
definitions given out of the box are things deemed to be widely applicable.
The means cells / networks which are specific to a particular problem or
problem domain are likely to be excluded. In the future, separate libraries
may be created for these if there is demand.

Pull requests are accepted for new cells / networks if they are generic
enough for inclusion in the library.

  </dd>

  <dt>How can I define a network with multiple types of cell?</dt>
  <dd>

There are two recommended ways to define a network with multiple types of cell,
depending on whether the cells have the same input and output types or not. For
the simple case, the cell configuration can simple have a constructor for each
variant of cell, i.e.

```haskell
data MyCell = DoA | DoB
```

For the more advanced case of cells which have different inputs / outputs,
`-XGADTs` can be used to describe the different variants of cell. For example:

```haskell
data CellType = A | B

data MyCell :: CellType -> Type where
  DoA :: MyCell 'A
  DoB :: MyCell 'B
```

By using `-XFlexibleContexts` to define instances for `SystolicCell`, the
different variants can have different `CellInput` and `CellOutput` types:

```haskell
instance SystolicCell (MyCell 'A) where
  type CellInput  (MyCell 'A) = InputA
  type CellOutput (MyCell 'A) = OutputA

  systolicCell DoA = ...

instance SystolicCell (MyCell 'B) where
  type CellInput  (MyCell 'B) = InputB
  type CellOutput (MyCell 'B) = OutputB

  systolicCell DoB = ...
```

Networks can be defined to work over multiple types of cell by having a
recursive configuration type and using type equality constraints to ensure that
input and output types for cells line up. See the library module
`Clash.Systolic.Network.Pipeline` for an example of this.

  </dd>

  <dt>How can I change the connections / flow of data in a network?</dt>
  <dd>

As connections are specified by implementations of the `SystolicNetwork` class,
the best way to define variations of a network are to either

  * specify choices as static configuration in the network type
  * define related network types for different connection methods

This is largely a matter of personal taste, although generally speaking for
variations of a network that are sufficiently different it is better to define
a separate network type to keep the implementation of `systolicNetwork` easily
understandable. The `-XGADTs` trick from the preceding answer can also be used
to solve this problem.

  </dd>

  <dt>How can I use hidden state within a cell / network?</dt>
  <dd>

Hidden state can be used in a cell by defining cells using the `mealy` or
`moore` functions from [clash-prelude](https://hackage.haskell.org/package/clash-prelude).
Hidden state in a network can be achieved by having an input and output from
cells containing the shared internal state. The connections for the network
must connect these signals together such that no input or output is part of
the `NetworkInput` or `NetworkOutput` for the network. The behaviour of the
cell type must specify when to preserve or propagate state amongst cells.

  </dd>

  <dt>How can I define networks with irregular delays?</dt>
  <dd>

Systolic networks should have a regular delay between cells. If you are
trying to implement something where this is undesirable, this is likely not
the library you want. You may have more luck with the
[clash-protocols](https://github.com/clash-lang/clash-protocols) library.

  </dd>
</dl>

# License

`clash-systolic` is licensed under the BSD 2 Clause license, see
[LICENSE](LICENSE).

