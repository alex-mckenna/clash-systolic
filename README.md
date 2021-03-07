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

# Example

# License

`clash-systolic` is licensed under the BSD 2 Clause license, see
[LICENSE](LICENSE).

