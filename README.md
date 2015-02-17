# Haskell ASM Benchmark

This repo contains implementation of a simple hypothetical processor with its
own assembly language.

The purpose of this tiny project is to measure the performance of various ways
to implement a Monad (exactly for such kind of applications), and to pick
the best one.

## Structure

Every branch in this repo represents a different way of implementing a monad.
The monad being implemented is `CPU`. This monad executes the instructions and
models the processor state, so its performance is important.

There are five branches so far:
* `trunk` - the baseline branch, with `transformers` package used for `CPU` monad;
* `mtl` - uses `mtl` package for `CPU` monad;
* `contstuff` - uses `contstuff` package for `CPU` monad;
* `unroll` - contains a hand-unrolled implementation of `CPU` monad;
* `cps` - contains a hand-written CPS implementation of `CPU` monad.

## Building and executing

No `.cabal` file so far, sorry for that, though building is as simple as just:

    ghc --make -O3 Asm.hs

Various options (`-fllvm`, `-funbox-strict-fields`) do not help much.

The test application (`Asm`) accepts a plain list of integers on `stdin` and
produces a plain list of square roots on `stdout`. Square roots are calculated
in `CPU` monad using Heron's method (in 20 iterations).

To measure the performance, just run:

    ./measure

The output value of this script is average execution time, in seconds.

## Results

Results are usually machine-dependent and will be placed on Wiki.
