# What is the fastest way of getting the leaves of a tree?

Add your solution, add a benchmark in `bench/LeavesOfaTreeBench.hs` and run the
benchmarks using stack:

    stack bench

The benchmark program uses [Criterion](http://www.serpentine.com/criterion/) as
benchmarking library.

Alternatively, individual versions of the "leaves-of-a-tree" variants can be
run as follows:

    stack exec leaves-of-a-tree -- -v VERSION -s SIZE

where `VERSION` is the string name of the variant, and `SIZE` is an integer
specifying the size of a tree.

To profile your solution make sure that a local version of `ghc` is used. The
`ghc` version used by Stack can be seen by running:

    stack exec -- which ghc

If this returns a global path instead of a local path (having the prefix
`~/.stack/`), then a global installation is used. Correct this by either
removing the global installation or setting `system-ghc` to `false` in
`stack.yaml` and then run:

    stack setup

To build the project with profiling support build the main application as
follows:

    stack build --executable-profiling --library-profiling --ghc-options="-rtsopts"

Then run the main program using the `+RTS -p` flag. For instance:

    stack exec leaves-of-a-tree -- -v leavesT -s 10000000 +RTS -p

Is your solution faster?
