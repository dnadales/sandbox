This is a tutorial on how to setup a Haskell project using Nix.

# Creating the .gitignore



# Setting up the cabal project

See [haskell-nix-demo](./haskell-nix-demo.cabal "cabal file").

# Install nix and configure cache

Make sure you have `nix` installed, and the cache configured as stated
[here][0].

# Install niv and setup the sources

```sh
niv init
niv add -n iohk-nixpkgs input-output-hk/nixpkgs
niv add -n hackage-nix  input-output-hk/hackage.nix
niv add -n stackage-nix input-output-hk/stackage.nix
niv add -n haskell-nix  input-output-hk/haskell.nix
```

Go and look into the `sources.json` file.

# Add a default nix file

We will be pinning the hackage, stackage, and haskell.nix tarballs using the
sources created by `niv`.

## Using `sources.nix` in `default.nix`

## Building the library

```sh
nix-build -A haskell-nix-demo.components.library
```

## Running the tests

```sh
nix-build -A haskell-nix-demo.components.tests
```

Run the tests

```sh
result/bin/spec
```

# Install and configure direnv

# Install and configure lorri

# Using ghcid


[0]: https://input-output-hk.github.io/haskell.nix/tutorials/getting-started/
[1]: https://input-output-hk.github.io/haskell.nix/tutorials/hackage-stackage/#updating-and-pinning-hackagenix-and-stackagenix
