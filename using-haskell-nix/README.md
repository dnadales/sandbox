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

# Add a nix shell

Why? Test the build and run the tests locally. Use the build tools we have at
our disposal, plus some additional tools we might need.

Before running the shell do a dry run:

```sh
nix-build shell.nix --dry-run
```

# Install and configure direnv


# Install and configure lorri

```sh
lorri init
direnv allow
```

# Using ghcid

```sh
ghcid
```

# Installing haskell language server and configuring in emacs

Set lsp-haskell-server-path

Emacs configuration:

```elisp
;; direnv mode setup
(use-package direnv
 :config
 (direnv-mode))
;; We need direnv to execute before lsp starts. So we start lsp in deferred
;; mode when opening haskell files.
(use-package lsp-mode
  :hook (haskell-mode . lsp-deferred)
  :diminish lsp-mode
  :commands (lsp lsp-deferred))
(use-package lsp-ui)
(use-package lsp-haskell)
```

# Questions

## When is GHC built?

Why did `nix-shell` (re)built GHC? Why isn't this cached and why did building
width `nix` build did not rebuild GHC.

[0]: https://input-output-hk.github.io/haskell.nix/tutorials/getting-started/
[1]: https://input-output-hk.github.io/haskell.nix/tutorials/hackage-stackage/#updating-and-pinning-hackagenix-and-stackagenix
