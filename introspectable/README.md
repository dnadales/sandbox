# An ad-hoc introspectable class

To build this project
use [stack](https://docs.haskellstack.org/en/stable/README/):

```bash
stack build
```

Then, play with the functions defined in the `src` repository by firing up the
REPL:

```bash
stack ghci
```

```
*Main Introspectable> getValue v1 (AString "color")
Just "red"
*Main Introspectable> fmap ("dark " ++ ) $ getValue v1 (AString "color")
Just "dark red"
```
