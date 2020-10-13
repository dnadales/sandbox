{
}:
let
    sources = import ./nix/sources.nix {};
    haskellNix = import sources.haskell-nix {};
    ## TODO: which number to put here? Where do we get the available numbers from?
    nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
    nixpkgsArgs = haskellNix.nixpkgsArgs ;
    pkgs  = import nixpkgsSrc nixpkgsArgs;
in
pkgs.haskell-nix.project {
      # 'cleanGit' cleans a source directory based on the files known by git
      src = pkgs.haskell-nix.haskellLib.cleanGit {
          name = "haskell-nix-project";
          src = ./.;
      };
      # For `cabal.project` based projects specify the GHC version to use.
      compiler-nix-name = "ghc884"; # Not used for `stack.yaml` based projects.
  }
