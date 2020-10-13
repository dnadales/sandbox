{
}:
let
    sources = import ./nix/sources.nix {};
    haskellNix = import sources.haskell-nix {};
    ## To query the available versions try:
    ##
    ## > nix-repl
    ## > sources = import ./nix/sources.nix {}
    ## > hnix = import sources.haskell-nix {}
    ## > hnix.sources.nixpkgs.<TAB>
    nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
    nixpkgsArgs = haskellNix.nixpkgsArgs ;
    pkgs  = import nixpkgsSrc nixpkgsArgs;
in
rec { hsPkgs =
      pkgs.haskell-nix.project {
          # 'cleanGit' cleans a source directory based on the files known by git
          src = pkgs.haskell-nix.haskellLib.cleanGit {
              name = "haskell-nix-project";
              src = ./.;
          };
          # For `cabal.project` based projects specify the GHC version to use.
          compiler-nix-name = "ghc884"; # Not used for `stack.yaml` based projects.
      };
  shell =
      hsPkgs.shellFor {
          # Include only the *local* packages of your project.
          packages = ps: with ps; [
          ];

          # Builds a Hoogle documentation index of all dependencies,
          # and provides a "hoogle" command to search the index.
          withHoogle = true;

          # You might want some extra tools in the shell (optional).

          # Some common tools can be added with the `tools` argument
          tools = { cabal = "3.2.0.0";
                    hlint = "2.2.11";
                  };
          # See overlays/tools.nix for more details

          # Some you may need to get some other way.
          buildInputs = with pkgs.haskellPackages;
              [ ghcid ];

          # Prevents cabal from choosing alternate plans, so that
          # *all* dependencies are provided by Nix.
          exactDeps = true;
      };
}
