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
  # It is important that the haskell language server and the haskell project
  # use the same GHC version.
  compiler-nix-name = "ghc8102";
  hls =
    pkgs.haskell-nix.cabalProject {
      inherit compiler-nix-name;
      src = pkgs.fetchFromGitHub {
        name = "haskell-language-server";
        owner = "haskell";
        repo = "haskell-language-server";
        rev = "0.5.0";
        sha256 = "0vkh5ff6l5wr4450xmbki3cfhlwf041fjaalnwmj7zskd72s9p7p";
        fetchSubmodules = true;
      };
      # We need to include a hash map for the github repositories in
      # referenced in the haskell-language-server cabal file.
      #
      # See:
      # https://input-output-hk.github.io/haskell.nix/tutorials/source-repository-hashes/#avoiding-modifying-cabalproject-and-stackyaml
      sha256map = {
        "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
      };
      # Plan issues with the benchmarks, can try removing later
      configureArgs = "--disable-benchmarks";
      # Invalidate and update if you change the version
      plan-sha256 = "1vyriqi905kl2yrx1xg04cy11wfm9nq1wswny7xm1cwv03gyj6y8";
      modules = [{
        # Tests don't pass for some reason, but this is a somewhat random revision.
        packages.haskell-language-server.doCheck = false;
      }];
    };
in
rec {
  hsPkgs =
    pkgs.haskell-nix.project {
      # 'cleanGit' cleans a source directory based on the files known by git
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "haskell-nix-project";
        src = ./.;
      };
      # For `cabal.project` based projects specify the GHC version to use.
      inherit compiler-nix-name;
      # Not used for `stack.yaml` based projects.
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
        tools = {
          cabal = "3.2.0.0";
          hlint = "2.2.11";
        };
        # See overlays/tools.nix for more details

        # Some you may need to get some other way.
        buildInputs = with pkgs.haskellPackages;
        [ ghcid
          hls.haskell-language-server.components.exes.haskell-language-server
        ];

        # Prevents cabal from choosing alternate plans, so that
        # *all* dependencies are provided by Nix.
        exactDeps = true;
      };
}
