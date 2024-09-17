{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs:
  inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default  = self'.packages.fortran-src-ghc92-fortran-src;
        devShells.default = self'.devShells.fortran-src-ghc92;

        haskellProjects.ghc92 = import ./haskell-flake-ghc92.nix pkgs;
        haskellProjects.fortran-src-ghc92 = {
          basePackages = config.haskellProjects.ghc92.outputs.finalPackages;
          devShell = {
            tools = hp: {
              # use nixpkgs cabal-install
              cabal-install = pkgs.cabal-install;

              # disable these while unused (often slow/annoying to build)
              haskell-language-server = null;
              ghcid = null;
              hlint = null;
            };
          };
        };

        haskellProjects.ghc94 = import ./haskell-flake-ghc94.nix pkgs;
        haskellProjects.fortran-src-ghc94 = {
          basePackages = config.haskellProjects.ghc94.outputs.finalPackages;
          devShell = {
            tools = hp: {
              # use nixpkgs cabal-install
              cabal-install = pkgs.cabal-install;

              # disable these while unused (often slow/annoying to build)
              haskell-language-server = null;
              ghcid = null;
              hlint = null;
            };
          };
        };

        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          devShell = {
            tools = hp: {
              # disable these while unused (often slow/annoying to build)
              haskell-language-server = null;
              ghcid = null;
              hlint = null;
            };
          };
        };

        haskellProjects.ghc98 = {
          basePackages = pkgs.haskell.packages.ghc98;
          devShell = {
            tools = hp: {
              # disable these while unused (often slow/annoying to build)
              haskell-language-server = null;
              ghcid = null;
              hlint = null;
            };
          };
        };

      };
    };
}
