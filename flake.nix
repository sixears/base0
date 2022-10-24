# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "Prelude replacement, external packages only, no tests";

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "base0";
      in
        {
          packages.${packageName} =
            haskellPackages.callCabal2nix packageName self rec {
              # Dependency overrides go here
            };

          defaultPackage =
            haskellPackages.callCabal2nix packageName self rec {
              # Dependency overrides go here
            };

          devShell = pkgs.mkShell {
            buildInputs =
              with haskellPackages;
              [
                haskellPackages.haskell-language-server # you must build it with
                                                        # your ghc to work
                ghcid
                ## cabal-install
              ];
            inputsFrom = builtins.attrValues self.packages.${system};
          };
        }
    );
}
