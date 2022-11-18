{
  description = "Prelude replacement, external packages only, no tests";

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url = "github:sixears/flake-build-utils/r1.0.0.12";
  };

  outputs = { self, nixpkgs, build-utils }:
    build-utils.lib.hOutputs self nixpkgs "base0" {
      ghc = p: p.ghc8107; # for tfmt
      callPackage = { mkDerivation, lib, system
                    , base, base-unicode-symbols, data-default, data-textual
                    , hashable, lens, mtl, safe
                    }:
        let
          pkg = build-utils.lib.flake-def-pkg system;
        in
          mkDerivation {
            pname = "base0";
            version = "0.0.4.7";
            src = ./.;
            libraryHaskellDepends = [
              base base-unicode-symbols data-default data-textual hashable lens
              mtl safe
            ];
            description = "Prelude replacement, external packages only, no tests";
            license = lib.licenses.mit;
          };
    };

}
