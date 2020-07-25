{ pkgs ? import <nixpkgs> { } }:

let
  nix-hs =
    import (fetchGit "https://github.com/pjones/nix-hs.git") { inherit pkgs; };
  hex =
    (fetchGit "https://github.com/TimPut/Hexagonal.git");
  stl =
    (fetchGit "https://github.com/TimPut/STL-Linear.git");

in nix-hs {
  cabal = ./hTile.cabal;
  enableFullyStaticExecutables = true;

  overrides = lib: self: super: with lib; {
    Hexagonal = (super.callCabal2nix "Hexagonal" hex {});
    STL-Linear = (super.callCabal2nix "STL-Linear" stl {});
    mkDerivation = args: super.mkDerivation (args // {
      doCheck = false;
        });
    };
  staticBuildInputs = static: with static; [ zlib_both ];
}
