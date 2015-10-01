{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring, containers
      , profunctors, stdenv
      }:
      mkDerivation {
        pname = "aencode";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          attoparsec base bytestring containers profunctors
        ];
        homepage = "https://github.com/nickspinale/aencode.git";
        description = "Efficient bencode parsers and serialization";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
