{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, happstack-server, harp, hsp
      , hsx2hs, mtl, stdenv, syb, text, utf8-string, cabal-install
      }:
      mkDerivation {
        pname = "happstack-hsp";
        version = "7.3.7.3";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring happstack-server hsp hsx2hs mtl syb text
          utf8-string cabal-install
        ];
        homepage = "http://www.happstack.com/";
        description = "Support for using HSP templates in Happstack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
