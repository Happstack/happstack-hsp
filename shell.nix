with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, bytestring, happstack-server, harp, hsp
             , hsx2hs, mtl, stdenv, syb, text, utf8-string
             }:
             mkDerivation {
               pname = "happstack-hsp";
               version = "7.3.4";
               src = ./.;
               buildDepends = [
                 base bytestring happstack-server harp hsp hsx2hs mtl syb text
                 utf8-string
               ];
               homepage = "http://www.happstack.com/";
               description = "Support for using HSP templates in Happstack";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
