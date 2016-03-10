{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation
      , base
      , text
      , filepath
      , containers
      , transformers
      , unix

      , persistent
      , persistent-template
      , persistent-sqlite
      , esqueleto

      , lens
      , regex-base
      , regex-pcre
      , stdenv }:
      mkDerivation {
        pname = "homelinker";
        version = "0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
            base
            text
            filepath
            containers
            transformers
            unix

            persistent
            persistent-template
            persistent-sqlite
            esqueleto

            lens
            regex-base
            regex-pcre
        ];
        license = stdenv.lib.licenses.bsd2;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
