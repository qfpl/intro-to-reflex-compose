{ nixpkgsOld ? import <nixpkgs> {}
, compiler   ? "ghcjs"
} :
let
  reflex-platform = import (nixpkgsOld.pkgs.fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "b365d0b3f55ebcca0c6f8517b20cf5d0b7b1cd37";
    sha256 = "198vckj8lls5d6ig4ayizk4mw7lsnbnynf187agagk2yfab0v6ll";
  }) {} ;

  nixpkgs = reflex-platform.nixpkgs;
  pkgs = nixpkgs.pkgs;

  fixUp = if compiler == "ghcjs" then pkgs.haskell.lib.dontHaddock else nixpkgs.lib.trivial.id;

  reflex-materials-code-base = fixUp (reflex-platform.${compiler}.callPackage ./examples.nix {});

  reflex-materials-code = pkgs.haskell.lib.overrideCabal reflex-materials-code-base (drv: {
    executableToolDepends = [pkgs.closurecompiler pkgs.zopfli];
    postInstall = ''
      mkdir -p $out

      mkdir -p $out/css
      cp ./css/* $out/css/

      mkdir -p $out/js
      cp $out/bin/examples-exe.jsexe/all.js $out/js/examples.js

      cd $out/bin/examples-exe.jsexe
      closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > $out/js/examples.min.js
      rm -Rf $out/bin/examples-exe.jsexe
      rm -Rf $out/bin

      cd $out/js
      zopfli -i1000 examples.min.js

      rm -Rf $out/lib
      rm -Rf $out/nix-support
      rm -Rf $out/share
    '';
  });
in
  reflex-materials-code
