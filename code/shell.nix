{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc"
} : 
let
  inherit (nixpkgs) pkgs;
  drv = import ./. { inherit compiler; };
in
  if pkgs.lib.inNixShell then drv.env else drv
