{ mkDerivation, base, containers, data-default, directory, filepath
, ghcjs-dom, jsaddle, jsaddle-warp, lens, mtl, reflex
, reflex-dom-core, stdenv, text, time, wai-middleware-static, warp
, websockets
}:
mkDerivation {
  pname = "examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers data-default directory filepath ghcjs-dom jsaddle
    jsaddle-warp lens mtl reflex reflex-dom-core text time
    wai-middleware-static warp websockets
  ];
  license = stdenv.lib.licenses.bsd3;
}
