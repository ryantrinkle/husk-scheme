{ mkDerivation, array, base, bytestring, containers, directory
, filepath, ghc-paths, haskeline, knob, mtl, parsec, process
, stdenv, time, transformers, utf8-string, file-embed
}:
mkDerivation {
  pname = "husk-scheme";
  version = "3.19.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    array base bytestring containers directory filepath ghc-paths
    haskeline knob mtl parsec process time transformers utf8-string
    file-embed
  ];
  homepage = "http://justinethier.github.com/husk-scheme";
  description = "R5RS Scheme interpreter, compiler, and library";
  license = stdenv.lib.licenses.mit;
}
