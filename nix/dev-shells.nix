{
  path,
  lib,

  haskellPackages,
  pkg-config,
  zlib,
  icu,
}:
let
  buildInputs = [
    haskellPackages.cabal-install
    pkg-config
    zlib.dev
    zlib.out
    icu
  ];
in
haskellPackages.shellFor {
  inherit buildInputs;

  packages = p: [ p.cornelis ];

  # Ensure nix commands do not use the global <nixpkgs> channel:
  NIX_PATH = "nixpkgs=" + path;

  # Ensure system libraries (zlib.so, etc.) are visible to GHC:
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
