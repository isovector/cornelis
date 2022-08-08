{ pkgs,
  haskellPackages ? pkgs.haskellPackages,
  packages,
  ...
}:
let
  # Wrap `stack` so that `stack --nix` is executed by default:
  stack = pkgs.symlinkJoin {
    name = "stack-nix";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack --add-flags "--nix --no-nix-pure"
      '';
  };

  buildInputs = [
    stack
    pkgs.pkgconfig
    pkgs.zlib.dev
    pkgs.zlib.out
    pkgs.icu
  ];

  extraTestInputs = [
    pkgs.agda
    pkgs.neovim
  ];

  mkShell = buildInputs:
    haskellPackages.shellFor {
      inherit packages buildInputs;

      # Ensure nix commands do not use the global <nixpkgs> channel:
      NIX_PATH = "nixpkgs=" + pkgs.path;

      # Ensure system libraries (zlib.so, etc.) are visible to GHC:
      LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
    };
in

{
  default = mkShell buildInputs;
  testShell = mkShell (buildInputs ++ extraTestInputs);
}
