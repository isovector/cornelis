{
  description = "Agda mode for vim";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-compat = {
      url = github:edolstra/flake-compat;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    let
      ghcVersion = "8107";
      compiler = "ghc${ghcVersion}";
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        hsPkgs = pkgs.haskell.packages.${compiler}.override {
          overrides = hfinal: hprev: {
            cornelis = hfinal.callCabal2nix "cornelis" ./. { };
          };
        };
      in
      rec {
        packages = flake-utils.lib.flattenTree {
          cornelis = hsPkgs.cornelis;
          cornelis-vim = pkgs.vimUtils.buildVimPlugin {
            name = "cornelis";
            src = ./.;
          };
        };

        defaultPackage = packages.cornelis;
        app = {
          cornelis = flake-utils.lib.mkApp { name = "cornelis"; drv = packages.cornelis; };
        };
        defaultApp = app.cornelis;
      });
}
