{
  description = "Agda mode for vim";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }: let
    ghcVersions = ["8107" "902" "924"];
    defaultVersion = "8107";
    compilerFor = ghc: "ghc${ghc}";
    perGHC = nixpkgs.lib.genAttrs (map compilerFor ghcVersions);
    # recursive flattening of attrSets
    flattenAttrs = with nixpkgs.lib;
      sep: attrs: let
        recurse = p:
          mapAttrsToList
          (n: v: let
            p' =
              if p == ""
              then p
              else p + sep;
          in
            if (isAttrs v && !(isDerivation v))
            then recurse (p' + n) v
            else {${p' + n} = v;});
      in
        foldr (a: b: a // b) {} (flatten (recurse "" attrs));
  in
    with flake-utils.lib;
      eachDefaultSystem (system: let
        pkgs = import nixpkgs {inherit system;};

        hsPkgs = perGHC (ver:
          pkgs.haskell.packages.${ver}.override {
            overrides = hfinal: hprev: {
              cornelis = hfinal.callCabal2nix "cornelis" ./. {};
            };
          });
      in rec {
        packages = flattenAttrs "-" rec {
          cornelis = perGHC (ver: hsPkgs.${ver}.cornelis);
          cornelis-vim = pkgs.vimUtils.buildVimPluginFrom2Nix {
            pname = "cornelis";
            inherit (self.packages.${system}.default) version;
            src = ./.;
            dependencies = lib.attrValues {
              inherit (pkgs.vimPlugins) nvim-hs-vim vim-textobj-user;
            };
          };
          default = cornelis.${compilerFor defaultVersion};
        };

        apps = flattenAttrs "-" rec {
          cornelis = perGHC (ver:
            mkApp {
              name = "cornelis";
              drv = packages."cornelis-${ver}";
            });
          default = cornelis.${compilerFor defaultVersion};
        };

        formatter = pkgs.alejandra;
      });
}
