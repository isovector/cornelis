{
  description = "Agda mode for vim";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    agda.url = "github:agda/agda/4d36cb37f8bfb765339b808b13356d760aa6f0ec";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
  };

  outputs = { self, nixpkgs, flake-utils, agda, ... }:
    let
      name = "cornelis";
      # Update `./.github/workflows/nix.yml` if changed.
      # Build currently failing on `ghc902`.
      ghcVersions = map (v: "ghc${v}") [ "8107" "902" "924" ];
      defaultGhcVersion = "ghc8107";
    in
    {
      overlays = {
        ${name} = final: prev: {
          haskell = prev.haskell // {
            packageOverrides = final.lib.composeExtensions
              prev.haskell.packageOverrides
              (hfinal: _: { ${name} = hfinal.callCabal2nix name ./. { }; });
          };

          ${name} = final.haskell.packages.${defaultGhcVersion}.${name};

          vimPlugins = prev.vimPlugins.extend (pfinal: _: {
            ${name} = final.vimUtils.buildVimPluginFrom2Nix {
              pname = name;
              inherit (final.${name}) version;
              src = ./.;
              dependencies = builtins.attrValues {
                inherit (pfinal) nvim-hs-vim vim-textobj-user;
              };
            };
          });
        };
      };
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = builtins.attrValues self.overlays; };
        agdaPkgs = import nixpkgs { inherit system; overlays = [ agda.overlay ]; };
        agdaPackage = agdaPkgs.agda.withPackages (p: [
          (p.standard-library.overrideAttrs (oldAttrs: {
            version = "2.0-experimental";
            src = pkgs.fetchFromGitHub {
              repo = "agda-stdlib";
              owner = "agda";
              rev = "experimental";
              sha256 = "sha256-l2+8myyJSufXpt1Opf65AJaTMiHMDeYYbuvkvzbCjDo=";
            };
          }))
        ]);
      in
      {
        packages =
          builtins.listToAttrs
            (
              map
                (v: {
                  name = "${name}-${v}";
                  value = pkgs.haskell.packages.${v}.${name};
                })
                ghcVersions
            )
          // {
            "${name}-vim" = pkgs.vimPlugins.${name};

            ${name} = pkgs.${name};
            default = pkgs.${name};
            agda = agdaPackage;
          };

        apps = {
          agda = flake-utils.lib.mkApp { drv = self.packages.${system}.agda; exePath = "/bin/agda"; };
        };

      }
    );
}
