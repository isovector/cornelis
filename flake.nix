{
  description = "Agda mode for vim";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

    # See https://github.com/isovector/cornelis#agda-version
    agda.url = "github:agda/agda/4d36cb37f8bfb765339b808b13356d760aa6f0ec";
    agda.inputs.flake-utils.follows = "flake-utils";
    agda-stdlib = { url = "github:agda/agda-stdlib/experimental"; flake = false; };
  };

  outputs = { self, nixpkgs, ... }@inputs:
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
    } // inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.${name} inputs.agda.overlay ];
        };
        agda = pkgs.agda.withPackages (p: nixpkgs.lib.singleton (
          p.standard-library.overrideAttrs (_: {
            version = "2.0-experimental";
            src = inputs.agda-stdlib;
          })
        ));
      in
      {
        packages = {
          inherit agda;
          ${name} = pkgs.${name};
          "${name}-vim" = pkgs.vimPlugins.${name};
          default = pkgs.${name};

          # Create `cornelis` package for all `ghcVersions`.
        } // builtins.listToAttrs (map
          (v: { name = "${name}-${v}"; value = pkgs.haskell.packages.${v}.${name}; })
          ghcVersions
        );
      }
    );
}
