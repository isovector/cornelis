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
    name = "cornelis";
    # Update `./.github/workflows/./.github/workflows/haskell.yml` if changed.
    # Build currently failing on `ghc902`.
    ghcVersions = map (v: "ghc${v}") ["8107" "902" "924"];
    defaultGhcVersion = "ghc8107";
  in
    {
      overlays = {
        ${name} = final: prev: {
          haskell =
            prev.haskell
            // {
              packageOverrides =
                final.lib.composeExtensions
                prev.haskell.packageOverrides
                (hfinal: _: {${name} = hfinal.callCabal2nix name ./. {};});
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
    }
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = builtins.attrValues self.overlays;
        };
      in {
        packages =
          builtins.listToAttrs (
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
          };

        formatter = pkgs.alejandra;
      }
    );
}
