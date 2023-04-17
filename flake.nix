{
  description = "Agda mode for vim";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

    # See https://github.com/isovector/cornelis#agda-version
    agda.url = "github:agda/agda/8d4416a462719bffbf21617b9f2fa00234e185ac";
    agda.inputs.flake-utils.follows = "flake-utils";
    agda.inputs.nixpkgs.follows = "nixpkgs";
    agda-stdlib-source = { url = "github:agda/agda-stdlib/v1.7.2"; flake = false; };

    # TODO: Remove when `nvim-hs` hits version >= 2.3.2.2 in `nixpkgs`.
    # https://search.nixos.org/packages?channel=unstable&query=nvim-hs
    nvim-hs-source.url = "github:neovimhaskell/nvim-hs/eaf826d4156b0281ef7ce9dec35ba720b5c45f09";
    nvim-hs-source.flake = false;
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      name = "cornelis";
      # Update `./.github/workflows/nix.yml` if changed.
      # `ghc902` excluded due to build issues.
      ghcVersions = map (v: "ghc${v}") [ "8107" "927" "944" ];
      # Ensure resolver in `./stack.yaml` is in sync ith `defaultGhcVersion`.
      defaultGhcVersion = "ghc927";
    in
    {
      overlays = {
        ${name} = final: prev: {
          haskell = prev.haskell // {
            packageOverrides = final.lib.composeExtensions
              prev.haskell.packageOverrides
              (hfinal: hprev: {
                # TODO: Remove when `nvim-hs` hits version >= 2.3.2.2 in `nixpkgs`.
                # https://search.nixos.org/packages?channel=unstable&query=nvim-hs
                nvim-hs = final.haskell.lib.overrideSrc hprev.nvim-hs {
                  src = inputs.nvim-hs-source;
                };
                ${name} = hfinal.callCabal2nix name ./. { };
              });
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
            version = "1.7.2";
            src = inputs.agda-stdlib-source;
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
