# cornelis

![Cornelis in Action](https://raw.githubusercontent.com/isovector/cornelis/master/cast.gif)


## Dedication

> I'll ask to stand up \
> With a show about a rooster, \
> Which was old and worn out, \
> Impotent and weathered. \
> The chickens complained and whined \
> Because he did not satisfy them.
>
> -- [Cornelis Vreeswijk](https://www.youtube.com/watch?v=oKUscEWPVAM)


## Overview

`cornelis` is agda-mode, but for neovim. It's written in Haskell, which means
it's maintainable and significantly less likely to bit-rot like any
vimscript/lua implementations.


## Features

It supports highlighting, goal listing, type-context, refinement, auto, solving,
case splitting, go-to definition, normalization, and helper functions. These are
exposed via the vim commands:

```
:CornelisLoad
:CornelisGoals
:CornelisTypeContext <RW>
:CornelisTypeContextInfer <RW>
:CornelisRefine
:CornelisGive
:CornelisElaborate
:CornelisAuto
:CornelisSolve <RW>
:CornelisMakeCase
:CornelisGoToDefinition
:CornelisPrevGoal
:CornelisNextGoal
:CornelisWhyInScope
:CornelisNormalize <CM>
:CornelisHelperFunc <RW>
:CornelisQuestionToMeta
```

Commands with an `<RW>` argument take an optional normalization mode argument,
one of `AsIs`, `Instantiated`, `HeadNormal`, `Simplified` or `Normalised`. When
omitted, defaults to `Normalised`.

Commands with a `<CM>` argument take an optional compute mode argument,
one of `DefaultCompute`, `HeadCompute`, `IgnoreAbstract` or `UseShowInstance`.
When omitted, defaults to `DefaultCompute`.

If you need to restart the plugin (eg if Agda is stuck in a loop), you can
restart everything via `:CornelisRestart`.

`:CornelisQuestionToMeta` will replace any `?` goals with `{! !}`s.


### Agda Input

There is reasonably good support for agda-input via your `<LocalLeader>` in
insert mode. See
[agda-input.vim](https://github.com/isovector/cornelis/blob/master/agda-input.vim)
for available bindings, or slap your `<LocalLeader>` while in insert mode.

If you'd like to use a prefix other than your `<LocalLeader>`, add the following
to your `.vimrc`:

```viml
let g:cornelis_agda_prefix = "<Tab>" " Replace with your desired prefix
```

#### Disabling Default Bindings

If you don't want any of the default bindings, add the following to your `.vimrc`:

```viml
let g:cornelis_no_agda_input = 1
```


#### Adding Bindings

Custom bindings can be added by calling the `cornelis#bind_input` function in
`.vimrc`. For example:

```viml
call cornelis#bind_input("nat", "ℕ")
```

will add `<LocalLeader>nat` as an input remapping for `ℕ`.

### Text Objects

Use the `iz`/`az` text objects to operate on text between `⟨` and `⟩`. Somewhat
surprisingly for i/a text objects, `iz` targets the _spaces_ between these
brackets, and `az` targets the spaces. Neither textobj targets the brackets
themselves.

Also `ii`/`ai` will operate on `⦃` and `⦄`, but in the way you'd expect
text objects to behave.

`ih`/`ah` will operate on `{!` and `!}`.



## Installation

Make sure you have [`stack`](https://docs.haskellstack.org/en/stable/install_and_upgrade/) on your PATH!

```
Plug 'kana/vim-textobj-user'
Plug 'neovimhaskell/nvim-hs.vim'
Plug 'liuchengxu/vim-which-key'
Plug 'isovector/cornelis', { 'do': 'stack build' }
```


### Agda Version

`cornelis` is tested only against `agda-2.6.3`. If you run into weird error
messages from vim, it's probably because you're running an old version of
`agda`. If possible, try upgrading, if not, file a bug and I'll see if I can
help.

In addition, there are some bugs in the most recent version of `agda` that
negatively affect `cornelis`. For best results, build from head, ensuring you
have the following patches:

- https://github.com/agda/agda/pull/5752
- https://github.com/agda/agda/pull/5776


### Installation with Nix

You can install both the vim plugin and the cornelis binary using nix flakes!
You can access the binary as `cornelis.packages.<my-system>.cornelis` and the
vim plugin as `cornelis.packages.<my-system>.cornelis-vim`. Below is a sample
configuration to help you understand where everything plugs in.

<details>
<summary>Nix details</summary>

```nix
# flake.nix
{
  description = "my-config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cornelis.url = "github:isovector/cornelis";
    cornelis.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    { home-manager
    , nixpkgs
    , cornelis
    , ...
    }: {
    nixosConfigurations = {
      bellerophon = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.my-home = (import ./my-home.nix) {
                cornelis = cornelis.packages."x86_64-linux".cornelis;
                cornelis-vim = cornelis.packages."x86_64-linux".cornelis-vim;
              };
            }
        ];
      };
    };
  };
}

# my-home.nix
{cornelis, cornelis-vim}: {pkgs, ...}:
{
  home = {
    packages = [cornelis pkgs.agda];
  };
  programs = {
    neovim = {
      enable = true;
      extraConfig = builtins.readFile ./init.vim;
      plugins = with pkgs.vimPlugins; [
        cornelis-vim
        vim-textobj-user
        nvim-hs-vim
      ];
    };
  };
}
```
</details>

Make sure you enable the global binary option in your vim config. Since
`/nix/store` is immutable cornelis will fail when `nvim-hs` tries to run stack,
which it will do if the global binary option isn't enabled.


#### Use global binary instead of stack

Vimscript:

```vimscript
let g:cornelis_use_global_binary = 1
```

Lua:

```lua
vim.g.cornelis_use_global_binary = 1
```


## Example Configuration

Once you have `cornelis` installed, you'll probably want to add some keybindings
for it! This is enough to get you started:

```viml
au BufRead,BufNewFile *.agda call AgdaFiletype()
function! AgdaFiletype()
    nnoremap <buffer> <leader>l :CornelisLoad<CR>
    nnoremap <buffer> <leader>r :CornelisRefine<CR>
    nnoremap <buffer> <leader>d :CornelisMakeCase<CR>
    nnoremap <buffer> <leader>, :CornelisTypeContext<CR>
    nnoremap <buffer> <leader>. :CornelisTypeContextInfer<CR>
    nnoremap <buffer> <leader>n :CornelisSolve<CR>
    nnoremap <buffer> <leader>a :CornelisAuto<CR>
    nnoremap <buffer> gd        :CornelisGoToDefinition<CR>
    nnoremap <buffer> [/        :CornelisPrevGoal<CR>
    nnoremap <buffer> ]/        :CornelisNextGoal<CR>
endfunction
```

Feeling spicy? Automatically run `CornelisLoad` every time you save the file.

```viml
au BufWritePost *.agda execute "normal! :CornelisLoad\<CR>"
```

If you'd like to automatically load files when you open them too, try this:

```viml
function! CornelisLoadWrapper()
  if exists(":CornelisLoad") ==# 2
    CornelisLoad
  endif
endfunction

au BufReadPre *.agda call CornelisLoadWrapper()
au BufReadPre *.lagda* call CornelisLoadWrapper()
```

This won't work on the first Agda file you open due to a bug, but it will
successfully load subsequent files.


### Configuring Cornelis' Behavior

The max size of the info window can be set via:

```viml
let g:cornelis_max_size = 30
```

If you'd prefer your info window to appear somewhere else, you can set
`g:cornelis_split_location` (previously `g:cornelis_split_direction`), e.g.

```viml
let g:cornelis_split_location = 'vertical'
```

The following configuration options are available:

- `horizontal`: The default, opens in a horizontal split respecting `splitbelow`.
- `vertical`: Opens in a vertical split respecting `splitright`.
- `top`: Opens at the top of the window.
- `bottom`: Opens at the bottom of the window.
- `left`: Opens at the left of the window.
- `right`: Opens at the right of the window.


## Contributing

I'm a noob at Agda, and I don't know what I don't know. If this plugin doesn't
have some necessary feature for you to get work done, please file a bug,
including both what's missing, and how you use it in your workflow. I'd love to
learn how to use Agda better! I can move quickly on feature requests.

If you'd like to get involved, feel free to tackle an issue on the tracker and
send a PR. I'd love to have you on board!

## Architecture

Cornelis spins up a new `BufferStuff` for each Agda buffer it encounters.
`BufferStuff` contains a handle to a unique `agda` instance, which can be used
to send commands. It also tracks things like the information window buffer,
in-scope goals, and whatever the last `DisplayInfo` response from `agda` was.

For each `BufferStuff`, we also spin up a new thread, blocking on responses
from `agda`. These responses all get redirected to a global worker thread, which
is responsible for dispatching on each command. Commands are typesafe, parsed
from JSON, and associated with the buffer they came from.
