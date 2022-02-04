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

It supports highlighting, goal listing, type-context, refinement, solving,
case splitting, and go-to definition. These are exposed via the vim commands:

```
:CornelisLoad
:CornelisGoals
:CornelisTypeContext
:CornelisRefine
:CornelisSolve
:CornelisMakeCase
:CornelisGoToDefinition
```

A note on `CornelisGoToDefinition` --- for whatever weird technical reason
(vim's fault), goto definition only works if your cursor is on the first
character of the identifier name.

Additionally, there is minor support for agda-input via your `<LocalLeader>` in
insert mode. See [agda-input.vim](https://github.com/isovector/cornelis/blob/master/agda-input.vim)
for available bindings.


## Text Objects

Use the `iz`/`az` text objects to operate on text between `≡⟨` and `⟩`.

Also `ii`/`ai` will operate on `⦃` and `⦄`.



## Installation

```
Plug 'kana/vim-textobj-user'
Plug 'neovimhaskell/nvim-hs.vim'
Plug 'isovector/cornelis'
```

Make sure you have [`stack`](https://docs.haskellstack.org/en/stable/install_and_upgrade/) on your PATH!


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
    nnoremap <buffer> <leader>n :CornelisSolve<CR>
    nnoremap <buffer> gd        :CornelisGoToDefinition<CR>
endfunction
```

Feeling spicy? Automatically run `CornelisLoad` every time you save the file.

```haskell
au BufWritePost *.agda execute "normal! :CornelisLoad\<CR>"
```

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


