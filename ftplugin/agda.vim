if exists("b:cornelis_ftplugin")
  finish
endif
let b:cornelis_ftplugin = 1

if exists("g:cornelis_use_global_binary")
  call remote#host#Register('cornelis', '*', rpcstart('cornelis', []))
else
  call nvimhs#start(expand('<sfile>:p:h:h'), 'cornelis', ['-v', 'DEBUG', '-l', '/tmp/cornelis.log'])
endif

nnoremap <F5> :call nvimhs#compileAndRestart('cornelis')<CR>

runtime agda-input.vim
runtime agda-matchpairs.vim

call textobj#user#plugin('cornelis', {
\   'zbrackets': {
\     'pattern': ['⟨', '⟩'],
\     'select-i': 'az',
\   },
\   'zspacebrackets': {
\     'pattern': ['⟨ ', ' ⟩'],
\     'select-i': 'iz',
\   },
\   'ibrackets': {
\     'pattern': ['⦃', '⦄'],
\     'select-a': 'ai',
\     'select-i': 'ii',
\   },
\   'hole': {
\     'pattern': ['{!', '!}'],
\     'select-a': 'ah',
\     'select-i': 'ih',
\   },
\ })

setlocal formatoptions-=t
setlocal formatoptions+=croql

setlocal comments=sfl:{-,mb1:--,ex:-},:--
setlocal commentstring=--\ %s

setlocal iskeyword=@,!-~,^\,,^\(,^\),^\",^\',192-255

