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

call textobj#user#plugin('cornelis', {
\   'zbrackets': {
\     'pattern': ['⟨', '⟩'],
\     'select-a': 'az',
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

setlocal commentstring=--\ %s

