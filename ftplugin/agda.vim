call nvimhs#start(expand('<sfile>:p:h:h'), 'cornelis', ['-v', 'DEBUG', '-l', '/tmp/cornelis.log'])
nnoremap <F5> :call nvimhs#compileAndRestart('cornelis')<CR>

runtime agda-input.vim

call textobj#user#plugin('cornelis', {
\   'zbrackets': {
\     'pattern': ['⟨ ', ' ⟩'],
\     'select-a': 'az',
\     'select-i': 'iz',
\   },
\ })

setlocal commentstring=--\ %s

