call nvimhs#start(expand('<sfile>:p:h:h'), 'cornelis', ['-v', 'DEBUG', '-l', '/tmp/cornelis.log'])
nnoremap <F5> :call nvimhs#compileAndRestart('cornelis')<CR>

runtime agda-input.vim
