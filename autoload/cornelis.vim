function! cornelis#bind_input(key, result)
    let l:cornelis_agda_prefix = get(g:, "cornelis_agda_prefix", "<localleader>")
    let str = "<buffer> " . l:cornelis_agda_prefix . substitute(a:key, "|", "<bar>", "g") . " " . a:result
    exec "silent inoremap" . str
    exec "silent cnoremap" . str

    if !exists("g:agda_input")
      let g:agda_input = {}
    endif

    call extend(g:agda_input, {a:key[0:0]: {}}, "keep")
    let rest = ""
    if len(a:key[1:]) == 0
      let rest = "\<CR\>"
    else
      let rest = a:key[1:]
    endif

    call extend(g:agda_input[a:key[0:0]], {rest : [a:result, a:result]})
endfunction

function! cornelis#cleanup_and_quit()
  CornelisCloseInfoWindows 
  if bufnr() == 1 
    qa
  endif 
endfunction

augroup cornelis#Quit
      autocmd! * <buffer>
      autocmd QuitPre <buffer>
        \ if len(win_findbuf(expand('<abuf>'))) == 1 | call cornelis#cleanup_and_quit() | endif
augroup END
