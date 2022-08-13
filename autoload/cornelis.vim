function! cornelis#bind_input(key, result)
    let l:agda_prefix = get(g:, "cornelis_agda_prefix", "<localleader>")
    let str = "<buffer>" . l:agda_prefix . substitute(a:key, "|", "<bar>", "g") . " " . a:result
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
