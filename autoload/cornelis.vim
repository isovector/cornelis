function! cornelis#prompt_input()
  if !empty(globpath(&rtp, "autoload/which_key.vim"))
    call which_key#start(0, !0, g:agda_input)
  else
    echoerr "Please install https://github.com/liuchengxu/vim-which-key before using #prompt_input()"
  endif
endfunction

function! cornelis#standard_bind_input(key, result)
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

function! cornelis#bind_input(key, result)
  if exists("g:cornelis_bind_input_hook")
    call nvim_call_function(g:cornelis_bind_input_hook, [a:key, a:result])
  else
    call cornelis#standard_bind_input(a:key, a:result)
  endif
endfunction

