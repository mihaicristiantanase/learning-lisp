let g:LispTermBuffer = 2

function! SendToTerm(keys)
  if bufexists(g:LispTermBuffer)
    let keys = substitute(a:keys, '\n$', '', '')
    call term_sendkeys(g:LispTermBuffer, keys . "\<cr>")
  else
    echom "Error: No terminal"
  endif
endfunction

fun! ExecuteFunctionInREPLFun()
  :norm mzyap`z
  let keys = substitute(@*, '^\n*\(.\{-}\)\n*$', '\1', '')
  :call SendToTerm(keys)
endf

fun! ExecuteFileInREPLFun()
  :norm mzgg0yG`z
  let keys = substitute(@*, '^\n*\(.\{-}\)\n*$', '\1', '')
  :call SendToTerm(keys)
endf

nmap \r :call ExecuteFunctionInREPLFun()<CR>
nmap \R :call ExecuteFileInREPLFun()<CR>
