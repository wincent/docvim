" This test shows how we disambiguate between `:endf` and `:endfo`.
fu! A()
  for l:item in getqflist()
    echo l:item
  endfo
endf

fu! B()
  for l:item in getqflist()
    echo l:item
  endfor
endf

fu! C()
  for l:item in getqflist()
    echo l:item
  endfo
endfuncti

fu! D()
  for l:item in getqflist()
    echo l:item
  endfo
endfunction
