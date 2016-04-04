" Bang form.
lexpr! system(&grepprg . ' ' . g:command)

" Vanilla.
lexpr s:MyFancyFunc()

" We support the abbreviation too.
lex autoloaded#function("blah").
