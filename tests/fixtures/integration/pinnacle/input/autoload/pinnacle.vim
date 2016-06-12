""
" @plugin Pinnacle Highlight group manipulation for Vim
"
" # Intro
"
" Pinnacle provides functions for manipulating |:highlight| groups.
"
"
" # Installation
"
" To install Pinnacle, use your plug-in management system of choice.
"
" If you don't have a "plug-in management system of choice", I recommend
" Pathogen (https://github.com/tpope/vim-pathogen) due to its simplicity and
" robustness. Assuming that you have Pathogen installed and configured, and that
" you want to install vim-docvim into `~/.vim/bundle`, you can do so with:
"
" ```
" git clone https://github.com/wincent/pinnacle.git ~/.vim/bundle/pinnacle
" ```
"
" Alternatively, if you use a Git submodule for each Vim plug-in, you could do
" the following after `cd`-ing into the top-level of your Git superproject:
"
" ```
" git submodule add https://github.com/wincent/pinnacle.git ~/vim/bundle/pinnacle
" git submodule init
" ```
"
" To generate help tags under Pathogen, you can do so from inside Vim with:
"
" ```
" :call pathogen#helptags()
" ```
"
"
" # Website
"
" The official Pinnacle source code repo is at:
"
"   http://git.wincent.com/pinnacle.git
"
" Mirrors exist at:
"
"   - https://github.com/wincent/pinnacle
"   - https://gitlab.com/wincent/pinnacle
"   - https://bitbucket.org/ghurrell/pinnacle
"
" Official releases are listed at:
"
"   http://www.vim.org/scripts/script.php?script_id=5360
"
"
" # License
"
" Copyright (c) 2016-present Greg Hurrell
"
" Permission is hereby granted, free of charge, to any person obtaining
" a copy of this software and associated documentation files (the
" "Software"), to deal in the Software without restriction, including
" without limitation the rights to use, copy, modify, merge, publish,
" distribute, sublicense, and/or sell copies of the Software, and to
" permit persons to whom the Software is furnished to do so, subject to
" the following conditions:
"
" The above copyright notice and this permission notice shall be
" included in all copies or substantial portions of the Software.
"
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
" EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
" MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
" NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
" LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
" OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
" WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
"
"
" # Development
"
" ## Contributing patches
"
" Patches can be sent via mail to greg@hurrell.net, or as GitHub pull requests
" at: https://github.com/wincent/pinnacle/pulls
"
" ## Cutting a new release
"
" At the moment the release process is manual:
"
" - Perform final sanity checks and manual testing
" - Update the |pinnacle-history| section of the documentation
" - Verify clean work tree:
"
" ```
" git status
" ```
"
" - Tag the release:
"
" ```
" git tag -s -m "$VERSION release" $VERSION
" ```
"
" - Publish the code:
"
" ```
" git push origin master --follow-tags
" git push github master --follow-tags
" ```
"
" - Produce the release archive:
"
" ```
" git archive -o vim-docvim-$VERSION.zip HEAD -- .
" ```
"
" - Upload to http://www.vim.org/scripts/script.php?script_id=5360
"
"
" # Authors
"
" Pinnacle is written and maintained by Greg Hurrell <greg@hurrell.net>.
"
"
" # History
"
" ## 0.1 (30 March 2016)
"
" - Initial release.

" Replaces newlines with spaces.
function! pinnacle#sub_newlines(string) abort
  return tr(a:string, "\r\n", '  ')
endfunction

" Runs a command and returns the captured output as a single line.
"
" Useful when we don't want to let long lines on narrow windows produce unwanted
" embedded newlines.
function! pinnacle#capture_line(command) abort
  redir => l:capture
  execute a:command
  redir END

  return pinnacle#sub_newlines(l:capture)
endfunction

" Gets the current value of a highlight group.
function! pinnacle#capture_highlight(group) abort
  return pinnacle#capture_line('0verbose silent highlight ' . a:group)
endfunction

" Extracts a highlight string from a group, recursively traversing linked
" groups, and returns a string suitable for passing to `:highlight`.
function! pinnacle#extract_highlight(group) abort
  let l:group = pinnacle#capture_highlight(a:group)

  " Traverse links back to authoritative group.
  while l:group =~# 'links to'
    let l:index = stridx(l:group, 'links to') + len('links to')
    let l:linked = strpart(l:group, l:index + 1)
    let l:group = pinnacle#capture_highlight(l:linked)
  endwhile

  " Extract the highlighting details (the bit after "xxx")
  let l:matches = matchlist(l:group, '\<xxx\>\s\+\(.*\)')
  let l:original = l:matches[1]
  return l:original
endfunction

" Returns an italicized copy of `group` suitable for passing to `:highlight`.
function! pinnacle#italicize(group) abort
  return pinnacle#decorate('italic', a:group)
endfunction

" Returns a bold copy of `group` suitable for passing to `:highlight`.
function! pinnacle#embolden(group) abort
  return pinnacle#decorate('bold', a:group)
endfunction

" Returns a copy of `group` decorated with `style` (eg. "bold", "italic" etc)
" suitable for passing to `:highlight`.
function! pinnacle#decorate(style, group) abort
  let l:original = pinnacle#extract_highlight(a:group)

  for l:lhs in ['gui', 'term', 'cterm']
    " Check for existing setting.
    let l:matches = matchlist(
      \   l:original,
      \   '^\([^ ]\+ \)\?' .
      \   '\(' . l:lhs . '=[^ ]\+\)' .
      \   '\( .\+\)\?$'
      \ )
    if l:matches == []
      " No setting, add one with just a:style in it
      let l:original .= ' ' . l:lhs . '=' . a:style
    else
      " Existing setting; check whether a:style is already in it.
      let l:start = l:matches[1]
      let l:value = l:matches[2]
      let l:end = l:matches[3]
      if l:value =~# '.*' . a:style . '.*'
        continue
      else
        let l:original = l:start . l:value . ',' . a:style . l:end
      endif
    endif
  endfor

  return pinnacle#sub_newlines(l:original)
endfunction
