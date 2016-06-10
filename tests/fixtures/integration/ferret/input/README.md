<p align="center">
<img src="https://raw.githubusercontent.com/wincent/ferret/media/ferret.jpg" />
<img src="https://raw.githubusercontent.com/wincent/ferret/media/ferret.gif" />
</p>
# ferret

## Intro

> "ferret (verb)<br />(ferret something out) search tenaciously for and find something: she had the ability to ferret out the facts."

<p align="right"><a name="ferret-features" href="#user-content-ferret-features"><code>ferret-features</code></a></p>
Ferret improves Vim's multi-file search in four ways:

### 1. Powerful multi-file search

Ferret provides an <strong>[`:Ack`](#user-content-ack)</strong> command for searching across multiple files using The Silver Searcher (https://github.com/ggreer/the_silver_searcher), Ack (http://beyondgrep.com/), or Grep (http://www.gnu.org/software/grep/). Support for passing options through to the underlying search command exists, along with the ability to use full regular expression syntax without doing special escaping.

Shortcut mappings are provided to start an <strong>[`:Ack`](#user-content-ack)</strong> search (<leader>a) or to search for the word currently under the cursor (<leader>s).

Results are normally displayed in the <strong>`quickfix`</strong> window, but Ferret also provides a <strong>[`:Lack`](#user-content-lack)</strong> command that behaves like <strong>[`:Ack`](#user-content-ack)</strong> but uses the <strong>`location-list`</strong> instead, and a <leader>l mapping as a shortcut to <strong>[`:Lack`](#user-content-lack)</strong>.

Finally, Ferret offers integration with dispatch.vim (https://github.com/tpope/vim-dispatch), which enables asynchronous searching despite the fact that Vim itself is single-threaded.

### 2. Streamlined multi-file replace

The companion to <strong>[`:Ack`](#user-content-ack)</strong> is <strong>[`:Acks`](#user-content-acks)</strong> (mnemonic: "Ack substitute", accessible via shortcut <leader>r), which allows you to run a multi-file replace across all the files placed in the <strong>`quickfix`</strong> window by a previous invocation of <strong>[`:Ack`](#user-content-ack)</strong>.

### 3. Quickfix listing enhancements

The <strong>`quickfix`</strong> listing itself is enhanced with settings to improve its usability, and natural mappings that allow quick removal of items from the list (for example, you can reduce clutter in the listing by removing lines that you don't intend to make changes to).

Additionally, Vim's <strong>`:cn`</strong>, <strong>`:cp`</strong>, <strong>`:cnf`</strong> and <strong>`:cpf`</strong> commands are tweaked to make it easier to immediately identify matches by centering them within the viewport.

### 4. Easy operations on files in the quickfix listing

Finally, Ferret provides a <strong>[`:Qargs`](#user-content-qargs)</strong> command that puts the files currently in the <strong>`quickfix`</strong> listing into the <strong>`:args`</strong> list, where they can be operated on in bulk via the <strong>`:argdo`</strong> command. This is what's used under the covers by <strong>[`:Acks`](#user-content-acks)</strong> to do its work.

## Installation

To install Ferret, use your plug-in management system of choice.

If you don't have a "plug-in management system of choice", I recommend Pathogen (https://github.com/tpope/vim-pathogen) due to its simplicity and robustness. Assuming that you have Pathogen installed and configured, and that you want to install Ferret into `~/.vim/bundle`, you can do so with:

```
git clone https://github.com/wincent/ferret.git ~/.vim/bundle/ferret
```

Alternatively, if you use a Git submodule for each Vim plug-in, you could do the following after `cd`-ing into the top-level of your Git superproject:

```
git submodule add https://github.com/wincent/ferret.git ~/vim/bundle/ferret
git submodule init
```

To generate help tags under Pathogen, you can do so from inside Vim with:

```
:call pathogen#helptags()
```

## Options

## Commands

<p align="right"><a name="ack" href="#user-content-ack"><code>:Ack</code></a></p>
### `:Ack {pattern} {options}`

Searches for {pattern} in all the files under the current directory (see <strong>`:pwd`</strong>), unless otherwise overridden via {options}, and displays the results in the <strong>`quickfix`</strong> listing.

`ag` (The Silver Searcher) will be used preferentially if present on the system, because it is faster, falling back to `ack` and then `grep` as needed.

If dispatch.vim is installed the search process will run asynchronously via the <strong>`:Make`</strong> command, otherwise it will be run synchronously via <strong>`:cexpr`</strong>. Asynchronous searches are preferred because they do not block, despite the fact that Vim itself is single threaded. The <strong>`g:FerretDispatch`</strong> option can be used to prevent the use of dispatch.vim.

The {pattern} is passed through as-is to the underlying search program, and no escaping is required other than preceding spaces by a single backslash. For example, to search for "\bfoo[0-9]{2} bar\b" (ie. using `ag`'s Perl-style regular expression syntax), you could do:

```
:Ack \bfoo[0-9]{2}\ bar\b
```

Likewise, {options} are passed through. In this example, we pass the `-w` option (to search on word boundaries), and scope the search to the "foo" and "bar" subdirectories: >

```
:Ack -w something foo bar
```

As a convenience <leader>a is set-up (<strong>[`<Plug>(FerretAck)`](#user-content-plugferretack)</strong>) as a shortcut to enter <strong>`Cmdline-mode`</strong> with `:Ack` inserted on the <strong>`Cmdline`</strong>. Likewise <leader>s (<strong>[`<Plug>(FerretAckWord)`](#user-content-plugferretackword)</strong>) is a shortcut for running <strong>[`:Ack`](#user-content-ack)</strong> with the word currently under the cursor.

<p align="right"><a name="lack" href="#user-content-lack"><code>:Lack</code></a></p>
### `:Lack {pattern} {options}`

Just like <strong>[`:Ack`](#user-content-ack)</strong>, but instead of using the <strong>`quickfix`</strong> listing, which is global across an entire Vim instance, it uses the <strong>`location-list`</strong>, which is a per-window construct.

Note that <strong>[`:Lack`](#user-content-lack)</strong> always runs synchronously via <strong>`:cexpr`</strong>, because dispatch.vim doesn't currently support the <strong>`location-list`</strong>.

<p align="right"><a name="acks" href="#user-content-acks"><code>:Acks</code></a></p>
### `:Acks /{pattern}/{replacement}/`

Takes all of the files currently in the <strong>`quickfix`</strong> listing and performs a substitution of all instances of {pattern} (a standard Vim search <strong>`pattern`</strong>) by {replacement}.

A typical sequence consists of an <strong>[`:Ack`](#user-content-ack)</strong> invocation to populate the <strong>`quickfix`</strong> listing and then <strong>[`:Acks`](#user-content-acks)</strong> (mnemonic: "Ack substitute") to perform replacements. For example, to replace "foo" with "bar" across all files in the current directory:

```
:Ack foo
:Acks /foo/bar/
```

<p align="right"><a name="qargs" href="#user-content-qargs"><code>:Qargs</code></a></p>
### `:Qargs`

This is a utility function that is used by the <strong>[`:Acks`](#user-content-acks)</strong> command but is also generally useful enough to warrant being exposed publicly.

It takes the files currently in the <strong>`quickfix`</strong> listing and sets them as <strong>`:args`</strong> so that they can be operated on en masse via the <strong>`:argdo`</strong> command.

## Mappings

### Circumstances where mappings do not get set up

Note that Ferret will not try to set up the <leader> mappings if any of the following are true:

- A mapping for already exists.
- An alternative mapping for the same functionality has already been set up from a <strong>`.vimrc`</strong>.
- The mapping has been suppressed by setting <strong>`g:FerretMap`</strong> to 1 in your <strong>`.vimrc`</strong>.

### Mappings specific to the quickfix window

Additionally, Ferret will set up special mappings in <strong>`quickfix`</strong> listings, unless prevented from doing so by <strong>`g:FerretQFMap`</strong>:

- `d` (<strong>`visual-mode`</strong>): delete visual selection
- `dd` (<strong>`Normal-mode`</strong>): delete current line
- `d`{motion} (<strong>`Normal-mode`</strong>): delete range indicated by {motion}

### `<Plug>(FerretAck)`

Ferret maps <leader>a to <strong>[`<Plug>(FerretAck)`](#user-content-plugferretack)</strong>, which triggers the <strong>[`:Ack`](#user-content-ack)</strong> command. To use an alternative mapping instead, create a different one in your <strong>`.vimrc`</strong> instead using <strong>`:nmap`</strong>:

```
" Instead of <leader>a, use <leader>x.
nmap <leader>x <Plug>(FerretAck)
```

### `<Plug>(FerretLack)`

Ferret maps <leader>l to <strong>[`<Plug>(FerretLack)`](#user-content-plugferretlack)</strong>, which triggers the <strong>[`:Lack`](#user-content-lack)</strong> command. To use an alternative mapping instead, create a different one in your <strong>`.vimrc`</strong> instead using <strong>`:nmap`</strong>:

```
" Instead of <leader>l, use <leader>y.
nmap <leader>y <Plug>(FerretLack)
```

### `<Plug>(FerretAckWord)`

Ferret maps <leader>s (mnemonix: "selection) to <strong>[`<Plug>(FerretAckWord)`](#user-content-plugferretackword)</strong>, which uses <strong>[`:Ack`](#user-content-ack)</strong> to search for the word currently under the cursor. To use an alternative mapping instead, create a different one in your <strong>`.vimrc`</strong> instead using <strong>`:nmap`</strong>:

```
" Instead of <leader>s, use <leader>z.
nmap <leader>z <Plug>(FerretAckWord)
```

### `<Plug>(FerretAcks)`

Ferret maps <leader>r (mnemonic: "replace") to <strong>[`<Plug>(FerretAcks)`](#user-content-plugferretacks)</strong>, which triggers the <strong>[`:Acks`](#user-content-acks)</strong> command and fills the prompt with the last search term from Ferret. to use an alternative mapping instead, create a different one in your <strong>`.vimrc`</strong> instead using <strong>`:nmap`</strong>:

```
" Instead of <leader>r, use <leader>u.
nmap <leader>u <Plug>(FerretAcks)
```

<p align="right"><a name="gferretdispatch" href="#user-content-gferretdispatch"><code>g:FerretDispatch</code></a></p>
### `g:FerretDispatch` (boolean, default: 1)

Controls whether to use vim-dispatch (and specifically, <strong>`:Make`</strong>) to run <strong>[`:Ack`](#user-content-ack)</strong> searches asynchronously, when available. To prevent vim-dispatch from being used, set to 0:

```
let g:FerretDispatch=0
```

<p align="right"><a name="gferrethlsearch" href="#user-content-gferrethlsearch"><code>g:FerretHlsearch</code></a></p>
### `g:FerretHlsearch` (boolean, default: none)

Controls whether Ferret should attempt to highlight the search pattern when running <strong>[`:Ack`](#user-content-ack)</strong> or <strong>[`:Lack`](#user-content-lack)</strong>. If left unset, Ferret will respect the current 'hlsearch' setting. To force highlighting on or off irrespective of 'hlsearch', set <strong>`g:FerretHlsearch`</strong> to 1 (on) or 0 (off):

```
let g:FerretHlsearch=0
```

<p align="right"><a name="gferretqfoptions" href="#user-content-gferretqfoptions"><code>g:FerretQFOptions</code></a></p>
### `g:FerretQFOptions` (boolean, default: 1)

Controls whether to set up setting overrides for <strong>`quickfix`</strong> windows. These are various settings, such as <strong>`norelativenumber`</strong>, <strong>`nolist`</strong> and <strong>`nowrap`</strong>, that are intended to make the <strong>`quickfix`</strong> window, which is typically very small relative to other windows, more usable.

A full list of overridden settings can be found in <strong>[`ferret-overrides`](#user-content-ferret-overrides)</strong>.

To prevent the custom settings from being applied, set <strong>`g:FerretQFOptions`</strong> to 0:

```
let g:FerretQFOptions=0
```

<p align="right"><a name="gferretqfmap" href="#user-content-gferretqfmap"><code>g:FerretQFMap</code></a></p>
### `g:FerretQFMap` (boolean, default: 1)

Controls whether to set up mappings in the <strong>`quickfix`</strong> results window for deleting results. The mappings include:

- `d` (<strong>`visual-mode`</strong>): delete visual selection
- `dd` (<strong>`Normal-mode`</strong>): delete current line
- `d`{motion} (<strong>`Normal-mode`</strong>): delete range indicated by {motion}

To prevent these mappings from being set up, set to 0:

```
let g:FerretQFMap=0
```

<p align="right"><a name="gferretloaded" href="#user-content-gferretloaded"><code>g:FerretLoaded</code></a></p>
### `g:FerretLoaded` (any, default: none)

To prevent Ferret from being loaded, set <strong>`g:FerretLoaded`</strong> to any value in your <strong>`.vimrc`</strong>. For example:

```
let g:FerretLoaded=1
```

<p align="right"><a name="gferretmap" href="#user-content-gferretmap"><code>g:FerretMap</code></a></p>
### `g:FerretMap` (boolean, default: 1)

Controls whether to set up the Ferret mappings, such as <strong>[`<Plug>(FerretAck)`](#user-content-plugferretack)</strong> (see <strong>[`ferret-mappings`](#user-content-ferret-mappings)</strong> for a full list). To prevent any mapping from being configured, set to 0:

```
let g:FerretMap=0
```

<p align="right"><a name="gferretqfcommands" href="#user-content-gferretqfcommands"><code>g:FerretQFCommands</code></a></p>
### `g:FerretQFCommands` (boolean, default: 1)

Controls whether to set up custom versions of the <strong>`quickfix`</strong> commands, <strong>`:cn`</strong>, <strong>`:cnf`</strong>, <strong>`:cp`</strong> an <strong>`:cpf`</strong>. These overrides vertically center the match within the viewport on each jump. To prevent the custom versions from being configured, set to 0:

```
let g:FerretQFCommands=0
```

## Custom autocommands

<p align="right"><a name="ferretdidwrite" href="#user-content-ferretdidwrite"><code>FerretDidWrite</code></a> <a name="ferretwillwrite" href="#user-content-ferretwillwrite"><code>FerretWillWrite</code></a></p>
For maximum compatibility with other plug-ins, Ferret runs the following "User" autocommands before and after running the file writing operations during <strong>[`:Acks`](#user-content-acks)</strong>:

- FerretWillWrite
- FerretDidWrite

For example, to call a pair of custom functions in response to these events, you might do:

```
autocmd! User FerretWillWrite
autocmd User FerretWillWrite call CustomWillWrite()
autocmd! User FerretDidWrite
autocmd User FerretDidWrite call CustomDidWrite()
```

## Overrides

Ferret overrides the 'grepformat' and 'grepprg' settings, preferentially setting `ag`, `ack` or `grep` as the 'grepprg' (in that order) and configuring a suitable 'grepformat'.

Additionally, Ferret includes an <strong>`ftplugin`</strong> for the <strong>`quickfix`</strong> listing that adjusts a number of settings to improve the usability of search results.

<p align="right"><a name="ferret-nolist" href="#user-content-ferret-nolist"><code>ferret-nolist</code></a></p>
'nolist'

Turned off to reduce visual clutter in the search results, and because 'list' is most useful in files that are being actively edited, which is not the case for <strong>`quickfix`</strong> results.

<p align="right"><a name="ferret-norelativenumber" href="#user-content-ferret-norelativenumber"><code>ferret-norelativenumber</code></a></p>
'norelativenumber'

Turned off, because it is more useful to have a sense of absolute progress through the results list than to have the ability to jump to nearby results (especially seeing as the most common operations are moving to the next or previous file, which are both handled nicely by <strong>`:cnf`</strong> and <strong>`:cpf`</strong> respectively).

<p align="right"><a name="ferret-nowrap" href="#user-content-ferret-nowrap"><code>ferret-nowrap</code></a></p>
'nowrap'

Turned off to avoid ugly wrapping that makes the results list hard to read, and because in search results, the most relevant information is the filename, which is on the left and is usually visible even without wrapping.

<p align="right"><a name="ferret-number" href="#user-content-ferret-number"><code>ferret-number</code></a></p>
'number'

Turned on to give a sense of absolute progress through the results.

<p align="right"><a name="ferret-scrolloff" href="#user-content-ferret-scrolloff"><code>ferret-scrolloff</code></a></p>
'scrolloff'

Set to 0 because the <strong>`quickfix`</strong> listing is usually small by default, so trying to keep the current line away from the edge of the viewpoint is futile; by definition it is usually near the edge.

<p align="right"><a name="ferret-nocursorline" href="#user-content-ferret-nocursorline"><code>ferret-nocursorline</code></a></p>
'nocursorline'

Turned off to reduce visual clutter.

To prevent any of these <strong>`quickfix`</strong>-specific overrides from being set up, you can set <strong>`g:FerretQFOptions`</strong> to 0 in your <strong>`.vimrc`</strong>:

```
let g:FerretQFOptions=0
```

## Troubleshooting

<p align="right"><a name="ferret-quotes" href="#user-content-ferret-quotes"><code>ferret-quotes</code></a></p>
### Ferret fails to find patterns containing spaces

As described in the documentation for <strong>[`:Ack`](#user-content-ack)</strong>, the search pattern is passed through as-is to the underlying search command, and no escaping is required other than preceding spaces by a single backslash.

So, to find "foo bar", you would search like:

```
:Ack foo\ bar
```

Unescaped spaces in the search are treated as argument separators, so a command like the following means pass the `-w` option through, search for pattern "foo", and limit search to the "bar" directory:

```
:Ack -w foo bar
```

Note that including quotes will not do what you intend.

```
 " Search for '"foo' in the 'bar"' directory:
 :Ack "foo bar"

 " Search for "'foo' in the "bar'" directory:
 :Ack 'foo bar'
```

This approach to escaping is taken in order to make it straightfoward to use powerful Perl-compatible regular expression syntax in an unambiguous way without having to worry about shell escaping rules:

```
:Ack \blog\((['"]).*?\1\) -i --ignore-dir=src/vendor src dist build
```

## FAQ

### Why do Ferret commands start with "Ack", "Lack" and so on?

Ferret was originally the thinnest of wrappers (7 lines of code in my <strong>`.vimrc`</strong>) around `ack`. The earliest traces of it can be seen in the initial commit to my dotfiles repo in May, 2009 (https://wt.pe/h).

So, even though Ferret has a new name now and actually prefers `ag` over `ack` when available, I prefer to keep the command names intact and benefit from years of accumulated muscle-memory.

## Related

Just as Ferret aims to improve the multi-file search and replace experience, Loupe does the same for within-file searching:

https://github.com/wincent/loupe

## Website

The official Ferret source code repo is at:

http://git.wincent.com/ferret.git

A mirror exists at:

https://github.com/wincent/ferret

Official releases are listed at:

http://www.vim.org/scripts/script.php?script_id=5220

## License

Copyright 2015-present Greg Hurrell. All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

## Development

### Contributing patches

Patches can be sent via mail to greg@hurrell.net, or as GitHub pull requests at: https://github.com/wincent/ferret/pulls

### Cutting a new release

At the moment the release process is manual:

- Perform final sanity checks and manual testing
- Update the <strong>[`ferret-history`](#user-content-ferret-history)</strong> section of the documentation
- Verify clean work tree:

```
git status
```

- Tag the release:

```
git tag -s -m "$VERSION release" $VERSION
```

- Publish the code:

```
git push origin master --follow-tags
git push github master --follow-tags
```

- Produce the release archive:

```
git archive -o ferret-$VERSION.zip HEAD -- .
```

- Upload to http://www.vim.org/scripts/script.php?script_id=5220

## Authors

Ferret is written and maintained by Greg Hurrell <greg@hurrell.net>.

The idea for vim-dispatch integration was taken from Miles Sterrett's ack.vim plug-in (https://github.com/mileszs/ack.vim).

Other contributors that have submitted patches include (in alphabetical order):

- Daniel Silva
- Joe Lencioni
- Nelo-Thara Wallus
- Vaibhav Sagar

## History

### 1.2a (16 May 2016)

- Add optional support for running searches asynchronously using Vim's <strong>`+job`</strong> feature (enabled by default in sufficiently recent versions of Vim); see <strong>`g:FerretJob`</strong>, <strong>`:FerretCancelAsync`</strong> and <strong>`:FerretPullAsync`</strong>.

### 1.1.1 (7 March 2016)

- Fix another edge case when searching for patterns containing "#", only manifesting under dispatch.vim.

### 1.1 (7 March 2016)

- Fix edge case when searching for strings of the form "<foo>".
- Fix edge case when searching for patterns containing "#" and "%".
- Provide completion for `ag` and `ack` options when using <strong>[`:Ack`](#user-content-ack)</strong> and <strong>[`:Lack`](#user-content-lack)</strong>.
- Fix display of error messages under dispatch.vim.

### 1.0 (28 December 2015)

- Fix broken <strong>[`:Qargs`](#user-content-qargs)</strong> command (patch from Daniel Silva).
- Add <strong>`g:FerretQFHandler`</strong> and <strong>`g:FerretLLHandler`</strong> options (patch from Daniel Silva).
- Make <strong>`<Plug>`</strong> mappings accessible even <strong>`g:FerretMap`</strong> is set to 0.
- Fix failure to report filename when using `ack` and explicitly scoping search to a single file (patch from Daniel Silva).
- When using `ag`, report multiple matches per line instead of just the first (patch from Daniel Silva).
- Improve content and display of error messages.

### 0.3 (24 July 2015)

- Added highlighting of search pattern and related <strong>`g:FerretHlsearch`</strong> option (patch from Nelo-Thara Wallus).
- Add better error reporting for failed or incorrect searches.

### 0.2 (16 July 2015)

- Added <strong>[`FerretDidWrite`](#user-content-ferretdidwrite)</strong> and <strong>[`FerretWillWrite`](#user-content-ferretwillwrite)</strong> autocommands (patch from Joe Lencioni).
- Add <strong>[`<Plug>(FerretAcks)`](#user-content-plugferretacks)</strong> mapping (patch from Nelo-Thara Wallus).

### 0.1 (8 July 2015)

- Initial release, extracted from my dotfiles (https://github.com/wincent/wincent).
