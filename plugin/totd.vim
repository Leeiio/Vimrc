" File:  "totd.vim"
" URL:  http://vim.sourceforge.net/script.php?script_id=88
" LAST MODIFICATION: "Wed, 19 Nov 2006"
" Sourcing this script will display a "Tip Of The Day" when starting vim.
" Requires:  Vim version 6.0 or later.
"
" A skeleton file vimtips.txt should be included with this file.  You should
" be able to download an updated version from
" http://www.vim.org/tips/tip_download.php .
"
" Installation:  See doc/totd.txt, packaged with this file.

" TODO Make it fault-tolerant:  test for filereadable("vimtips.txt")
" Remind the user how to turn this feature off.
" Enable random selection of a tip.

" Prevent duplicate loading.
if exists("loaded_totd")
  finish
endif
let loaded_totd = 1

let s:save_cpo = &cpo
setlocal cpo&vim

let s:thisFile = expand("<sfile>:p")

command! TipOfTheDay call TipOfTheDay(s:thisFile, 'yes')

" Since plugins are loaded too early, use an autocommand to start up.
" Note that the functions and script variables are all defined when
" TipOfTheDay() gets called.
augroup TipOfTheDay
  autocmd! VimEnter * call TipOfTheDay(s:thisFile, 'no')
augroup END

fun! TipOfTheDay(file, force)
  let rtDir = fnamemodify(a:file, ":h:h")
  let currDate = strftime("%Y%m%d") + 0
  let currDate = strftime("%Y%m%d") + 0
  let currVimtips = strftime("%Y%m%d",
	\ getftime(rtDir . "/plugin/vimtips/vimtips.txt")) + 0
  " Bail out if we already had a tip today, unless a:force is 'yes',
  " or the vimtips.txt file has been updated.
  if currDate <= s:prevDate && a:force != 'yes' && currVimtips == s:vimtipsDate
    return
  endif
  let s:prevDate = currDate
  let s:vimtipsDate = currVimtips

  let save_cpo = &cpo
  let save_equalalways = &equalalways
  let save_lazyredraw = &lazyredraw
  setlocal cpo&vim noequalalways lazyredraw
  let save_errmsg = v:errmsg
  let v:errmsg = ""
  " Open the tips file:
  silent! help vimtips.txt
  if v:errmsg != ""
    call s:Install(rtDir)
    let v:errmsg = ""
    silent! help vimtips.txt
  endif
  if v:errmsg == ""
    " Find the next tip:
    execute s:prevLine
    call search('^VimTip\s\+\d\+:', 'w')
    " Save the new line number:
    let s:prevLine = line(".")
    " Later, restore this buffer with :execute restore
    let restore = "buffer " . bufnr("%")
    " Edit this file to update the persistent variables.
    silent execute "edit" a:file
    setlocal modifiable noswapfile linebreak
    let sbuf = bufnr("%") " buffer number of this script file
    " Update the values in the file.
    call s:SetPersistentNumber("prevLine", s:prevLine)
    call s:SetPersistentNumber("prevDate", s:prevDate)
    call s:SetPersistentNumber("vimtipsDate", s:vimtipsDate)
    silent write!
    " Return to the help window and delete the script buffer.
    execute restore
    execute "bwipe" sbuf
    " Position the tip at the top of the screen:
    normal! zt
  endif " v:errmsg == ""
  let v:errmsg = save_errmsg
  let &l:lazyredraw = save_lazyredraw
  let &l:equalalways = save_equalalways
  let &l:cpo = save_cpo
endfun

" Call this when the plugin is first installed, and after any time the user
" runs :helptags to rebuild doc/tags .
fun! s:Install(rtDir)
  " Later, restore this buffer with :execute restore
  if strlen(@%)	" This is not an empty buffer
    let restore = "buffer " . bufnr("%")
  else
    let restore = "enew"
  endif
  " Edit the tags file.
  execute "edit" a:rtDir . "/doc/tags"
  setlocal modifiable noswapfile
  let tagbuf = bufnr("%")
  " binary search...
  let top = 0	" Invariant:  the vimtips line goes after top.
  let bot = line("$")
  while top < bot
    let mid = (top + bot + 1)/2
    let midtag = matchstr(getline(mid), '[^\t]*')  " the tag on this line
    if s:Strcmp(midtag, "vimtips.txt") < 0
      let top = mid
    else
      let bot = mid - 1
    endif
  endwhile
  " Add the tag for vimtips.txt .  I know this function is only called when
  " the vimtips tag is not found, otherwise I would test whether it is already
  " there.
  call append(top, "vimtips.txt\t../plugin/vimtips/vimtips.txt\t/*vimtips.txt*")
  write!
  " Return to where we started and remove the extra buffer.
  execute restore
  execute "bwipeout" tagbuf
endfun

" Compare two strings by ASCII value:  suitable for binary search in a tags
" file.  Examples:
"   s:Strcmp("a", "ab") returns -98
"   s:Strcmp("ab", "ab") returns 0
"   s:Strcmp("ac", "ab") returns 1
fun! s:Strcmp(foo, bar)
  let i = 0
  while i < strlen(a:bar) && char2nr(a:foo[i]) == char2nr(a:bar[i])
    let i = i+1
  endwhile
  return char2nr(a:foo[i]) - char2nr(a:bar[i])
endfun

" Use the current file to store persistent variables.  Return 1 if not found.
fun! s:SetPersistentNumber(name, value)
  " Search, from end of file, for "let name = ..."
  $
  if !search('^\s*let\s\+s:' . a:name . '\s*=', 'bW')
    return 1
  endif
  silent execute 's/=.*/=' a:value
endfun

let s:prevDate = 20111114
let s:prevLine = 22
let s:vimtipsDate = 20111114

let &l:cpo = s:save_cpo

" vim:sw=2:sts=2:ff=unix:
