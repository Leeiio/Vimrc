" Vim syntax file
" Language:	CSS (version 5)
" Maintainer:	hotoo <mail@xianyun.org>
" URL:		http://blog.xianyun.org
" Last Change:  2009 Dec 6
" License:      Public domain
"               (but let me know if you like :) )
"
" Note: This file just adds the new tags, prop, and attr from CSS 5
"       and don't replace default html.vim syntax file
"       put it to ~/.vim/syntax/        (*nix)
"                 $VIM/vimfiles/syntax/ (windows)

" HTML 5 tags
syn keyword cssTagName article aside audio bb canvas command datagrid
syn keyword cssTagName datalist details dialog embed figure footer
syn keyword cssTagName header hgroup keygen mark meter nav output
syn keyword cssTagName progress time ruby rt rp section time video

" HTML 5 Attribute
syn keyword cssCommonAttr contained contenteditable contextmenu draggable hidden item
syn keyword cssCommonAttr contained itemprop list subject spellcheck
" User-interface
syn match cssUIProp contained "\<box-sizing\>" containedin=ALL
syn match cssUIProp contained "\<outline-\(width\|style\|offset\|color\)\>" containedin=ALL
syn match cssUIProp contained "\<nav-\(index\|up\|right\|down\|left\)\>" containedin=ALL
syn keyword cssUIProp contained resize outline
" Other modules
syn keyword cssCommonAttr contained columns containedin=ALL
syn match cssCommonAttr contained "\<column-\(width\|span\|rule\|gap\|fill\|count\)\>" containedin=ALL
syn match cssCommonAttr contained "\<column-rule-\(color\|width\|style\)\>" containedin=ALL
syn match cssCommonAttr contained "\<column-break-\(after\|before\)\>" containedin=ALL

syn match cssBoxProp "\<border-\(image\|radius\)\=\>" contained containedin=ALL
syn match cssBoxProp "\<\(box-shadow\)\>" contained containedin=ALL

syn keyword cssColorProp contained opacity containedin=ALL

syn match cssTextAttr contained "\<text-shadow\|text-overflow\|word-wrap\>" containedin=ALL

syn match cssColorProp contained "\<background\(-\(origin\|clip\|size\)\)\=" containedin=ALL
