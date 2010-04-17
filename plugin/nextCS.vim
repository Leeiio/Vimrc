"nextCS.vim: change your CS (color scheme)
"Press key:
"   F12         next scheme
"   F11         previous scheme

if exists("loaded_nextCS") || exists('loaded_setcolors')
    finish
endif

let loaded_nextCS = 1

function! l:avoidECN()
    if exists('g:colors_name')
        let result = index(g:colorSchemesDetected, g:colors_name)
        if result == -1
            let g:colorSchemesDetected = remove(g:colorSchemesDetected, i)
        endif
    else
        if exists('g:current')
            let g:current += 1
        else
            let g:current = -1
        endif
    endif
endfunction

function! l:getCS() "getColorSheme
    "this search in the color directories for *.vim files and add them to
    "colorSchemesDetected 
    let g:colorSchemesDetected = map(split(globpath(&runtimepath, "colors/*.vim", "\n")), 'fnamemodify(v:val, ":t:r")')

    if empty(g:colorSchemesDetected)
        echo 'You do not have any color file'
        finish
    endif
    "echo g:colorSchemesDetected
    "sometimes g:colors_name and file names don't match
    call l:avoidECN() "avoidEvilColorNames

    let g:current = index(g:colorSchemesDetected, g:colors_name)
    let g:CSloaded = 1
endfunction

function! NextCS()
    if (!exists('g:CSloaded'))
        call l:getCS()
    endif

    let g:current += 1

    if !(0 <= g:current && g:current < len(g:colorSchemesDetected))
       let g:current = (g:current == len(g:colorSchemesDetected) ? 0 : len(g:colorSchemesDetected)-1)  
    endif
    try
        execute 'colorscheme' . " " . g:colorSchemesDetected[g:current]
    catch /E185:/
        call l:avoidECN()
    endtry
    redraw!
    echo g:colorSchemesDetected[g:current]
endfunction

function! PreviousCS()
    if (!exists('g:CSloaded'))
        call l:getCS()
    endif

    let g:current -= 1

    if !(0 <= g:current && g:current < len(g:colorSchemesDetected))
       let g:current = (g:current == len(g:colorSchemesDetected) ? 0 : len(g:colorSchemesDetected)-1)  
    endif
    try
        execute 'colorscheme' . " " . g:colorSchemesDetected[g:current]
    catch /E185:/
        call l:avoidECN()
    endtry
    redraw!
    echo g:colorSchemesDetected[g:current]
endfunction

nnoremap <c-F3> :call NextCS() <CR>
nnoremap <c-F4> :call PreviousCS() <CR>
