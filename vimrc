"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
"             Leeiio的 Vim 配置文件
"
"         Author: Leeiio<guaniu@gmail.com>
"        Website: http://leeiio.me/
"          Since: 2010-02-22
"  Last Modified: 2010-10-14 14:28:01 leeiio
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"source $VIMRUNTIME/vimrc_example.vim

if v:version < 700
    echoerr 'This _vimrc requires Vim 7 or later.'
    quit
endif

" 获取当前目录
func! GetPWD()
    return substitute(getcwd(), "", "", "g")
endf

" 跳过页头注释，到首行实际代码
func! GotoFirstEffectiveLine()
    let l:c = 0
    while l:c<line("$") && (
                \ getline(l:c) =~ '^\s*$'
                \ || synIDattr(synID(l:c, 1, 0), "name") =~ ".*Comment.*"
                \ || synIDattr(synID(l:c, 1, 0), "name") =~ ".*PreProc$"
                \ )
        let l:c = l:c+1
    endwhile
    exe "normal ".l:c."Gz\<CR>"
endf

" 返回当前时期
func! GetDateStamp()
    return strftime('%Y-%m-%d')
endfunction

" 全选
func! SelectAll()
    let s:current = line('.')
    exe "norm gg" . (&slm == "" ? "VG" : "gH\<C-O>G")
endfunc

" =====================
" 环境配置
" =====================
" 中文帮助
set helplang=cn

" 保留历史记录
set history=500

" 行控制
set linebreak " 英文单词在换行时不被截断
set nocompatible " 设置不兼容VI
"set textwidth=80 " 设置每行80个字符自动换行，加上换行符
set wrap " 设置自动折行

" 标签页
set tabpagemax=15 " 最多15个标签
set showtabline=2 " 总是显示标签栏

" 关闭遇到错误时的声音提示
set noerrorbells
set novisualbell
set t_vb= " close visual bell

" 行号和标尺
set ruler " 显示标尺
set number " 行号
set rulerformat=%15(%c%V\ %p%%%)

" 命令行于状态行
set cmdheight=1 " 设置命令行的高度
set laststatus=2 " 始终显示状态行
set stl=\ [File]\ %F%m%r%h%y[%{&fileformat},%{&fileencoding}]\ %w\ \ [PWD]\ %r%{GetPWD()}%h\ %=\ [Line]%l/%L\ %=\[%P] "设置状态栏的信息

" 搜索
set hlsearch  " 高亮显示搜索的内容
set noincsearch " 关闭显示查找匹配过程
"set magic     " Set magic on, for regular expressions
"set showmatch " Show matching bracets when text indicator is over them
"set mat=2     " How many tenths of a second to blink

" 制表符(设置所有的tab和缩进为4个空格)
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab " 使用空格来替换tab
set smarttab

" 状态栏显示目前所执行的指令
set showcmd

" 缩进
set autoindent " 设置自动缩进
set smartindent " 设置智能缩进

" 自动重新读入
set autoread " 当文件在外部被修改，自动更新该文件

" 设定在任何模式下鼠标都可用
set mouse=a

" 插入模式下使用 <BS>、<Del> <C-W> <C-U>
set backspace=indent,eol,start

" 备份和缓存
set nobackup
set nowb
"set noswapfile

" 自动完成
set complete=.,w,b,k,t,i
set completeopt=longest,menu " 只在下拉菜单中显示匹配项目，并且会自动插入所有匹配项目的相同文本

" 代码折叠
set foldmethod=indent

" 带有如下符号的单词不要被换行分割
set iskeyword+=_,$,@,%,#,-

" 显示tab和空格
set list
" 设置tab和空格样式
set listchars=tab:\|\ ,nbsp:%,trail:-

" 设定行首tab为灰色
highlight LeaderTab guifg=#666666
" 匹配行首tab
match LeaderTab /\t/

set wildmenu "打开 wildmenu 选项，启动具有菜单项提示的命令行自动完成。
set matchpairs=(:),{:},[:],<:>
set whichwrap=b,s,<,>,[,]

"光标可以定位在没有实际字符的地方
set ve=block

" 启动后自动全屏
"set fullscreen

" 搜索时无视大小写
set ignorecase

if has("gui_running")
    " Turn undofile on
    set undofile
    " Set undofile path
    set undodir=~/tmp/vim/undofile/
endif

" Set hidden to undo buffer
set hidden

"cliboard seting
set clipboard+=unnamed " set clipboard

" =====================
"    默认为 UTF-8 编码
" =====================
if has("multi_byte")
    set encoding=utf-8
    " English messages only
    "language messages zh_CN.utf-8

    if has('win32')
        language english
        let &termencoding=&encoding " 处理consle输出乱码
    endif

    set fencs=utf-8,gbk,chinese,latin1
    set formatoptions+=mM
    set nobomb " 不使用 Unicode 签名

    if v:lang =~? '^\(zh\)\|\(ja\)\|\(ko\)'
        set ambiwidth=double
    endif
else
    echoerr "Sorry, this version of (g)vim was not compiled with +multi_byte"
endif


" =====================
" 图形界面
" =====================
if has('gui_running')
    "set guioptions=mcr " 只显示菜单
    "set guioptions=   " 隐藏全部的gui选项
    "set guioptions+=r " 显示gui右边滚动条
    "Toggle Menu and Toolbar 使用F2隐藏/显示菜单
    set guioptions-=m
    set guioptions-=T
    map <silent> <F3> :if &guioptions =~# 'T' <Bar>
            \set guioptions-=T <Bar>
            \set guioptions-=m <bar>
        \else <Bar>
            \set guioptions+=T <Bar>
            \set guioptions+=m <Bar>
        \endif<CR>

    if has('gui_macvim')
        set guioptions+=e
    endif

    if has("win32")
        " Windows 兼容配置
        source $VIMRUNTIME/mswin.vim

        " F11 最大化
        map <f11> :call libcallnr('fullscreen.dll', 'ToggleFullScreen', 0)<cr>

        " 字体配置
        "exec 'set guifont='.iconv('Courier_New', &enc, 'gbk').':h10:cANSI'
        "exec 'set guifontwide='.iconv('微软雅黑', &enc, 'gbk').':h10'
        set guifont=YaHei_Consolas_Hybrid:h12:cANSI
        set guifontwide=YaHei_Consolas_Hybrid:h12
    endif

    if has("unix") && !has('gui_macvim')
        set guifont=Courier\ 11\ Pitch\ 12
    endif

    if has("mac") || has("gui_macvim")
        if has("gui_macvim")
            " 开启抗锯齿渲染
            set anti
            " MacVim 下的字体配置
            "set guifont=Courier_New:h14
            "set guifontwide=YouYuan:h14
            set guifont=YaHei_Consolas_Hybrid:h13
            set guifontwide=YaHei_Consolas_Hybrid:h12

            "set transparency=8
            set lines=222 columns=222

            " 使用 MacVim 原生的全屏幕功能
            let s:lines=&lines
            let s:columns=&columns
            func! FullScreenEnter()
                set lines=999 columns=999
                set fu
            endf

            func! FullScreenLeave()
                let &lines=s:lines
                let &columns=s:columns
                set nofu
            endf

            func! FullScreenToggle()
                if &fullscreen
                    call FullScreenLeave()
                else
                    call FullScreenEnter()
                endif
            endf
        endif
    endif
endif


"去除当前所编辑文件的路径信息，只保留文件名
set guitablabel=%{ShortTabLabel()}
function ShortTabLabel()
    let bufnrlist = tabpagebuflist(v:lnum)
    let label = bufname(bufnrlist[tabpagewinnr(v:lnum) -1])
    let filename = fnamemodify(label, ':t')
    return filename
endfunction


" =====================
" 主题配色
" =====================
if has('syntax')
    " 保证语法高亮
    syntax on

    set background=light

    if has('gui_running')
        colorscheme solarized
        let g:colors_name="solarized"
    endif

    " 默认编辑器配色
    " au BufNewFile,BufRead,BufEnter,WinEnter * colo yytextmate

    " 各不同类型的文件配色不同
    "au BufNewFile,BufRead,BufEnter,WinEnter *.wiki colo moria

endif

"Highlight current
if has("gui_running")
    set cursorline
    set cursorcolumn
    "hi cursorline guibg=#0D142C
    "hi cursorline guibg=#FCF5C9
    "hi CursorColumn guibg=#FCF5C9
    "hi CursorColumn guibg=#FCF5C9
endif


" =====================
" AutoCmd 自动运行
" =====================
if has("autocmd")
    filetype plugin indent on " 打开文件类型检测

    "根据当前buffer切换到该文件所在工作目录
    "autocmd BufRead * :lcd! %:p:h

    augroup vimrcEx " 记住上次文件位置
        au!
        autocmd FileType text setlocal textwidth=80
        autocmd BufReadPost *
                    \ if line("'\"") > 0 && line("'\"") <= line("$") |
                    \   exe "normal g`\"" |
                    \ endif
    augroup END

    " Auto Check Syntax
    au BufWritePost,FileWritePost *.js,*.php call CheckSyntax(1)

    " JavaScript 语法高亮
    au FileType html,javascript let g:javascript_enable_domhtmlcss = 1
    au BufRead,BufNewFile *.js setf jquery

    " 给各语言文件添加 Dict
    if has('win32')
        au FileType php setlocal dict+=$VIM/vimfiles/dict/php_funclist.dict
        au FileType css setlocal dict+=$VIM/vimfiles/dict/css.dict
        au FileType javascript setlocal dict+=$VIM/vimfiles/dict/javascript.dict
    else
        au FileType php setlocal dict+=~/.vim/dict/php_funclist.dict
        au FileType css setlocal dict+=~/.vim/dict/css.dict
        au FileType javascript setlocal dict+=~/.vim/dict/javascript.dict
    endif

    " 格式化 JavaScript 文件
    au FileType javascript map <leader>jb :call g:Jsbeautify()<cr>
    au FileType javascript set omnifunc=javascriptcomplete#CompleteJS

    " 增加 ActionScript 语法支持
    au BufNewFile,BufRead,BufEnter,WinEnter,FileType *.as setf actionscript

    " CSS3 语法支持
    "au BufRead,BufNewFile *.css set ft=css syntax=css3

    " 增加 Objective-C 语法支持
    au BufNewFile,BufRead,BufEnter,WinEnter,FileType *.m,*.h setf objc

    " 将指定文件的换行符转换成 UNIX 格式
    au FileType php,javascript,html,css,python,vim,vimwiki set ff=unix
endif

" 自动载入VIM配置文件
autocmd! bufwritepost vimrc source $MYVIMRC

" 关闭VIM的时候保存会话，按F122读取会话
set sessionoptions=buffers,sesdir,help,tabpages,winsize
au VimLeave * mks! ~/Session.vim
nmap <F7> :so ~/Session.vim<CR>

" 自动刷新firefox
autocmd BufWriteCmd *.html,*.js,*.css,*.gtpl :call Refresh_firefox()
function! Refresh_firefox()
if &modified
write
silent !echo ‘vimYo = content.window.pageYOffset;
\ vimXo = content.window.pageXOffset;
\ BrowserReload();
\ content.window.scrollTo(vimXo,vimYo);
\ repl.quit();’ |
\ nc localhost 4242 2>&1 > /dev/null
endif
endfunction


" =====================
" 快捷键
" =====================
inoremap <C-A> <Home>
inoremap <C-E> <End>
inoremap <C-F> <Right>
inoremap <C-B> <Left>

"设置','为leader快捷键
let mapleader = ","
let g:mapleader = ","

"设置快速保存和退出
"快速保存为,s
"快速退出（保存）为,w
"快速退出（不保存）为,q
nmap <leader>s :w!<cr>
nmap <leader>w :wq!<cr>
nmap <leader>q :q!<cr>

nmap <C-t>   :tabnew<cr>
nmap <C-p>   :tabprevious<cr>
nmap <C-n>   :tabnext<cr>
nmap <C-k>   :tabclose<cr>
nmap <C-Tab> :tabnext<cr>

"切换buffer
nmap bn :bn<cr>
nmap bp :bp<cr>

" 插件快捷键
nmap <C-d> :NERDTree<cr>
nmap <C-e> :BufExplorer<cr>
nmap <f2>  :BufExplorer<cr>

" 插入模式按 F4 插入当前时间
imap <f4> <C-r>=GetDateStamp()<cr>

" 新建 XHTML 、PHP、Javascript 文件的快捷键
nmap <leader>cxhtml :NewQuickTemplateTab xhtml<cr>
nmap <leader>cphp :NewQuickTemplateTab php<cr>
nmap <leader>cjs :NewQuickTemplateTab javascript<cr>
nmap <leader>ccss :NewQuickTemplateTab css<cr>
nmap <leader>cjquery :NewQuickTemplateTab jquery<cr>

" 直接查看第一行生效的代码
nmap <C-g><C-f> :call GotoFirstEffectiveLine()<cr>

" 按下 Q 不进入 Ex 模式，而是退出
nmap Q :x<cr>

" 打开日历快捷键
map ca :Calendar<cr>

"Use spacebar toggle fold
nnoremap <space> @=((foldclosed(line('.')) < 0) ? 'zc' : 'zo')<CR>

"关闭自动检测编码用F6控制(fencview.vim)
let g:fencview_autodetect=0
map <F6> :FencView<cr>

" 快速修改 vimrc 文件
if has("win32")
    map <silent> <leader>ee :e $VIM/vimfiles/vimrc<cr>
    map <silent> <leader>rc :source $VIM/vimfiles/vimrc<cr> " 快速载入 vimrc 文件
else
    map <silent> <leader>ee :e ~/.vim/vimrc<cr>
    map <silent> <leader>rc :source ~/.vim/vimrc<cr> " 快速载入 vimrc 文件
endif

" 选中一段文字并全文搜索这段文字
vnoremap  *  y/<C-R>=escape(@", '\\/.*$^~[]')<CR><CR>
vnoremap  #  y?<C-R>=escape(@", '\\/.*$^~[]')<CR><CR>

map <leader>jt  <Esc>:%!json_xs -f json -t json-pretty<CR>

" 搭配minibuffer切换buffer
noremap <leader><leader> <C-^>

" 基本完美解决buffer切换，用buffer号加B 键
function! BufPos_ActivateBuffer(num)
    if a:num == 0
        echo "No buffer " . a:num "!"
        return
    endif
    if a:num > bufnr("$")
        echo "No buffer " . a:num "!"
    else
        if buflisted(a:num) && getbufvar(a:num, "&modifiable")
            exe "buffer " . a:num
        endif
    endif
endfunction
nmap b :<C-U>call BufPos_ActivateBuffer(v:count)<CR>


" =====================
" 插件配置
" =====================
"easyGrep
map f/ <esc>:Grep

let tlist_html_settings = 'html;h:Headers;o:Objects(ID);c:Classes'
let tlist_xhtml_settings = 'html;h:Headers;o:Objects(ID);c:Classes'

" showmarks setting
" Enable ShowMarks
let showmarks_enable = 1
" Show which marks
let showmarks_include = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890"
" Ignore help, quickfix, non-modifiable buffers
let showmarks_ignore_type = "hqm"
" Hilight lower & upper marks
let showmarks_hlline_lower = 1
let showmarks_hlline_upper = 1

" markbrowser setting
nmap <silent> <leader>bm :MarksBrowser<cr> 

"对NERD_commenter的设置
let NERDShutUp=1
"支持单行和多行的选择，//格式
map <c-h> ,c<space>

let g:neocomplcache_min_syntax_length = 3

" Tag list (ctags)
let Tlist_Show_One_File = 1            "不同时显示多个文件的tag，只显示当前文件的
let Tlist_Exit_OnlyWindow = 1          "如果taglist窗口是最后一个窗口，则退出vim
let Tlist_Use_Right_Window = 1         "在右侧窗口中显示taglist窗口

"修改zencoding快捷鍵
let g:user_zen_expandabbr_key = '<c-e>'

" Javascript in CheckSyntax
if has('win32')
    let g:checksyntax_cmd_javascript  = 'jsl -conf '.shellescape($VIM . '\vimfiles\plugin\jsl.conf')
else
    let g:checksyntax_cmd_javascript  = 'jsl -conf ~/.vim/plugin/jsl.conf'
endif
let g:checksyntax_cmd_javascript .= ' -nofilelisting -nocontext -nosummary -nologo -process'

" Under the Mac(MacVim)
if has("gui_macvim")
    " Mac 下，按 \ff 切换全屏
    map <Leader>ff  :call FullScreenToggle()<cr>

    " I like TCSH :^)
    set shell=/bin/tcsh

    " Set input method off
    set imdisable

    " Set QuickTemplatePath
    let g:QuickTemplatePath = $HOME.'/.vim/templates/'

    " 如果为空文件，则自动设置当前目录为桌面
    "lcd ~/Desktop/
endif

" VimWiki 配置
if !exists("g:vimwiki_list")
    let g:vimwiki_list = [
                \{"path": "~/Sites/VimWiki/wikiIndex", "path_html": "~/Sites/VimWiki/wikiHtml",
                \   "html_footer": "~/Sites/VimWiki/wikiTemplate/footer.tpl", "html_header": "~/Sites/VimWiki/wikiTemplate/header.tpl",
                \   "auto_export": 1}]
    let g:vimwiki_auto_checkbox = 0
    let g:vimwiki_use_calendar = 0  "禁用calendar
    if has('win32')
        " 注意！
        " 1、如果在 Windows 下，盘符必须大写
        " 2、路径末尾最好加上目录分隔符
        let s:vimwiki_root = "F:/My Dropbox/VimWiki"
        let g:vimwiki_list = [
                    \{"path": s:vimwiki_root."/wikiIndex/",
                    \   "html_footer": s:vimwiki_root."/wikiTemplate/footer.tpl",
                    \   "html_header": s:vimwiki_root."/wikiTemplate/header.tpl",
                    \   "path_html": s:vimwiki_root."/wikiHtml/", "auto_export": 1}]
        let g:vimwiki_w32_dir_enc = 'cp936'
    endif

    au FileType vimwiki set ff=unix fenc=utf8 noswapfile nobackup
    "au FileType vimwiki imap <C-t> <c-r>=TriggerSnippet()<cr>

    nmap <C-i><C-i> :VimwikiTabGoHome<cr>
endif

" 日历插件
if has("win32")
    let g:calendar_diary = "D:\\Program Files\\Vim\\vimfiles\\calendar"
    autocmd BufNewFile *.cal read $VIM\vimfiles\template\calendar_morning_diary.tpl | normal ggdd "日历套用模版
endif
if has("gui_macvim")
    let g:calendar_smnd = 1
    let g:calendar_monday = 1 " week start with monday.
    let g:calendar_weeknm = 1 " don't work with g:calendar_diary
    let g:calendar_mark = 'left-fit' " let plus(+) near the date, like +8.
    let g:calendar_mruler = '一月,二月,三月,四月,五月,六月,七月,八月,九月,十月,十一月,十二月'
    let g:calendar_wruler = '日 一 二 三 四 五 六'
    let g:calendar_navi_label = '上月,本月,下月'
    let g:calendar_list = [
        \ {'name': 'Works[Doit.im]', 'path': '~/diary/works/Doit.im', 'ext': 'wiki'},
        \ {'name': 'Works', 'path': '~/diary/works', 'ext': 'task'},
        \ {'name': 'Tasks', 'path': '~/diary/tasks', 'ext': 'task'},
        \ {'name': 'Diary', 'path': '~/diary/diary', 'ext': 'diary'},
        \ ]
    autocmd BufNewFile *.cal read $HOME/.vim/templates/calendar_morning_diary.tpl | normal ggdd "日历套用模版
endif

" on Windows, default charset is gbk
if has("win32")
    let g:fontsize#encoding = "cp936"
endif

" vim: set et sw=4 ts=4 sts=4 fdm=marker ft=vim ff=unix fenc=utf8:
