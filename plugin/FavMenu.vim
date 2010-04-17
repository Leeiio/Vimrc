" Author: Gergely Kontra <kgergely@mcl.hu>
" Version: 0.32
" Description:
"    Adds a new menu to vim
"    You can add your favourite files (and directories) into it
"
" FEEDBACK PLEASE
"
" Installation: Drop it into your plugin directory
"
" History:
"    0.1: Initial release
"    0.2:
"	  * Fixed bug, which caused same files to hide each other
"         * Your favourite files must be located at $FAVOURITES
"	  * You can Edit the favourites. Menus will updated, when you save
"         * When you click on the menu, it invokes the fav_fun function
"         * You can choose cascade delete menu by defining fav_cascade_del
"	    (at startup!)
"         * You can add directories to your favourites
"           Thanks to the_intellectual_person <arun_kumar_ks@hotmail.com>,
"           who gave me a patch for this
"    0.21:* Fixed bug, which caused not to update you menu, when you remove
"	    one of your favourites
"	  * Fixed REGEXP bug (When removing files)
"    0.22:* Nice fallbacks, if $FAVOURITES is not defined.
"         * fav_fun is renamed to OPEN_FUNC, so you can store it in your
"           viminfo file, and can be reused in my MRU script. Sorry for the
"           inconvinience.
"    0.23:* You can limit the width of the path appearing in the menu, by
"           defining (and setting) the PATHSIZELIMIT variable.
"         * Added menu: Refresh (will be removed in a later release)
"    0.24:* Close the file, even when 'hidden' is set
"            Thanks to Roger Pilkey for the bug report
"    0.3: * Use clientserver feature to synchronize the menu instances
"    0.31:* Shut up clientserver stuff
"    0.32:* Hungarian translation
"
" TODO:
"    Are all valid filenames escaped? (Feedback please!)
let s:cascade_del=exists('fav_cascade_del')

if !exists('$FAVOURITES')
    if has('unix')
        let $FAVOURITES=$HOME.'/.vimfavourites'
    el
        let $FAVOURITES=$VIM.'\_vimfavourites'
    en
en

if !exists('SpWhenModified') "integration with FavMenu
    fu! SpWhenModified(f)
        if &mod
            exe 'sp '.a:f
        el
            exe 'e '.a:f
        en
    endf
    fu! SpWhenNamedOrModified(f)
        if bufname('')!='' || &mod
            exe 'sp '.a:f
        el
            exe 'e '.a:f
        en
    endf
    fu! OpenFile()
        if exists('g:OPEN_FUNC')
            retu g:OPEN_FUNC
        el
            retu 'SpWhenModified'
        en
    endf
    fu! TruncPath(path)
        let p=a:path
        let pathlen=strlen(p)
        if exists('g:PATHSIZELIMIT') && pathlen>g:PATHSIZELIMIT
            let cut=match(p,'[/\\]',pathlen-g:PATHSIZELIMIT)
            if cut>0 && cut<pathlen
                let p='\.\.\.'.strpart(p,cut)
            en
        en
        retu p
    endf
en

fu! s:AddThisFile(name)
    let fullname=fnamemodify(a:name,':p')
    let path=TruncPath(escape(fnamemodify(fullname,':p:h'),'\. #%'))

    let fn=escape(fnamemodify(fullname,':p:t'),'\. #%')
    if strlen(fn)
        let item='[&'.s:cnt.']\ \ '.fn.'<Tab>'.path
    el
        let item='[&'.s:cnt.']\ \ <DIR><Tab>'.path
    en
    let s:cnt=s:cnt+1
    exe 'amenu Fa&vourites.'.item." :cal \<C-r>=OpenFile()<CR>('".escape(fullname,'#%')."')<CR>"
    if s:cascade_del
        exe 'amenu Fa&vourites.&Remove.'.item." :cal <SID>RemoveThisFile('".fullname."')<CR>"
    en
endf

fu! s:AddThisFilePermanent(name)
    let fullname=fnamemodify(a:name,':p')
    cal s:AddThisFile(a:name)
    let v=virtcol('.')|vs $FAVOURITES|se nobl bh=delete|0
    if search('^\V'.escape(fullname,'\').'\$','w')
        cal confirm('This is already in your favourites file!',' :-/ ',1,'W')
    el
        exe 'norm Go'.fullname."\<Esc>"
    en
    " No patching
    let pm=&pm|let &pm=''|wq|let &pm=pm|exe 'norm' v.'|'
    sil! cal s:RefreshAll()
endf

fu! s:RemoveThisFile(name)
    let fullname=fnamemodify(a:name,':p')
    vs $FAVOURITES|set nobl noro ma|0
    if search('^\V'.escape(fullname,'\').'\$','w')
        d _
    el
        cal confirm('Cannot find this file in your favourites file!',' :-/ ',1,'e')
    en
    let pm=&pm|let &pm=''|let hid=&hid|se nohid|wq|let &pm=pm|let &hid=hid
    cal FavmenuInit()
    sil! cal s:RefreshAll()
endf

fu! s:RefreshAll()
    if has('clientserver')
        let servers=serverlist()
        let pos=0
        let re="[^\n]\\+"  "Thanx to Mark Hillebrand
        wh match(servers,re,pos) != -1
            let s=matchstr(servers,re,pos)
            let pos=pos+strlen(s)+1
            if v:servername!=s
                cal remote_expr(s,'FavmenuInit()')
            en
        endw
    en
endf

fu! FavmenuInit()
    let s:cnt=1
    sil! aun Fa&vourites
    amenu 65.1 Fa&vourites.&Add\ current\ file :cal <SID>AddThisFilePermanent(@%)<CR>
    amenu 65.3 Fa&vourites.&Edit\ favourites :cal <C-r>=OpenFile()<CR>($FAVOURITES)<CR>:au BufWritePost <C-r>% cal FavmenuInit()<CR>
    amenu 65.4 Fa&vourites.Re&fresh :cal FavmenuInit()<CR>
    amenu 65.5 Fa&vourites.-sep-	<nul>
    "if s:cascade_del
    "    amenu 65.2 Fa&vourites.&Remove.Dummy <Nop>
    "el
    "    amenu 65.2 Fa&vourites.&Remove\ current\ file :cal <SID>RemoveThisFile(@%)<CR>
    "en

    if filereadable($FAVOURITES)
        sv $FAVOURITES|se bh=delete
        let s=@/
        g/\S/cal s:AddThisFile(getline('.'))
        let @/=s
        q
        sil! aun Fa&vourites.&Remove.Dummy
    en
endf

sil! cal FavmenuInit()
