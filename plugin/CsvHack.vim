if (!exists('g:CsvHack#activate_global')) | let g:CsvHack#activate_global = v:false | endif
if (!exists('g:CsvHack#layout_file')) | let g:CsvHack#layout_file = v:true | endif
if (!exists('g:CsvHack#pin_header')) | let g:CsvHack#pin_header = v:true | endif
if (!exists('g:CsvHack#aggressive_jumplist')) | let g:CsvHack#aggressive_jumplist = v:true | endif
if (!exists('g:CsvHack#col_l_mapping')) | let g:CsvHack#col_l_mapping = '<a-h>' | endif
if (!exists('g:CsvHack#col_r_mapping')) | let g:CsvHack#col_r_mapping = '<a-l>' | endif
if (!exists('g:CsvHack#row_u_mapping')) | let g:CsvHack#row_u_mapping = '<a-k>' | endif
if (!exists('g:CsvHack#row_d_mapping')) | let g:CsvHack#row_d_mapping = '<a-j>' | endif
if (!exists('g:CsvHack#goto_column')) | let g:CsvHack#goto_column = 'ö' | endif
if (!exists('g:CsvHack#expand_mapping')) | let g:CsvHack#expand_mapping = '<space><space>' | endif
if (!exists('g:CsvHack#quit_buffer_mapping')) | let g:CsvHack#quit_buffer_mapping = '<esc>' | endif
if (!exists('g:CsvHack#search_column_mapping')) | let g:CsvHack#search_column_mapping = '/' | endif

let s:debug = v:false

command! CsvHackLockColumn call CsvHack#ColumnsLockFzf()
command! CsvHackEnable call CsvHack#ActivateLocal(v:true, "")
command! -bang CsvHackDisable call CsvHack#ActivateLocal(v:false, "<bang>")

function! s:search_first_line(regex)
    let a:saved_cursor = winsaveview()
    call cursor(1, 1)
    let result = searchpos(a:regex, 'nW', 1)
    call winrestview(a:saved_cursor)
    return result
endfunction

function! CsvHack#ActivateLocal(should_activate, force)
    if (a:should_activate)
        call CsvHack#CreateMappings(b:seperator_char)
        call CsvHack#DetectSeperatorChar()
        if g:CsvHack#layout_file
            call CsvHack#TableModeAlign()
            augroup CsvHack#Local
                autocmd BufRead <buffer> call CsvHack#TableModeAlign()
                autocmd BufWriteCmd <buffer> call CsvHack#HijackSaving()
            augroup end
        endif
        if g:CsvHack#aggressive_jumplist
            augroup CsvHack#Local
                " autocmd CursorMoved <buffer> call CsvHack#UpdateCursorMoved()
            augroup end
        endif
        if g:CsvHack#pin_header
            call CsvHack#SetupScrolllock('hor', [])
            augroup CsvHack#Local
                autocmd WinEnter,BufEnter <buffer> if (!CsvHack#HasScrolllockWin()) |  call CsvHack#SetupScrolllock('hor', []) | else | call CsvHack#AlignView() | endif
            augroup END
        endif
    else
        if (&modified && a:force != "!")
            throw "Unsaved changes! Save or force with CsvHackDisable!"
        endif
        doautocmd User CsvHack_CloseScrollock
        augroup CsvHack#Local
            au!
        augroup END
        call CsvHack#RemoveMappings()
        e!
    endif
endfunc

function! s:get_area_limits_by_nr(col_nr, sep_char)
    let l:regex = s:regex_for_count(a:col_nr, a:sep_char, '\zs', '')
    let a:saved_cursor = winsaveview()
    call cursor(1, 1)
    call search(l:regex, 'W', 1)
    let result = s:get_area_limits(virtcol("."), a:sep_char)
    call winrestview(a:saved_cursor)
    return result
endfunc

function! CsvHack#HasScrolllockWin()
    if !exists("b:csvhack_scrolllock_buf")
        call s:decho( "checkscrollwin, not set")
        return v:false
    elseif(b:csvhack_scrolllock_buf == 'pending')
        call s:decho( "checkscrollwin, buf pending")
        return v:true
    elseif bufexists(b:csvhack_scrolllock_buf)
        call s:decho ("checkscrollwin, buf exists")
        return v:true
    end
    call s:decho( "checkscrollwin, buf removed")
    return v:false
endfunc

function! CsvHack#SetupScrolllock(mode, col) 
    call s:timestep()
    call s:decho( " Setup Scrollock, mode: " . a:mode)
    call s:timestep()
    call CsvHack#CloseWin(win_getid())
    let b:csvhack_scrolllock_buf = 'pending'
    call s:timestep()
    let l:main_win = win_getid()
    let l:main_buf = bufnr("")
    if (a:mode =~ "^hor")
        let b:csvhack_scrolllock_mode = "hor"
        let l:saved_view = s:get_scrolllock_view("hor")
        setlocal scrollbind scrollopt=hor
        call s:timestep()
        let a:line1 = getline(1)
        above sp +enew
        call s:timestep()
        call setline(1, a:line1)
        set nomodified
        call s:timestep()
        resize 1
        call s:timestep()
        call setbufvar(l:main_buf, "csvhack_scrolllock_buf", bufnr(""))
        call s:timestep()
        setlocal scrollbind scrollopt=hor
        set bufhidden=wipe nowrap
        call s:timestep()
        call winrestview(l:saved_view)
        call s:timestep()
        let l:new_buf = bufnr("")
        wincmd w
        call s:timestep()

        augroup Scrolllock
            au!
            " we only want the header bar if the currently active windodw has is a
            " cvs file.
            " winleave disables the bar when switching windows, bufleave when changing the currently displayed file
            " bufleave doesn't trigger when switching between windows with the
            " same buffer
            exec "autocmd WinLeave,BufLeave <buffer> call CsvHack#CloseWin(" . l:main_win . ")"
            exec "autocmd User CsvHack_CloseScrollock call CsvHack#CloseWin(" l:main_win . ")"
        augroup END
    elseif (a:mode =~ '^ver')
        let b:csvhack_scrolllock_mode = "ver"
        set signcolumn=yes
        let l:saved_view = s:get_scrolllock_view('ver')
        if (!exists("b:seperator_char"))
            throw "unknown seperator char"
        endif
        if (a:mode =~ 'by_nr$')
            let [l:l, l:r] = s:get_area_limits_by_nr(a:col, b:seperator_char)
        else
            let [l:l, l:r] = s:get_area_limits(a:col, b:seperator_char)
        endif
        setlocal scrollopt=ver scrollbind
        let l:i = 0
        let a:lines = getline(1, '$')
        while (l:i < len(a:lines))
            if (l:r == -1) 
                let a:lines[i] = strpart(a:lines[i], l:l)
            else
                let a:lines[i] = strpart(a:lines[i], l:l, l:r - l:l)
            endif
            let l:i += 1
        endwhile
        call s:timestep()
        leftabove vsplit +enew
        call s:timestep()
        call setline(1, a:lines)
        call s:timestep()
        set nomodified
        set bufhidden=wipe nowrap
        call s:timestep()
        exec "vertical resize " . (l:r - l:l + 1)
        setlocal scrollopt=ver scrollbind nonu nornu fdc=0 winfixheight winfixwidth
        call s:timestep()
        call setbufvar(l:main_buf, "csvhack_scrolllock_buf", bufnr(""))
        call s:timestep()
        call winrestview(l:saved_view)
        call s:timestep()
        wincmd w
        call s:timestep()
    else
        throw "Unknown mode " . a:mode
    endif
    call s:timestep()
endfunc
function! s:get_scrolllock_view(mode)
    let l:saved_view = winsaveview()
    if (a:mode == 'ver')
        let l:saved_view['leftcol'] = 0
        let l:saved_view['col'] = 0
        let l:saved_view['coladd'] = 0
        let l:saved_view['colskip'] = 0
    else
        let l:saved_view['lnum'] = 1
    endif
    return l:saved_view
endfunc
function! CsvHack#AlignView()
    if (b:csvhack_scrolllock_mode != 'ver')
        return
    endif
    let l:view  = s:get_scrolllock_view(b:csvhack_scrolllock_mode)
    let l:buf = b:csvhack_scrolllock_buf
    let l:cur_buf = bufnr("")
    exec bufwinnr(l:buf) . "wincmd w "
    call winrestview(l:view)
    exec bufwinnr(l:cur_buf) . "wincmd w "
endfunc
function! s:decho(arg)
    if s:debug
        echo a:arg
    endif
endfunc
function! s:timestep()
    if s:debug
        redraw! | call getchar()
    endif
endfunc
function! CsvHack#CloseWin(main_win)
    call setbufvar(a:main_win, '&signcolumn', 'no')
    augroup Scrolllock
        au!
    augroup END
    if (!CsvHack#HasScrolllockWin())
        return
    endif
    let l:scroll_win = bufwinid(b:csvhack_scrolllock_buf)
    let [l:tab_num, l:win_num] = win_id2tabwin(l:scroll_win)
    call s:decho("closing buf : " . b:csvhack_scrolllock_buf . " win: " . l:win_num)
    if (l:tab_num != 0 || l:win_num != 0)
        exec l:tab_num . 'tabdo ' . l:win_num . "wincmd c"
    endif
    let [l:tab_num, l:win_num] = win_id2tabwin(a:main_win)
    if (l:tab_num != 0 || l:win_num != 0)
        exec l:tab_num . 'tabdo ' . l:win_num . "windo  setlocal noscrollbind"
    endif
    unlet b:csvhack_scrolllock_buf
endfunction
function! CsvHack#RemoveMappings()
    silent! exec 'unnoremap <buffer> '.g:CsvHack#col_l_mapping
    silent! exec 'unnoremap <buffer> '.g:CsvHack#col_r_mapping
    silent! exec 'unnoremap <buffer> '. g:CsvHack#row_d_mapping
    silent! exec 'unnoremap <buffer> '. g:CsvHack#row_u_mapping
    silent! exec 'unnoremap <buffer> '. g:CsvHack#expand_mapping
    silent! exec 'unnoremap <buffer> '. g:CsvHack#search_column_mapping
    silent! exec 'unnoremap <buffer> '. g:CsvHack#goto_column
endfunc
function! CsvHack#CreateMappings(sep_char)
    setlocal nowrap
    setlocal virtualedit=all
    setlocal nostartofline
    exec 'nnoremap <buffer> '.g:CsvHack#col_l_mapping . ' :call CsvHack#JumpCol("'.a:sep_char . '", v:true)<cr>'
    exec 'nnoremap <buffer> '.g:CsvHack#col_r_mapping . ' :call CsvHack#JumpCol("'.a:sep_char . '", v:false)<cr>'
    exec 'nnoremap <buffer> '. g:CsvHack#row_d_mapping.' :call CsvHack#VertMovement(1)<cr>'
    exec 'nnoremap <buffer> '. g:CsvHack#row_u_mapping.' :call CsvHack#VertMovement(-1)<cr>'
    exec 'nnoremap <buffer> '. g:CsvHack#expand_mapping.' :call CsvHack#ExpandScript()<cr>'
    exec 'nnoremap <buffer> '. g:CsvHack#goto_column.' :call CsvHack#ColumnsFzf()<cr>'
    exec 'nnoremap <buffer> '. g:CsvHack#search_column_mapping.' :call CsvHack#SearchColumn(virtcol("."), "' . a:sep_char .'")<cr>'
    nnoremap <buffer> <c-o> ``
endfunc
function! CsvHack#ClearUndo()
    let l:old_undolevels = &undolevels
    setlocal undolevels=-1
    exe "normal a \<BS>\<Esc>"
    exec "setlocal undolevels =" . l:old_undolevels
    setlocal nomodified
endfunc
function! s:count_before(char, col, string)
    let l:substr = strpart(a:string, 0, a:col)
    return count(l:substr, a:char)
endfunction
function! CsvHack#SearchColumn(col, char)
    let [l:l, l:r] = s:get_area_limits(a:col, a:char)
    let l:command = '/\v' . s:area_limit_to_regex(l:l) . s:area_limit_to_regex(l:r)
    let l:i = 0
    while (l:i < len(l:r))
        let l:i = l:i + 1
        let l:command = l:command. "\<Left>"
    endwhile
    call feedkeys(l:command, 'n')
endfunc
function! s:get_area_limits(col, char)
    let l:p = '\v^(.*'.a:char.')\zs[^'.a:char.']*%'.a:col .'c'
    let l:line = getline('.')
    let l:len_l = match(l:line, l:p)
    let l:len_r = match(l:line, a:char, l:len_l)
    if (l:len_l == (-1))
        let l:p_l = 0
    else
        let l:p_l = l:len_l
    endif
    if (l:len_r == (-1))
        let l:p_r = -1
    else
        let l:p_r = l:len_r
    endif
    return [l:p_l,l:p_r]
endfunction
function! s:area_limit_to_regex(s)
    if (a:s <= 0)
        return ""
    endif
    return "%" . a:s . "c
endfunction
function! s:regex_for_count(count, char, sep_1, sep_2)
    let l:result = "\\v^("
    let l:i = 0
    let l:part = "[^" . a:char . "]*"
    while (l:i < a:count)
        let l:result = l:result . l:part . a:char
        let l:i += 1
    endwhile
    let l:result = l:result . ")" . a:sep_1 . "(" . l:part
    let l:result = l:result . ")". a:sep_2 . "(" . a:char . ".*)?$"
    return l:result
endfunction
function! CsvHack#ExpandScript()
    if (!exists("b:seperator_char"))
        throw "unknown seperator char"
    endif
    let l:old_buf = bufnr("%")

    let l:lnum = line(".")
    let l:cnum = virtcol(".")
    let l:old_line = getline(l:lnum)
    let l:csv_col = s:count_before(b:seperator_char, l:cnum, l:old_line)
    let l:pat = s:regex_for_count(l:csv_col, b:seperator_char, '', '')
    let l:res = trim(substitute(l:old_line, l:pat, '\2', ''))

    let l:is_func = s:is_column_function(l:old_buf, l:pat)
    let l:callback = "call CsvHack#CompactScript(" . l:old_buf . ","  .  l:lnum . ",'" . l:pat . "', '" . b:seperator_char . "', ". l:is_func . ")"
    let l:buf_name = expand("%:t")
    new
    set buftype=acwrite
    set bufhidden=wipe
    call setline(1, l:res)
    if (l:is_func)
        set ft=haxe
    end
    exec "autocmd BufWriteCmd <buffer> " . l:callback
    exec 'nnoremap <buffer> ' . g:CsvHack#quit_buffer_mapping . ' :q<cr>'

    call CsvHack#Unescape(l:is_func)
    call CsvHack#ClearUndo()
    exec "file [" . l:buf_name . "]:" . l:lnum . ":" . (l:csv_col+1)
endfunc
function! s:flags()
    if &gdefault
        return ""
    else
        return "g"
    endif
endfunction
function! CsvHack#Unescape(is_func)
    if (a:is_func)
        for [l:from, l:to] in g:CsvHack#ScriptEscapeChars
            exec 'silent! %s/' . s:regex_escape(l:from) . '/' . l:to . '/' . s:flags()
        endfor
        if (search('\n.', 'n') == 0)
            silent! %s/^\s*\/\/\$/\0\r
            silent! %s/\v%(([{};])%(\s*)@>)%(else|;)@!/\1\r
            silent! %s/\*\//*\/\r
        endif
        norm =ie
        %g/^$/d
    else
        for [l:from, l:to] in g:CsvHack#TextEscapeChars
            exec 'silent! %s/' . s:regex_escape(l:from) . '/' . l:to . '/' . s:flags()
        endfor
    end
endfunction
let g:CsvHack#ScriptEscapeChars = [['  ', '\r'], ['[;]' , ','], ['~' , '"'], ['|' , ','], ['#' , '||']]
let g:CsvHack#TextEscapeChars = [ ['[;]' , ','], ['|' , "\r"] ]
function! s:is_column_function(buffer_id, regex)
    let l:line = getbufline(a:buffer_id, 1)[0]
    let l:part = substitute(l:line, a:regex, '\2', 'g')
    return l:part =~# 'Script'
endfunction
function! CsvHack#JumpCol(sepchar, backwards)
    norm! hh
    let l:flags = a:backwards ? 'b' : ''
    call search('\v^|'.a:sepchar.'|$', l:flags, line('.'))
    if (virtcol(".") < col("$")-1)
        norm! zs
    endif
    if (col(".") > 1)
        norm! ll
    endif
endfunc
if exists("s:working")
    unlet s:working
endif
function! CsvHack#UpdateCursorMoved()
    let l:vline = line(".")
    let l:vcol = virtcol(".") 
    let l:jumplist = getjumplist()
    let l:a = !exists('w:csvhack_line') 
    let l:b = !exists('w:csvhack_col') 
    let l:c = !exists('w:csvhack_jumplist') 
    if (l:a|| l:b||l:c || w:csvhack_jumplist != l:jumplist)
        let w:csvhack_line = l:vline
        let w:csvhack_col = l:vcol
        let w:csvhack_jumplist = l:jumplist 
        return
    endif

    let l:line_moved = abs(l:vline - w:csvhack_line)
    let l:col_moved = abs(l:vcol - w:csvhack_col)

    if (l:line_moved + l:col_moved >= 4)
        call cursor(w:csvhack_line, w:csvhack_col)
        norm! m'
        call cursor(l:vline, l:vcol)
    endif
    let w:csvhack_line = l:vline
    let w:csvhack_col = l:vcol
    let w:csvhack_jumplist = getjumplist()
endfunc
function! CsvHack#VertMovement(dir)
    let l:line = line(".")
    let l:col = virtcol(".")
    let l:target = s:findNextLine(l:line+a:dir, l:col, a:dir)
    exec l:target
endfunction
function! s:isWhitespace(line, col)
    let l:cur_line = getline(a:line)
    " this is a bit fishy with unicode characters but whatever
    if (len(l:cur_line)< a:col) | return v:true | endif
    let l:cur_char = matchstr(l:cur_line, '\%' . a:col . 'c.')
    return (l:cur_char =~# '\s')
endfunction
function! s:findNextLine(line, col, step)
    let l:cur = a:line
    let l:end = line("$")
    while (l:cur > 0 && l:cur < l:end)
        if (!s:isWhitespace(l:cur, a:col))
            return l:cur
        endif
        let l:cur = l:cur + a:step
    endwhile
    return l:cur
endfunction
function! s:regex_escape(char)
    if (a:char == "\r") 
        return "\\M\n"
    end
    return '\M' . a:char
endfunction
function! CsvHack#Escape(is_func, lines, seperator_char)
    if (a:is_func)
        let l:buffer = trim(join(map(a:lines, "trim(v:val)"), "\r"))
        for [l:k, l:v] in reverse(copy(g:CsvHack#ScriptEscapeChars))
            let l:buffer = substitute(l:buffer, s:regex_escape(l:v), l:k, 'g')
        endfor
    else
        let l:buffer = join(map(a:lines, "trim(v:val)"), "\n")
        for [l:k, l:v] in reverse(copy(g:CsvHack#TextEscapeChars))
            let l:buffer = substitute(l:buffer, s:regex_escape(l:v), l:k, 'g')
        endfor
    end
    return l:buffer
endfunction

function! CsvHack#CompactScript(buf_nr, line, pat, seperator_char, is_func)
    let l:lines = getline(1, '$')
    let l:buffer = CsvHack#Escape(a:is_func, l:lines, a:seperator_char)
    let l:old_line = getbufline(a:buf_nr, a:line)[0]
    let l:prefix = substitute(l:old_line, a:pat, '\1', "")
    let l:old_len = len(substitute(l:old_line, a:pat, '\2', ""))
    let l:suffix = substitute(l:old_line, a:pat, '\3', "")
    let l:buffer = ' ' . l:buffer . ' '
    let l:buffer = l:buffer . repeat(' ', max([0, l:old_len - len(l:buffer)]))
    call setbufline(a:buf_nr, a:line, l:prefix . l:buffer . l:suffix)
    set nomodified
    call CsvHack#DoSave(a:buf_nr)
endfunction
function! CsvHack#HijackSaving()
    call s:decho("HIJACKED")
    if (!exists("b:seperator_char"))
        throw "unknown seperator char"
    endif
    return CsvHack#DoSave(bufnr("%"))
endfunc
function! CsvHack#DoSave(buf)
    let l:sep_char = getbufvar(a:buf, 'seperator_char')
    let l:path = expand("#". a:buf)
    let l:buf_content = join(getbufline(a:buf, 1, '$'), "\n")
    call s:decho( "NIJACKED")
    let l:normalized = substitute(l:buf_content, '\v\s*' . l:sep_char . ' ', l:sep_char, 'g')
    call writefile(split(l:normalized, '\n', 1), l:path, 'b')
    set nomodified
    call setbufvar(a:buf, "&modified", 0)
    return 1
endfunc
function! CsvHack#DetectSeperatorChar()
    if(match(getline(1), ',') != -1)
        let b:seperator_char = ','
    elseif (match(getline(1), ';') != -1)
        let b:seperator_char = ';'
    else
        throw "Couldn't detect seperator char in: " . getline(1)
    endif
endfunc
function! CsvHack#TableModeAlign()
    let l:old_reg = @"
    if (!exists("b:seperator_char"))
        throw "unknown seperator char"
    endif
    if (!exists(":TableModeRealign"))
        throw "dhruvasagar/vim-table-mode required for alignment"
    endif
    let l:pos = getcurpos()[1:]

    let l:old_modified = &modified
    silent! %s/\v\|/§
    silent! exec "%s/\\v".b:seperator_char."/|"
    norm ggG0I| 
    TableModeRealign
    silent! exec "%s/\\v\\|/" . b:seperator_char
    silent! exec "%s/\\v^" . b:seperator_char . " //"
    silent! %s/§/|
    let &modified = l:old_modified
    call cursor(l:pos)
    let @" = l:old_reg
endfunc

function! s:fzf(callback)
    if (!exists('b:seperator_char'))
        throw "Not a CsvHack buffer!"
    endif
    if (!exists('*fzf#run'))
        throw "Requires fzf to be installed"
    endif
    let s:saved_view = winsaveview()
    return fzf#run(fzf#wrap('columns', {
                \ 'source':  map(split(getline(1), b:seperator_char), "trim(v:val)"),
                \ 'sink*':   a:callback,
                \ 'options': ['+m', '--prompt', 'Columns> ', '--ansi', '--extended', '--layout=reverse-list', '--tabstop=1']
                \}, 0))
endfunc
function! CsvHack#ColumnsFzf()
    call s:fzf(function('s:handle_fzf_col'))
endfunction
function! CsvHack#ColumnsLockFzf()
    call s:fzf(function("s:lock_col"))
endfunction
function! s:lock_col(arg)
    call winrestview(s:saved_view)
    " if (len(a:arg) == 0) | return | endif
    let [l:line, l:col] = s:search_first_line('\M'.a:arg[0])
    call CsvHack#SetupScrolllock('ver', l:col)
endfunc
function! s:handle_fzf_col(arg)
    call winrestview(s:saved_view)
    if (len(a:arg) == 0) | return | endif
    let [l:line, l:col] = s:search_first_line('\M'.a:arg[0])
    exec "norm " . l:col . "|"
    norm! hh
    if (virtcol(".") < col("$")-1)
        norm! zs
    endif
    if (col(".") > 1)
        norm! ll
    endif
endfunc
