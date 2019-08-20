if (!exists('g:CsvHack#activate_global')) | let g:CsvHack#activate_global = v:false | endif
if (!exists('g:CsvHack#layout_file')) | let g:CsvHack#layout_file = v:true | endif
if (!exists('g:CsvHack#pin_header')) | let g:CsvHack#pin_header = v:true | endif
if (!exists('g:CsvHack#col_l_mapping')) | let g:CsvHack#col_l_mapping = '<a-h>' | endif
if (!exists('g:CsvHack#col_r_mapping')) | let g:CsvHack#col_r_mapping = '<a-l>' | endif
if (!exists('g:CsvHack#row_u_mapping')) | let g:CsvHack#row_u_mapping = '<a-k>' | endif
if (!exists('g:CsvHack#row_d_mapping')) | let g:CsvHack#row_d_mapping = '<a-j>' | endif
if (!exists('g:CsvHack#expand_mapping')) | let g:CsvHack#expand_mapping = '<space><space>' | endif
if (!exists('g:CsvHack#quit_buffer_mapping')) | let g:CsvHack#quit_buffer_mapping = '<esc>' | endif
if (!exists('g:CsvHack#search_column_mapping')) | let g:CsvHack#search_column_mapping = '/' | endif

command! CsvHackEnable call CsvHack#ActivateLocal(v:true, "")
command! -bang CsvHackDisable call CsvHack#ActivateLocal(v:false, "<bang>")
function! CsvHack#ActivateLocal(should_activate, force)
    if (a:should_activate)
        augroup CsvHack#Local
            au!
            autocmd BufRead <buffer> call CsvHack#DetectSeperatorChar()
        augroup end
        call CsvHack#DetectSeperatorChar()
        if g:CsvHack#layout_file
                call CsvHack#TableModeAlign()
            augroup CsvHack#Local
                autocmd BufRead <buffer> call CsvHack#TableModeAlign()
                autocmd BufWriteCmd <buffer> call CsvHack#HijackSaving()
            augroup end
        endif
        if g:CsvHack#pin_header
            call CsvHack#SetupScrolllock()
            augroup CsvHack#Local
                autocmd WinEnter,BufEnter <buffer> call CsvHack#SetupScrolllock()
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

function! CsvHack#SetupScrolllock() 
    call CsvHack#CreateMappings(b:seperator_char)
    if (exists("b:csvhack_scrolllock_win"))
        return
    endif
    if (!exists("b:seperator_char"))
        throw "unknown seperator char"
    endif
    let b:csvhack_scrolllock_win = v:true
    setlocal scrollbind
    setlocal scrollopt=hor
    setlocal nowrap
    split
    wincmd k
    let l:scrolllock_win = win_getid()
    resize 1
    norm! gg
    wincmd j
    let l:main_win = win_getid()
    augroup Scrolllock
        au!
        " we only want the header bar if the currently active windodw has is a
        " cvs file.
        " winleave disables the bar when switching windows, bufleave when changing the currently displayed file
        " bufleave doesn't trigger when switching between windows with the
        " same buffer
        exec "autocmd WinLeave,BufLeave <buffer> call CsvHack#CloseWin(" . l:scrolllock_win . ", ". l:main_win . ")"
        exec "autocmd User CsvHack_CloseScrollock call CsvHack#CloseWin(" . l:scrolllock_win . ", ". l:main_win . ")"
    augroup END
    setlocal nostartofline
endfunc
function! CsvHack#RemoveMappings()
    silent! exec 'unnoremap <buffer> '.g:CsvHack#col_l_mapping
    silent! exec 'unnoremap <buffer> '.g:CsvHack#col_r_mapping
    silent! exec 'unnoremap <buffer> '. g:CsvHack#row_d_mapping
    silent! exec 'unnoremap <buffer> '. g:CsvHack#row_u_mapping
    silent! exec 'unnoremap <buffer> '. g:CsvHack#expand_mapping
    silent! exec 'unnoremap <buffer> '. g:CsvHack#search_column_mapping
endfunc
function! CsvHack#CreateMappings(sep_char)
    exec 'nnoremap <buffer> '.g:CsvHack#col_l_mapping . ' :call CsvHack#JumpCol("'.a:sep_char . '", v:true)<cr>'
    exec 'nnoremap <buffer> '.g:CsvHack#col_r_mapping . ' :call CsvHack#JumpCol("'.a:sep_char . '", v:false)<cr>'
    exec 'nnoremap <buffer> '. g:CsvHack#row_d_mapping.' :call CsvHack#VertMovement(1)<cr>'
    exec 'nnoremap <buffer> '. g:CsvHack#row_u_mapping.' :call CsvHack#VertMovement(-1)<cr>'
    exec 'nnoremap <buffer> '. g:CsvHack#expand_mapping.' :call CsvHack#ExpandScript()<cr>'
    exec 'nnoremap <buffer> '. g:CsvHack#search_column_mapping.' :call CsvHack#SearchColumn(col("."), "' . a:sep_char .'")<cr>'
endfunc
function! CsvHack#ClearUndo()
	let l:old_undolevels = &undolevels
	setlocal undolevels=-1
	exe "normal a \<BS>\<Esc>"
	exec "setlocal undolevels =" . l:old_undolevels
    setlocal nomodified
endfunc
function! CsvHack#CloseWin(scrolllock_win, main_win)
    augroup Scrolllock
        au!
    augroup END
    if (exists("b:csvhack_scrolllock_win"))
        unlet b:csvhack_scrolllock_win
    endif
    let [l:tab_num, l:win_num] = win_id2tabwin(a:scrolllock_win)
    if (l:tab_num != 0 || l:win_num != 0)
        exec l:tab_num . 'tabdo ' . l:win_num . "wincmd c"
    endif
    let [l:tab_num, l:win_num] = win_id2tabwin(a:main_win)
    if (l:tab_num != 0 || l:win_num != 0)
        exec l:tab_num . 'tabdo ' . l:win_num . "windo  setlocal noscrollbind"
    endif
endfunction
function! s:count_before(char, col, string)
    let l:substr = strpart(a:string, 0, a:col)
    return count(l:substr, a:char)
endfunction
function! CsvHack#SearchColumn(col, char)
    let [l:l, l:r] = s:get_area_limits(a:col, a:char)
    let l:command = '/\v' . l:l . l:r
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
    if (l:len_r == (-1))
        let l:p_r = ""
    else
        let l:p_r = '%<' . l:len_r . 'c'
    endif
    let l:p_l = '%>' . l:len_l . 'c'
    return [l:p_l,l:p_r]
endfunction
function! s:regex_for_count(count, char)
    let l:result = "\\v^("
    let l:i = 0
    let l:part = "[^" . a:char . "]*"
    while (l:i < a:count)
        let l:result = l:result . l:part . a:char
        let l:i += 1
    endwhile
    let l:result = l:result . ")(" . l:part
    let l:result = l:result . ")(" . a:char . ".*)?$"
    return l:result
endfunction
function! CsvHack#ExpandScript()
    if (!exists("b:seperator_char"))
        throw "unknown seperator char"
    endif
    let l:old_buf = bufnr("%")

    let l:lnum = line(".")
    let l:cnum = col(".")
    let l:old_line = getline(l:lnum)
    let l:csv_col = s:count_before(b:seperator_char, l:cnum, l:old_line)
    let l:pat = s:regex_for_count(l:csv_col, b:seperator_char)
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
        silent! %s/\v%(([{};])\s*)@>%(else)@!/\1\r
        norm =ie
        %g/^$/d
    else
        for [l:from, l:to] in g:CsvHack#TextEscapeChars
            exec 'silent! %s/' . s:regex_escape(l:from) . '/' . l:to . '/' . s:flags()
        endfor
    end
endfunction
let g:CsvHack#ScriptEscapeChars = [ ['[;]' , ','], ['~' , '"'], ['|' , ','], ['#' , '||']]
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
    if (col(".") < col("$")-1)
        norm! zs
    endif
    if (col(".") > 1)
        norm! ll
    endif
endfunc
function! CsvHack#VertMovement(dir)
    let l:line = line(".")
    let l:col = col(".")
    let l:target = s:findNextLine(l:line+a:dir, l:col, a:dir)
    let l:dif = abs(l:line - l:target)
    if (abs(l:line -l:target) > 1)
        norm! m'
    endif
    exec l:target
endfunction
function! s:isWhitespace(line, col)
    let l:cur_line = getline(a:line)
     " this is a bit fishy with unicode characters but whatever
    if (len(l:cur_line)< a:col) | return v:true | endif
    let l:cur_char = matchstr(l:cur_line, '\%' . col('.') . 'c.')
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
        let l:buffer = trim(join(map(a:lines, "trim(v:val)"), " "))
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
    let l:buffer = substitute(l:buffer, '\s\+', ' ', 'g')
    let l:buffer = l:buffer . repeat(' ', max([0, l:old_len - len(l:buffer)]))
    call setbufline(a:buf_nr, a:line, l:prefix . l:buffer . l:suffix)
    set nomodified
endfunction
function! CsvHack#HijackSaving()
  if (!exists("b:seperator_char"))
      throw "unknown seperator char"
  endif
  let l:content = join(getline(1,'$'), "\n")
  let l:normalized = substitute(l:content, '\v\s*' . b:seperator_char . ' ', b:seperator_char, 'g')
  call writefile(split(l:normalized, '\n', 1), expand('%'), 'b')
  set nomodified
  return 1
endfunc
function! CsvHack#DetectSeperatorChar()
    let b:seperator_char = match(getline(1), ',') != -1 ? ',' : ';'
endfunc
function! CsvHack#TableModeAlign()
    if (!exists("b:seperator_char"))
        throw "unknown seperator char"
    endif
    if (!exists(":TableModeRealign"))
        throw "dhruvasagar/vim-table-mode required for alignment"
    endif

    let l:old_modified = &modified
    silent! %s/\v\|/§
    silent! exec "%s/\\v".b:seperator_char."/|"
    norm ggG0I| 
    TableModeRealign
    silent! exec "%s/\\v\\|/" . b:seperator_char
    silent! exec "%s/\\v^" . b:seperator_char . " //"
    silent! %s/§/|
    let &modified = l:old_modified
endfunc
