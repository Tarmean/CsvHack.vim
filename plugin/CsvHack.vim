if (!exists('g:CsvHack#activate_global')) | let g:CsvHack#activate_global = v:false | endif
if (!exists('g:CsvHack#layout_file')) | let g:CsvHack#layout_file = v:true | endif
if (!exists('g:CsvHack#pin_header')) | let g:CsvHack#pin_header = v:true | endif
if (!exists('g:CsvHack#col_l_mapping')) | let g:CsvHack#col_l_mapping = '<a-h>' | endif
if (!exists('g:CsvHack#col_r_mapping')) | let g:CsvHack#col_r_mapping = '<a-l>' | endif
if (!exists('g:CsvHack#row_u_mapping')) | let g:CsvHack#row_u_mapping = '<a-k>' | endif
if (!exists('g:CsvHack#row_d_mapping')) | let g:CsvHack#row_d_mapping = '<a-j>' | endif
if (!exists('g:CsvHack#expand_mapping')) | let g:CsvHack#expand_mapping = '<space><space>' | endif
if (!exists('g:CsvHack#quit_buffer_mapping')) | let g:CsvHack#quit_buffer_mapping = '<esc>' | endif

command! CsvHackEnable call CsvHack#ActivateLocal()
command! CsvHackDisable call CsvHack#DeactivateLocal()
" augroup CsvHack#ScrolllockFileGlobal
"     au!
"     if g:CsvHack#activate_global
"         autocmd BufRead *.csv call CsvHack#DetectSeperatorChar()
"         if g:CsvHack#layout_file
"             autocmd BufRead *.csv call CsvHack#TableModeAlign()
"             autocmd BufWriteCmd *.csv call CsvHack#HijackSaving()
"         endif
"         if g:CsvHack#pin_header
"             autocmd WinEnter,BufEnter *.csv call CsvHack#SetupScrolllock()
"         endif
"     endif
" augroup END
function! CsvHack#ActivateLocal()
    call CsvHack#DetectSeperatorChar()
    augroup CsvHack#Local
        au!
        autocmd BufRead <buffer> call CsvHack#DetectSeperatorChar()
        if g:CsvHack#layout_file
            call CsvHack#TableModeAlign()
            autocmd BufRead <buffer> call CsvHack#TableModeAlign()
            autocmd BufWriteCmd <buffer> call CsvHack#HijackSaving()
        endif
        if g:CsvHack#pin_header
            autocmd WinEnter,BufEnter <buffer> call CsvHack#SetupScrolllock()
            call CsvHack#SetupScrolllock()
        endif
    augroup END
endfunc
function! CsvHack#DeactivateLocal()
    augroup CsvHack#Local
        au!
    augroup end
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
        exec "autocmd WinLeave,BufLeave <buffer> call CsvHack#CloseWin(" . l:scrolllock_win . ", ". l:main_win . ")"
    augroup END
    setlocal nostartofline
endfunc
function! CsvHack#CreateMappings(sep_char)
    exec 'nnoremap <buffer> '.g:CsvHack#col_l_mapping . ' :call CsvHack#JumpCol("'.a:sep_char . '", v:true)<cr>'
    exec 'nnoremap <buffer> '.g:CsvHack#col_r_mapping . ' :call CsvHack#JumpCol("'.a:sep_char . '", v:false)<cr>'
    exec 'nnoremap <buffer> '. g:CsvHack#row_d_mapping.' <c-f>'
    exec 'nnoremap <buffer> '. g:CsvHack#row_u_mapping.' <c-b>'
    exec 'nnoremap <buffer> '. g:CsvHack#expand_mapping.' :call CsvHack#ExpandScript()<cr>'
endfunc
function! CsvHack#JumpCol(sepchar, backwards)
    let l:flags = a:backwards ? 'b' : ''
    call search('\v^|'.a:sepchar.'|$', l:flags, line('.'))
endfunc
function! CsvHack#ClearUndo()
	let l:old_undolevels = &undolevels
	setlocal undolevels=-1
	exe "normal a \<BS>\<Esc>"
	exec "setlocal undolevels =" . l:old_undolevels
    setlocal nomodified
endfunc
function! CsvHack#CloseWin(scrolllock_win, main_win)
    if (!exists("b:csvhack_scrolllock_win"))
        return
    end
    unlet b:csvhack_scrolllock_win
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
    if (len(l:res))
        new
        set buftype=acwrite
        set bufhidden=wipe
        call append(0, l:res)
        if (l:is_func)
            set ft=haxe
        end
        exec "autocmd BufWriteCmd <buffer> " . l:callback
        exec 'nnoremap <buffer> ' . g:CsvHack#quit_buffer_mapping . ' :q<cr>'

        call CsvHack#Unescape(l:is_func)
        call CsvHack#ClearUndo()
        file Row Entry
    endif
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
        silent! %s/\v%(([{};])\s*)@>%(else)@!/\1\r
        norm =ie
        %g/^$/d
        for [l:from, l:to] in g:CsvHack#ScriptEscapeChars
            exec 'silent! %s/' . s:regex_escape(l:from) . '/' . l:to . '/' . s:flags()
        endfor
    else
        for [l:from, l:to] in g:CsvHack#TextEscapeChars
            exec 'silent! %s/' . s:regex_escape(l:from) . '/' . l:to . '/' . s:flags()
        endfor
    end
endfunction
let g:CsvHack#ScriptEscapeChars = [ ['~' , '"'], ['|' , ','], ['#' , '||']]
let g:CsvHack#TextEscapeChars = [ ['[;]' , ','], ['|' , "\r"] ]
function! s:is_column_function(buffer_id, regex)
    let l:line = getbufline(a:buffer_id, 1)[0]
    let l:part = substitute(l:line, a:regex, '\2', 'g')
    return l:part =~# 'Script'
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
        for [l:k, l:v] in reverse(g:CsvHack#ScriptEscapeChars)
            let l:buffer = substitute(l:buffer, s:regex_escape(l:v), l:k, 'g')
        endfor
    else
        let l:buffer = join(map(a:lines, "trim(v:val)"), "\n")
        if (len(l:buffer) == 0)
            return ""
        else
            let l:buffer = l:buffer[0:-2]
        end
        for [l:k, l:v] in reverse(g:CsvHack#TextEscapeChars)
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
