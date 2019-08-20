# CsvHack.vim

### Overview:

[![Watch the video](http://i3.ytimg.com/vi/uzT-x84eLsk/hqdefault.jpg)](https://www.youtube.com/watch?v=uzT-x84eLsk)


The plugin is enabled on a file level with `:CsvHackEnable`. This

- decides whether the file uses `,` or `;` as seperator
- aligns the file via dhruvasagar/vim-table-mode
- setup autocommands to store the file unaligned
- pins the header line above the buffer
- registers some mappings and autocommands

Alignment isn't updated while the file is edited for performance reasons, save and reload to do so manually.

### Mappings:

The default mappings are:

     let g:CsvHack#col_l_mapping = '<a-h>'
     let g:CsvHack#col_r_mapping = '<a-l>'
     let g:CsvHack#row_u_mapping = '<a-k>'
     let g:CsvHack#row_d_mapping = '<a-j>'
     let g:CsvHack#expand_mapping = '<space><space>'
     let g:CsvHack#quit_buffer_mapping = '<esc>'

Expanding a cell opens a buffer with the unescaped contents of the cell.
Saving that buffer stores the escaped contents back into the cell.

### Escaping:

The default escape mappings are 

    let g:CsvHack#ScriptEscapeChars = [ ['~' , '"'], ['|' , ','], ['#' , '||']]
    let g:CsvHack#TextEscapeChars = [ ['[;]' , ','], ['|' , "\r"] ]

Note the ordering in ScriptEscapeChars, we don't want '#' to map into ',,'.

### Installation:

With plug.vim:

    Plug 'Tarmean/CsvHack.vim'
    Plug 'dhruvasagar/vim-table-mode', { 'on':  ['TableModeEnable', 'TableModeRealign'] }

### Disclaimer:

This is a plugin to work with the DiceyDungeon files. It might work for other csv files but things probably will break at the seams.
That said, keep backups and a high undolevels even if you work with the game files.