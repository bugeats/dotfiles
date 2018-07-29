" Reset -----------------------------------------------------------------------

set background=dark

highlight clear

if exists("syntax_on")
    syntax reset
endif

let g:colors_name = "moon"

hi clear Normal
hi Normal guifg=#FF0000
