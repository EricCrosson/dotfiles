" File: .vimrc
" Author: Dallas Kaman
" Created: Wed Oct 23 22:26:09 CDT 2013


"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Contents ('t) <= will take you back here
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" 1). General Setting ('g)
" 2). Colors and fonts ('c)
" 3). Key Mappings ('k)
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""


"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

"---Set to auto read when file is modified from another source
set autoread

"---Set line numbers
set nu

"---Make the backspace work correctly
set backspace=indent,eol,start

"---show current mode
set showmode

"---Enable  plugins
filetype plugin on
filetype indent on

"---Enable Pathogen to work
execute pathogen#infect()

"---Change mapleader to , instad of \
let mapleader=","

"---Dark terminal
set bg=dark

"---Indentation and whitespace
set autoindent
set smartindent
set expandtab
set shiftwidth=4
set tabstop=4
set softtabstop=4

"---Turn off what <C-j> orginally did
let g:C_Control_j = 'off'

"---Makes view for a buffer when you enter, and remembers it when you leave
autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent loadview

"---LaTeX suite settings
let g:tex_flavor="latex"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Colors and fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

"---Set vim to use 256 colors
set t_Co=256

"---Set syntax highlighting
syntax on

"---Set the default colorscheme
colorscheme darkocean

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

"---Normal mode bindings

    "---Switching key functionality
    nnoremap : ;
    nnoremap ; :
    nnoremap ' `
    nnoremap ` '

    "---Play macro without having to hit the @ key
    nnoremap Q @q

    "---Yank to end of line with Y
    nnoremap Y y$

    "---Window navigation
    nmap <C-h> <C-w>h
    nmap <C-j> <C-w>j
    nmap <C-k> <C-w>k
    nmap <C-l> <C-w>l

    "---Easy searching with space
    nmap <SPACE> /
    nmap <SPACE><SPACE> //<CR>

"---Insert mode bindings
    
    "---Make jj work like <ESC> for insert mode (kinda weird, still trying
    "this out (growing on me though)
    inoremap jj <ESC>

"---Visual mode bindings

    "---When you indent, reselects visual selection
    vmap > >gv
    vmap < <gv

"---Leader bindings (custom stuff)
    
    "---Handling the vimrc file (this file)
    nmap <Leader>ev ;e $MYVIMRC<CR>
    nmap <Leader>sv ;so $MYVIMRC<CR>

    "---Windowing
    nmap <Leader>vs ;vsplit<CR>
    nmap <Leader>hs ;split<CR>
    nmap <Leader>wc <C-w>c

    "---NERDTree
    nmap <Leader>nt ;NERDTreeToggle<CR>
    
    "---Puts a semicolon at the end of the line and restores where you were in
    "the line
    nmap <Leader>; maA;<ESC>'a

    "---Easy manual folding
    nmap <Leader>f vipzf
    
    "---Append to file with ,G 'a to return
    nmap <Leader>G maGo

    "---Prepend to file with ,g 'a to return
    nmap <Leader>g maggO

    "---Auto indent the file
    nmap <Leader>ai magg=G'a

    "---Paste from the system clipboard
    nmap <Leader>p "*p


"---Colon commands

    "---Write with sudo!
    cmap w!! !sudo tee % >/dev/null



