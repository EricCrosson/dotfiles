set timeout timeoutlen=1000 ttimeoutlen=100
set <f13>=fd
imap <F13> <esc>

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

set splitbelow
set splitright

" =======================================
" Display
" =======================================
set linespace=3
set ruler
set title
set cmdheight=1
set showtabline=2
" set winwidth=100
set colorcolumn=80
set nowrap
" set number

" Spell checking and auto-wrapping to got commit messages
autocmd Filetype gitcommit setlocal spell textwidth=72

" === Plug
" Specify a directory for plugins (for Neovim: ~/.local/share/nvim/plugged)
call plug#begin('~/.vim/plugged')

Plug 'kristijanhusak/vim-hybrid-material'

" Wakatime config
Plug 'wakatime/vim-wakatime'

" Initialize plugin system
call plug#end()

set background=dark
colorscheme hybrid_reverse
