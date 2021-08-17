" =======================================
" Behavior
" =======================================
set timeout timeoutlen=1000 ttimeoutlen=100
inoremap fd <Esc>

" FIXME: windmove for vim not working yet
nnoremap <S-Left> <C-W><j>
nnoremap <S-Down> <C-W><k>
nnoremap <S-Right> <C-W><l>
nnoremap <S-Up> <C-W><h>

set splitbelow
set splitright

set nocompatible
set backspace=indent,eol,start
set updatetime=750
set spelllang=en_us

" =======================================
" Display
" =======================================
set cmdheight=1
set colorcolumn=80
set linespace=3
set nowrap
set number
set ruler
set showtabline=2
set title

" Spell checking and auto-wrapping for git commit messages
autocmd Filetype gitcommit setlocal spell textwidth=72

" =======================================
" Plugins
" =======================================
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
call plug#begin('~/.vim/plugged')

Plug 'airblade/vim-gitgutter'
Plug 'axelf4/vim-strip-trailing-whitespace'
Plug 'editorconfig/editorconfig-vim'
Plug 'google/vim-jsonnet', { 'for': ['jsonnet'] }
Plug 'haishanh/night-owl.vim'
"Plug 'honza/vim-snippets'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'kevinoid/vim-jsonc', { 'for': ['json'] }
Plug 'knsh14/vim-github-link'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'prettier/vim-prettier', { 'for': ['typescript', 'javascript', 'markdown'] }
"Plug 'SirVer/ultisnips'
Plug 'sjl/gundo.vim'
Plug 'tpope/vim-fugitive'
Plug 'triglav/vim-visual-increment' ", { 'on': ['FIXME'] }
Plug 'wakatime/vim-wakatime'
" Plug 'ycm-core/YouCompleteMe', { 'do': './install.py --clangd-completer --cs-completer --go-completer --rust-completer --java-completer --ts-completer' }

" let g:ycm_key_list_select_completion = ['n', '<Down>']
" let g:ycm_key_list_previous_completion = ['p', '<Up>']

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

" Initialize plugin system
call plug#end()

" =======================================
" Theme
" =======================================
set t_ut=
if (has("termguicolors"))
 set termguicolors
endif

syntax enable
colorscheme night-owl
set guifont=Hack

" =======================================
" Leader
" =======================================
nnoremap <Space> <NOP>
let mapleader = " "

nnoremap <leader>fd :write<CR>
nnoremap <leader>bd :bd<CR>

nmap <leader>gl :GetCommitLink<CR>
xmap <leader>gl :GetCommitLink<CR>

nmap <leader>ga <Plug>(GitGutterStageHunk)
nmap <leader>gu <Plug>(GitGutterUndoHunk)
nmap <leader>gp <Plug>(GitGutterPreviewHunk)

" nnoremap <leader>ld :YcmCompleter GoToDefinition<CR>
" nnoremap <leader>lr :YcmCompleter GoToReferences<CR>
" nnoremap <leader>li :YcmCompleter GoToImplementation<CR>
" nnoremap <leader>lt :YcmCompleter GoToType<CR>

nmap <Leader>ff <Plug>(PrettierAsync)
xmap <Leader>ff :PrettierPartial<CR>

" =======================================
" fzf
" =======================================
function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction

command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)

nnoremap <leader>pe :Files<CR>
nnoremap <leader>pf :Files<CR>
nnoremap <leader>pa :RG<CR>

" =======================================
" CoC
" =======================================
" let g:coc_enable_locationlist = 0
" autocmd User CocLocationsChange CocList -no-quit --first --normal location
"  list.normalMappings": {
"    '<M-n>': "command:CocNext",
"    '<M-p>': "command:CocPrev",
"  },

" inoremap <M-n> :CocNext<CR>

"let g:ycm_key_list_select_completion = ['n', '<Down>']
"let g:ycm_key_list_previous_completion = ['p', '<Up>']
