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
Plug 'dense-analysis/ale'
Plug 'editorconfig/editorconfig-vim'
Plug 'google/vim-jsonnet', { 'for': ['jsonnet'] }
Plug 'haishanh/night-owl.vim'
Plug 'hwayne/tla.vim'
"Plug 'honza/vim-snippets'
Plug 'LunarWatcher/auto-pairs'
Plug 'kevinoid/vim-jsonc', { 'for': ['json'] }
Plug 'knsh14/vim-github-link'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
Plug 'nvim-treesitter/nvim-treesitter-textobjects'
Plug 'prettier/vim-prettier', { 'for': ['typescript', 'javascript', 'markdown'] }
Plug 'rust-lang/rust.vim'
Plug 'sindrets/winshift.nvim'
"Plug 'SirVer/ultisnips'
Plug 'sjl/gundo.vim'
Plug 'tpope/vim-fugitive'
Plug 'wakatime/vim-wakatime'

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
" Telescope
" =======================================
nnoremap <leader>pe <cmd>Telescope find_files<cr>
nnoremap <leader>pf <cmd>Telescope find_files<cr>
nnoremap <leader>pa <cmd>Telescope live_grep<cr>
nnoremap <leader>b <cmd>Telescope buffers<cr>
nnoremap <leader>h <cmd>Telescope help_tags<cr>

" =======================================
" Tree sitter
" =======================================
lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  sync_install = false, -- install languages synchronously (only applied to `ensure_installed`)
  ignore_install = { }, -- List of parsers to ignore installing
  highlight = {
    enable = true,      -- false will disable the whole extension
    disable = { },      -- list of language that will be disabled
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
}
EOF

" =======================================
" Tree sitter text objects
" =======================================
lua <<EOF
require'nvim-treesitter.configs'.setup {
  textobjects = {
    select = {
      enable = true,

      -- Automatically jump forward to textobj, similar to targets.vim
      lookahead = true,

      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
        ["aa"] = "@parameter.outer",
        ["ia"] = "@parameter.inner",
        ["a;"] = "@comment.outer",
        ["ab"] = "@block.outer",
        ["ib"] = "@block.inner",

        -- Or you can define your own textobjects like this
        -- ["iF"] = {
        --   python = "(function_definition) @function",
        --   cpp = "(function_definition) @function",
        --   c = "(function_definition) @function",
        --   java = "(method_declaration) @function",
        -- },
      },
    },
  },
}
EOF

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

" =======================================
" Rust configuration
" =======================================
let g:rustfmt_autosave = 1

" =======================================
" Docker configuration
" =======================================
autocmd BufNewFile,BufRead Dockerfile* set syntax=dockerfile

" =======================================
" Winshift configuration
" =======================================
lua <<EOF
require("winshift").setup({
  highlight_moving_win = true,  -- Highlight the window being moved
  focused_hl_group = "Visual",  -- The highlight group used for the moving window
  moving_win_options = {
    -- These are local options applied to the moving window while it's
    -- being moved. They are unset when you leave Win-Move mode.
    wrap = false,
    cursorline = false,
    cursorcolumn = false,
    colorcolumn = "",
  },
  -- The window picker is used to select a window while swapping windows with
  -- ':WinShift swap'.
  -- A string of chars used as identifiers by the window picker.
  window_picker_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",
  window_picker_ignore = {
    -- This table allows you to indicate to the window picker that a window
    -- should be ignored if its buffer matches any of the following criteria.
    filetype = {  -- List of ignored file types
      "NvimTree",
    },
    buftype = {   -- List of ignored buftypes
      "terminal",
      "quickfix",
    },
    bufname = {   -- List of regex patterns matching ignored buffer names
      [[.*foo/bar/baz\.qux]]
    },
  },
})
EOF

nnoremap <C-H> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <C-M-H> <Cmd>WinShift left<CR>
nnoremap <C-M-J> <Cmd>WinShift down<CR>
nnoremap <C-M-K> <Cmd>WinShift up<CR>
nnoremap <C-M-L> <Cmd>WinShift right<CR>
