" junnegunn/vim-plug
" need .vim/autoload/plug.vim
" call :PlugInstall to install and update
call plug#begin()

" org mode
Plug 'tpope/vim-speeddating' | Plug 'jceb/vim-orgmode'

" good settings
Plug 'tpope/vim-sensible'

" syntax highlighting for Julia
Plug 'JuliaLang/julia-vim'

" syntax highlighting for COOL
Plug 'vim-scripts/cool.vim'
au BufNewFile,BufRead *.cool setf cool 
au BufNewFile,BufRead *.cl setf cool 

" linters!!!!!
Plug 'scrooloose/syntastic'

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_tex_checkers = ['chktex']

let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11'

" treat contents of some tex environments as verbatim text
au filetype tex syntax region texZone start='\\begin{lstlisting}' end='\\end{lstlisting}'
au filetype tex syntax region texZone start='\\begin{python3code}' end='\\end{python3code}'
au filetype tex syntax region texZone start='\\begin{bashcode}' end='\\end{bashcode}'
au filetype tex syntax region texZone start='\\begin{pyconcode}' end='\\end{pyconcode}'

" C and Bison/Flex
Plug 'justinmk/vim-syntax-extra'

" status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
let g:airline_theme='silver'

" markdown
Plug 'plasticboy/vim-markdown'
Plug 'godlygeek/tabular'
call plug#end()

" map semicolon to colon
:nmap ; :

" indentation
set expandtab
set shiftwidth=4
set tabstop=4

filetype plugin indent on

" show line numbers
set number
set relativenumber

" load local vim settings
if filereadable(".vim.custom")
    so .vim.custom
endif

if filereadable(".vimrc.local")
    so .vimrc.local
endif

" reload a file if it's changed by another process
set autoread

" crosshairs
set cursorcolumn
set cursorline
hi  CursorLine cterm=NONE ctermbg=black ctermfg=white guibg=black guifg=white
hi  CursorColumn cterm=NONE ctermbg=black ctermfg=white guibg=black guifg=white

" highlight searches
set hlsearch
hi IncSearch
set incsearch

" unsets the "last search pattern" register by hitting return
nnoremap <CR> :nohlsearch<CR><CR>

" highlight column 79
set colorcolumn=79
