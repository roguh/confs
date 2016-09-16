" junnegunn/vim-plug
" need .vim/autoload/plug.vim
" call :PlugInstall to install and update
call plug#begin()

" org mode
Plug 'tpope/vim-speeddating' | Plug 'jceb/vim-orgmode'

" good settings
Plug 'tpope/vim-sensible'

" syntax highlighting for COOL
Plug 'vim-scripts/cool.vim'
au BufNewFile,BufRead *.cool setf cool 
au BufNewFile,BufRead *.cl setf cool 

" linters!!!!!
Plug 'scrooloose/syntastic'
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_tex_checkers = ['chktex']

au filetype tex syntax region texZone start='\\begin{lstlisting}' end='\\end{lstlisting}'
au filetype tex syntax region texZone start='\\begin{python3code}' end='\\end{python3code}'
au filetype tex syntax region texZone start='\\begin{bashcode}' end='\\end{bashcode}'
au filetype tex syntax region texZone start='\\begin{pyconcode}' end='\\end{pyconcode}'

" LaTeX
Plug 'LaTeX-Box-Team/LaTeX-Box'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
let g:airline_theme='serene'

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

" killer status line
set statusline=
set statusline +=%5*%{&ff}%*            "file format
set statusline +=%3*%y%*                "file type
set statusline +=%1*%=%5l%*             "current line
set statusline +=%2*/%L%*               "total lines
set statusline +=%4*\ %<%F%*            "full path

if filereadable(".vim.custom")
    so .vim.custom
endif
