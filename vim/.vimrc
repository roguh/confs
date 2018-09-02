" junnegunn/vim-plug
" need .vim/autoload/plug.vim
" call :PlugInstall to install and update
call plug#begin()

" Various language syntax definitions, includes fish
Plug 'sheerun/vim-polyglot'

" Completion
Plug 'ervandew/supertab'

" Completion for ocaml using merlin
" install merline with
" opam install merlin
au FileType ocaml call SuperTabSetDefaultCompletionType("<c-x><c-o>")

" PYTHON MAGIC
Plug 'davidhalter/jedi-vim'
Plug 'nvie/vim-flake8'

" Themes
Plug 'dylanaraps/wal.vim'

" JS React Native
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'reasonml-editor/vim-reason'


" JS Elm
Plug 'ElmCast/elm-vim'

" Pandoc
Plug 'vim-pandoc/vim-pandoc-syntax'
au! BufRead,BufNewFile,BufFilePre *.md setf markdown.pandoc

" handlebar and mustache templates
Plug 'mustache/vim-mustache-handlebars'
Plug 'evidens/vim-twig'
Plug 'lepture/vim-jinja'

" Rust
Plug 'rust-lang/rust.vim'
let g:rustfmt_autosave = 1

Plug 'neomake/neomake'
highlight NeomakeMessage guifg=red   guibg=white  cterm=underline ctermfg=red   ctermbg=white
highlight NeomakeError   guifg=red   guibg=white  cterm=underline ctermfg=red   ctermbg=white
highlight NeomakeWarning guifg=green guibg=white  cterm=underline ctermfg=green ctermbg=white
highlight NeomakeInfo    guifg=black guibg=orange cterm=underline ctermfg=black ctermbg=white

" autocmd! BufWritePost,BufEnter *.rs Neomake cargo

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

" cargo syntastic checker 
Plug 'Nonius/cargo.vim'

let g:syntastic_rust_checkers = ["cargo"]

let g:syntastic_python_checkers = ["python3"]

let g:syntastic_ocaml_checkers = ['merlin']

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

call plug#end()

if filereadable(expand("~/.vimrc.minimal"))
    so ~/.vimrc.minimal
endif

" let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
" execute "set rtp+=" . g:opamshare . "/merlin/vim"
