" junnegunn/vim-plug
" need .vim/autoload/plug.vim
" call :PlugInstall to install and update
call plug#begin()

" Theme
Plug 'dylanaraps/wal.vim'

" good default settings
Plug 'tpope/vim-sensible'

" Various language syntax definitions, includes fish
" LOADS MUCH FASTER THAN INDIVIDUAL PLUGINS
" as of 09 2018:
" ansible, apiblueprint, applescript, arduino, asciidoc, autohotkey, blade, c++11, c/c++, caddyfile, carp, cjsx, clojure, cmake, coffee-script, cql, cryptol, crystal, cucumber, dart, dockerfile, elixir, elm, emberscript, emblem, erlang, ferm, fish, fsharp, git, glsl, gmpl, gnuplot, go, graphql, groovy, haml, handlebars, haproxy, haskell, haxe, html5, i3, jasmine, javascript, jenkins, json5, json, jst, jsx, julia, kotlin, latex, less, liquid, livescript, lua, mako, markdown, mathematica, nginx, nim, nix, objc, ocaml, octave, opencl, perl, pgsql, php, plantuml, powershell, protobuf, pug, puppet, purescript, python-compiler, python-ident, python, qml, r-lang, racket, ragel, raml, rspec, ruby, rust, sbt, scala, scss, slim, slime, solidity, stylus, swift, sxhkd, systemd, terraform, textile, thrift, tmux, tomdoc, toml, twig, typescript, vala, vbnet, vcl, vifm, vm, vue, xls, yaml, yard
Plug 'sheerun/vim-polyglot'
let g:polyglot_disabled = ['python', 'ocaml']

" Completion
Plug 'ervandew/supertab'

" merlin, autocomplete for ocaml
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
" execute "set rtp+=" . g:opamshare . "/merlin/vim"
execute "set rtp+=" . g:opamshare . "/ocp-indent/vim"
" Completion for ocaml using merlin
" install merline with:
" opam install merlin
au FileType ocaml call SuperTabSetDefaultCompletionType("<c-x><c-o>")

Plug 'rgrinberg/vim-ocaml', { 'for': 'ocaml' }

Plug 'reasonml-editor/vim-reason-plus'

" PYTHON MAGIC
Plug 'davidhalter/jedi-vim', { 'for': 'python' }

" J, APL derivative
Plug 'guersam/vim-j'

" JS, React Native
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }

" JS Elm
" Plug 'ElmCast/elm-vim'

" Pandoc
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'

" handlebar and mustache templates
" Plug 'mustache/vim-mustache-handlebars'
" Plug 'evidens/vim-twig'
" Plug 'lepture/vim-jinja'

" ++ or -- dates/times/more using Ctrl-A Ctrl-X
" Plug 'tpope/vim-speeddating' 

" org mode
Plug 'jceb/vim-orgmode', { 'for': 'org' }

" syntax highlighting for COOL
au BufNewFile,BufRead *.cool setf cool 
au BufNewFile,BufRead *.cl setf cool 
Plug 'vim-scripts/cool.vim', { 'for': 'cool' }

" linters!!!!!
Plug 'scrooloose/syntastic'

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

let g:syntastic_rust_checkers = ["cargo"]

let g:syntastic_python_checkers = ["python3", "flake8"]

let g:syntastic_tex_checkers = ['chktex']

let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11'

let g:syntastic_ocaml_checkers = ['merlin']

" treat contents of some tex environments as verbatim text
au filetype tex syntax region texZone start='\\begin{lstlisting}' end='\\end{lstlisting}'
au filetype tex syntax region texZone start='\\begin{python3code}' end='\\end{python3code}'
au filetype tex syntax region texZone start='\\begin{bashcode}' end='\\end{bashcode}'
au filetype tex syntax region texZone start='\\begin{pyconcode}' end='\\end{pyconcode}'

" Syntax highlighting for C (improved), Bison, and Flex
Plug 'justinmk/vim-syntax-extra'

call plug#end()

" read the real vim config
if filereadable(expand("~/.vimrc.minimal"))
    so ~/.vimrc.minimal
endif

" switch to wal colorscheme if wal theme exists 
if filereadable(expand("~/.vim/plugged/wal.vim/colors/wal.vim"))
    execute "silent colorscheme wal"
endif
