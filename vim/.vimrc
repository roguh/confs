" use Vim defaults instead of Vi's
" warning: keep this near the top of the config file
set nocompatible

" junnegunn/vim-plug
" need .vim/autoload/plug.vim
" call :PlugInstall to install and update
call plug#begin()

" Theme
" Plug 'dylanaraps/wal.vim'
" execute 'silent colorscheme wal'

" Plug 'sonph/onehalf', { 'rtp': 'vim' }
" execute 'silent colorscheme onehalfdark'
" let g:airline_theme='onehalfdark'

Plug 'drewtempelmeyer/palenight.vim'

" Reload files edited externally
Plug 'djoshea/vim-autoread'

" Plug 'ayu-theme/ayu-vim'
" colorscheme ayu

" Plug 'arcticicestudio/nord-vim'
" let g:nord_italic = 1
" let g:nord_underline = 1
" let g:nord_italic_comments = 1
" let g:nord_comment_brightness = 20
" let g:nord_cursor_line_number_background = 1
" augroup nord
"   autocmd!
"   autocmd ColorScheme nord highlight shDerefSimple ctermfg=6 guifg=#88C0D0
"   autocmd ColorScheme nord highlight shDerefVar ctermfg=6 guifg=#88C0D0
"   autocmd ColorScheme nord highlight shVariable ctermfg=6 guifg=#88C0D0
" augroup END
" autocmd VimEnter * execute 'silent colorscheme nord'
"
" https://github.com/beloglazov/vim-online-thesaurus
" https://github.com/tpope/vim-abolish

" Good statusline
Plug 'vim-airline/vim-airline'
let g:airline#extensions#ale#enabled = 1

" Support opening filename:line:column
" vim, gF, :e[dit] /path/to/file:100:12
Plug 'wsdjeg/vim-fetch'

" Git gutter
Plug 'airblade/vim-gitgutter'

" THE BEST GIT PORCELAIN IN THE VIM WORLD
Plug 'tpope/vim-fugitive'

" Strip trailing whitespace on changed lines only
" Use :WStrip to clean all trailing whitespace
Plug 'tweekmonster/wstrip.vim'

" Plug 'scrooloose/nerdtree'
" Plug 'Xuyuanp/nerdtree-git-plugin'

" good default settings
" "one step above the nocompatible setting"
" A taste of its features:
"   'backspace': Backspace through anything in insert mode.
"   'incsearch': Start searching before pressing enter.
"   'listchars': Makes :set list (visible whitespace) prettier.
"   'scrolloff': Always show at least one line above/below the cursor.
"   'autoread': Autoload file changes. You can undo by pressing u.
"   runtime! macros/matchit.vim: Load the version of matchit.vim that ships with Vim.
" Plug 'tpope/vim-sensible'

" Various language syntax definitions. Loads much faster than individual plugins.
" Languages supported as of 09/2018:
" ansible, apiblueprint, applescript, arduino, asciidoc, autohotkey, blade,
" c++11, c/c++, caddyfile, carp, cjsx, clojure, cmake, coffee-script, cql,
" cryptol, crystal, cucumber, dart, dockerfile, elixir, elm, emberscript,
" emblem, erlang, ferm, fish, fsharp, git, glsl, gmpl, gnuplot, go, graphql,
" groovy, haml, handlebars, haproxy, haskell, haxe, html5, i3, jasmine,
" javascript, jenkins, json5, json, jst, jsx, julia, kotlin, latex, less,
" liquid, livescript, lua, mako, markdown, mathematica, nginx, nim, nix,
" objc, ocaml, octave, opencl, perl, pgsql, php, plantuml, powershell,
" protobuf, pug, puppet, purescript, python-compiler, python-ident, python,
" qml, r-lang, racket, ragel, raml, rspec, ruby, rust, sbt, scala, scss,
" slim, slime, solidity, stylus, swift, sxhkd, systemd, terraform, textile,
" thrift, tmux, tomdoc, toml, twig, typescript, vala, vbnet, vcl, vifm, vm,
" vue, xls, yaml, yard
Plug 'sheerun/vim-polyglot'
let g:polyglot_disabled = ['python', 'ocaml', 'org']

let g:jsx_ext_required = 0

Plug 'tpope/vim-speeddating'

" Deoplete code completion framework
" https://github.com/Shougo/deoplete.nvim/wiki/Completion-Sources
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

let g:deoplete#enable_at_startup = 1
let g:deoplete#complete_method = "complete" " merlin compat?
let g:deoplete#auto_complete_delay = 0

Plug 'carlitux/deoplete-ternjs', { 'do': 'echo run npm install -g tern' }

" Use tern_for_vim.
let g:tern#command = ["tern"]
let g:tern#arguments = ["--persistent"]

" USE TAB!!!!!!
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

" Completion based on syntax
Plug 'Shougo/neco-syntax'

" merlin, autocomplete for ocaml
" TODO only load for ocaml
" TODO only load if opam exists
""  let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
""  execute 'set rtp+=' . g:opamshare . '/merlin/vim'
""  execute 'set rtp+=' . g:opamshare . '/ocp-indent/vim'
""
" Completion for ocaml using merlin
" install merline with:
" opam install merlin

Plug 'rgrinberg/vim-ocaml', { 'for': 'ocaml' }

" deoplete and ocaml
Plug 'copy/deoplete-ocaml', { 'for': 'ocaml' }

" Reason
Plug 'reasonml-editor/vim-reason-plus'

" Haskell
" needs ghc-mod
Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

" Python code-completion, many other features
Plug 'davidhalter/jedi-vim', { 'for': 'python' }
Plug 'zchee/deoplete-jedi'

" J, APL derivative
Plug 'guersam/vim-j'

" HTML, CSS
Plug 'othree/html5.vim'
Plug 'ap/vim-css-color'

" JS
Plug 'pangloss/vim-javascript', { 'for': [ 'javascript', 'javascript.jsx' ] }

Plug 'othree/javascript-libraries-syntax.vim', { 'for': 'javascript' }
Plug 'moll/vim-node', { 'for': [ 'javascript', 'javascript.jsx' ] }

" Rust
Plug 'racer-rust/vim-racer', { 'for': 'rust' }

" JS Elm
" Plug 'ElmCast/elm-vim'

" Pandoc
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'

" handlebar and mustache templates
" Plug 'mustache/vim-mustache-handlebars'
" Plug 'evidens/vim-twig'
" Plug 'lepture/vim-jinja'

" org mode
" Plug 'jceb/vim-orgmode', { 'for': 'org' }
Plug 'jwiegley/org-mode', { 'for': 'org' }

" syntax highlighting for COOL
au BufNewFile,BufRead *.cool setf cool
au BufNewFile,BufRead *.cl setf cool
Plug 'vim-scripts/cool.vim', { 'for': 'cool' }

" fuzzy file finding
if !filereadable('/usr/local/opt/fzf')
  Plug '/usr/local/opt/fzf'
endif

Plug 'junegunn/fzf.vim'

" if using git, find files in project, not just cwd
function! s:find_project_root()
  return system('git rev-parse --show-toplevel 2> /dev/null')[:-2]
endfunction

command! ProjectFiles execute 'Files' s:find_project_root()

" use ctrl-p to find files
map <C-p> :ProjectFiles<CR>

" use ag or ack to search for text in files
" use the :Ack command instead of :grep
Plug 'mileszs/ack.vim'

if executable('ag')
  let g:ackprg = 'ag --vimgrep --ignore dist/'
endif

" asynchronous grep results
" default search tool is whatever is available first from ag, ack, grep, findstr, rg, pt, git
" leave an empty query to search for word under the cursor
Plug 'mhinz/vim-grepper'

" search from project root
" let g:grepper.repo = ['.project', '.git', '.SVN', 'node-packages.json']

command! ProjectGrepper execute 'Grepper -dir repo'
map <C-f> :ProjectGrepper<CR>

" file browser
Plug 'francoiscabrol/ranger.vim'
if has('nvim')
  Plug 'rbgrouleff/bclose.vim'
end

let g:ranger_replace_netrw = 1
let g:ranger_map_keys = 0
map <C-n> :Ranger<CR>

" async linting. lints as you type
Plug 'w0rp/ale', { 'for': ['javascript', 'python', 'c'] }

" open window
" let g:ale_open_list = 1
let g:ale_linters = {
\   'javascript': ['standard', 'eslint'],
\   'c': ['clang', 'gcc', 'cppcheck', 'flawfinder']
\}

let g:ale_javascript_standard_options = '--parser babel-eslint'

let g:ale_fix_on_save = 1

let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'javascript': ['standard'],
\   'python': ['autopep8'],
\   'c': ['clang', 'gcc', 'cppcheck', 'uncrustify']
\}

" C-k and C-j to move between ALE errors
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

" linters (syntax checkers, other code checkers)
Plug 'scrooloose/syntastic', { 'for': ['html', 'python', 'rust', 'css', 'scss', 'typescript', 'cpp', 'c', 'ocaml', 'java'] }

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
" show warnings AND errors
let g:syntastic_quiet_messages = {'level': 'none'}

let g:syntastic_rust_checkers = ['cargo']

let g:syntastic_python_checkers = ['python', 'flake8', 'pycodestyle', 'pylint']
let g:syntastic_python_pylint_exe = 'pylint'

let g:syntastic_tex_checkers = ['chktex']

let g:syntastic_javascript_checkers = []

" Prefer local node_modules instead of global
Plug 'mtscout6/syntastic-local-eslint.vim'
" if executable('node_modules/.bin/eslint')
"       let b:syntastic_javascript_eslint_exec = 'node_modules/.bin/eslint'
" endif

let g:syntastic_html_checkers = ['tidy', 'jshint']

let g:syntastic_css_checkers = ['csslint', 'stylelint', 'scss_lint']
let g:syntastic_scss_checkers = ['scss_lint']

let g:syntastic_typescript_checkers = ['tslint']

let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11'

let g:syntastic_c_checkers = ['clang_check', 'gcc', 'splint', 'clang_tidy']
let g:syntastic_cpp_checkers = ['clang_check', 'gcc', 'splint', 'clang_tidy']

let g:syntastic_ocaml_checkers = ['merlin']

" treat contents of some tex environments as verbatim text
au filetype tex syntax region texZone start='\\begin{lstlisting}' end='\\end{lstlisting}'
au filetype tex syntax region texZone start='\\begin{python3code}' end='\\end{python3code}'
au filetype tex syntax region texZone start='\\begin{bashcode}' end='\\end{bashcode}'
au filetype tex syntax region texZone start='\\begin{pyconcode}' end='\\end{pyconcode}'

" Folding for C, Fortran, Java, CPP
Plug 'pseewald/anyfold', { 'for': ['c', 'cpp', 'java', 'fortran', 'javascript', 'javascript.jsx'] }
autocmd Filetype c,cpp,java,fortran,javascript,javascript.jsx AnyFoldActivate
let g:anyfold_fold_comments=1

" close folds
set foldlevel=0

" Improved syntax highlighting for C, add syntax highlighting for Bison, and Flex
Plug 'justinmk/vim-syntax-extra', { 'for': [ 'c', 'cpp', 'y', 'l' ] }

Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'

Plug 'jceb/vim-orgmode'

" Save session every X minutes
let g:session_autosave_periodic = 1

let g:session_autosave_silent = 1

call plug#end()

set background=dark
colorscheme palenight

" read the real vim config
if filereadable(expand('~/.vimrc.minimal'))
    so ~/.vimrc.minimal
endif
