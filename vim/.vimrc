" Hugo O. Rivera's vim/neovim config

" Use Vim defaults instead of Vi's.
" Warning: keep this near the top of the config file.
set nocompatible

" junnegunn/vim-plug
" Need .vim/autoload/plug.vim
" Call :PlugInstall or :PlugUpdate.

call plug#begin()

" Color themes
Plug 'sonph/onehalf', { 'rtp': 'vim' }
Plug 'drewtempelmeyer/palenight.vim'
Plug 'dylanaraps/wal.vim'
Plug 'AlessandroYorba/Sierra'
Plug 'ayu-theme/ayu-vim'
Plug 'habamax/vim-colors-defminus'
Plug 'habamax/vim-colors-lessthan'

" Reload files edited externally
Plug 'djoshea/vim-autoread'

" Writer's plugins

" Use :OnlineThesaurusCurrentWord or :Thesaurus word
Plug 'beloglazov/vim-online-thesaurus'
let g:online_thesaurus_map_keys = 0

" Good statusline
Plug 'vim-airline/vim-airline'
let g:airline#extensions#ale#enabled = 1

" Support opening filename:line:column
" vim, gF, :e[dit] /path/to/file:100:12
Plug 'wsdjeg/vim-fetch'

" Git gutter
" Plug 'airblade/vim-gitgutter'
" Plug 'mhinz/vim-signify'

" Show git diff in window when writing git commit message
Plug 'rhysd/committia.vim'

" THE BEST GIT PORCELAIN IN THE VIM WORLD
Plug 'tpope/vim-fugitive'

" Strip trailing whitespace on changed lines only
" Use :WStrip to clean all trailing whitespace
Plug 'tweekmonster/wstrip.vim'

" Plug 'scrooloose/nerdtree'
" Plug 'Xuyuanp/nerdtree-git-plugin'

" Advanced search and replace (or grep):
" :%Subvert/facilit{y,ies}/building{,s}/g
" :Subvert/child{,ren}/adult{,s}/g
" :Subvert/di{e,ce}/spinner{,s}/g
"
" I know how to spell "separate". I know how to spell "desperate". My fingers, however, have trouble distinguishing between the two, and I invariably have a 50 percent chance of typing "seperate" or "desparate" each time one of these comes up.
" :Abolish {despa,sepe}rat{e,es,ed,ing,ely,ion,ions,or}  {despe,sepa}rat{}
Plug 'tpope/vim-abolish'

" Good default settings
" "one step above the nocompatible setting"
" A taste of its features:
"   'backspace': Backspace through anything in insert mode.
"   'incsearch': Start searching before pressing enter.
"   'listchars': Makes :set list (visible whitespace) prettier.
"   'scrolloff': Always show at least one line above/below the cursor.
"   'autoread': Autoload file changes. You can undo by pressing u.
"   runtime! macros/matchit.vim: Load the version of matchit.vim that ships with Vim.
" I enable most of these manually.
" Plug 'tpope/vim-sensible'

" Various language syntax definitions. Loads much faster than individual plugins.
" Languages supported as of 02/2020:
" acpiasl (syntax), ansible (syntax, indent, ftplugin), apiblueprint (syntax), applescript (syntax, indent), arduino (syntax, indent), asciidoc (syntax), autohotkey (indent), blade (syntax, indent, ftplugin), c++11 (syntax), c/c++ (syntax), caddyfile (syntax, indent, ftplugin), carp (syntax), cjsx (syntax, ftplugin), clojure (syntax, indent, autoload, ftplugin), cmake (syntax, indent), coffee-script (syntax, compiler, indent, autoload, ftplugin), cql (syntax), cryptol (syntax, compiler, ftplugin), crystal (syntax, indent, autoload, ftplugin), csv (syntax, autoload, ftplugin), cucumber (syntax, indent, compiler, ftplugin), cue (syntax), dart (syntax, indent, autoload, ftplugin), dhall (syntax, ftplugin), dlang (syntax, indent), dockerfile (syntax, indent, ftplugin), elixir (syntax, indent, compiler, autoload, ftplugin),
" elm (syntax, indent, autoload, ftplugin), emberscript (syntax, indent, ftplugin), emblem (syntax, indent, ftplugin), erlang (syntax, indent), ferm (syntax), fish (syntax, indent, compiler, autoload, ftplugin), flatbuffers (syntax), fsharp (syntax, indent), git (syntax, indent, ftplugin), glsl (syntax, indent), gmpl (syntax), gnuplot (syntax), go (syntax, compiler, indent), gradle (compiler), graphql (syntax, indent, autoload, ftplugin, after), groovy-indent (indent), groovy (syntax), haml (syntax, indent, compiler, ftplugin), handlebars (syntax, indent, ftplugin), haproxy (syntax),
" haskell (syntax, indent, ftplugin), haxe (syntax), hcl (syntax, indent, ftplugin), helm (syntax), hive (syntax, ftplugin), html5 (syntax, indent, autoload, ftplugin), i3 (syntax, ftplugin), idris (syntax, indent, ftplugin), ion (syntax, ftplugin), jasmine (syntax), javascript (syntax, indent, compiler, ftplugin, extras), jenkins (syntax, indent), jinja (syntax, indent), json5 (syntax), json (syntax, indent, ftplugin), jst (syntax, indent), jsx (autoload, after),
" julia (syntax, indent, autoload, ftplugin), kotlin (syntax, indent, ftplugin), latex (syntax, indent, ftplugin), less (syntax, indent, ftplugin), lilypond (syntax, indent, compiler, ftplugin), livescript (syntax, indent, compiler, ftplugin), llvm (syntax, indent, ftplugin), log (syntax), lua (syntax, indent), mako (syntax, indent, ftplugin), markdown (syntax, indent, ftplugin), mathematica (syntax, ftplugin), mdx (syntax), meson (syntax, indent, ftplugin), moonscript (syntax, indent, ftplugin), nginx (syntax, indent, ftplugin), nim (syntax, compiler, indent), nix (syntax, indent, compiler, ftplugin), objc (ftplugin, syntax, indent), ocaml (syntax, indent, compiler, ftplugin),
" octave (syntax, indent), opencl (syntax, indent, ftplugin), perl (syntax, indent, ftplugin), pgsql (syntax, indent), php (syntax), plantuml (syntax, indent, ftplugin), pony (syntax, indent, autoload, ftplugin), powershell (syntax, indent, ftplugin), protobuf (syntax, indent), pug (syntax, indent, ftplugin), puppet (syntax, indent, autoload, ftplugin), purescript (syntax, indent, ftplugin), python-compiler (compiler, autoload), python-indent (indent), python (syntax), qmake (syntax), qml (syntax, indent, ftplugin), r-lang (syntax), racket (syntax, indent, ftplugin), ragel (syntax), raml (syntax, ftplugin),
" reason (syntax, indent), rspec (syntax), rst (syntax, indent, autoload, ftplugin), ruby (syntax, indent, compiler, autoload, ftplugin), rust (syntax, indent, compiler, autoload, ftplugin), sbt (syntax), scala (syntax, indent, compiler, ftplugin), scss (syntax, indent, ftplugin), slim (syntax, indent, ftplugin), slime (syntax, indent), smt2 (syntax, autoload, ftplugin), solidity (syntax, indent, ftplugin), stylus (syntax, indent, ftplugin), svelte (syntax, indent), svg-indent (indent),
" svg (syntax), swift (syntax, indent, ftplugin), sxhkd (syntax), systemd (syntax, ftplugin), terraform (syntax, indent, autoload, ftplugin), textile (syntax, ftplugin), thrift (syntax), tmux (syntax, ftplugin), tomdoc (syntax), toml (syntax, ftplugin), tptp (syntax), twig (syntax, indent, ftplugin), typescript (syntax, indent, compiler, ftplugin), v (syntax, indent, ftplugin), vala (syntax, indent, ftplugin), vbnet (syntax), vcl (syntax), vifm (syntax, autoload, ftplugin), vm (syntax, indent), vue (syntax, indent, ftplugin), xdc (syntax), xls (syntax), xml (syntax), yaml (syntax, ftplugin), yard (syntax), zephir (syntax), zig (syntax, autoload, ftplugin)
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

let g:deoplete#enable_at_startup = 0
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

" Pandoc
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'

" Jinja HTML templates
Plug 'lepture/vim-jinja'

" Org mode
Plug 'jwiegley/org-mode', { 'for': 'org' }
Plug 'jceb/vim-orgmode', { 'for': 'org' }

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

" Asynchronous grep results.
" Default search tool is whatever is available first from:
" ag, ack, grep, findstr, rg, pt, git
" Leave an empty query to search for word under the cursor.
Plug 'mhinz/vim-grepper'

" Search starting from project root.
" let g:grepper.repo = ['.project', '.git', '.SVN', 'node-packages.json']

command! ProjectGrepper execute 'Grepper -dir repo'
map <C-f> :ProjectGrepper<CR>

" File browser.
Plug 'francoiscabrol/ranger.vim'
if has('nvim')
  Plug 'rbgrouleff/bclose.vim'
end

let g:ranger_replace_netrw = 1
let g:ranger_map_keys = 0
map <C-n> :Ranger<CR>

" Async linting lints as you type.
Plug 'w0rp/ale', { 'for': ['javascript', 'python', 'c', 'css', 'scss'] }

let g:ale_cache_executable_check_failures = 1

" let g:ale_close_preview_on_insert = 1
let g:ale_cursor_detail = 0

" show errors on save
let g:ale_open_list = 0
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 1

" no sign in gutter
let g:ale_set_signs = 1
let g:ale_sign_error = '>'
let g:ale_sign_warning = '-'

" open window
" let g:ale_open_list = 1
" pacman -S {python,python2}-{pydocstyle,isort} mypy
let g:ale_linters = {
\   'javascript': ['standard', 'eslint', 'prettier'],
\   'python': ['pydocstyle', 'flake8', 'isort', 'mypy', 'black'],
\   'css': ['stylelint'],
\   'c': ['clang', 'gcc', 'cppcheck', 'flawfinder']
\}

let g:ale_javascript_standard_options = '--parser babel-eslint'

let g:ale_fix_on_save = 1

let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'javascript': ['standard', 'eslint', 'prettier'],
\   'css': ['prettier', 'stylelint'],
\   'python': [
\     'autopep8',
\     'black',
\     'isort',
\     'remove_trailing_lines',
\     'reorder-python-imports',
\     'trim_whitespace'],
\   'c': ['uncrustify']
\}

" C-k and C-j to move between ALE errors
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

" Linters (syntax checkers, other code checkers):
Plug 'scrooloose/syntastic', { 'for': ['html', 'python', 'rust', 'typescript', 'cpp', 'c', 'ocaml', 'java'] }

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

" Show warnings AND errors.
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
Plug 'pseewald/anyfold', { 'for': ['c', 'cpp', 'java', 'fortran', 'javascript', 'javascript.jsx', 'python', 'markdown'] }

" Improved syntax highlighting for C, add syntax highlighting for Bison, and Flex
Plug 'justinmk/vim-syntax-extra', { 'for': [ 'c', 'cpp', 'y', 'l' ] }

" Use :OpenSession :SaveSession
" Useful: :RestartVim to save session, close and restart vim, and then reload
" session. Great for debugging vim scripts.
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'

" Save session every X minutes
let g:session_autosave_periodic = 1
let g:session_autosave_silent = 1
let g:session_autoload = 'no'

Plug 'nathanaelkane/vim-indent-guides'
let g:indent_guides_enable_on_vim_startup = 1
" Sample:
                                                                 let test_var = 0

call plug#end()

" Must go outside of vim-plug's config block
autocmd Filetype c,cpp,java,fortran,javascript,javascript.jsx,python,markdown AnyFoldActivate
let g:anyfold_fold_comments=1

try
  set background=dark
  let g:sierra_Pitch = 1
  " colorscheme sierra

  " colorscheme wal

  " colorscheme onehalfdark
  " let g:airline_theme='onehalfdark'

  " set termguicolors     " enable true colors support
  " let ayucolor="light"  " for light version of theme
  " let ayucolor="mirage" " for mirage version of theme
  " let ayucolor="dark"   " for dark version of theme
  " colorscheme ayu

  colorscheme defminus
  " colorscheme lessthan
catch
  " no colors!
endtry

" Read the real vim config.
if filereadable(expand('~/.vimrc.minimal'))
    so ~/.vimrc.minimal
endif
