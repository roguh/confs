" Use Vim defaults instead of Vi's.
" Warning: keep this near the top of the config file.
set nocompatible

" junnegunn/vim-plug
" Need .vim/autoload/plug.vim
" Call :PlugInstall or :PlugUpdate.

call plug#begin()

"""""""""""""""" Load lightweight, essential, or quick-loading plugins first

" Measure startup time
Plug 'dstein64/vim-startuptime'

" Color themes
" $ trizen -S gruvbox-dark-gtk lxappearance-gtk3
" $ lxappearance
Plug 'sainnhe/gruvbox-material'

" Reload files edited externally
Plug 'djoshea/vim-autoread'

" Good statusline
Plug 'vim-airline/vim-airline'
let g:airline_powerline_fonts = 1

" Show git diff in window when writing git commit message
Plug 'rhysd/committia.vim'

" Org mode
Plug 'jwiegley/org-mode', { 'for': 'org' }
Plug 'jceb/vim-orgmode', { 'for': 'org' }

Plug 'ctrlpvim/ctrlp.vim'

" use ctrl-p to find files
map <C-p> :CtrlPMixed<CR>

" ignore files in .gitignore, include dotfiles
let g:ctrlp_user_command = 'find %s -ipath \*.git -prune -or -type f'

" Asynchronous grep results.
" Default search tool is whatever is available first from:
" ag, ack, grep, findstr, rg, pt, git
" Leave an empty query to search for word under the cursor.
Plug 'mhinz/vim-grepper'

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

" Folding for C, Fortran, Java, CPP
Plug 'pseewald/anyfold'

" Activate advanced folding
let g:anyfold_fold_comments=1
autocmd Filetype * AnyFoldActivate

" Open all folds
set foldlevel=20
set foldlevelstart=20

" Use :OpenSession :SaveSession
" Useful: :RestartVim to save session, close and restart vim, and then reload
" session. Great for debugging vim scripts.
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'

" Save session every X minutes
let g:session_autosave_periodic = 1
let g:session_autosave_silent = 1
let g:session_autoload = 'no'
let g:session_autosave = 'no'

" Rainbow coloring for parenthesis and other paired characters
" Uncomment following line for demo
" ( (( [ ] ]]] )) )
Plug 'frazrepo/vim-rainbow'
let g:rainbow_active = 1

Plug 'nathanaelkane/vim-indent-guides'
let g:indent_guides_enable_on_vim_startup = 1
" Sample:
                                                                 let test_var = 0

Plug 'lark-parser/vim-lark-syntax'

Plug 'machakann/vim-highlightedyank'
let g:highlightedyank_highlight_duration = 10000

" THE BEST GIT PORCELAIN IN THE VIM WORLD
Plug 'tpope/vim-fugitive'

" Language server protocol (LSP) for completion and other fancy IDE-like features
Plug 'neoclide/coc.nvim', {'branch': 'release', 'for': ['java', 'typescript', 'javascript', 'javascript.jsx', 'python', 'rust', 'json'] }
let g:coc_global_extensions = ['coc-syntax', 'coc-json', 'coc-git', 'coc-java', 'coc-tsserver', 'coc-pyright', 'coc-rust-analyzer']

autocmd BufWritePost *.py call CocAction('format')
autocmd BufWritePost *.py CocCommand python.sortImports
autocmd BufWritePost *.py CocCommand python.runLinting

nmap <silent> <C-h> <Plug>(coc-diagnostic-next)
nmap <silent> <C-l> <Plug>(coc-diagnostic-prev)
nmap <C-LeftMouse> :call CocAction('jumpDefinition')<CR>
nmap <F2> :call CocAction('jumpDefinition')<CR>

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Use tab for selecting completion
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

Plug 'ervandew/supertab'


Plug 'stevearc/vim-arduino'

" Strip trailing whitespace on changed lines only
" Use :WStrip to clean all trailing whitespace
Plug 'tweekmonster/wstrip.vim'

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
let g:polyglot_disabled = ['ocaml', 'org', 'python']

Plug 'ap/vim-css-color'

"""""""""""""""" Load heavy and/or extra plugins
if $VIM_LOAD_EXTRA_PLUGINS == "true"
    echo "Loading all plugins"

    let g:jsx_ext_required = 0

    " Completion based on syntax
    Plug 'Shougo/neco-syntax'

    Plug 'rgrinberg/vim-ocaml', { 'for': 'ocaml' }

    " Reason
    Plug 'reasonml-editor/vim-reason-plus'

    " Haskell
    " needs ghc-mod
    Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }
    let g:haskellmode_completion_ghc = 0
    autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

    " J, APL derivative
    Plug 'guersam/vim-j'

    " HTML, CSS
    Plug 'othree/html5.vim'
    Plug 'ap/vim-css-color'

    " JS
    Plug 'pangloss/vim-javascript', { 'for': [ 'javascript', 'javascript.jsx' ] }

    Plug 'othree/javascript-libraries-syntax.vim', { 'for': 'javascript' }
    Plug 'moll/vim-node', { 'for': [ 'javascript', 'javascript.jsx' ] }

    " Pandoc
    Plug 'vim-pandoc/vim-pandoc'
    Plug 'vim-pandoc/vim-pandoc-syntax'

    " Jinja HTML templates
    Plug 'lepture/vim-jinja'

    " syntax highlighting for COOL
    au BufNewFile,BufRead *.cool setf cool
    au BufNewFile,BufRead *.cl setf cool
    Plug 'vim-scripts/cool.vim', { 'for': 'cool' }


    " Async linting lints as you type.
    " SLOW TO LOAD
    " Use Coc for Python instead
    Plug 'w0rp/ale', { 'for': ['javascript', 'c', 'css', 'scss'] }

    let g:ale_cache_executable_check_failures = 1

    let g:ale_close_preview_on_insert = 1
    let g:ale_cursor_detail = 0

    " show errors on save
    let g:ale_open_list = 1
    let g:ale_list_vertical = 0

    let g:ale_completion_max_suggestions = 200

    " When to lint
    let g:ale_lint_on_save = 1
    let g:ale_lint_on_text_changed = 0

    " Signs in gutter
    let g:ale_set_signs = 1
    let g:ale_sign_error = '>'
    let g:ale_sign_warning = '-'

    " Default ALE behavior is to enable as many linters as possible.

    let g:ale_javascript_standard_options = '--parser babel-eslint'

    let g:ale_fix_on_save = 1

    " Use Coc for Python instead
    let g:ale_fixers = {
    \   '*': ['remove_trailing_lines', 'trim_whitespace'],
    \   'javascript': ['standard', 'eslint', 'prettier'],
    \   'css': ['prettier', 'stylelint'],
    \   'python': [
    \   ],
    \   'c': ['uncrustify']
    \}


    " C-k and C-j to move between ALE errors
    nmap <silent> <C-k> <Plug>(ale_previous_wrap)
    nmap <silent> <C-j> <Plug>(ale_next_wrap)
    " RIP syntastic

    " treat contents of some tex environments as verbatim text
    au filetype tex syntax region texZone start='\\begin{lstlisting}' end='\\end{lstlisting}'
    au filetype tex syntax region texZone start='\\begin{python3code}' end='\\end{python3code}'
    au filetype tex syntax region texZone start='\\begin{bashcode}' end='\\end{bashcode}'
    au filetype tex syntax region texZone start='\\begin{pyconcode}' end='\\end{pyconcode}'

    " Improved syntax highlighting for C, add syntax highlighting for Bison, and Flex
    Plug 'justinmk/vim-syntax-extra', { 'for': [ 'c', 'cpp', 'y', 'l' ] }
endif

call plug#end()

" Set colors
try
  set background=dark
  colorscheme gruvbox-material
catch
  echo 'NO COLORS'
endtry

function! FixColorScheme()
  " 256color 249 is Grey70
  " highlight Comment guifg=249 ctermfg=249 ctermbg=NONE cterm=NONE guibg=NONE gui=NONE
  " highlight SpecialComment guifg=249 ctermfg=249 ctermbg=NONE cterm=NONE guibg=NONE gui=NONE
  " highlight vimCommentTitle guifg=249 ctermfg=249 ctermbg=NONE cterm=NONE guibg=NONE gui=NONE
  " highlight javaCommentTitle guifg=249 ctermfg=249 ctermbg=NONE cterm=NONE guibg=NONE gui=NONE
  " highlight erubyComment guifg=249 ctermfg=249 ctermbg=NONE cterm=NONE guibg=NONE gui=NONE
endfunction

augroup FixColorSchemeGroup
  au!
  au ColorScheme * call FixColorScheme()
augroup END

" Read the real vim config.
if filereadable(expand('~/.vimrc.minimal'))
    so ~/.vimrc.minimal
endif
