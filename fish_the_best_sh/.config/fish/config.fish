set -U fish_user_paths $HOME/bin $HOME/.local/bin

function load_file
    if test -e $argv[1]
        source $argv[1]
    end
end

set -U EDITOR vim
set -U VISUAL vim

if command -v vimpager > /dev/null
    set -U PAGER vimpager
end

# Load aliases
load_file $HOME/.aliases

# Load OCaml
load_file $HOME/.opam/opam-init/init.fish

function plugin_install
    fisher install done
    fisher install laughedelic/pisces
    fisher install shark
    fisher install spin
    fisher install fzf
    fisher install edc/bass

    echo
    echo use bass by prefixing bash command with bass
    echo bass export X=3
    echo
    echo Ctrl-r,f - search history, find file
    echo Ctrl-o,g - open with EDITOR, or with xdg-open
    echo Alt-o,Shift-o - recursive cd into subdirs, or include hidden subdirs
end

# Fish does lots of things by default:
# ignore dups and blank lines in history
# interactive cd and autocompletion
