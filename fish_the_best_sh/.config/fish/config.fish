set -U LC_ALL en_US.UTF-8  
set -U LANG en_US.UTF-8

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
    set -U pager vimpager
    set -U MANPAGER vimpager
    set -U SYSTEMD_PAGER vimpager
    alias less=vimpager
end

# Load aliases
load_file $HOME/.aliases

# Load OCaml
load_file $HOME/.opam/opam-init/init.fish

# Load pywal theme
load_theme

function plugin_install
    fisher install done
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

set -gx MANPATH $MANPATH /usr/share/man /usr/local/share/man/

# Fish does lots of things by default:
# ignore dups and blank lines in history
# interactive cd and autocompletion
