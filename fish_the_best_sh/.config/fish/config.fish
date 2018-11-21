set -U LC_ALL en_US.UTF-8  
set -U LANG en_US.UTF-8

function addpath
    if test -e $argv[1]
        set -U fish_user_paths $fish_user_paths $argv[1]
    end
end

addpath $HOME/bin
addpath $HOME/.local/bin
addpath $HOME/.fzf/bin
addpath $HOME/.gem/ruby/2.5.0/bin

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

function install_plugins
    # manage node and npm versions
    fisher add jorgebucaran/fnm

    # "frecency" aware directory switching z
    fisher add jethrokuan/z

    # notifications when commands are done
    fisher add franciscolourenco/done

    # bash like syntax
    # bass export X=4
    fisher add edc/bass
end

set -gx MANPATH $MANPATH /usr/share/man /usr/local/share/man/

if status is-interactive
    if command keychain > /dev/null; 
        keychain --eval --quick --quiet id_ed25519 | source
    end

    if not functions -q fisher
        set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
        curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
        fish -c fisher
    end

    function fish_user_key_bindings
        fzf_key_bindings
    end
end

# Fish does lots of things by default:
# ignore dups and blank lines in history
# interactive cd and autocompletion
