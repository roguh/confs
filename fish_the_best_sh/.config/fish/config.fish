set -U LC_ALL en_US.UTF-8
set -U LANG en_US.UTF-8

function addpaths
    contains -- $argv $fish_user_paths
       or set -U fish_user_paths $fish_user_paths $argv
end

addpaths $HOME/bin
addpaths $HOME/.local/bin
addpaths $HOME/.gem/ruby/2.5.0/bin

function load_file
    if test -e $argv[1]
        source $argv[1]
    end
end

set -U EDITOR vim
set -U VISUAL vim

if type vimpager > /dev/null 2>&1
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
    # make sure to install nvm:
    # git clone https://github.com/creationix/nvm ~/.nvm
    fisher add FabioAntunes/fish-nvm

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
    if type keychain > /dev/null 2>&1
        keychain --eval --quick --quiet id_ed25519 | source
    end

    if not functions -q fisher
        set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
        curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
        fish -c fisher
    end

    function fish_user_key_bindings
        # Use Ctrl-R to find command in history
        fzf_key_bindings

        # Use Ctrl-P to find files
        bind \cp fzf-file-widget
        if bind -M insert > /dev/null 2>&1 2>&1
            bind -M insert \cp fzf-file-widget
        end
    end

end

# Have fzf use ag to find files
if type ag > /dev/null 2>&1
    set -gx FZF_DEFAULT_COMMAND 'ag -g ""'
    set -gx FZF_CTRL_T_COMMAND "$FZF_DEFAULT_COMMAND"
end

# Fish does lots of things by default:
# ignore dups and blank lines in history
# interactive cd and autocompletion
