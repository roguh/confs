# Set path to binaries
export PATH="$HOME/bin:$PATH:$HOME/.local/bin"
export REACT_EDITOR=none

export LC_ALL=en_US.UTF-8  
export LANG=en_US.UTF-8

# Source file if it exists
load_file() {
    [ -f "$1" ] && . "$1"
}

# Load extra .bashrc
load_file "$HOME/.bashrc_extra"

# Load ocaml config
load_file $HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null

# Set editor
export VISUAL=vim

# Do not save history to a file
unset HISTFILE
export HISTSIZE=99999
export HISTCONTROL=ignoredups:ignorespace
export HISTTIMEFORMAT="$(echo -e\ '\r\e[K\')"

##### Set PS1
load_file "$HOME/.bashrc_ps1"

# # Run tmux if there's no GUI but it's an interactive shell
# [[ $- == *i* ]] && [ -z "$DISPLAY" ] && [ -z "$TMUX" ] && exec tmux

if command -v fzf > /dev/null ; then
    # Load fuzzy finder settings
    load_file "/usr/share/fzf/key-bindings.bash"
    load_file "/usr/share/fzf/completions.bash"

    if command -v ag > /dev/null ; then
        export FZF_DEFAULT_COMMAND='ag -g ""'
        export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    fi

    # Use ctrl-p to find files. (Same as ctrl-t)
    if [ $BASH_VERSINFO -gt 3 ]; then
        bind -x '"\C-p": "fzf-file-widget"'
    elif __fzf_use_tmux__; then
        bind '"\C-p": "\C-x\C-a$a \C-x\C-addi`__fzf_select_tmux__`\C-x\C-e\C-x\C-a0P$xa"'
    else
        bind '"\C-p": "\C-x\C-a$a \C-x\C-addi`__fzf_select__`\C-x\C-e\C-x\C-a0Px$a \C-x\C-r\C-x\C-axa "'
    fi
    bind -m vi-command '"\C-p": "i\C-p"'
fi

# Set pager command
if command -v vimpager > /dev/null ; then
    export PAGER=vimpager
elif command -v most > /dev/null ; then
    export PAGER=most
else
    export PAGER=less
fi
export MANPAGER="$PAGER"
export SYSTEMD_PAGER="$PAGER"

# Load aliases
load_file "$HOME/.tryalias.sh"
load_file "$HOME/.aliases"

##### Set commands in interactive mode
if [[ $- == *i* ]]; then
    # Keep aliases when running sudo
    alias sudo='sudo '

    # Globally enable bash completion for Python scripts using argcomplete
    load_file "$HOME/.bash_completion.d/python-argcomplete.sh"

    # Use , as an improved cd command
    load_file "$HOME/.commacd.bash"

    # Sensible defaults 
    load_file "$HOME/.sensible.bash"
    PROMPT_DIRTRIM=0
    PROMPT_COMMAND=

    # Show man page with Alt+h
    bind '"\eh": "\C-a\eb\ed\C-y\e#man \C-y\C-m\C-p\C-p\C-a\C-d\C-e"'

    # Set window title.
    # PROMPT_COMMAND="echo -ne \"\033]0;${PS1_END_PLAIN}\"${BASH_COMMAND}\" (on $HOSTNAME)\007\""

    # Show notifications when a command finishes
    export LONG_RUNNING_COMMAND='echo lol'
    export LONG_RUNNING_COMMAND_TIMEOUT=3
    load_file /etc/profile.d/undistract-me.sh >/dev/null 2>/dev/null

    if test -e "$HOME/n" > /dev/null ; then
        export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).
    fi

    tty -s
    if command -v keychain > /dev/null && [ "$0" == "$?" ] ; then
        eval $(keychain --eval --quick --quiet id_ed25519)
    fi
fi

# Load node version manager, if available
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
