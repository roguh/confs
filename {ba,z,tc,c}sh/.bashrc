# Set path to binaries
export PATH="$HOME/bin:$PATH:$HOME/.local/bin"

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
fi
