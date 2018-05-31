# Set path to binaries
export PATH="$HOME/bin:$PATH:$HOME/.local/bin"

load_file() {
    if [ -f "$1" ] ; then
        . "$1"
    fi
}

# Load aliases
load_file "$HOME/.tryalias"
load_file "$HOME/.aliases"

# Load extra .bashrc
load_file "$HOME/.bashrc_extra"

export VISUAL=vim

# Do not save history to a file
unset HISTFILE
export HISTSIZE=10000
export HISTCONTROL=ignoredups:ignorespace

# COLORS
load_file .cache/wal/colors-tty.sh
(cat ~/.cache/wal/sequences &)

##### Set PS1
# Check if colored output is supported
COLORED_PS1=false
if test -t 1; then
    ncolors=$(tput colors)
    if test -n "$ncolors" && test $ncolors -ge 8; then
        COLORED_PS1=true
    fi
fi

E="\["
D="\]"
INVERT="$E\e[7m$D"
BOLD="$E\e[1m$D"

# These are all background colors
COLOR_RED="$E\e[41m$D"
COLOR_GREEN="$E\e[42m$D"
COLOR_BLUE="$E\e[44m$D"
COLOR_ORANGE="$E\e[48;5;208m$D"
COLOR_CYAN="$E\e[46m$D"
FCOLOR_BLACK="$E\e[30m$D"
END="$E\e[0m$D"

RELEASE=
if command -v python > /dev/null ; then
    RELEASE=`python -mplatform`
fi

shopt -s nocasematch
case "$RELEASE" in
    *ubuntu*) COLOR_HIGHLIGHT="$FCOLOR_BLACK$COLOR_ORANGE"
    ;;
    *debian*) COLOR_HIGHLIGHT="$FCOLOR_BLACK$COLOR_RED"
    ;;
    *cent*) COLOR_HIGHLIGHT="$FCOLOR_BLACK$COLOR_GREEN"
    ;;
    *arch*) COLOR_HIGHLIGHT="$FCOLOR_BLACK$COLOR_CYAN"
    ;;
    *) COLOR_HIGHLIGHT="$FCOLOR_BLACK$COLOR_RED"
    ;;
esac

### Set beginning in PS1
PS1_BEGIN="\u @ $PS1_H"
PS1_H='$(hostname | cut -d. -f1)'
if [[ $COLORED_PS1 == "true" ]] ; then
if [[ $(whoami) == root ]] ; then
    PS1_BEGIN="${INVERT}${COLOR_HIGHLIGHT}\u${END}${INVERT} @ $PS1_H${END}"
else
    PS1_BEGIN="${INVERT}\u @ $PS1_H${END}"
fi
fi

### Set extra information in PS1
PS1_TIME='$(date +%H:%M:%S)' 

if [[ $(whoami) == root ]] ; then
    PS1_END="# "
else
    PS1_END="$ "
fi
PS1_END_PLAIN=$PS1_END

if [[ $COLORED_PS1 == "true" ]] ; then
    PS1_END="${BOLD}${PS1_END}${END}"
fi

### PWD and git info in PS1
if command -v trimdir.py > /dev/null && command -v python3 > /dev/null ; then
    PS1_PWD='$(echo "\w" | trimdir.py)'
else
    PS1_PWD="\w"
fi

if command -v git > /dev/null ; then
    PS1_GITBRANCH='$(gitinfo.sh)'
    if [[ $COLORED_PS1 == "true" ]] ; then
        PS1_GITBRANCH="${BOLD}${PS1_GITBRANCH}${END}"
    fi
else
    PS1_GITBRANCH=""
fi

### Set end of PS1
if [[ $COLORED_PS1 == "true" ]] ; then
    PS1_TIME="${COLOR_HIGHLIGHT}${PS1_TIME}${END}" 
fi

export PS1="${PS1_BEGIN} ${PS1_TIME} ${PS1_PWD} ${PS1_GITBRANCH}${PS1_END}"

# Run tmux if there's no GUI but it's an interactive shell
# [[ $- == *i* ]] && [ -z "$DISPLAY" ] && [ -z "$TMUX" ] && exec tmux

if command -v most > /dev/null ; then
    export PAGER=most
    export MANPAGER=$PAGER
    export SYSTEMD_PAGER=$PAGER
fi

##### Set commands in interactive mode
if [[ $- == *i* ]]; then
    # Keep aliases when running sudo
    alias sudo='sudo '

    # Use , as an improved cd command
    load_file "$HOME/.commacd.bash"

    # Sensible defaults 
    load_file "$HOME/.sensible.bash"
    PROMPT_DIRTRIM=0
    PROMPT_COMMAND=
    
    # Show man page with Alt+h
    bind '"\eh": "\C-a\eb\ed\C-y\e#man \C-y\C-m\C-p\C-p\C-a\C-d\C-e"'
    
    # Set title. This should be the last command in .bashrc
    trap 'printf "\033]0;%s\007" "$PS1_END_PLAIN${BASH_COMMAND//[^[:print:]]/} (on $HOSTNAME)" >&2' DEBUG
    
    # Load undistract-me
    export LONG_RUNNING_COMMAND_TIMEOUT=3
    load_file /etc/profile.d/undistract-me.sh
fi
