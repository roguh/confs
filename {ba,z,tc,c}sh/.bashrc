load_file() {
    if [ -f "$1" ] ; then
        . "$1"
    fi
}
# Load system .bashrc
load_file /etc/bashrc
load_file "$HOME/.bashrc_extra"

# Do not save history to a file
unset HISTFILE
export HISTSIZE=10000
export HISTCONTROL=ignoredups:ignorespace

export PATH="$HOME/bin:$PATH:$HOME/.local/bin"

# Load aliases
load_file "$HOME/.mk_alias"
load_file "$HOME/.aliases"

# Sensible defaults 
load_file "$HOME/.sensible.bash"
PROMPT_DIRTRIM=0

# Use , as an improved cd command
load_file "$HOME/.commacd.bash"

COLORED_PS1=false

# Check if colored output is supported
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
COLOR_RED="$E\e[31m$D"
END="$E\e[0m$D"

PS1_BEGIN="\u @ \H"
if [[ $(whoami) == root ]] ; then
    PS1_BEGIN="${BOLD} ${PS1_BEGIN} ${END}"
fi

PS1_TIME='$(date +%H:%M:%S)' 

if [[ $(whoami) == root ]] ; then
    PS1_END="# "
else
    PS1_END="$ "
fi

if [[ $COLORED_PS1 == "true" ]] ; then
    PS1_BEGIN="${INVERT}${PS1_BEGIN}${END}"
    PS1_END="${BOLD}${PS1_END}${END}"
fi

if command -v trimdir > /dev/null ; then
    PS1_PWD='$(echo "\w" | trimdir) '
else
    PS1_PWD="\w "
fi

if command -v git > /dev/null ; then
    PS1_GITBRANCH='$(git rev-parse --abbrev-ref HEAD 2>/dev/null) '
    if [[ $COLORED_PS1 == "true" ]] ; then
        PS1_GITBRANCH="${BOLD}${PS1_GITBRANCH}${END}"
    fi
else
    PS1_GITBRANCH=""
fi

if [[ $COLORED_PS1 == "true" ]] ; then
    PS1_TIME="${INVERT}${COLOR_RED}${PS1_TIME}${END}" 
fi

export PS1="${PS1_BEGIN} ${PS1_TIME} ${PS1_PWD}${PS1_GITBRANCH}${PS1_END}"

# Run tmux if there's no GUI but it's an interactive shell
# [[ $- == *i* ]] && [ -z "$DISPLAY" ] && [ -z "$TMUX" ] && exec tmux

if command -v most > /dev/null ; then
    export MANPAGER=most
fi
