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

# 1. read stdin
# 2. split by '/'
# 3. replace all parent directory names with their 1st or 1st and 2nd characters
export PS1_DIR_SIMPLIFIER="\
import sys, string ; \
d = sys.stdin.read().split(\"/\") ; \
print(\"/\".join(\
 [w[0:2 if w[0:1] in string.punctuation else 1] \
 for w in d[:-1]] + d[-1:]))"

# Set the primary prompt to a colored and bolded PS1
# \u @ \H \w >
COLORED_PS1=true
E="\["
D="\]"
INVERT="$E\e[7m$D"
BOLD="$E\e[1m$D"
COLOR_RED="$E\e[31m$D"
END="$E\e[0m$D"

if [[ $COLORED_PS1 == "true" ]] ; then
    PS1_BEGIN="${INVERT}${BOLD}${COLOR_RED}\u${END}${INVERT}${BOLD} @ \H${END} "
    PS1_END="${BOLD}- ${END}"
else
    PS1_BEGIN="\u @ \H "
    PS1_END="- "
fi

if command -v python > /dev/null ; then
    PS1_PWD="\`echo '\w' | python -c '$PS1_DIR_SIMPLIFIER' 2>/dev/null\` "
else
    PS1_PWD="\w "
fi

if command -v git > /dev/null ; then
    PS1_GITBRANCH="${BOLD}\`git rev-parse --abbrev-ref HEAD 2>/dev/null \`${END} "
else
    PS1_GITBRANCH=" "
fi

if [[ $COLORED_PS1 == "true" ]] ; then
    PS1_TIME="${INVERT}${COLOR_RED}\`date +%H:%M:%S\`${END} " 
else
    PS1_TIME="\`date +%H:%M:%S\` " 
fi

export PS1="${PS1_BEGIN}${PS1_TIME}${PS1_PWD}${PS1_GITBRANCH}${PS1_END}"

# Run tmux if there's no GUI but it's an interactive shell
# [[ $- == *i* ]] && [ -z "$DISPLAY" ] && [ -z "$TMUX" ] && exec tmux

if command -v most > /dev/null ; then
    export MANPAGER=most
fi
