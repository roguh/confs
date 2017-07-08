# Load system .bashrc
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# Do not save history to a file
unset HISTFILE
export HISTSIZE=100
export HISTCONTROL=ignoredups:ignorespace

# Load aliases
source $HOME/.mk_alias
source $HOME/.aliases

export PATH=$HOME/bin:$PATH

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
if [[ $COLORED_PS1 == "true" ]] ; then
    PS1_BEGIN="\e[1m\e[36m\u \e[0m@\e[32m \H\e[0m"
    PS1_END="\e[1m>\e[0m "
else
    PS1_BEGIN="\u @ \H"
    PS1_END="> "
fi

if command -v python > /dev/null ; then
    # If it's installed, use Python to print an abbreviated $PWD
    export PS1="$PS1_BEGIN \`echo \w | python -c '$PS1_DIR_SIMPLIFIER'\` $PS1_END"
     
else
    export PS1="$PS1_BEGIN $PS1_END"
fi

# Run tmux if there's no GUI but it's an interactive shell
[[ $- == *i* ]] && [ -z "$DISPLAY" ] && [ -z "$TMUX" ] && exec tmux

if command -v most > /dev/null ; then
    export MANPAGER=most
fi
