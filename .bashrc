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

# Set the primary prompt.
export PS1_DIR_SIMPLIFIER="\
import sys ; \
d = sys.stdin.read().split(\"/\") ; \
print(\"/\".join(\
   [w[0:2 if w.startswith(\".\") else 1] \
    for w in d[:-1]] + d[-1:]))"

# If it's installed, use Python to print an abbreviated $PWD
if command -v python > /dev/null ; then
    export PS1="\u @ \H \`echo '\w' | python -c '$PS1_DIR_SIMPLIFIER'\` \$ "
else
    export PS1="\u @ \H \w \$ "
fi

# Run tmux if there's no GUI but it's an interactive shell
[[ $- == *i* ]] && [ -z "$DISPLAY" ] && [ -z "$TMUX" ] && exec tmux

if command -v most > /dev/null ; then
    export MANPAGER=most
fi
