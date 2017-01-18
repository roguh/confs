# Load system .bashrc
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# Do not save history to a file
unset HISTFILE
export HISTSIZE=100

export PATH=$HOME/bin:$PATH

# Load aliases
source $HOME/.mk_alias
source $HOME/.aliases
