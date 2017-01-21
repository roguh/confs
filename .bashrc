# Load system .bashrc
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Do not save history to a file
unset HISTFILE
export HISTSIZE=100

# Load aliases
source $HOME/.mk_alias
source $HOME/.aliases

export PATH=$HOME/bin:$PATH
