# Load system .bashrc
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Load aliases
source $HOME/.mk_alias
source $HOME/.aliases

# Do not save history to a file
unset HISTFILE
export HISTSIZE=100

# If running interactively, try to run the preferred shell
PREFERRED_SHELL=
function run_preferred_shell() {
	case $- in
	    *i*)
	        if type $PREFERRED_SHELL >/dev/null 2>&1 ; then 
	            $PREFERRED_SHELL
	            exit
	        else
	            echo $PREFERRED_SHELL not found
	        fi
	esac
}
