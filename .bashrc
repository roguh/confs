# Load aliases
source $HOME/.mk_alias
source $HOME/.aliases

export TERM='xterm-256color'

unset HISTFILE
export HISTSIZE=100

export PREFERRED_SHELL=/bin/mksh
if [[ "$SHELL" != $PREFERRED_SHELL ]] ; then
    if type $PREFERRED_SHELL >/dev/null 2>&1 ; then 
        if ! $PREFERRED_SHELL ; then
            exit
        fi
    else
        echo $PREFERRED_SHELL not found
    fi
fi
