alias m=mupdf-x11 
alias e=evince
alias em="emacs -nw"
alias emc="emacsclient -nw --alternate-editor=''"
alias l='ls -Fa --group-directories-first'
alias ll='ls -lFah --group-directories-first'

export TERM='xterm-256color'

unset HISTFILE
export HISTSIZE=100

export PREFERRED_SHELL=/bin/zsh
if [[ "$SHELL" != $PREFERRED_SHELL ]] ; then
    if type $PREFERRED_SHELL >/dev/null 2>&1 ; then 
        $PREFERRED_SHELL
        exit
    else
        echo $PREFERRED_SHELL not found
    fi
fi
