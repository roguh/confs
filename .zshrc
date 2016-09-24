# Path to oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="ys"

# Use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Enable command auto-correction.
ENABLE_CORRECTION="true"

# Display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# No ls colors.
DISABLE_LS_COLORS="true"

# Plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
plugins=(gitfast python)

export PATH=$HOME/bin:/usr/local/bin:$PATH

source $ZSH/oh-my-zsh.sh

alias m=mupdf-x11
alias e=evince
alias em='emacs -nw'
alias emc='emacsclient -nw --alternate-editor=""'
alias l='ls -Fa --group-directories-first'
alias ll='ls -lFah --group-directories-first'
alias unison=$HOME/bin/unison

export TERM='xterm-256color'

unset HISTFILE
export HISTSIZE=100
