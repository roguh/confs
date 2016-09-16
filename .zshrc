# Path to oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="ys"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Plugins can be found in ~/.oh-my-zsh/plugins/*
plugins=(gitfast python)

export PATH=$HOME/bin:/usr/local/bin:$PATH

source $ZSH/oh-my-zsh.sh

# May need to manually set your language environment
# export LANG=en_US.UTF-8

alias m=mupdf-x11
alias e=evince
alias em='emacs -nw'
alias emc='emacsclient -nw --alternate-editor=""'
alias ll='ls -Fa --group-directories-first'

# Disable history
unset HISTFILE
export HISTSIZE=0
