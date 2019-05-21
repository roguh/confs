# oh-my-zsh Configuration
load_file() {
    if [ -f "$1" ] ; then
        . "$1"
    fi
}

PS1='$ '

# Path to oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
plugins=(gitfast python stack)

# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="geoffgarside"
# PROMPT='%{$bg[white]$fg[black]%}%n @ %m%{$reset_color%} %{$bg[blue]$fg[black]%}%*%{$reset_color%} $(echo "%~" | trimdir.py)%{$fg_bold[white]%}$(git_prompt_info) %(!.#.$)%{$reset_color%} '
# 
# ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[yellow]%}("
# ZSH_THEME_GIT_PROMPT_SUFFIX=")%{$reset_color%}"

# Load oh-my-zsh
load_file $ZSH/oh-my-zsh.sh

load_file "$HOME/.tryalias"
load_file "$HOME/.aliases"
load_file "$HOME/.zshrc_extra"

if command -v fzf > /dev/null; then
    load_file /usr/share/fzf/key-bindings.zsh
    load_file /usr/share/fzf/completion.zsh
fi

unset HISTFILE
export HISTSIZE=99999
export HISTCONTROL=ignoredups:ignorespace


# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
