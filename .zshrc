# oh-my-zsh Configuration

# Path to oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="fishy"

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
plugins=(gitfast python stack)

export PATH=$HOME/bin:/usr/local/bin:$PATH

# Load oh-my-zsh
source $ZSH/oh-my-zsh.sh



################################################################################



# Colors
export TERM='xterm-256color'

# Save no history
unset HISTFILE
export HISTSIZE=100

# Greeting
export message="$(whoami) @ $(hostname)"

function printmessage {
    if type figlet >/dev/null 2>&1 ; then 
        figlet -w $COLUMNS $message
    else
        echo $message 
    fi
    echo
    if type fortune >/dev/null 2>&1 ; then 
        fortune
    fi
    for i in {1..$COLUMNS} ; do echo -n _ ; done
}

# Load aliases
source $HOME/.mk_alias
source $HOME/.aliases
