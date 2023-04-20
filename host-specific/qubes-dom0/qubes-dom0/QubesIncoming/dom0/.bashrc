# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

export VISUAL=nvim
export EDITOR=nvim

export HISTSIZE=9999999
export HISTCONTROL=ignoredups:ignorespace

if command -v starship > /dev/null; then
  eval "$(starship init bash)"
fi

# User specific aliases and functions
alias v=nvim
