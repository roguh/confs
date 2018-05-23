exec alacritty --working-directory "$(readlink -e /proc/"$(pgrep -oP "$(xdo pid)")"/cwd)"
