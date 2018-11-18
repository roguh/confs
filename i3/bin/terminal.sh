#!/bin/sh
exec gnome-terminal || exec alacritty || exec kitty || exec terminal2.sh || exec xterm
