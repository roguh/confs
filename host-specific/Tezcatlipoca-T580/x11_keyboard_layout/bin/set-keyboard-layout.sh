#!/bin/sh
# localectl list-x11-keymap-layouts
# localectl list-x11-keymap-variants LAYOUT_CODE

setxkbmap -layout us,ru
# Unset variant to use official russian keyboard, instead of english qwerty analogue
setxkbmap -variant ,phonetic
setxkbmap -option 'grp:ctrls_toggle'
