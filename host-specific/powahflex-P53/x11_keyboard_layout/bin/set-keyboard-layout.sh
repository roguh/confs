#!/bin/sh
# localectl list-x11-keymap-layouts
# localectl list-x11-keymap-variants LAYOUT_CODE

setxkbmap -layout us,es
# setxkbmap -variant ,
setxkbmap -option 'grp:ctrls_toggle'
