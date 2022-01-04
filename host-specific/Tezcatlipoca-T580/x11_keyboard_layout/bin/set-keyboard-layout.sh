#!/bin/sh
setxkbmap -layout us,ru
# Unset variant to use official russian keyboard, instead of english qwerty analogue
setxkbmap -variant ,phonetic
setxkbmap -option 'grp:ctrls_toggle'
