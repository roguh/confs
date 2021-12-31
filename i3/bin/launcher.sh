#!/bin/bash -
if command -v rofi > /dev/null
then
    if command -v rofi > /dev/null
    then
        rofi -padding 50 -show ${1:-run} -modi run,window,keys -sidebar-mode -lines 25 -disable-history -matching fuzzy
    else
        rofi -padding 50 -show ${1:-run} -modi run,window,keys -sidebar-mode -lines 25 -disable-history -matching fuzzy -sorting-method fzf
    fi
else
    dmenu_run
fi
