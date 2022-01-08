#!/bin/bash -
if command -v rofi > /dev/null
then
    rofi -padding 50 -show ${1:-run} -modi run,window,keys -sidebar-mode -lines 25 -sorting-method fzf
else
    dmenu_run
fi
