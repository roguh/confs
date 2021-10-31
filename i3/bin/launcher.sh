#!/bin/bash -
if type rofi >/dev/null 2>&1
then
    rofi -padding 50 -show ${1:-run} -modi run,window,keys -sidebar-mode -lines 25 -disable-history
else
    dmenu_run
fi
