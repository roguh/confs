#!/bin/sh -
if type rofi >/dev/null 2>&1
then
    rofi -padding 150 -show ${1:-run} -modi run,window,keys -sidebar-mode -lines 30 -disable-history 
else
    dmenu
fi
