#!/usr/bin/env bash

# Autostart
feh --bg-fill "Pictures/mt hope, antarctica.jpeg" &
backup-y580.sh &
backup-common.sh &
nm-applet &
redshift &

for CMD in {hsync,local}-{unison,osync} ; do
    lxterminal -l -e "bash -c $CMD" &
done

xrandr-rotate-left.sh &

# Screen locker
xautolock -detectsleep -secure -time 15 -locker locker.sh &
