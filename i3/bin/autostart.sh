#!/usr/bin/env bash

# Autostart
backup-y580.sh &
nm-applet &
redshift &
lxterminal -l -e 'bash -c u_hsync' &
lxterminal -l -e 'bash -c u_local' &
feh --bg-fill "Pictures/mt hope, antarctica.jpeg" &

# Screen locker
xautolock -detectsleep -secure -time 15 -locker locker.sh &
