#!/bin/sh

$HOME/py/polybar-themer/use_pywal_theme.py &
$HOME/src/wal_steam/wal_steam.py &
launcher.sh &
dunst.sh &
polybar.sh &
conky.sh &
xrdb -merge .Xresources &
