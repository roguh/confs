#!/bin/sh
DIR=$HOME/Dropbox/sync/media/wallpaper/
if [ -f $DIR/$(HOSTNAME).png ]; then
  feh --bg-tile $DIR/$(HOSTNAME).png
else if [ -f $DIR/$(HOSTNAME).jpg]; then
  feh --bg-tile $DIR/$(HOSTNAME).jpg
else
  feh --bg-tile $DIR/Red-Roses-Pattern.png
fi
