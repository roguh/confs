#!/bin/bash

DIR=$HOME/Dropbox/sync/media/wallpaper/
if [ -f $DIR/$(hostname).png ]; then
  feh --bg-tile $DIR/$(hostname).png
elif [ -f $DIR/$(hostname).jpg ]; then
  feh --bg-tile $DIR/$(hostname).jpg
else
  feh --bg-tile $DIR/Red-Roses-Pattern.png
fi
