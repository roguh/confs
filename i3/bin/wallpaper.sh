#!/bin/bash
DIR="$HOME/Dropbox/sync/media/wallpaper/"

FEH_OPTS=--bg-tile
if [ -f "$DIR/$(hostname).feh_opts" ]; then
  FEH_OPTS="$(cat "$DIR/$(hostname).feh_opts")"
fi

for ext in jpg jpeg png; do
  if [ -f "$DIR/$(hostname).$ext" ]; then
    feh $FEH_OPTS "$DIR/$(hostname).$ext"
    exit 0
  fi
done

feh $FEH_OPTS "$DIR/Red-Roses-Pattern.png"
