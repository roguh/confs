#!/bin/sh
BASE_DIR="$HOME/.screenlayout"

# Check which displays are connected
# Run the script named after the displays joined by underscores ending in .sh
# or run xrandr --auto if that script does not exist
# Example script name:
# ~/.screenlayout/eDP1_HDMI-1.sh

if command -v "xrandr" > /dev/null; then
  FILENAME=""
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1)
  do
    if [ -z "$FILENAME" ];
    then
      FILENAME="$m"
    else
      FILENAME="${FILENAME}_${m}"
    fi
  done
  DISPLAY_SCRIPT="$BASE_DIR/$FILENAME.sh"
  if [ -f "$DISPLAY_SCRIPT" ]
  then
    set -x
    "$DISPLAY_SCRIPT"
  else
    set -x
    xrandr --auto
  fi
  sleep 1
  wallpaper.sh
else
  echo Command xrandr does not exist >&2
  exit 1
fi
