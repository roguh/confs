#!/bin/sh
MAPNAME="${1:-manjaro}"
set -x
cryptsetup luksOpen /dev/xvdi "$MAPNAME"
mount "/dev/mapper/$MAPNAME" /home/user/manjaro/
