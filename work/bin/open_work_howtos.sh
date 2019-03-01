#!/bin/sh
DIR="$HOME/private/work.gocryptfs/howtos/"
ENC_DIR="$HOME/agilemd/work.gocryptfs/"

if [ ! -d "$DIR" ]; then
  echo "$DIR" does not exist, attempting to mount
  echo gocryptfs_mount.sh "$ENC_DIR"
  if gocryptfs_mount.sh "$ENC_DIR"; then
    mkdir "$DIR"
  else
    exit 1
  fi
fi

exec nvim "$DIR"
