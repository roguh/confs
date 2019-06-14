#!/bin/sh
DIR="$HOME/private/truepill.gocryptfs/howtos/"
ENC_DIR="$HOME/sync/work/truepill/truepill.gocryptfs"

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
