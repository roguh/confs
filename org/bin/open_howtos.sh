#!/bin/sh
DIR="$HOME/private/org.gocryptfs/howtos/"
ENC_DIR="$HOME/agilemd/org.gocryptfs/"

if [ -d "$DIR" ]; then
  echo "$DIR" does not exist, attempting to mount
  echo gocryptfs_mount.sh "$ENC_DIR"
  gocryptfs_mount.sh "$ENC_DIR"
fi

exec nvim "$DIR"
