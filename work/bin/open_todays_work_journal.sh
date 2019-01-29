#!/bin/sh
DIR="$(todays_journal.sh)"
ENC_DIR="$HOME/agilemd/work.gocryptfs/"

if [ -d "$DIR" ]; then
  echo "$DIR" does not exist, attempting to mount
  echo gocryptfs_mount.sh "$ENC_DIR"
  gocryptfs_mount.sh "$ENC_DIR"
fi

exec nvim "$DIR/log.org"
