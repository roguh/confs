#!/bin/sh
DIR="$(todays_work_journal.sh)"
ENC_DIR="$HOME/sync/work/truepill/truepill.gocryptfs"

if [ ! -d "$DIR" ]; then
  echo "$DIR" does not exist, attempting to mount "$ENC_DIR"
  echo gocryptfs_mount.sh "$ENC_DIR"
  gocryptfs_mount.sh "$ENC_DIR"
fi

exec nvim "$DIR/log.org"
