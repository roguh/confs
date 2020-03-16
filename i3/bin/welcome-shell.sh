#!/bin/bash
# Commands that should not appear in my public confs git repo.
BAK=~/edf-renewables/src.bak.gocryptfs/
if [ -d $BAK ]; then
  cat ~/.super-secret-src-backup-gocryptfs-key
  gocryptfs_mount.sh $BAK
fi
backup.sh
fish
