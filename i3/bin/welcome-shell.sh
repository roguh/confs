#!/bin/sh
# Commands that should not appear in my public confs git repo.
cat ~/.super-secret-src-backup-gocryptfs-key
gocryptfs_mount.sh ~/edf-renewables/src.bak.gocryptfs/
backup.sh
fish
