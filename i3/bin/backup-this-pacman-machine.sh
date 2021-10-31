#!/bin/bash

# delete extraneous files from destination
ROOT=$(get-backup-root.sh)

mkdir -p $ROOT/pacman-Qii
mkdir -p $ROOT/bin

RSYNC_OPTS="--archive --human-readable --progress --delete-after --verbose --recursive"

rsync --relative $RSYNC_OPTS $(pacman -Qii | awk '/^MODIFIED/ {print $2}') $ROOT/pacman-Qii
pacman -Qie --native > $ROOT/pacman-Qie
pacman -Qie --foreign > $ROOT/pacman-Qie-AUR

rsync $RSYNC_OPTS $HOME/bin/ $ROOT/bin
rsync $RSYNC_OPTS $HOME/.screenlayout/ $ROOT/dotscreenlayout

mkdir -p $ROOT/history/
cp ~/.local/share/fish/fish_history $ROOT/history/fish_history
cp /var/log/pacman.log $ROOT/history/pacman.log
