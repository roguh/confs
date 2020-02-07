#!/bin/sh

# delete extraneous files from destination
ROOT=$HOME/sync/technical/$(lsb_release --short --id)-$(hostname)

mkdir -p $ROOT/pacman-Qii
mkdir -p $ROOT/bin

RSYNC_OPTS="--archive --human-readable --progress --delete-after --verbose --recursive"

rsync --relative $RSYNC_OPTS $(pacman -Qii | awk '/^MODIFIED/ {print $2}') $ROOT/pacman-Qii
pacman -Qie --native > $ROOT/pacman-Qie
pacman -Qie --foreign > $ROOT/pacman-Qie-AUR
cp $HOME/.local/share/fish/fish_history $ROOT/local-share-fish-fish_history

rsync $RSYNC_OPTS $HOME/bin/ $ROOT/bin

rsync-for-src.sh $HOME/src $HOME/Dropbox/src-$(hostname)-bak
