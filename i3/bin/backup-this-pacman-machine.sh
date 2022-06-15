#!/bin/bash

# delete extraneous files from destination
ROOT=$(get-backup-root.sh)

mkdir -p $ROOT/pacman-Qii
mkdir -p $ROOT/bin

RSYNC_OPTS="--archive --human-readable --progress --delete-after --verbose --recursive --ignore-missing-args"

rsync --relative $RSYNC_OPTS $(pacman -Qii | awk '/^MODIFIED/ {print $2}') $ROOT/pacman-Qii
pacman -Qie --native > $ROOT/pacman-Qie
pacman -Qie --foreign > $ROOT/pacman-Qie-AUR

rsync $RSYNC_OPTS $HOME/bin/ $ROOT/bin
rsync $RSYNC_OPTS $HOME/.screenlayout/ $ROOT/dotscreenlayout

set -x
mkdir -p $ROOT/history/
rsync $RSYNC_OPTS \
  ~/.local/share/fish/fish_history \
  /var/log/pacman.log \
  ~/.bash_history \
  ~/.zsh_history \
  $ROOT/history

mkdir -p $ROOT/personal_dictionaries/
cp ~/.aspell.*.pws $ROOT/personal_dictionaries/
