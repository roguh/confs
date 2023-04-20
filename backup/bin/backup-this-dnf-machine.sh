#!/bin/bash

# delete extraneous files from destination
ROOT=$(get-backup-root.sh)/$(date +%Y-%m)

set -x

mkdir -p $ROOT/bin

RSYNC_OPTS="--archive --human-readable --delete-after --recursive --ignore-missing-args"

echo dnf history userinstalled > $ROOT/dnf-history-userinstalled
dnf history userinstalled >> $ROOT/dnf-history-userinstalled

rsync $RSYNC_OPTS $HOME/bin/ $ROOT/bin

mkdir -p $ROOT/history/
rsync $RSYNC_OPTS \
  /var/log/dnf.librepo.log /var/log/dnf.log /var/log/dnf.rpm.log \
  ~/.local/share/fish/fish_history \
  ~/.bash_history \
  ~/.zsh_history \
  $ROOT/history

# -L 2 Only print two levels of the hierarchy
# -h Print size, -i No indentation, -f Print entire filepath
tree -L 2 -h -i -f ~/src/ > "$ROOT/src_tree_L2"

set +x
ls -1 ~/src/*/* > "$ROOT/src_ls_star_star_filenames"
