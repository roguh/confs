#!/usr/bin/env bash

# Exit on error, undeclared variable reference, and set pipeline exit code 
# to that of failing command.
set -eu

if [[ "$#" != "2" || ( "$1" != restore && "$1" != backup ) ]]; then
  echo "USAGE: $0 [ backup | restore ] dir"
  exit 1
fi

VERBOSE=false

# Either "restore" or "backup"
MODE=$1

# The location of this script
CONF_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Store backups here
BACKUP_B_DIR=$CONF_DIR/backup-bak
BACKUP_R_DIR=$CONF_DIR/restore-bak

if [ "$MODE" == backup ] ; then
    SRC=$2
    DST=$CONF_DIR
    echo "Backups are located in $BACKUP_B_DIR"

else 
    SRC=$CONF_DIR
    DST=$2
    echo "Backups are located in $BACKUP_R_DIR"
fi

# Confirm changes
echo "Press ENTER to $MODE files from $SRC to $DST"
read -r changes_ok

if [[ "$changes_ok" != "" ]] ; then
  exit 1
fi

RSYNC_OPTS="--human-readable --recursive"
RSYNC_BAK="$RSYNC_OPTS"
RSYNC_BACKUP="$RSYNC_OPTS --delete-after --relative"
RSYNC_RESTORE="$RSYNC_OPTS --relative"

log() {
    if [[ "$VERBOSE" == true ]] ; then
        echo $@
    fi
}

# USAGE: copy_confs_for section_name [file1 [file2 ...]] 
#
# Either copy all files from section_name to the destination
# or copy all files to section_name, while preserving directory structure.
# Make backups before copying.
copy_confs_for() {
    SECTION=$1
    shift
    printf "\n--------- $SECTION ---------\n"
    echo "$@"
    mkdir -p "$CONF_DIR/$SECTION"

    FILES=
    if [ "$MODE" == backup ] ; then
        mkdir -p "$BACKUP_B_DIR/$SECTION"
        FROM="$SRC"
    else
        mkdir -p "$BACKUP_R_DIR/$SECTION"
        FROM="$DST"
    fi

    for f in $@ ; do
        FILES="$FILES $FROM/./$f"
    done

    if [ "$MODE" == backup ] ; then
        log rsync $RSYNC_BAK "$DST/$SECTION" "$BACKUP_B_DIR"
        rsync $RSYNC_BAK "$DST/$SECTION" "$BACKUP_B_DIR"
        log rsync $RSYNC_BACKUP $FILES "$DST/$SECTION"
        rsync $RSYNC_BACKUP $FILES "$DST/$SECTION"
    else
        log rsync $RSYNC_BAK --relative --ignore-missing-args $FILES "$BACKUP_R_DIR/$SECTION"
        rsync $RSYNC_BAK --relative --ignore-missing-args $FILES "$BACKUP_R_DIR/$SECTION"
        log rsync $RSYNC_RESTORE "$SRC/$SECTION/./" $DST 
        rsync $RSYNC_RESTORE "$SRC/$SECTION/./" $DST 
    fi
}

copy_confs_for vim \
  .vim/autoload/plug.vim \
  .vimrc \
  .vimrc.minimal

copy_confs_for vis \
  .config/vis/visrc.lua \
  .config/vis/prep.sh

copy_confs_for ipython \
  .ipython/profile_default/ipython_config.py

copy_confs_for emacs.d \
  .emacs.d/init.el \
  .emacs.d/ui.el

copy_confs_for zathura \
  .config/zathura/zathurarc

copy_confs_for polybar .config/polybar/config

copy_confs_for i3 \
  .i3status.conf .config/i3/config \
  bin/locker.sh bin/terminal.sh bin/launcher.sh \
  bin/i3status.py bin/i3txt.py

copy_confs_for x11 \
  .xinitrc .xbindkeysrc bin/autostart.sh

copy_confs_for dunst \
  .config/dunst .config/dunst/dunstrc

copy_confs_for "{ba,z,tc,c}sh" \
  .bashrc .bash_profile .zshrc .cshrc .mk_alias .aliases bin/trimdir.py

copy_confs_for cli \
  .tmux.conf \
  .config/lxterminal/lxterminal.conf \
  .config/terminator/config \
  .config/xfce4/terminal/terminalrc


copy_confs_for git .gitconfig

copy_confs_for qterminal \
  .config/qterminal.org/qterminal.ini

copy_confs_for unison \
  .unison/default.prf

copy_confs_for osync .osync.conf

copy_confs_for conky \
  .top_conkyrc .bot_conkyrc .clocks_conkyrc
