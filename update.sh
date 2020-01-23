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
# Either copy all files from section_name/ to the destination
# or copy all files to section_name/, while preserving directory structure.
# Make backups before copying.
copy_confs_for() {
    SECTION="$1"
    shift

    if [[ ! -d "$SECTION" ]]; then
      return
    fi

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

    for f in "$@" ; do
        FILES="$FILES $FROM/./$f"
    done
    echo FILES: "$FILES"

    if [ "$MODE" == backup ] ; then
        log rsync $RSYNC_BAK "$DST/$SECTION" "$BACKUP_B_DIR"
        rsync $RSYNC_BAK "$DST/$SECTION" "$BACKUP_B_DIR" || true
        log rsync $RSYNC_BACKUP $FILES "$DST/$SECTION"
        rsync $RSYNC_BACKUP $FILES "$DST/$SECTION" || true
    else
        log rsync $RSYNC_BAK --relative --ignore-missing-args $FILES "$BACKUP_R_DIR/$SECTION"
        rsync $RSYNC_BAK --relative --ignore-missing-args $FILES "$BACKUP_R_DIR/$SECTION" || true
        log rsync $RSYNC_RESTORE "$SRC/$SECTION/./" $DST
        rsync $RSYNC_RESTORE "$SRC/$SECTION/./" $DST || true
    fi
}

copy_confs_for vim \
  .vimrc \
  .vimrc.minimal

copy_confs_for vis \
  .config/vis/visrc.lua \
  .config/vis/prep.sh

copy_confs_for zathura \
  .config/zathura/zathurarc

copy_confs_for polybar .config/polybar/config \
  .config/polybar/config \
  .config/polybar/openweathermap-fullfeatured.sh \
  .config/polybar/player-ctrl.sh \
  .config/polybar/player-mpris-tail.py \
  .config/polybar/system-nvidia-smi.sh \
  bin/polybar.sh

copy_confs_for x11 \
  .xinitrc .xbindkeysrc bin/autostart.sh bin/signal.sh

copy_confs_for "{ba,z,tc,c}sh" \
  .bashrc .bashrc_ps1 .bash_profile .zshrc .cshrc .tryalias.sh .aliases bin/trimdir.py bin/gitinfo.sh bin/projectroot.sh bin/real-deal-turbo-charged-cd.sh

copy_confs_for git \
    .gitconfig bin/gitdiff.sh

copy_confs_for utils \
  bin/cpufreq.sh bin/systemload.sh bin/mem.sh bin/screenshot.sh bin/screenshot-select.sh \
  .xsession .profile

copy_confs_for alacritty .config/alacritty/alacritty.yml bin/alacritty-cwd.sh

copy_confs_for cli \
  .tmux.conf \
  .config/lxterminal/lxterminal.conf \
  .config/terminator/config

copy_confs_for compton .config/compton.conf

copy_confs_for conky bin/conky.sh .conkyrc.d/

copy_confs_for dunst .config/dunst .config/dunst/dunstrc bin/dunst.sh

copy_confs_for emacs.d .emacs.d/init.el .emacs.d/ui.el

copy_confs_for fish_the_best_sh .config/fish/{config,functions/{tryalias,load_theme,fisher,fish_{title,prompt,greeting}}}.fish .aliases bin/real-deal-turbo-charged-cd.sh

copy_confs_for fluxbox .fluxbox/menu .fluxbox/keys

copy_confs_for gocryptfs bin/gocryptfs_mount.sh bin/gocryptfs_umount.sh

copy_confs_for htop .config/htop/htoprc

copy_confs_for i3 \
  .i3status.conf \
  .i3/config \
  bin/locker.sh \
  bin/backup.sh \
  bin/wallpaper.sh \
  bin/terminal2.sh \
  bin/terminal.sh \
  bin/launcher.sh \
  bin/backlightoff.sh \
  bin/i3empty.py \
  bin/i3txt.py

copy_confs_for ipython .ipython/profile_default/ipython_config.py

copy_confs_for kitty .config/kitty/kitty.conf

copy_confs_for nvm .nvm/default-packages

copy_confs_for work bin/open_work_howtos.sh bin/open_todays_work_journal.sh bin/todays_work_journal.sh bin/borg-backup-work.sh

copy_confs_for org bin/open_todays_org_journal.sh bin/todays_org_journal.sh

copy_confs_for top .config/procps/toprc

copy_confs_for readline .inputrc

copy_confs_for pywal bin/wal-set-theme.sh bin/theme-post.sh .cache/wal/{sequences,colors.*}

copy_confs_for ranger .config/ranger/rc.conf

copy_confs_for unison .unison/default.prf

copy_confs_for osync .osync.conf

copy_confs_for tern .tern-config

copy_confs_for hyper .hyper.js

copy_confs_for asdf .default-python-packages .default-npm-packages .tool-versions

copy_confs_for fanciness bin/showoff-linux-desktop-unixporn-sexy-oh-wow-screenshot-time-here-we-go-fancy-extra-nice-UNDO.sh bin/showoff-linux-desktop-unixporn-sexy-oh-wow-screenshot-time-here-we-go-fancy-extra-nice.sh

# git@github.com:roguh/git-quick-stats.git
copy_confs_for performance bin/cerberus.sh bin/cerberus-notify-send.sh

copy_confs_for feh_sane_defaults nin/feh-sane-defaults.sh bin/feh-sane-defaults-gallery.sh

copy_confs_for pacman_scripts bin/defuck-a-pacman-install.sh

copy_confs_for arandr .screenlayout/select-arandr-display.sh
