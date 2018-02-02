# Exit on error, undeclared variable reference, and set pipeline exit code 
# to that of failing command.
set -eu

if [[ "$#" != "2" || ( "$1" != restore && "$1" != backup ) ]]; then
  echo "USAGE: $0 [ backup | restore ] dir"
  exit 1
fi

# "restore" or "backup"
MODE=$1

# store backups here
BACKUP=$PWD/confs-backup

if [ "$MODE" == backup ] ; then
    SRC=$2
    DST=$PWD
else 
    SRC=$PWD
    DST=$2
fi

echo "Press ENTER to $MODE files from $SRC to $DST"
echo "Backup directory in $BACKUP"

read -r changes_ok

if [[ "$changes_ok" != "" ]] ; then
  echo cancelled
  exit 1
fi

mkdir_conf() {
    if [ "$MODE" == backup ] ; then
      mkdir -p "$BACKUP/$SECTION/$1"
      echo mkdir -p "$DST/$SECTION/$1"
      mkdir -p "$DST/$SECTION/$1"
    else
      mkdir -p "$BACKUP/$SECTION/$1"
      echo mkdir -p "$DST/$1"
      mkdir -p "$DST/$1"
    fi
}

loudcp() {
    echo cp "$1" "$2"
    cp "$1" "$2"
}

copy_conf() {
    if [ "$MODE" == backup ] ; then
      if [ -f "$DST/$SECTION/$1" ] ; then
        cp "$DST/$SECTION/$1" "$BACKUP/$SECTION/$1"
      fi
      loudcp "$SRC/$1" "$DST/$SECTION/$1"
    else
      if [ -f "$DST/$1" ] ; then
        cp "$DST/$1" "$BACKUP/$SECTION/$1"
      fi
      loudcp "$SRC/$SECTION/$1" "$DST/$1"
    fi
}

SECTION="none"

section() {
    echo
    echo "--------- $1 --------- "
    mkdir -p "$BACKUP/$1"
    SECTION=$1
}

section vim
mkdir_conf {.vim,.config/nvim,tmp}/{backup,swap,undo}
mkdir_conf .vim/autoload
copy_conf .vim/autoload/plug.vim
copy_conf .vimrc
copy_conf .vimrc.minimal

section vis
mkdir_conf .config/vis
mkdir_conf .config/vis/local-plugins
copy_conf .config/vis/visrc.lua
copy_conf .config/vis/prep.sh

section ipython
mkdir_conf .ipython/profile_default
copy_conf .ipython/profile_default/ipython_config.py

section emacs.d
mkdir_conf .emacs.d
copy_conf .emacs.d/init.el
copy_conf .emacs.d/ui.el

section zathura
mkdir_conf .config/zathura
copy_conf .config/zathura/zathurarc

section i3
mkdir_conf .config/i3
copy_conf .i3status.conf
copy_conf .config/i3/config

section "{ba,z,tc,c}sh"
copy_conf .bashrc
copy_conf .zshrc
copy_conf .cshrc
copy_conf .mk_alias
copy_conf .aliases

section cli
mkdir_conf .config/lxterminal
mkdir_conf .config/terminator
copy_conf .config/lxterminal/lxterminal.conf
copy_conf .config/terminator/config
copy_conf .tmux.conf

section git
copy_conf .gitconfig

section xfce4
mkdir_conf .config/xfce4/xfconf/xfce-perchannel-xml
copy_conf .config/xfce4/xfconf/xfce-perchannel-xml/xfce4-keyboard-shortcuts.xml
copy_conf .config/xfce4/xfconf/xfce-perchannel-xml/xfce4-panel.xml

section qterminal
mkdir_conf .config/qterminal.org
copy_conf .config/qterminal.org/qterminal.ini

section unison
mkdir_conf .unison
copy_conf .unison/default.prf

if [ "$MODE" == restore ] ; then
  echo
  echo --------- installing external confs --------- 
  EXT_DIR=$SRC/external
  loudcp "$EXT_DIR/bash-sensible/sensible.bash" "$DST/.sensible.bash"
  loudcp "$EXT_DIR/commacd/commacd.bash" "$DST/.commacd.bash"
  echo
fi

echo TODO: may need to run "git clone https://github.com/martanne/vis"
echo "Backups are located in $BACKUP"
