if [ "$#" != "1" ];
then echo USAGE: $0 destination_dir
     exit
fi

SRC=$1
DST=$PWD
BACKUP=$PWD/confs-backup

echo Backup directory in $BACKUP
echo Kill script to cancel copy from source directory $SRC to $DST
echo Press ENTER to continue

read changes_ok

mkdir_conf() {
    echo mkdir -p "$DST/$SECTION/$1"
    mkdir -p "$BACKUP/$SECTION/$1"
    mkdir -p "$DST/$SECTION/$1"
}

copy_conf() {
    echo cp "$SRC/$1" "$DST/$SECTION/$1"
    cp "$DST/$SECTION/$1" "$BACKUP/$SECTION/$1"
    cp "$SRC/$1" "$DST/$SECTION/$1"
}

SECTION="none"

section() {
    echo
    echo --------- $1 --------- 
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

echo TODO: may need to run "git clone https://github.com/martanne/vis"
echo Backups are located in $BACKUP
