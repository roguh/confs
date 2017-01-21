if [ "$#" != "2" ];
then echo USAGE: $0 source_dir destination_dir
     exit
fi

echo $1 to $2
SRC_DIR=$1
DST_DIR=$2
BACKUP_DIR=./confs-backup

echo Backup directory in $BACKUP_DIR

echo Press ENTER to copy configuration files from source directory $SRC_DIR to $DST_DIR
read changes_ok

mkdir_conf() {
    echo mkdir -p $DST_DIR/$1
    mkdir -p $DST_DIR/$1
    mkdir -p $BACKUP_DIR/$1
}

copy_conf() {
    cp $SRC_DIR/$1 $BACKUP_DIR/$1
    echo cp $SRC_DIR/$1 $DST_DIR/$1
    cp $SRC_DIR/$1 $DST_DIR/$1
}

section() {
    echo
    echo --------- $1 --------- 
}

section vim
mkdir_conf {.vim,.config/nvim,tmp}/{backup,swap,undo}
mkdir_conf .vim/autoload
copy_conf .vim/autoload/plug.vim
copy_conf .vimrc
copy_conf .vimrc.minimal

section vis
mkdir_conf .config/vis
copy_conf .config/vis/visrc.lua

section emacs.d
mkdir_conf .emacs.d
copy_conf .emacs.d/init.el
copy_conf .emacs.d/ui.el

section i3
mkdir_conf .config/i3
copy_conf .i3status.conf
copy_conf .config/i3/config
copy_conf .config/i3/config.base
copy_conf .config/i3/config.apps

section {ba,z,tc,c}sh
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

section unison
mkdir_conf .unison
copy_conf .unison/default.prf

echo TODO: may need to run "git clone https://github.com/okraits/j4-make-config"
echo TODO: may need to run "git clone https://github.com/robbyrussell/oh-my-zsh"
echo TODO: may need to run "git clone https://github.com/martanne/vis"
echo TODO: may need to symlink vis/lua to ~/.config/vis
echo Backups are located in $BACKUP_DIR
