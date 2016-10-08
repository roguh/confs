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

echo vim
mkdir_conf .vim/autoload
copy_conf .vimrc
copy_conf .vim/autoload/plug.vim

echo emacs.d
mkdir_conf .emacs.d
copy_conf .emacs.d/init.el
copy_conf .emacs.d/ui.el

echo i3
mkdir_conf .config/i3
copy_conf .i3status.conf
copy_conf .config/i3/config
copy_conf .config/i3/config.base
copy_conf .config/i3/startup.sh

echo bash
copy_conf .bashrc

echo zsh
copy_conf .zshrc

echo terminator
mkdir_conf .config/terminator
copy_conf .config/terminator/config

echo tmux
copy_conf .tmux.conf

echo TODO: may need to run 'ln -s ~/.vim ~/.config/nvim'
echo TODO: may need to run 'ln -s ~/.vimrc ~/.config/nvim/init.vim'

echo TODO: may need to run "git clone https://github.com/okraits/j4-make-config"

echo TODO: may need to run "git clone https://github.com/robbyrussell/oh-my-zsh"

echo Backups are located in $BACKUP_DIR
