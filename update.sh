if [ "$#" != "2" ];
then echo USAGE: $0 source_dir destination_dir 
     exit
fi

echo $1 to $2
SRC_DIR=$1
DST_DIR=$2

echo Press ENTER to copy configuration files from source directory $SRC_DIR to $DST_DIR
read a 

# vim
mkdir -p $DST_DIR/.vim/autoload
cp $SRC_DIR/.vimrc $DST_DIR/.vimrc
cp $SRC_DIR/.vim/autoload/plug.vim $DST_DIR/.vim/autoload/plug.vim

# emacs.d
mkdir -p $DST_DIR/.emacs.d
cp $SRC_DIR/.emacs.d/init.el $DST_DIR/.emacs.d/init.el
cp $SRC_DIR/.emacs.d/ui.el   $DST_DIR/.emacs.d/ui.el

# i3
mkdir -p $DST_DIR/.config/i3
cp $SRC_DIR/.i3status.conf         $DST_DIR/.i3status.conf
cp $SRC_DIR/.config/i3/config      $DST_DIR/.config/i3/config
cp $SRC_DIR/.config/i3/config.base $DST_DIR/.config/i3/config.base
cp $SRC_DIR/.config/i3/startup.sh  $DST_DIR/.config/i3/startup.sh

echo TODO: may need to run "git clone https://github.com/okraits/j4-make-config"

# zsh
cp $SRC_DIR/.zshrc $DST_DIR/.zshrc
echo TODO: may need to run "git clone https://github.com/robbyrussell/oh-my-zsh"

# terminator
mkdir -p $DST_DIR/.config/terminator
cp $SRC_DIR/.config/terminator/config $DST_DIR/.config/terminator/config
