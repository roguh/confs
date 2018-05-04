#!/usr/bin/env bash
SRC=.
DST=~

function link () {
    if [ -f "$1" ]; then
        if ! [ -f "$2" ]; then
            ln -s $1 $2
        else
            echo $2 already exists
        fi
    else
        echo $1 does not exist
    fi
}
link $DST/.bashrc $DST/.mkshrc
link $DST/.vim $DST/.config/nvim
link $DST/.vimrc $DST/.config/nvim/init.vim
mkdir -p $DST/{.vim,.config/nvim,tmp}/{backup,swap,undo}

echo --------- downloading and installing external confs --------- 
git submodule update --init --recursive
EXT_DIR=$SRC/external
cp "$EXT_DIR/bash-sensible/sensible.bash" "$DST/.sensible.bash"
cp "$EXT_DIR/commacd/commacd.bash" "$DST/.commacd.bash"
