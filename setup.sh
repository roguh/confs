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

echo ------- mkshrc is bashrc -------
link $DST/.bashrc $DST/.mkshrc

echo ------- linking .vimrc to .config/nvim. creating vim backup dirs ------- 
mkdir -p $DST/.config/nvim
mkdir -p $DST/{.vim,.config/nvim,tmp}/{backup,swap,undo}
link $DST/.vimrc $DST/.config/nvim/init.vim

echo ------- downloading plug.vim -------
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

echo ------- downloading and installing external confs -------
git submodule update --init --recursive
EXT_DIR=$SRC/external
cp "$EXT_DIR/bash-sensible/sensible.bash" "$DST/.sensible.bash"
cp "$EXT_DIR/commacd/commacd.bash" "$DST/.commacd.bash"
