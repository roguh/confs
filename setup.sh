#!/usr/bin/env bash

function link () {
	if ! [ -f "$2" ]; then
		ln -s $1 $2
	else
		echo $2 already exists
	fi
}
link ~/.bashrc ~/.mkshrc
link ~/.vim ~/.config/nvim
link ~/.vimrc ~/.config/nvim/init.vim
mkdir -p ~/{.vim,.config/nvim,tmp}/{backup,swap,undo}
git submodule update --init --recursive
