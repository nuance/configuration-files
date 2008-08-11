#!/bin/bash

$CONFIG_DIR=`pwd`

pushd ~

# Set up configuration files
ln -s $CONFIG_DIR/emacs/dotemacs .emacs
ln -s $CONFIG_DIR/dotfiles/dotzshrc .zshrc
ln -s $CONFIG_DIR/dotfiles/dotscreenrc .screenrc

popd