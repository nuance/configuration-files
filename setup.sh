#!/bin/bash

CONFIG_DIR=`pwd`

pushd ~

# Link emacs directory (so the paths in the emacs config work)
ln -s $CONFIG_DIR/emacs emacs

# Set up configuration files
ln -s $CONFIG_DIR/emacs/dotemacs .emacs
ln -s $CONFIG_DIR/dotfiles/dotzshrc .zshrc
ln -s $CONFIG_DIR/dotfiles/dotscreenrc .screenrc

# Add scripts
mkdir bin
ln -s $CONFIG_DIR/bin/epylint bin/

# Add shell includes
mkdir includes
ln -s $CONFIG_DIR/includes/* includes/

mkdir -p .config/awesome
ln -s $CONFIG_DIR/awesome/rc.lua .config/awesome/

popd

echo "Configured!"