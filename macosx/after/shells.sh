#!/bin/sh

ZSH_PORT=/opt/local/bin/zsh

if ! grep -q $ZSH_PORT /etc/shells; then
  echo $ZSH_PORT | sudo sh -c 'cat >> /etc/shells'
fi

chsh -s $ZSH_PORT
