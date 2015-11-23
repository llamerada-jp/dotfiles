#!/usr/bin/env bash

sudo -v

if [ "$(uname)" == 'Darwin' ]; then
  sh ./script/osx.sh

elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
  if [ -e /etc/lsb-release ]; then
    bash script/ubuntu.sh
  fi
fi 
