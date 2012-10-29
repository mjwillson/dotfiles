#!/bin/sh

# Very basic convenience for symlinking dotfiles from a .dotfiles dir.
# There are fancy frameworks for this but I think they overcomplicate things.

if [ ! -e ~/.dotfiles/$1 ]
then
  mkdir -p `dirname ~/.dotfiles/$1`
  mv ~/$1 ~/.dotfiles/$1
  ln -s ~/.dotfiles/$1 ~/$1
#  cd ~/.dotfiles
#  git add ~/.dotfiles/$1
  exit 0
fi

if [ ! -e ~/$1 ]
then
  mkdir -p `dirname ~/$1`
  ln -s ~/.dotfiles/$1 ~/$1
  exit 0
fi

echo You have a local version ~/$1 and a checked-in version ~/.dotfiles/$1 sort it out yourself
