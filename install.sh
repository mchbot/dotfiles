#!/usr/bin/env bash

SRC=$(readlink -f "${BASH_SOURCE[0]}")
DIR="$( cd "$( dirname "${SRC}" )" >/dev/null 2>&1 && pwd )"

# Set up git
ln -s "${DIR}/.gitconfig" ~/.gitconfig

# Install github and bitbucket host keys
mkdir -p ~/.ssh
cp "${DIR}/.ssh/known_hosts" ~/.ssh/
chmod 0500 ~/.ssh
chmod 0400 ~/.ssh/*

