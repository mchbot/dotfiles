#!/usr/bin/env bash

SRC=$(readlink -f "${BASH_SOURCE[0]}")
DIR="$( cd "$( dirname "${SRC}" )" >/dev/null 2>&1 && pwd )"

ln -s "${DIR}/.gitconfig" ~/.gitconfig
