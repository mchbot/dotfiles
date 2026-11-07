#!/usr/bin/env bash

SRC=$(readlink -f "${BASH_SOURCE[0]:-$0}")
DIR="$( cd "$( dirname "${SRC}" )" >/dev/null 2>&1 && pwd )"

# Set up git
cp "${DIR}/.gitconfig" ~/

# Install github and bitbucket host keys
mkdir -p ~/.ssh
cp ${DIR}/.ssh/* ~/.ssh/
chmod 0500 ~/.ssh
chmod 0400 ~/.ssh/*

# Install mise (https://mise.jdx.dev)
export PATH="$HOME/.local/bin:$PATH"
if ! command -v mise >/dev/null 2>&1; then
  curl -fsSL https://mise.run | MISE_INSTALL_PATH="$HOME/.local/bin/mise" sh
fi
MISE_BIN="$(command -v mise || echo "$HOME/.local/bin/mise")"

# Detect the shell that is running this script (fall back to $SHELL)
if [ -n "${ZSH_VERSION:-}" ] || [ -n "${ZSH_NAME:-}" ]; then
  shell=zsh
elif [ -n "${BASH_VERSION:-}" ]; then
  shell=bash
elif [ -n "${FISH_VERSION:-}" ]; then
  shell=fish
else
  shell="$(basename "${SHELL:-/bin/sh}")"
fi

# Append mise's activation script to the rc file for the detected shell
case "$shell" in
  bash)
    rc="$HOME/.bashrc"
    line="eval \"\$(${MISE_BIN} activate bash)\""
    ;;
  zsh)
    rc="${ZDOTDIR:-$HOME}/.zshrc"
    line="eval \"\$(${MISE_BIN} activate zsh)\""
    ;;
  fish)
    rc="$HOME/.config/fish/config.fish"
    line="${MISE_BIN} activate fish | source"
    ;;
  *)
    echo "install.sh: unsupported shell '${shell}' for mise activation; add this to the matching rc file manually:" >&2
    echo "  eval \"\$(${MISE_BIN} activate ${shell})\"" >&2
    exit 0
    ;;
esac

if grep -qF "$line" "$rc" 2>/dev/null; then
  : # already activated
else
  mkdir -p "$(dirname "$rc")"
  printf '\n# Added by dotfiles/install.sh\n%s\n' "$line" >> "$rc"
fi