#!/usr/bin/env bash

SRC=$(readlink -f "${BASH_SOURCE[0]:-$0}")
DIR="$( cd "$( dirname "${SRC}" )" >/dev/null 2>&1 && pwd )"

# Set up git
cp "${DIR}/.gitconfig" ~/

# Set up the SSH directory and install the SSH config + known hosts.
mkdir -p ~/.ssh
chmod 0700 ~/.ssh
cp -n "${DIR}/.ssh/config" "${DIR}/.ssh/known_hosts" ~/.ssh/ 2>/dev/null || true
chmod 0644 ~/.ssh/config ~/.ssh/known_hosts 2>/dev/null || true

# Generate or provide SSH keys, but only if they don't already exist.
AUTH_KEY="authentication_id_ed25519"
SIGN_KEY="signing_id_ed25519"
if [ -e ~/.ssh/"${AUTH_KEY}" ] && [ -e ~/.ssh/"${SIGN_KEY}" ]; then
  : # both keys already present
else
  bash "${DIR}/scripts/install-ssh-keys.sh"
fi

# Install mise (https://mise.jdx.dev)
bash "${DIR}/scripts/install-mise.sh"