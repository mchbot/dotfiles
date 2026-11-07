#!/usr/bin/env bash

# Set up SSH keys for GitHub authentication and commit signing.
# Prompts the user to either generate new passwordless ed25519 key pairs
# or provide their own key files.

set -euo pipefail

AUTH_KEY="authentication_id_ed25519"
SIGN_KEY="signing_id_ed25519"

mkdir -p ~/.ssh
chmod 0700 ~/.ssh

# Both keys already present; nothing to do.
if [ -e ~/.ssh/"${AUTH_KEY}" ] && [ -e ~/.ssh/"${SIGN_KEY}" ]; then
  exit 0
fi

echo
echo "Setting up SSH keys for GitHub authentication and commit signing:"
echo "  - Authentication key: ~/.ssh/${AUTH_KEY}   (authenticates to github.com)"
echo "  - Signing key:        ~/.ssh/${SIGN_KEY}   (signs git commits)"
echo
echo "How would you like to provide these keys?"
echo "  1) Generate new passwordless ed25519 key pairs"
echo "  2) Provide my own key files"
printf "Enter your choice [1/2]: "
read -r key_choice

case "$key_choice" in
  1)
    for key in "$AUTH_KEY" "$SIGN_KEY"; do
      if [ -e ~/.ssh/"${key}" ] || [ -e ~/.ssh/"${key}.pub" ]; then
        echo "Skipping ${key}: a key already exists at ~/.ssh/${key}"
      else
        ssh-keygen -t ed25519 -f ~/.ssh/"${key}" -N "" -C "${key}" >/dev/null
        echo "Generated ~/.ssh/${key} and ~/.ssh/${key}.pub"
      fi
    done
    echo
    echo "Add these public keys to your GitHub account:"
    echo "  - Authentication: https://github.com/settings/keys"
    echo "  - Signing:        https://github.com/settings/ssh"
    echo
    if [ -f ~/.ssh/"${AUTH_KEY}.pub" ]; then
      echo "  Authentication: $(cat ~/.ssh/${AUTH_KEY}.pub)"
    fi
    if [ -f ~/.ssh/"${SIGN_KEY}.pub" ]; then
      echo "  Signing:        $(cat ~/.ssh/${SIGN_KEY}.pub)"
    fi
    ;;
  2)
    echo
    echo "Place your key files at the following locations:"
    echo
    echo "  Authentication key (private + public):"
    echo "    ~/.ssh/${AUTH_KEY}"
    echo "    ~/.ssh/${AUTH_KEY}.pub"
    echo
    echo "  Signing key (private + public):"
    echo "    ~/.ssh/${SIGN_KEY}"
    echo "    ~/.ssh/${SIGN_KEY}.pub"
    echo
    echo "Once the files are in place, re-run install.sh to set the correct"
    echo "permissions, or set them manually:"
    echo "    chmod 0600 ~/.ssh/${AUTH_KEY} ~/.ssh/${SIGN_KEY}"
    echo "    chmod 0644 ~/.ssh/${AUTH_KEY}.pub ~/.ssh/${SIGN_KEY}.pub"
    ;;
  *)
    echo "install-ssh-keys.sh: invalid choice '${key_choice}'; skipping SSH key setup" >&2
    ;;
esac

# Ensure correct permissions on any keys that are now present
chmod 0600 ~/.ssh/"${AUTH_KEY}" ~/.ssh/"${SIGN_KEY}" 2>/dev/null || true
chmod 0644 ~/.ssh/"${AUTH_KEY}.pub" ~/.ssh/"${SIGN_KEY}.pub" 2>/dev/null || true
