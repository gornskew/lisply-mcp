#!/bin/bash
#
# Setup SSH keys for skewed-emacs -> lisply-mcp authentication
# Run this script once in the skewed-emacs container to generate keys
#

SSH_DIR="$HOME/.ssh"
KEY_FILE="$SSH_DIR/lisply-mcp"
AUTHORIZED_KEYS_DIR="/projects/skewed-emacs/ssh-keys"

echo "Setting up SSH keys for lisply-mcp access..."

# Create .ssh directory if it doesn't exist
mkdir -p "$SSH_DIR"
chmod 700 "$SSH_DIR"

# Generate new key pair if it doesn't exist
if [ ! -f "$KEY_FILE" ]; then
    echo "Generating new SSH key pair..."
    ssh-keygen -t ed25519 -f "$KEY_FILE" -N "" -C "emacs-user@skewed-emacs"
    echo "Key pair generated."
else
    echo "SSH key already exists at $KEY_FILE"
fi

# Create the shared directory for authorized_keys
mkdir -p "$AUTHORIZED_KEYS_DIR"

# Copy public key to shared location for lisply-mcp to mount
cp "$KEY_FILE.pub" "$AUTHORIZED_KEYS_DIR/authorized_keys"
chmod 644 "$AUTHORIZED_KEYS_DIR/authorized_keys"

echo ""
echo "Setup complete!"
echo ""
echo "Public key has been copied to: $AUTHORIZED_KEYS_DIR/authorized_keys"
echo ""
echo "Add this to your docker-compose.yml for the lisply-mcp service:"
echo ""
echo "  volumes:"
echo "    - \${PROJECTS_DIR}/skewed-emacs/ssh-keys/authorized_keys:/home/node/.ssh/authorized_keys:ro"
echo ""
echo "Also add to the skewed-emacs service (for the SSH config):"
echo ""
echo "  volumes:"
echo "    - \${PROJECTS_DIR}/skewed-emacs/ssh-keys:/home/emacs-user/.ssh/lisply-keys:ro"
echo ""
echo "Or, if you prefer, configure SSH manually:"
echo "  Private key: $KEY_FILE"
echo "  Public key:  $KEY_FILE.pub"
