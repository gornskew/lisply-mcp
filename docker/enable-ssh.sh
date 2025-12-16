#!/bin/bash
#
# Enable SSH mode for lisply-mcp
# This script swaps the Dockerfile and entrypoint to SSH-enabled versions
#

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "Enabling SSH mode for lisply-mcp..."

# Backup originals if not already done
if [ ! -f Dockerfile.orig ]; then
    cp Dockerfile Dockerfile.orig
    echo "  Backed up Dockerfile -> Dockerfile.orig"
fi

if [ ! -f entrypoint.sh.orig ]; then
    cp entrypoint.sh entrypoint.sh.orig
    echo "  Backed up entrypoint.sh -> entrypoint.sh.orig"
fi

# Install SSH versions
cp Dockerfile.ssh Dockerfile
cp entrypoint-ssh.sh entrypoint.sh

echo ""
echo "SSH mode enabled. Next steps:"
echo "  1. Run: ./build"
echo "  2. Generate SSH keys (from skewed-emacs): ./setup-ssh-keys.sh"
echo "  3. Update docker-compose.yml (see SSH-MIGRATION.md)"
echo "  4. Restart containers: ./compose-dev down && ./compose-dev up -d"
echo ""
echo "To revert to non-SSH mode, run: ./disable-ssh.sh"
