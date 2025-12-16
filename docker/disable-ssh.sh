#!/bin/bash
#
# Disable SSH mode for lisply-mcp
# This script restores the original Dockerfile and entrypoint
#

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "Disabling SSH mode for lisply-mcp..."

if [ -f Dockerfile.orig ]; then
    cp Dockerfile.orig Dockerfile
    echo "  Restored Dockerfile from Dockerfile.orig"
else
    echo "  ERROR: Dockerfile.orig not found. Cannot restore."
    exit 1
fi

if [ -f entrypoint.sh.orig ]; then
    cp entrypoint.sh.orig entrypoint.sh
    echo "  Restored entrypoint.sh from entrypoint.sh.orig"
else
    echo "  ERROR: entrypoint.sh.orig not found. Cannot restore."
    exit 1
fi

echo ""
echo "SSH mode disabled. Next steps:"
echo "  1. Run: ./build"
echo "  2. Restore docker.sock mount in docker-compose.yml for skewed-emacs"
echo "  3. Use original claudly/geminly functions in bash_profile"
echo "  4. Restart containers"
