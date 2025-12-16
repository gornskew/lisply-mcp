#!/bin/bash

cleanup() {
    echo "Received SIGTERM, shutting down..."
    # Stop sshd gracefully
    if [ -f /var/run/sshd.pid ]; then
        kill $(cat /var/run/sshd.pid) 2>/dev/null
    fi
    exit 0
}

# Trap SIGTERM and call cleanup
trap cleanup SIGTERM

set -e

echo "Lisply-MCP Container starting."
echo ""

# === SSH Setup ===
# The authorized_keys file is injected by compose-dev after container starts
# via 'docker cp'. We just need to ensure the directory exists and start sshd.

SSH_DIR="/home/node/.ssh"
mkdir -p "$SSH_DIR"
chmod 700 "$SSH_DIR"
chown node:node "$SSH_DIR"

# Generate host keys if they don't exist
if [ ! -f /etc/ssh/ssh_host_rsa_key ]; then
    echo "Generating SSH host keys..."
    ssh-keygen -A
fi

# Start SSH server
echo "Starting SSH server..."
/usr/sbin/sshd

# Verify sshd is running
sleep 1
if pgrep -x sshd > /dev/null; then
    echo "SSH server started successfully on port 22."
else
    echo "WARNING: SSH server failed to start!"
fi

# Check if authorized_keys exists (injected by compose-dev)
if [ -f "$SSH_DIR/authorized_keys" ]; then
    echo "SSH authorized_keys found - SSH access from skewed-emacs is enabled."
else
    echo "NOTE: SSH authorized_keys not yet installed."
    echo "      Run './compose-dev up' to set up SSH access."
fi

# === Original Entrypoint Logic ===
# Source .bashrc to get claude alias for node user
export HOME=/home/node
source /home/node/.bashrc

if [ -t 0 ]; then
    echo "Interactive mode detected. Starting command loop..."
    echo "Entering interactive loop..."
    while true; do
        echo -n "node> "
        if ! read -r line; then
            echo ""
            echo "stdin closed - shutting down container"
            break
        fi

        if [[ -z "$line" ]]; then
            continue
        fi

        case "$line" in
            ".help")
                echo "Available commands:"
                echo "  .help    - Show this help"
                echo "  .quit    - Exit container"
                continue
                ;;
            ".quit" | "quit" | "exit")
                echo "Goodbye!"
                break
                ;;
            *)
                echo "Unknown command: $line"
                echo "Try: .help for available commands"
                continue
                ;;
        esac
    done
else
    echo "Detached mode detected, sleeping and waiting for signals..."
    while true; do
        sleep 3600 &  
        wait $!       
    done
fi

cleanup
