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

# === SSH Key Setup ===
# Check for authorized_keys mounted from skewed-emacs
# The public key should be mounted at /home/node/.ssh/authorized_keys
if [ -f /home/node/.ssh/authorized_keys ]; then
    echo "SSH authorized_keys found."
    chmod 600 /home/node/.ssh/authorized_keys
    chown node:node /home/node/.ssh/authorized_keys
else
    echo "WARNING: No SSH authorized_keys found at /home/node/.ssh/authorized_keys"
    echo "SSH access will not be available until key is configured."
fi

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
