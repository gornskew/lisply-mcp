#!/bin/bash

cleanup() {
    echo "Received SIGTERM, shutting down..."
    exit 0
}

# Trap SIGTERM and call cleanup
trap cleanup SIGTERM

set -e

# Source .bashrc to get claude alias for node user
export HOME=/home/node
source /home/node/.bashrc

echo "Lisply-MCP Container starting."
echo "" 

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
