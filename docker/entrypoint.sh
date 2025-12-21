#!/bin/bash
# Minimal entrypoint for lisply-mcp container This container serves as
# MCP middleware - backend Lisply servers are spawned as needed

cleanup() {
    echo "Received signal, shutting down..."
    exit 0
}

trap cleanup SIGTERM SIGINT

set -e

echo "Lisply-MCP Container starting."
echo "MCP wrapper available at: /app/scripts/mcp-wrapper.js"
echo ""

# Set up environment
export HOME=/home/node

if [ -t 0 ]; then
    echo "Interactive mode detected."
    echo "Commands: .help, .quit"
    while true; do
        echo -n "lisply-mcp> "
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
                echo ""
                echo "MCP wrapper: node /app/scripts/mcp-wrapper.js --help"
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
    echo "Detached mode - container running for MCP access..."
    while true; do
        sleep 3600 &  
        wait $!       
    done
fi

cleanup
