# SSH Migration Guide: docker exec → SSH

This guide documents how to migrate from `docker exec` to SSH for accessing
the lisply-mcp container from skewed-emacs. This improves security by removing
the need to mount `docker.sock` into the skewed-emacs container.

## Overview

**Before (insecure):**
- skewed-emacs has docker.sock mounted
- User runs `docker exec lisply-mcp ...` to run claude, gemini, codex

**After (secure):**
- skewed-emacs has NO docker.sock
- lisply-mcp runs sshd
- User runs `ssh node@lisply-mcp ...` to access tools

## Files Created/Modified

### New Files in lisply-mcp/docker/

1. **Dockerfile.ssh** - Modified Dockerfile that adds openssh-server
2. **entrypoint-ssh.sh** - Modified entrypoint that starts sshd
3. **setup-ssh-keys.sh** - Script to generate SSH key pair

### New Files in skewed-emacs/dot-files/

1. **ssh-aliases.sh** - New bash functions using SSH instead of docker exec

### Example Files

1. **docker-compose-ssh.yml.example** - Shows the required volume mount changes

## Migration Steps

### Step 1: Build the SSH-enabled lisply-mcp image

```bash
cd /projects/lisply-mcp/docker

# Use the SSH Dockerfile
cp Dockerfile Dockerfile.orig
cp Dockerfile.ssh Dockerfile
cp entrypoint.sh entrypoint.sh.orig  
cp entrypoint-ssh.sh entrypoint.sh

# Build the image
./build
```

### Step 2: Generate SSH keys (run in skewed-emacs container)

```bash
# From inside skewed-emacs container
cd /projects/lisply-mcp/docker
chmod +x setup-ssh-keys.sh
./setup-ssh-keys.sh
```

This creates:
- `~/.ssh/lisply-mcp` (private key)
- `~/.ssh/lisply-mcp.pub` (public key)  
- `/projects/skewed-emacs/ssh-keys/authorized_keys` (for mounting into lisply-mcp)

### Step 3: Update docker-compose.yml

Key changes to make:

**skewed-emacs service:**
```yaml
volumes:
  # REMOVE this line:
  # - /var/run/docker.sock:/var/run/docker.sock
  
  # ADD this (SSH keys directory):
  - ${PROJECTS_DIR}/skewed-emacs/ssh-keys:/home/emacs-user/.ssh:ro
```

**lisply-mcp service:**
```yaml
volumes:
  # ADD this (authorized_keys for SSH):
  - ${PROJECTS_DIR}/skewed-emacs/ssh-keys/authorized_keys:/home/node/.ssh/authorized_keys:ro
```

### Step 4: Update bash_profile

In `/projects/skewed-emacs/dot-files/bash_profile`, replace the docker exec
functions with SSH versions:

```bash
# Add at the end of bash_profile:
if [ -f "$HOME/skewed-emacs/dot-files/ssh-aliases.sh" ]; then
    . "$HOME/skewed-emacs/dot-files/ssh-aliases.sh"
fi
```

Or simply replace the `claudly`, `geminly` functions with the versions in
`ssh-aliases.sh`.

### Step 5: Restart containers

```bash
./compose-dev down
./compose-dev up -d
```

### Step 6: Test the connection

```bash
# From inside skewed-emacs
lisply-test  # Should show "Connection successful!"

# Then try the tools
claudly
geminly
codexly
```

## New Commands Available

After migration, these commands are available in skewed-emacs:

| Command | Description |
|---------|-------------|
| `claudly` | Start Claude Code with MCP integration |
| `geminly` | Start Gemini CLI with MCP integration |
| `codexly` | Start OpenAI Codex CLI |
| `lisply-shell` | Open an interactive shell in lisply-mcp |
| `lisply-test` | Test SSH connection to lisply-mcp |

## Troubleshooting

### "Permission denied" SSH errors

1. Check that authorized_keys is properly mounted:
   ```bash
   docker exec lisply-mcp cat /home/node/.ssh/authorized_keys
   ```

2. Check permissions in lisply-mcp:
   ```bash
   docker exec lisply-mcp ls -la /home/node/.ssh/
   ```
   Should show:
   - `.ssh/` directory: 700
   - `authorized_keys`: 600

3. Verify sshd is running:
   ```bash
   docker exec lisply-mcp pgrep -x sshd
   ```

### "Host key verification failed"

The SSH aliases use `-o StrictHostKeyChecking=no` to avoid this, but if
you're connecting manually, you may need to clear known_hosts:
```bash
ssh-keygen -R lisply-mcp
```

### Connection refused

1. Verify lisply-mcp is running and healthy
2. Check that both containers are on the same docker network
3. Verify sshd is running inside lisply-mcp

## Security Notes

1. **Key-based auth only**: Password authentication is disabled in sshd
2. **AllowUsers node**: Only the node user can SSH in
3. **PermitRootLogin no**: Root SSH is disabled
4. **No docker.sock in skewed-emacs**: The main security improvement
5. **Private key permissions**: Keep `~/.ssh/lisply-mcp` at mode 600

## Rolling Back

To revert to the docker exec method:

1. Restore original Dockerfile and entrypoint:
   ```bash
   cd /projects/lisply-mcp/docker
   cp Dockerfile.orig Dockerfile
   cp entrypoint.sh.orig entrypoint.sh
   ./build
   ```

2. Restore docker.sock mount in docker-compose.yml for skewed-emacs

3. Use the original `claudly`/`geminly` functions in bash_profile

