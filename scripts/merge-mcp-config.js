#!/usr/bin/env node

// Simple MCP config merger
// Merges base mcp-config.json with private-mcp-config.json

const fs = require('fs');
const path = require('path');

const baseConfigPath = '/app/docker/mcp-config.json';
const privateConfigPath = '/projects/skewed-emacs/private-mcp-config.json';
const outputPath = '/app/docker/merged-mcp-config.json';

try {
    // Read base config
    const baseConfig = JSON.parse(fs.readFileSync(baseConfigPath, 'utf8'));
    
    // Read private config if it exists
    let privateConfig = { mcpServers: {} };
    if (fs.existsSync(privateConfigPath)) {
        privateConfig = JSON.parse(fs.readFileSync(privateConfigPath, 'utf8'));
        console.log('Found private MCP config, merging...');
    } else {
        console.log('No private MCP config found, using base config only.');
    }
    
    // Merge configs - private config takes precedence
    const mergedConfig = {
        mcpServers: {
            ...baseConfig.mcpServers,
            ...privateConfig.mcpServers
        }
    };
    
    // Write merged config
    fs.writeFileSync(outputPath, JSON.stringify(mergedConfig, null, 4));
    
    console.log('MCP config merged successfully to:', outputPath);
    console.log('Available servers:', Object.keys(mergedConfig.mcpServers).join(', '));
    
} catch (error) {
    console.error('Error merging MCP configs:', error.message);
    process.exit(1);
}
