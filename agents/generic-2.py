#!/usr/bin/env python3
"""
Generic MCP Agent with Ollama and Terminal Interface

A simple terminal-based agent that uses Ollama for local LLM inference
and connects to MCP servers defined in mcp-config.json.
"""

import asyncio
import json
import os
import sys
import traceback
from pathlib import Path
from typing import Dict, Any, Optional, List, Tuple

import aiohttp
from dotenv import load_dotenv

try:
    from langchain_ollama import ChatOllama
    from langchain.prompts import ChatPromptTemplate
    from langchain_core.messages import HumanMessage, AIMessage, SystemMessage
except ImportError:
    print("Error: Please install langchain-ollama: pip install langchain-ollama")
    sys.exit(1)

try:
    from mcp_use import MCPAgent, MCPClient
except ImportError:
    print("Error: Please install mcp-use: pip install mcp-use")
    sys.exit(1)


class OllamaMCPAgent:
    """Terminal-based MCP agent using Ollama."""

    def __init__(self,
                 config_path: str = "mcp-config.json",
                 model_name: str = "llama3.2",
                 ollama_base_url: str = "http://localhost:11434",
                 max_steps: int = 10,
                 verbose: bool = True):
        """
        Initialize the agent.
        
        Args:
            config_path: Path to MCP configuration file
            model_name: Ollama model name
            ollama_base_url: Ollama server URL
            max_steps: Maximum steps per conversation
            verbose: Enable verbose logging
        """
        self.config_path = config_path
        self.model_name = model_name
        self.ollama_base_url = ollama_base_url
        self.max_steps = max_steps
        self.verbose = verbose
        
        # Initialize Ollama client
        self.llm = ChatOllama(
            model=model_name,
            base_url=ollama_base_url,
            temperature=0.7
        )
        
        # MCP clients will be initialized in setup
        self.mcp_clients: Dict[str, MCPClient] = {}
        self.available_tools: Dict[str, Dict] = {}
        
        # Permission tracking
        self.always_allow: Dict[str, bool] = {}  # tool_key -> allowed
        
        # Conversation history
        self.conversation_history = []

    async def setup(self):
        """Initialize MCP connections."""
        try:
            # Load MCP configuration
            with open(self.config_path, 'r') as f:
                config = json.load(f)
            
            if self.verbose:
                print(f"Loading MCP configuration from {self.config_path}")
            
            # Connect to each MCP server
            for server_name, server_config in config.get('mcpServers', {}).items():
                try:
                    if self.verbose:
                        print(f"Connecting to MCP server: {server_name}")
                    
                    client = MCPClient(
                        command=server_config['command'],
                        args=server_config['args']
                    )
                    
                    await client.connect()
                    self.mcp_clients[server_name] = client
                    
                    # Get available tools for this server
                    tools = await client.list_tools()
                    self.available_tools[server_name] = {
                        tool.name: tool for tool in tools
                    }
                    
                    if self.verbose:
                        tool_names = list(self.available_tools[server_name].keys())
                        print(f"  Available tools: {', '.join(tool_names)}")
                        
                except Exception as e:
                    print(f"Failed to connect to {server_name}: {e}")
                    if self.verbose:
                        traceback.print_exc()
        
        except FileNotFoundError:
            print(f"Error: Configuration file {self.config_path} not found")
            raise
        except json.JSONDecodeError as e:
            print(f"Error: Invalid JSON in {self.config_path}: {e}")
            raise

    def _format_tools_info(self) -> str:
        """Format information about available tools."""
        info_lines = ["Available MCP Tools:"]
        
        for server_name, tools in self.available_tools.items():
            info_lines.append(f"\n{server_name}:")
            for tool_name, tool in tools.items():
                info_lines.append(f"  - {tool_name}: {tool.description}")
        
        return "\n".join(info_lines)

    def _create_system_prompt(self) -> str:
        """Create system prompt with tool information."""
        tools_info = self._format_tools_info()
        
        return f"""You are a helpful AI assistant with access to MCP (Model Context Protocol) tools for Lisp development environments.

{tools_info}

You can use these tools to:
- Check if Lisp servers are running (ping_lisp)
- Evaluate Lisp code (lisp_eval) 
- Make HTTP requests (http_request)
- Get documentation (get_docs, get_docs_list)

When a user asks you to do something that requires these tools, ask for permission first, then use the appropriate tool. Always explain what you're doing and show the results clearly.

For code evaluation:
- Use gendl-ccl for CCL (Clozure Common Lisp)
- Use gendl-sbcl for SBCL (Steel Bank Common Lisp)  
- Use skewed-emacs for Emacs Lisp

Be helpful, clear, and ask for clarification when needed."""

    async def _call_tool(self, server_name: str, tool_name: str, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """Call an MCP tool."""
        if server_name not in self.mcp_clients:
            return {"error": f"Server {server_name} not available"}
        
        if server_name not in self.available_tools or tool_name not in self.available_tools[server_name]:
            return {"error": f"Tool {tool_name} not available on server {server_name}"}
        
        try:
            client = self.mcp_clients[server_name]
            result = await client.call_tool(tool_name, arguments)
            return {"result": result.content}
        except Exception as e:
            return {"error": str(e)}

    def _parse_tool_request(self, text: str) -> Optional[Tuple[str, str, Dict[str, Any]]]:
        """Parse tool request from LLM response."""
        # Simple parsing - look for patterns like:
        # USE_TOOL: server_name/tool_name {"arg": "value"}
        
        lines = text.split('\n')
        for line in lines:
            line = line.strip()
            if line.startswith('USE_TOOL:'):
                try:
                    parts = line[9:].strip().split(' ', 1)
                    server_tool = parts[0]
                    args_json = parts[1] if len(parts) > 1 else '{}'
                    
                    server_name, tool_name = server_tool.split('/')
                    arguments = json.loads(args_json)
                    
                    return server_name, tool_name, arguments
                except Exception:
                    continue
        
        return None

    async def process_query(self, user_input: str) -> str:
        """Process a user query with potential tool use."""
        # Add user message to history
        self.conversation_history.append(HumanMessage(content=user_input))
        
        # Create messages for the LLM
        messages = [SystemMessage(content=self._create_system_prompt())]
        messages.extend(self.conversation_history)
        
        try:
            # Get response from LLM
            response = await self.llm.ainvoke(messages)
            response_text = response.content
            
            # Check if LLM wants to use a tool
            tool_request = self._parse_tool_request(response_text)
            
            if tool_request:
                server_name, tool_name, arguments = tool_request
                tool_key = f"{server_name}/{tool_name}"
                
                # Check if we have blanket permission for this tool
                if tool_key in self.always_allow and self.always_allow[tool_key]:
                    print(f"🔧 Auto-calling {tool_key} (always allowed)...")
                    allowed = True
                else:
                    # Ask user for permission
                    print(f"\n🤖 I want to use the tool: {tool_key}")
                    print(f"   Arguments: {json.dumps(arguments, indent=2)}")
                    permission = input("   Allow? (y)es, (n)o, (a)lways: ").strip().lower()
                    
                    if permission in ['y', 'yes']:
                        allowed = True
                    elif permission in ['a', 'always']:
                        allowed = True
                        self.always_allow[tool_key] = True
                        print(f"   ✅ Will always allow {tool_key} from now on")
                    else:
                        allowed = False
                
                if allowed:
                    if tool_key not in self.always_allow or not self.always_allow[tool_key]:
                        print(f"🔧 Calling {tool_key}...")
                    
                    tool_result = await self._call_tool(server_name, tool_name, arguments)
                    
                    # Add tool result to conversation
                    tool_message = f"Tool result from {tool_key}:\n{json.dumps(tool_result, indent=2)}"
                    messages.append(AIMessage(content=response_text))
                    messages.append(HumanMessage(content=tool_message))
                    
                    # Get final response
                    final_response = await self.llm.ainvoke(messages)
                    response_text = final_response.content
                else:
                    response_text += "\n\n(Tool use cancelled by user)"
            
            # Add AI response to history
            self.conversation_history.append(AIMessage(content=response_text))
            
            return response_text
            
        except Exception as e:
            error_msg = f"Error processing query: {e}"
            if self.verbose:
                traceback.print_exc()
            return error_msg

    async def interactive_loop(self):
        """Run interactive terminal loop."""
        print("🚀 Ollama MCP Agent Started")
        print(f"📦 Model: {self.model_name}")
        print(f"🔗 Connected servers: {', '.join(self.mcp_clients.keys())}")
        print("💡 Type 'help' for commands, 'quit' to exit")
        print("🔐 Tool permissions: (y)es, (n)o, (a)lways\n")
        
        while True:
            try:
                user_input = input("👤 You: ").strip()
                
                if not user_input:
                    continue
                    
                if user_input.lower() in ['quit', 'exit', 'q']:
                    print("👋 Goodbye!")
                    break
                    
                if user_input.lower() == 'help':
                    self._show_help()
                    continue
                    
                if user_input.lower() == 'tools':
                    print(self._format_tools_info())
                    continue
                    
                if user_input.lower() == 'clear':
                    self.conversation_history.clear()
                    print("🧹 Conversation history cleared")
                    continue
                    
                if user_input.lower() == 'permissions':
                    self._show_permissions()
                    continue
                    
                if user_input.lower().startswith('reset-permissions'):
                    parts = user_input.split()
                    if len(parts) > 1:
                        tool_key = parts[1]
                        if tool_key in self.always_allow:
                            del self.always_allow[tool_key]
                            print(f"🔄 Reset permissions for {tool_key}")
                        else:
                            print(f"❌ No saved permissions for {tool_key}")
                    else:
                        self.always_allow.clear()
                        print("🔄 All permissions reset")
                    continue
                
                print("🤖 Assistant: ", end="", flush=True)
                response = await self.process_query(user_input)
                print(response)
                print()
                
            except KeyboardInterrupt:
                print("\n👋 Goodbye!")
                break
            except Exception as e:
                print(f"❌ Error: {e}")
                if self.verbose:
                    traceback.print_exc()

    def _show_permissions(self):
        """Show current permission settings."""
        if not self.always_allow:
            print("📋 No saved permissions")
            return
            
        print("📋 Saved Permissions:")
        for tool_key, allowed in self.always_allow.items():
            status = "✅ Always Allow" if allowed else "❌ Denied"
            print(f"  {tool_key}: {status}")

    def _show_help(self):
        """Show help information."""
        print("""
Available commands:
  help                    - Show this help
  tools                   - List available MCP tools
  permissions             - Show saved tool permissions
  reset-permissions [tool] - Reset permissions (all if no tool specified)
  clear                   - Clear conversation history
  quit                    - Exit the agent

To use MCP tools, just ask naturally! For example:
  "Can you check if the CCL server is running?"
  "Evaluate (+ 1 2 3) in SBCL"
  "Show me the documentation for gendl"

Permission options when prompted:
  (y)es    - Allow this one time
  (n)o     - Deny this request
  (a)lways - Allow this tool always (saves preference)

The agent will ask for permission before using any tools unless you've chosen "always allow".
        """)

    async def cleanup(self):
        """Clean up resources."""
        for client in self.mcp_clients.values():
            try:
                await client.disconnect()
            except Exception as e:
                if self.verbose:
                    print(f"Error disconnecting client: {e}")


async def main():
    """Main entry point."""
    # Load environment variables
    load_dotenv()
    
    # Parse command line arguments (simple version)
    import argparse
    parser = argparse.ArgumentParser(description='Ollama MCP Agent')
    parser.add_argument('--config', default='mcp-config.json', 
                       help='MCP configuration file path')
    parser.add_argument('--model', default='llama3.2',
                       help='Ollama model name')
    parser.add_argument('--ollama-url', default='http://localhost:11434',
                       help='Ollama server URL')
    parser.add_argument('--max-steps', type=int, default=10,
                       help='Maximum conversation steps')
    parser.add_argument('--verbose', action='store_true',
                       help='Enable verbose logging')
    
    args = parser.parse_args()
    
    # Create and setup agent
    agent = OllamaMCPAgent(
        config_path=args.config,
        model_name=args.model,
        ollama_base_url=args.ollama_url,
        max_steps=args.max_steps,
        verbose=args.verbose
    )
    
    try:
        await agent.setup()
        await agent.interactive_loop()
    except Exception as e:
        print(f"❌ Failed to start agent: {e}")
        if args.verbose:
            traceback.print_exc()
        return 1
    finally:
        await agent.cleanup()
    
    return 0


if __name__ == "__main__":
    try:
        exit_code = asyncio.run(main())
        sys.exit(exit_code)
    except KeyboardInterrupt:
        print("\n👋 Goodbye!")
        sys.exit(0)
