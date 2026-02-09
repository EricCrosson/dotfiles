# 1Password Shell Plugin for Claude Code

A custom 1Password shell plugin that provides Touch ID-protected GitHub token authentication for Claude Code, with smart authentication that skips biometric prompts for help and version flags.

## Architecture

This plugin enables Claude Code to work with 1Password's biometric authentication system, providing secure credential management without storing tokens in environment variables or config files.

### Components

1. **Plugin Binary** (`pkgs/op-plugin-claude/`)
   - Go-based RPC plugin using HashiCorp go-plugin framework
   - Defines credential schema for GitHub Personal Access Token
   - Configures when authentication is required:
     - Skips auth for `--help`, `-h`, `--version`, `-v`
     - Skips auth when run without arguments
   - Installed to: `~/.op/plugins/local/op-plugin-claude`

2. **Unwrapped Binary** (`claude-unwrapped`)
   - Symlink to the actual claude binary from claude-code package
   - This is what the plugin executes after injecting credentials
   - Added in: `profiles/bitgo/default.nix`

3. **Wrapper Script** (`claude`)
   - Shell script that calls `op plugin run -- claude-unwrapped`
   - Replaces the standard claude-code package wrapper
   - Added in: `profiles/bitgo/default.nix`

4. **Installation Activation**
   - Home-manager activation script to copy plugin to `~/.op/plugins/local/`
   - Sets correct permissions (700) on `.op` directories
   - Implemented in: `profiles/bitgo/default.nix`

## Installation

The plugin automatically installs on `home-manager switch`.
After rebuild, the plugin is ready to use!

## Usage

### Initial Setup

After installation, initialize the plugin:

```bash
op plugin init claude-unwrapped

# When prompted:
# 1. Authenticate with Touch ID
# 2. Select: claude-code-github-token from Nix-Secrets vault
# 3. Choose: "Use automatically globally" (or directory-specific if preferred)
```

### Running Claude Code

```bash
# Normal usage - requires Touch ID
claude

# Help and version - NO Touch ID required!
claude --help
claude --version
```

## How It Works

### Command Flow

```
User types: claude
     ↓
Wrapper calls: op plugin run -- claude-unwrapped
     ↓
Plugin checks arguments:
  - If --help/--version: Skip authentication, run directly
  - Otherwise: Continue to authentication
     ↓
1Password:
  - Prompts for Touch ID
  - Retrieves token from vault
     ↓
Plugin executes: claude-unwrapped with CLAUDE_CODE_GITHUB_TOKEN set
     ↓
Claude Code runs with authenticated token
```

### Smart Authentication Logic

The plugin uses `needsauth.NotForHelpOrVersion()` to automatically skip authentication when:

- User runs `claude --help` or `claude -h`
- User runs `claude --version` or `claude -v`
- User runs `claude` without any arguments

This provides a better UX by only requiring Touch ID when actually needed.

## Maintenance

### Updating Plugin Code

1. Modify `pkgs/op-plugin-claude/main.go`
2. Run `nix-shell -p go --run "go mod tidy"` if dependencies changed
3. Update `vendorHash` in `default.nix` if needed:
   ```bash
   cd pkgs/op-plugin-claude
   nix build --impure --expr '(import <nixpkgs> {}).callPackage ./default.nix {}' 2>&1 | grep "got:"
   # Update vendorHash with the hash from error message
   ```
4. Rebuild: `home-manager switch`

## References

- [1Password Shell Plugins Documentation](https://developer.1password.com/docs/cli/shell-plugins/)
- [1Password Shell Plugins SDK](https://github.com/1Password/shell-plugins)
- [Claude Code Documentation](https://claude.ai/claude-code)
