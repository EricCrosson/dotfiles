# 1Password Shell Plugin for git-disjoint

A custom 1Password shell plugin that provides Touch ID-protected GitHub token authentication for git-disjoint.

## Architecture

This plugin enables git-disjoint to work with 1Password's biometric authentication system, providing secure credential management without storing tokens in environment variables or config files.

### Components

1. **Plugin Binary** (`pkgs/op-plugin-git-disjoint/`)

   - Go-based RPC plugin using HashiCorp go-plugin framework
   - Defines credential schema for GitHub Personal Access Token
   - Configures when authentication is required (not for --help/--version)
   - Installed to: `~/.op/plugins/local/op-plugin-git-disjoint`

2. **Unwrapped Binary** (`git-disjoint-unwrapped`)

   - Symlink to the actual git-disjoint binary from the flake input
   - This is what the plugin executes after injecting credentials
   - Added in: `profiles/development/default.nix`

3. **Wrapper Script** (`git-disjoint`)

   - Shell script that calls `op plugin run -- git-disjoint-unwrapped`
   - Makes both `git-disjoint` and `git disjoint` work through the plugin
   - Added in: `profiles/development/default.nix`

4. **Installation Activation**
   - Home-manager activation script to copy plugin to `~/.op/plugins/local/`
   - Sets correct permissions (700) on `.op` directories
   - Implemented in: `profiles/bitgo/default.nix`

## Installation

The plugin automatically installs on `darwin-rebuild switch`.
After rebuild, the plugin is ready to use!

## Usage

### Initial Setup

After installation, initialize the plugin for your workspace:

```bash
# For BitGo repositories
cd ~/workspace/BitGo/some-repo
op plugin init git-disjoint-unwrapped

# When prompted:
# 1. Authenticate with Touch ID
# 2. Select: github-token-bitgo-git-disjoint from Nix-Secrets vault
# 3. Choose: "Use automatically when in this directory or subdirectories"
```

For personal projects, repeat in a different directory with a different token.

### Running git-disjoint

Both invocation methods work identically:

```bash
# Direct invocation
git-disjoint --base main

# Git subcommand syntax
git disjoint --base main
```

Help and version commands don't require Touch ID:

```bash
git-disjoint --help     # No auth prompt
git-disjoint --version  # No auth prompt
```

## How It Works

### Command Flow

```
User types: git disjoint --base main
     ↓
Git finds: git-disjoint wrapper in PATH
     ↓
Wrapper calls: op plugin run -- git-disjoint-unwrapped
     ↓
1Password:
  - Prompts for Touch ID
  - Reads ~/.op/config/plugins/gitdisjoint.json
  - Looks up token based on current directory
  - Retrieves token from vault
     ↓
Plugin executes: git-disjoint-unwrapped with GITHUB_TOKEN set
     ↓
git-disjoint runs with authenticated token
```

### Directory-Specific Tokens

The 1Password CLI maintains credential mappings in:

```
~/.op/config/plugins/git-disjoint.json
```

This file maps directory paths to 1Password item references. When you run the command, 1Password CLI:

1. Checks if current directory has a specific credential configured
2. Falls back to global credential if no directory-specific one exists
3. Prompts for Touch ID
4. Retrieves the token and injects it as `GITHUB_TOKEN`

## Maintenance

### Updating Plugin Code

1. Modify `pkgs/op-plugin-git-disjoint/main.go`
2. Run `nix-shell -p go --run "go mod tidy"` if dependencies changed
3. Update `vendorHash` in `default.nix` if needed:
   ```bash
   cd pkgs/op-plugin-git-disjoint
   nix build --impure --expr '(import <nixpkgs> {}).callPackage ./default.nix {}' 2>&1 | grep "got:"
   # Update vendorHash with the hash from error message
   ```
4. Rebuild: `darwin-rebuild switch --flake .`

## References

- [1Password Shell Plugins Documentation](https://developer.1password.com/docs/cli/shell-plugins/)
- [1Password CLI Reference](https://developer.1password.com/docs/cli/)
- [git-disjoint Repository](https://github.com/ericcrosson/git-disjoint)
- [Plugin Implementation Plan](../../2025-12-plan.md)
