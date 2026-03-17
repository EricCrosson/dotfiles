# Bootstrap Guide

Step-by-step instructions for activating this config on a factory-reset Mac.

## Prerequisites (manual, one-time)

1. Create macOS user `ericcrosson`
2. Install Xcode Command Line Tools:
   ```bash
   xcode-select --install
   ```
3. Install Nix (Determinate Systems installer):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
   ```
4. Install Homebrew:
   ```bash
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
   ```
5. Install 1Password.app from [1password.com](https://1password.com/downloads/mac/) — after first activation it will be managed by Homebrew
6. Import GPG keys into `~/.gnupg` (required for sops secret decryption):
   ```bash
   gpg --import <your-key-export.asc>
   ```
7. Write a temporary SSH config so the private flake inputs can be resolved:
   ```
   # ~/.ssh/config (temporary — will be overwritten by activation)
   Host github.com-bitgo
     HostName github.com
     IdentityAgent ~/.gnupg/S.gpg-agent.ssh  # if GPG keys imported (step 6)
     # IdentityFile ~/.ssh/id_ed25519        # alternative: bare key file
   ```
   The `IdentityAgent` path uses GPG agent SSH support (the real auth path on this
   machine). If you haven't yet imported GPG keys, use `IdentityFile` instead with
   a key authorized on the BitGo GitHub org.
8. Clone this repo:
   ```bash
   git clone git@github.com:EricCrosson/dotfiles.git ~/workspace/EricCrosson/dotfiles
   cd ~/workspace/EricCrosson/dotfiles
   ```

## First activation

### Option A — full (requires steps 5–7 above)

```bash
darwin-rebuild switch --flake .#MBP-0954
```

### Option B — stubbed private inputs (skips steps 6–7; no work tools)

Use this when you haven't yet provisioned GPG keys or the BitGo SSH key. It
replaces the five private flake inputs with empty stubs so evaluation succeeds.
Sops secrets are automatically disabled when GPG keys are absent.

```bash
nix run nix-darwin -- switch --flake .#MBP-0954 \
  --override-input aws-console-bitgo path:./stubs/private-input-stub \
  --override-input aws-saml-bitgo    path:./stubs/private-input-stub \
  --override-input atlas             path:./stubs/private-input-stub \
  --override-input cortex            path:./stubs/private-input-stub \
  --override-input gh-agent          path:./stubs/private-input-stub
```

Once GPG keys and the BitGo SSH key are in place, complete steps 5–7 and
re-run Option A.

## Known bootstrap dependencies

| Dependency                   | Why                                                                                            | Risk if missing                                                                                   |
| ---------------------------- | ---------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| Homebrew                     | nix-darwin Homebrew module manages casks and brews                                             | Prints warning and skips; casks/brews not installed                                               |
| 1Password.app                | git signing (`profiles/eric/modules/git.nix`) and SSH agent (`profiles/bitgo/modules/ssh.nix`) | Git commits unsigned; SSH agent unavailable                                                       |
| GPG keys in `~/.gnupg`       | sops-nix decrypts secrets via GPG host key listed in `.sops.yaml`                              | Sops secrets skipped; AWS/Bedrock tools non-functional until GPG keys imported and config rebuilt |
| `github.com-bitgo` SSH alias | Five private flake inputs use `git+ssh://git@github.com-bitgo/...`                             | Flake evaluation fails without `--override-input` stubs                                           |

## Known hardware-specific config

- **AeroSpace monitor assignments** (`profiles/eric/modules/aerospace.nix`) reference specific Dell monitor model strings — non-fatal if different monitors are attached.
- **Hostname** must be `MBP-0954` or specified explicitly with `--flake .#MBP-0954`.
- **`/opt/homebrew`** paths are hardcoded — standard for Apple Silicon; amd64 Macs use `/usr/local/homebrew`.
