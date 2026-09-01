# Bootstrap Guide

Step-by-step instructions for activating this config on a factory-reset Mac or Athens Linux host.

## Prerequisites (manual, one-time)

1. On Darwin only, create macOS user `ericcrosson`
2. On Darwin only, install Xcode Command Line Tools:

   ```bash
   xcode-select --install
   ```

3. Install Nix (Determinate Systems installer):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
   ```
4. On Darwin only, install Homebrew:
   ```bash
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
   ```
5. On Darwin only, install 1Password.app from [1password.com](https://1password.com/downloads/mac/) — after first activation it will be managed by Homebrew. Athens receives the Linux GUI from the shared `eric` profile; enroll it manually as described below.
6. On Darwin only, import GPG keys into `~/.gnupg` (required for sops secret decryption):
   ```bash
   gpg --import <your-key-export.asc>
   ```
7. On Darwin only, write a temporary SSH config so the private flake inputs can be resolved:
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

### Athens NixOS installation — no BitGo credentials required

Athens uses the public flake and a disko partition layout. From a NixOS
installer environment, partition and mount the target disk first:

```bash
nix run github:nix-community/disko -- --mode disko ./hosts/athens/disko.nix
```

Then install into the mounted target (`/mnt`). The `--root /mnt` argument is
required so the bootloader mount check sees the ESP at `/mnt/boot`:

```bash
nixos-install --root /mnt --flake .#athens
```

All Athens configuration builds should run on Athens itself. The public flake
contains no private SSH inputs.

### Athens Linux 1Password enrollment (manual, after activation)

The Linux `_1password-gui` package is installed by `profiles/eric/default.nix`; do not add a second system-level installation. Account enrollment and SSH-agent enablement are intentionally manual:

1. Complete the Athens installation and first activation, then sign in to the GNOME desktop as `eric` and open 1Password from the applications menu.
2. Sign in to the 1Password account that contains the existing SSH key used for Git signing. Verify that it is an `SSH Key` item with the configured public key and fingerprint. Do not generate a replacement key for this setup. Never put its private key in this repository, Nix expressions, or `secrets/main.yaml`.
3. In 1Password, open **Settings → Developer** and enable **Use the SSH agent**. Keep the default Linux socket location, `~/.1password/agent.sock`.
4. Start a new shell (or log out and in) so Home Manager's Linux session variable takes effect. Confirm that the configured socket is a Unix socket:
   ```bash
   test -S "$HOME/.1password/agent.sock" && echo "1Password SSH agent socket is present"
   test "$SSH_AUTH_SOCK" = "$HOME/.1password/agent.sock" && echo "SSH_AUTH_SOCK is configured"
   ssh-add -L
   ssh-add -L | ssh-keygen -lf - -E sha256
   ```
   `ssh-add -L` should list the public keys exposed by the agent. The configured signing key must have this fingerprint (SHA-256): `SHA256:MekH87sUQCkw56NXEKCYUvK54UNFd1xcTWw2PNuRW7U`.
5. To check a public key without exposing a private key, compare its fingerprint with:
   ```bash
   printf '%s\n' 'ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM9idpkqe6Rk8pLXKhqCfL6Bc3jGMHdfDj06C0AU5P3J eric.s.crosson@utexas.edu' | ssh-keygen -lf - -E sha256
   ```

Locking 1Password keeps the socket path but blocks agent operations; Git signing and SSH authentication will fail or be refused until the app is unlocked. Unlock 1Password and retry the operation—do not replace the socket path or copy a private key into `~/.ssh`.

This setup uses **SSH**, not GPG, for Git signing: Git is configured with `gpg.format=ssh`, the public `user.signingKey` above, and the repository's `allowed_signers` file; its signer is 1Password's `op-ssh-sign`. `SSH_AUTH_SOCK` points to the 1Password SSH agent. GPG keys in `~/.gnupg` remain a separate prerequisite for sops-nix secret decryption and are not used for these Git signatures.

### MBP-0954 Darwin activation

The Darwin configuration lives in `private/flake.nix` because it uses the four
private BitGo inputs. With the BitGo SSH alias and GPG keys configured:

```bash
darwin-rebuild switch --flake ./private#MBP-0954
```

### Stubbed Darwin activation

Use this when the BitGo SSH key or GPG keys are not yet available. It replaces
the four private inputs with empty local stubs; work tools and sops secrets are
not functional in this mode:

```bash
nix run nix-darwin -- switch --flake ./private#MBP-0954 \
  --override-input aws-console-bitgo path:./stubs/private-input-stub \
  --override-input aws-saml-bitgo    path:./stubs/private-input-stub \
  --override-input gh-endorse        path:./stubs/private-input-stub \
  --override-input gh-gantt          path:./stubs/private-input-stub
```

The four private inputs are `aws-console-bitgo`, `aws-saml-bitgo`, `gh-endorse`,
and `gh-gantt`.

## Known bootstrap dependencies

| Dependency                                | Why                                                                                                                                   | Risk if missing                                                                                   |
| ----------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| Homebrew                                  | nix-darwin Homebrew module manages casks and brews                                                                                    | Prints warning and skips; casks/brews not installed                                               |
| 1Password (Athens Linux GUI / Darwin app) | Linux `_1password-gui` is installed by the `eric` profile; Darwin Homebrew manages the app; both provide SSH-agent-backed Git signing | Git commits unsigned; SSH authentication and signing unavailable until enrollment and unlock      |
| GPG keys in `~/.gnupg`                    | sops-nix decrypts secrets via GPG host key listed in `.sops.yaml`                                                                     | Sops secrets skipped; AWS/Bedrock tools non-functional until GPG keys imported and config rebuilt |
| `github.com-bitgo` SSH alias              | Four private flake inputs use `git+ssh://git@github.com-bitgo/...`                                                                    | Private Darwin flake cannot evaluate; Athens remains unaffected                                   |

## Known hardware-specific config

- **AeroSpace monitor assignments** (`profiles/eric/modules/aerospace.nix`) reference specific Dell monitor model strings — non-fatal if different monitors are attached.
- **Hostname** must be `MBP-0954` or specified explicitly with `--flake ./private#MBP-0954`.
- **`/opt/homebrew`** paths are hardcoded — standard for Apple Silicon; amd64 Macs use `/usr/local/homebrew`.
