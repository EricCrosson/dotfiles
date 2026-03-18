{writeShellScriptBin}:
# Intercept Claude Code's O24 SSH probe to prevent 1Password Touch ID prompt.
# O24 runs: ssh -T -o BatchMode=yes -o ConnectTimeout=2 -o StrictHostKeyChecking=yes git@github.com
# The probe's unique signature: `-T` flag with `git@github.com` as the final argument.
# Real git operations don't use `-T` (it disables PTY allocation).
writeShellScriptBin "ssh" ''
  # Check for the O24 probe signature: -T flag + git@github.com as last arg
  has_T=false
  last_arg=""
  for arg in "$@"; do
    if [[ "$arg" == "-T" ]]; then
      has_T=true
    fi
    last_arg="$arg"
  done

  if [[ "$has_T" == true && "$last_arg" == "git@github.com" ]]; then
    # O24 probe detected — fail immediately without contacting 1Password
    echo "Permission denied (publickey)." >&2
    exit 255
  fi

  # Not the probe — pass through to real SSH
  exec /usr/bin/ssh "$@"
''
