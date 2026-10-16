#!/usr/bin/env bash
# Update one flake input on a fresh branch off origin/master, open its PR if
# needed, and enable auto-merge.
#
# Usage: update-flake-input.sh root <input>  # flake.lock (+ private/flake.lock mirror)
#        update-flake-input.sh dev <input>   # nix/flake.lock (dev shell)
#
# GH_TOKEN must be a PAT: merges enabled with GITHUB_TOKEN never trigger the
# post-merge push workflow.
set -euo pipefail

scope=$1
input=$2

git checkout --quiet --force --detach origin/master

case "$scope" in
  root)
    branch="update-flake-input-$input"
    title="Update $input flake input"

    # Public lock: every enumerated input is a direct input here.
    if jq -e --arg i "$input" '.nodes.root.inputs | has($i)' flake.lock > /dev/null; then
      nix flake lock --update-input "$input" \
        --commit-lock-file \
        --commit-lockfile-summary "chore(nix): update $input flake input"
    else
      echo "'$input' is not a public flake input; skipped the public lock"
    fi

    # Private lock: followed inputs re-resolve through 'public'
    # (path:.., reading the freshly updated public lock from disk);
    # direct non-ssh inputs update in place; git+ssh inputs cannot be
    # fetched from Actions (no SSH keys or host aliases). Re-resolving
    # 'public' also heals drift in the other mirrors. One commit per
    # lock keeps the public and private pins on the same rev.
    kind=$(jq -r --arg i "$input" '
      .nodes as $nodes
      | (.nodes.root.inputs[$i] // empty)
      | if type == "array" then "follow"
        elif ($nodes[$i].locked.url // "" | test("^ssh://")) then "ssh"
        else "direct" end' private/flake.lock)
    case "$kind" in
      follow)
        (cd private && nix flake lock --update-input public \
          --commit-lock-file \
          --commit-lockfile-summary "chore(nix): update $input in private lock via public sync") ;;
      direct)
        (cd private && nix flake lock --update-input "$input" \
          --commit-lock-file \
          --commit-lockfile-summary "chore(nix): update $input in private lock") ;;
      *)
        echo "'$input' is not updatable in the private lock; skipped" ;;
    esac
    ;;
  dev)
    branch="update-nix-flake-input-$input"
    title="Update $input in nix/flake.nix"
    (cd nix && nix flake lock --update-input "$input" \
      --commit-lock-file \
      --commit-lockfile-summary "chore(nix): update $input in nix/flake.nix")
    ;;
  *)
    echo "unknown scope '$scope' (expected root or dev)" >&2
    exit 2
    ;;
esac

if git diff --quiet origin/master HEAD; then
  echo "No lock update needed for $input ($scope)"
  exit 0
fi

git push --force origin "HEAD:refs/heads/$branch"

body="${RUNNER_TEMP:-/tmp}/pr-body-$scope-$input.md"
{
  echo "Automated changes by the auto-update workflow."
  echo
  echo '```'
  git log --reverse --format='%s%n%n%b' origin/master..HEAD
  echo '```'
} > "$body"

# Capture the list before matching: `gh … | grep -q` under pipefail can fail
# with SIGPIPE when grep exits on the first match.
labels=$(gh label list --limit 500 --json name --jq '.[].name')
if ! grep -Fxq -- "$input" <<<"$labels"; then
  gh label create "$input"
fi

if [ "$(gh pr list --head "$branch" --state open --json number --jq length)" -eq 0 ]; then
  gh pr create --head "$branch" --base master \
    --title "$title" --body-file "$body" \
    --assignee EricCrosson \
    --label dependencies --label nix --label "$input"
fi
gh pr merge --auto --merge "$branch"
