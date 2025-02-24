#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

log() {
  echo "[$(date -Iseconds)] $*" >&2
}

check_version_only_changes() {
  local pr_number="$1"
  local changed_files

  # Get list of changed files
  changed_files=$(gh pr diff "$pr_number" --name-only --repo "$owner/$repo")

  # Check if all changed files are yaml files in services/
  while IFS= read -r file; do
    if [[ ! "$file" =~ ^services/.*\.ya?ml$ ]]; then
      log "File $file is not a YAML file in services/"
      return 1
    fi
  done <<< "$changed_files"

  # For each changed file, check if only the version changed
  while IFS= read -r file; do
    log "Checking diff for $file"
    
    # Get the diff for this file
    local diff
    diff=$(gh pr diff "$pr_number" --patch --repo "$owner/$repo")
    
    # Count number of chunks in diff
    local chunk_count
    chunk_count=$(echo "$diff" | grep -c "^@@" || true)
    log "  Found $chunk_count diff chunks"
    
    # If more than one chunk changed, it's not just version
    if [ "$chunk_count" -gt 1 ]; then
      log "  Too many diff chunks"
      return 1
    fi
    
    # Check if the change includes version field
    if ! echo "$diff" | grep -q '^[+-][[:space:]]*version:[[:space:]]*[0-9.]*$'; then
      return 1
    fi
    
    # Count number of added/removed lines (excluding chunk header)
    local changes
    changes=$(echo "$diff" | grep -c '^[-+][^-+]' || true)
    log "  Found $changes changed lines"
    if [ "$changes" -ne 2 ]; then  # One removal, one addition
      log "  Wrong number of changes"
      return 1
    fi
  done <<< "$changed_files"

  log "All checks passed - this is a version-only change"
  return 0
}

readonly owner="BitGo"
readonly repo="api-docs"

log "Starting version-bump auto-merge script"

# Get open PRs
# shellcheck disable=SC2016
prs=$(gh api graphql -f query='
  query($owner: String!, $repo: String!) {
    repository(owner: $owner, name: $repo) {
      pullRequests(first: 100, states: OPEN) {
        nodes {
          number
          title
          autoMergeRequest { enabledAt }
          reviews(first: 100) {
            nodes {
              author { login }
              state
            }
          }
        }
      }
    }
  }' \
  -F owner="$owner" \
  -F repo="$repo" \
  | jq '.data.repository.pullRequests.nodes[]')

echo "$prs" | jq -c '.' | while read -r pr; do
  number=$(jq -r '.number' <<< "$pr")
  needs_merge=$(jq -r '.autoMergeRequest == null' <<< "$pr")
  needs_approval=$(jq -r '([.reviews.nodes[] | select(.author.login == "ericcrosson-bitgo" and .state == "APPROVED")] | length == 0)' <<< "$pr")
  
  log "Checking PR #$number"
  
  if check_version_only_changes "$number"; then
    if [[ "$needs_approval" == "true" ]]; then
      log "Approving $owner/$repo#$number"
      gh pr review --repo "$owner/$repo" "$number" --approve --body "Approving version bump"
    fi
    
    if [[ "$needs_merge" == "true" ]]; then
      log "Enabling auto-merge for $owner/$repo#$number"
      gh pr merge --repo "$owner/$repo" "$number" --auto --merge
    fi
  else
    log "PR #$number contains changes other than version bumps"
  fi
done

log "Completed version-bump auto-merge script"
