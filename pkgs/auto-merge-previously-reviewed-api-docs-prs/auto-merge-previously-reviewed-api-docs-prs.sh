#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

log() {
  echo "[$(date -Iseconds)] $*" >&2
}

readonly bot_name="dev-portal-updater"

process_repository() {
  local owner="" repo="" title_pattern="" approve_message_template=""

  while [[ $# -gt 0 ]]; do
    case "$1" in
      --owner) owner="$2"; shift 2 ;;
      --repo) repo="$2"; shift 2 ;;
      --title-pattern) title_pattern="$2"; shift 2 ;;
      --approve-message-template) approve_message_template="$2"; shift 2 ;;
      *) echo "Unknown parameter: $1"; return 1 ;;
    esac
  done

  log "Processing $owner/$repo"

  # Get PRs and their status in one query
  local prs
  # shellcheck disable=SC2016
  prs="$(gh api graphql -f query='
    query($owner: String!, $repo: String!) {
      repository(owner: $owner, name: $repo) {
        pullRequests(first: 100, states: OPEN) {
          nodes {
            number
            title
            author { login }
            autoMergeRequest { enabledAt }
            commits(first: 20) {
              nodes {
                commit {
                  author {
                    user { login }
                  }
                }
              }
            }
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
    | jq --arg bot "$bot_name" \
         --arg pattern "$title_pattern" \
      '.data.repository.pullRequests.nodes[]
      | select(
          .author.login == $bot
          and (.title | test($pattern))
          and (
            .commits.nodes | length <= 15
          )
          and (
            [.commits.nodes[] | .commit.author.user.login] | all(. == $bot)
          )
        )
      | {
          number,
          title,
          service_name: (.title | capture("Update API docs for (?<name>.+)").name // ""),
          needs_merge: (.autoMergeRequest == null),
          needs_approval: ([.reviews.nodes[] | select(.author.login == "ericcrosson-bitgo" and .state == "APPROVED")] | length == 0)
        }'
  )"

  echo "$prs" | jq -c '.' | while read -r pr; do
    local number needs_merge needs_approval service_name approve_message
    number=$(jq -r '.number' <<< "$pr")
    needs_merge=$(jq -r '.needs_merge' <<< "$pr")
    needs_approval=$(jq -r '.needs_approval' <<< "$pr")
    service_name=$(jq -r '.service_name' <<< "$pr")
    # shellcheck disable=SC2059
    approve_message=$(printf "$approve_message_template" "$service_name")

    log "Processing $owner/$repo#$number"

    if [[ "$needs_approval" == "true" ]]; then
      log "Approving $owner/$repo#$number"
      gh pr review --repo "$owner/$repo" "$number" --approve --body "$approve_message"
    fi

    if [[ "$needs_merge" == "true" ]]; then
      log "Enabling auto-merge for $owner/$repo#$number"
      gh pr merge --repo "$owner/$repo" "$number" --auto --merge
    fi
  done
}

# Process each repository
log "Starting auto-merge script"

process_repository \
  --owner "BitGo" \
  --repo "dev-portal" \
  --title-pattern "^Update API docs for \\S+" \
  --approve-message-template "Updates API docs for %s"

process_repository \
  --owner "BitGo" \
  --repo "api-changelog" \
  --title-pattern "^Update API reference" \
  --approve-message-template "Updates API reference"

log "Completed auto-merge script"
