# Check for open PRs from dev-portal-updater that update API docs
# and automatically approve and merge them if they pass CI

set -o errexit
set -o nounset
set -o pipefail

echo "[$(date -Iseconds)] Starting auto-merge script"
trap 'echo "[$(date -Iseconds)] Script execution completed"' EXIT

BOT_NAME="dev-portal-updater[bot]"

# Get open PRs from the bot with matching title and their titles, sorted by PR number
process_api_doc_prs() {
  local prs
  prs="$(
    gh api \
      -H "Accept: application/vnd.github+json" \
      -H "X-GitHub-Api-Version: 2022-11-28" \
      "/repos/BitGo/dev-portal/pulls?state=open" \
      | jq --arg BOT "$BOT_NAME" \
          '[.[] | select(.user.login == $BOT and (.title | test("Update API docs for \\S+"))) | {number: .number, title: .title}] | sort_by(.number)[]'
  )"

  if [[ -z "$prs" ]]; then
    echo "No matching BitGo/dev-portal PRs found"
    return 0
  fi

  echo "$prs" | jq --raw-output '"\(.number)\t\(.title)"' | while IFS=$'\t' read -r pr_number pr_title; do
    echo "Processing BitGo/dev-portal#$pr_number"
    
    # Extract everything after "Update API docs for "
    local service_name
    service_name=${pr_title#"Update API docs for "}

    # Mark the PR as auto-mergeable
    gh pr merge --repo BitGo/dev-portal "$pr_number" --auto --merge

    # Approve the PR
    gh pr review --repo BitGo/dev-portal "$pr_number" --approve --body "Updates API docs for $service_name"
  done
}

# Get open PRs from the bot with matching title and their titles, sorted by PR number
process_api_changelog_prs() {
  local prs
  prs="$(
    gh api \
      -H "Accept: application/vnd.github+json" \
      -H "X-GitHub-Api-Version: 2022-11-28" \
      "/repos/BitGo/api-changelog/pulls?state=open" \
      | jq --arg BOT "$BOT_NAME" \
          '[.[] | select(.user.login == $BOT and .title == "Update API reference on BitGo Developer Portal") | {number: .number, title: .title}] | sort_by(.number)[]'
  )"

  if [[ -z "$prs" ]]; then
    echo "No matching BitGo/api-changelog PRs found"
    return 0
  fi

  echo "$prs" | jq --raw-output '"\(.number)\t\(.title)"' | while IFS=$'\t' read -r pr_number pr_title; do
    echo "Processing BitGo/api-changelog#$pr_number"

    # Mark the PR as auto-mergeable
    gh pr merge --repo BitGo/api-changelog "$pr_number" --auto --merge

    # Approve the PR
    gh pr review --repo BitGo/api-changelog "$pr_number" --approve --body "Updates API reference"
  done
}

# Execute the main functions
process_api_doc_prs
process_api_changelog_prs
