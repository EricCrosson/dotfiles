# Check for open PRs from dev-portal-updater that update API docs
# and automatically approve and merge them if they pass CI

set -o errexit
set -o nounset
set -o pipefail

echo "[$(date -Iseconds)] Starting auto-merge script"
trap 'echo "[$(date -Iseconds)] Script execution completed"' EXIT

BOT_NAME="dev-portal-updater[bot]"

# Check PR status and take needed actions
process_pr() {
  local repo="$1"
  local pr_number="$2"
  local approve_message="$3"
  
  # Get PR details and reviews in separate API calls
  local pr_details reviews
  pr_details=$(gh api \
    -H "Accept: application/vnd.github+json" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    "/repos/$repo/pulls/$pr_number")

  reviews=$(gh api \
    -H "Accept: application/vnd.github+json" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    "/repos/$repo/pulls/$pr_number/reviews")

  # Combine the information
  pr_details=$(echo "$pr_details" | jq --argjson reviews "$reviews" '{
    auto_merge: (.auto_merge != null),
    reviews: [$reviews[] | select(.user.login == "ericcrosson-bitgo" and .state == "APPROVED")] | length
  }')

  local needs_merge=false
  local needs_approval=false

  # Check if PR needs auto-merge
  if [[ $(echo "$pr_details" | jq '.auto_merge') == "false" ]]; then
    needs_merge=true
  fi

  # Check if PR needs approval
  if [[ $(echo "$pr_details" | jq '.reviews') == "0" ]]; then
    needs_approval=true
  fi

  # Take needed actions
  if [[ "$needs_merge" == "true" ]]; then
    echo "Marking PR #$pr_number as auto-mergeable"
    gh pr merge --repo "$repo" "$pr_number" --auto --merge
  fi

  if [[ "$needs_approval" == "true" ]]; then
    echo "Approving PR #$pr_number"
    gh pr review --repo "$repo" "$pr_number" --approve --body "$approve_message"
  fi

  if [[ "$needs_merge" == "false" && "$needs_approval" == "false" ]]; then
    echo "PR #$pr_number already processed (auto-merge: yes, approved: yes)"
  fi
}

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

    process_pr "BitGo/dev-portal" "$pr_number" "Updates API docs for $service_name"
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

    process_pr "BitGo/api-changelog" "$pr_number" "Updates API reference"
  done
}

# Execute the main functions
process_api_doc_prs
process_api_changelog_prs
