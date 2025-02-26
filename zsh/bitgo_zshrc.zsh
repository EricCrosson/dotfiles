eval "$(keychain --eval --agents ssh --quiet id_rsa)"
eval "$(keychain --eval --agents ssh --quiet id_rsa_personal)"

aider() {
  AWS_PROFILE=dev \
  SMART_CD_GIT_STATUS=false \
  SMART_CD_LS=false \
    uvx \
      --python 3.9 \
      --from git+ssh://git@github.com/BitGo/aider \
      aider \
        "$@"
}

claude_code() {
  ANTHROPIC_MODEL="arn:aws:bedrock:us-west-2::foundation-model/anthropic.claude-3-7-sonnet-20250219-v1:0" \
  AWS_PROFILE=dev \
  AWS_REGION=us-west-2 \
  CLAUDE_CODE_USE_BEDROCK=1 \
  DISABLE_PROMPT_CACHING=1 \
    \npx @anthropic-ai/claude-code
}

alias cmd="llm cmd"
alias fabric="fabric --config ~/.config/fabric/config.yaml"
