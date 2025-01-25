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
        --no-gitignore \
        --watch-files \
        "$@"
}

# To configure llm, I ran
# ```
# llm models default bedrock-claude-sonnet
# ```
alias llm="AWS_PROFILE=dev llm"

export PATH=$PATH:/Users/ericcrosson/workspace/danielmiessler/fabric
