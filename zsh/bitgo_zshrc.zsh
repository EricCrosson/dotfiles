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

alias cmd="llm cmd"
alias fabric="fabric --config ~/.config/fabric/config.yaml"
