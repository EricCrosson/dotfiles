eval "$(keychain --eval --agents ssh --quiet id_rsa)"
eval "$(keychain --eval --agents ssh --quiet id_rsa_personal)"

aider() {
  AWS_PROFILE=dev \
    uvx \
      --python 3.9 \
      --from git+ssh://git@github.com/BitGo/aider \
      aider "$@"
}
