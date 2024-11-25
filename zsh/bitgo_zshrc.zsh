eval "$(keychain --eval --agents ssh --quiet id_rsa)"
eval "$(keychain --eval --agents ssh --quiet id_rsa_personal)"

aider() {
  AWS_PROFILE=dev ~/workspace/BitGo/aider/aider/bin/aider "$@"
}

cat <<EOF
The beginning of love is to let those we love be perfectly themselves,
and not to twist them to fit our own image.
Otherwise we love only the reflection of ourselves we find in them.
EOF
