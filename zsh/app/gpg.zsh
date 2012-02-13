################################################################################
# Outputs which key was used to encrypt a file.
gpg_which () {
  file=$1
  key=$(_gpg_which_key $file)
  gpg --list-keys $key
}

################################################################################
# Helper function.
_gpg_which_key () {
  gpg --batch --decrypt --list-only --status-fd 1 $1 2> /dev/null |\
    awk '/ENC_TO/ {print $3}'
}
