# Keep unique
typeset -U ssh_hosts

# Suck in hostnames from known_hosts
if [ -r ~/.ssh/known_hosts ]; then
    ssh_hosts=(`awk -F'[:, ]' '{gsub(/\[|\]/, "", $1); print($1)}' < ~/.ssh/known_hosts` $hosts)
    zstyle ':completion:*' hosts $ssh_hosts
fi
