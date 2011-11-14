# script
script ()
{
    script_options='-a'
    [[ $OSNAME == "FreeBSD" ]] && script_options="$script_options -k"
    (export INSIDE_SCRIPT=yes; command script `eval echo $script_options` $*)
}
