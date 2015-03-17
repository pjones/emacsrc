# stuff for every os
alias vared="IFS=\$'\n' vared"
alias bc='bc -q'
alias lc='fc -lnD -1'
alias h='history -fdD'
alias j='jobs -l'
alias f="find . -name"
alias g="egrep"

# ls command
if [[ $OSNAME == "FreeBSD" || $OSNAME == "Darwin" ]]; then
    alias ls='ls -G'
    alias l='ls -lFGh'
    alias ll='ls -lFGh'
    alias la='ls -lFGha'

elif [[ $OSNAME == "Linux"  || $OSNAME == "CYGWIN_NT-5.0" ]]; then
    alias ls='\ls --color=auto --group-directories-first'
    alias lsa='ls -A'
    alias l='\ls -lhF --color=auto --group-directories-first'
    alias ll='l'
    alias la='l -A'
    alias _df_awk="awk '{print \$2 \"\\t\" \$4 \"\\t\" \$5 \"\\t\" \$6}'"
    alias df="df -hP|_df_awk"
else
    alias l='ls -lF'
    alias ll='ls -lF'
    alias la='ls -lFa'

fi

# ps
if [[ $OSNAME == "FreeBSD" ]]; then
    alias p='ps -axwwopid,ppid,user,pcpu,vsz,rss,comm,args'
elif [[ $OSNAME == "Darwin" ]]; then
    alias p='ps -axwwopid,ppid,user,pcpu,vsz,rss,command'
elif [[ $OSNAME == "Linux"  || $OSNAME == "CYGWIN_NT-5.0" ]]; then
    alias p='ps -ewwopid,ppid,user,pcpu,vsz,rss,comm,args'
elif [[ $OSNAME == "AIX" || $OSNAME == "HP-UX" ]]; then
    alias p='ps -eopid,ppid,user,pcpu,vsz,comm,args'
else
    alias p='ps -eopid,ppid,user,pcpu,vsz,rss,comm,args'
fi

# ps | grep
alias pg="p|g"

# ldd
if [[ $OSNAME == "Darwin" ]]; then
    alias ldd='otool -L'
fi

# More complicated aliases that need to be functions.
dl () {dict "$@" | less}

# Just to shorten typing
alias tclock="tty-clock -c -C 4 -f '%b. %d, %Y'"
alias scclock='env TZ=America/New_York tty-clock -C 0 -c -f "South Carolina"'
alias iss_payload='chromium --app=http://www.ustream.tv/channel/iss-hdev-payload/pop-out'
alias rtm='chromium --app=https://www.rememberthemilk.com'
