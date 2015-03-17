#!zsh
xrandr --output DisplayPort-0 --primary --auto \
       --output DVI-0 --auto --left-of DisplayPort-0 \
       --output DVI-1 --auto --right-of DisplayPort-0
