#!/bin/sh

xmodmap ~/.emacs.d/os/Xmodmap

TOUCHPADID=$(/usr/bin/xinput list | grep -i touchpad | grep -o "id=[0-9][0-9]" | cut -d "=" -f2)
MOUSEID=$(/usr/bin/xinput list | grep -i mouse | grep -o "id=[0-9][0-9]" | cut -d "=" -f2)
/usr/bin/xinput set-prop $TOUCHPADID "libinput Natural Scrolling Enabled" 1
/usr/bin/xinput set-prop $MOUSEID "libinput Natural Scrolling Enabled" 1

exit 0
