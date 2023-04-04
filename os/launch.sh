#!/bin/sh

compton &

xss-lock -- slock &

exec dbus-launch --exit-with-session emacs -mm --debug-init
