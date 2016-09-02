#!/usr/bin/env bash

# Use caps lock as extra ctrl

# For Unity
dconf write /org/gnome/desktop/input-sources/xkb-options "['ctrl:nocaps']"

# For LXDE
setxkbmap -layout "$(setxkbmap -print | awk -F + '/xkb_symbols/ {print $2}')" -option ctrl:nocaps
