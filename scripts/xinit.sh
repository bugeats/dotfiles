#!/usr/bin/bash

# swap caps lock with escape on native keyboard only
# setxkbmap -device 'AT Translated Set 2 keyboard' -option caps:swapescape

# trackpad and mouse natural scrolling (reverse)
xinput set-prop 'ETPS/2 Elantech Touchpad' 'libinput Natural Scrolling Enabled' 1
xinput set-prop 'PixArt Microsoft USB Optical Mouse' 'libinput Natural Scrolling Enabled' 1

# trackpad push-to-click triggers button 1
xinput set-button-map 'ETPS/2 Elantech Touchpad' 1 1 3 4 5 6 7

# set custom background image
feh --bg-tile ~/.config/assets/xbg.png
