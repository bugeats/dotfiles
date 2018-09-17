#!/usr/bin/bash

# swap caps lock with escape on native keyboard only
# TODO use sed to get device id from name
# setxkbmap -device 'AT Translated Set 2 keyboard' -option caps:swapescape

# set laptop display under main display
xrandr --output eDP1 --pos 760x1400 --below DP1 --output DP1 --primary --above eDP1

# trackpad and mouse natural scrolling (reverse)
xinput set-prop 'ETPS/2 Elantech Touchpad' 'libinput Natural Scrolling Enabled' 1
xinput set-prop 'PixArt Microsoft USB Optical Mouse' 'libinput Natural Scrolling Enabled' 1
xinput set-prop 'PixArt Microsoft USB Optical Mouse' 'libinput Accel Speed' -0.5

# trackpad push-to-click triggers button 1
xinput set-button-map 'ETPS/2 Elantech Touchpad' 1 1 3 4 5 6 7

# set custom background image
feh --bg-tile ~/.config/assets/xbg.png
