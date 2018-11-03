remote_id=$(
    xinput list |
    sed -n 's/.*GASIA.*id=\([0-9]*\).*keyboard.*/\1/p'
)

echo "butts"
echo $remote_id

[ "$remote_id" ] || exit

# mkdir -p /tmp/xkb/symbols
# cat >/tmp/xkb/symbols/custom <<\EOF
# xkb_symbols "remote" {
#     key <KP5>  { [ KP_Right, KP_6, U2192, U21D2 ]       };
#     key <I129> { [ KP_Down, KP_2, U2193, U21D3 ]       };
#     key <AD12> { [ KP_Up, KP_8, U2191, U21D1 ]  };
#     key <LFSH> { [ Control_L ]        };
# };
# EOF
#
# setxkbmap -device $remote_id -print | sed 's/\(xkb_symbols.*\)"/\1+custom(remote)"/' | xkbcomp -I/tmp/xkb -i $remote_id -synch - $DISPLAY 2>/dev/null
