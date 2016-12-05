#!/bin/sh
KEYMAP=$1
FW=7
LH=16
X=$2
W=$4
KEYCOLOR=$6
CMDCOLOR=$7
COLS=$(($W / $FW))

INFO=$(awk -v cmdcolor=$CMDCOLOR -v keycolor=$KEYCOLOR \
           '/^'"$KEYMAP"'/,/^\s*\]$/ {
                # get the key entry and any following comment.
                split($0, fields, "--", seps)
                match($fields[1], /^.*\(\"(.*)\", *(.+)\).*/, cmd)

                # remove any leading spaces
                gsub(/^[ \t]/, "", cmd[2])
                gsub(/^[ \t]/, "", fields[2])

                # skip any empty records.
                if (length(cmd[1]) > 0){
                    # if there is a comment use that for the description.
                    if (length(f[2]) > 0) {
                            desc=fields[2]
                        } else {
                            desc=cmd[2]
                    }
                    printf ("^fg(%s)%6s ^fg(%s)%s\n", keycolor, cmd[1], cmdcolor, desc)
                }
            }' \
           ~/.xmonad/xmonad.hs \
              | column -c $COLS \
              | expand | sed 's/^/    /')

echo "$INFO"
N_LINES=$(wc -l <<< "$INFO")
Y=$(($3 + $5 - ($LH * ($N_LINES+3))))
sleep 1
# $KEYMAP ($2 , $3 , $4 , $5, $LH, $X, $Y, $W, $N_LINES, $COLS
(echo "^fg($KEYCOLOR)$KEYMAP"
 echo ""
 echo "$INFO"
 echo ""
 cat) | dzen2 -l $(($N_LINES+2)) -h $LH -x $X -y $Y -w $W -e onstart=uncollapse
