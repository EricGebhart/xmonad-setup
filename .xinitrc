# xinitrc for xmonad with a polybar

# Load resources
# with code stolen from /etc/X11/xinitrc

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

#xcompmgr -f -C -n -D 3 &
#xcompmgr -f &

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

#emacs --daemon


# Big pointer
xsetroot -cursor_name left_ptr &

# Set up an icon tray
#trayer --edge top --align right --SetDockType true --SetPartialStrut true \
    #       --expand true --width 10 --transparent true --tint 0x191970 --height 16 &

# Set the background color<
#xsetroot -solid midnightblue
feh --bg-scale /home/eric/Documents/Art/ocean_park_114.jpg

xscreensaver &

#gestures and virtual on screen keyboard
easystroke &
onboard &

redshift &

#if [ -x /usr/bin/nm-applet ] ; then
#    nm-applet & ## if using trayer add: --sm-disable &
#fi

nm-applet & ## if using trayer add: --sm-disable &

#/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
#blueberry-tray &

#xfce4-panel -d &
#xfce4-power-manager &
~/.config/polybar/launch.sh &

#xxrandr --setprovideroutputsource modesetting NVIDIA-0
#xrandr --auto
xrandr --fbmm 286x281

xcompmgr -c &

# Not needed with a display manager
# exec xmonad
