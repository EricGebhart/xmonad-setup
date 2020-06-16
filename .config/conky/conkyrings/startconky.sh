#!/bin/bash

sleep 5 #time (in s) for the DE to start; use ~20 for Gnome or KDE, less for Xfce/LXDE etc
conky -c ~/.Conky/rings & # the main conky with rings
sleep 5 #time for the main conky to start; needed so that the smaller ones draw above not below (probably can be lower, but we still have to wait 5s for the rings to avoid segfaults)
conky -c ~/.Conky/cpu &
conky -c ~/.Conky/mem &
conky -c ~/.Conky/notes &

# tilda #used to get a small desktop terminal (not in the screenshot); you need tilda installed (use your package manager), and the tilda config is included
