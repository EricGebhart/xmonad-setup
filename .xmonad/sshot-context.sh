#!/bin/sh
CTX=~/screenshots/$(ls -1 ~/screenshots --hide current-context | dmenu)
mkdir -p "$CTX"
ln -Tsf "$CTX" ~/screenshots/current-context
