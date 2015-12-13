#!/bin/bash

tmpbg="/tmp/tmpbg.png"

scrot "$tmpbg"
convert "$tmpbg" -scale 10% -scale 1000% -fill black -colorize 25% "$tmpbg"

i3lock -neui "$tmpbg"
