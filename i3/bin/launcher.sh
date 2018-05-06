if type rofi >/dev/null 2>&1
then
    rofi -show run -modi run,window,keys -sidebar-mode -lines 40 -disable-history 
else
    dmenu
fi
