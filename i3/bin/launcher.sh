if type rofi >/dev/null 2>&1
then
    rofi -combi-modi run,window -show combi -modi combi,window,keys -sidebar-mode -lines 40 -disable-history 
else
    dmenu
fi
