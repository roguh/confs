if [ "$TERM" = xterm ]
  set_global FISH_LOGO fish
else
  set_global_if_unset FISH_LOGO 🐠
end
