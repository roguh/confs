if [ "$TERM" = xterm ]
  set_global FISH_LOGO fish
else
  set_global_if_unset FISH_LOGO 🐠
end

set_global_if_unset OPENWEATHERMAP_API_KEY 0ffcf43848aee07273c99d1d8f5592c7
