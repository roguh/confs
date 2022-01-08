function fish_greeting
  echo Welcome to $FISH_LOGO, the FRIENDLY INTERACTIVE SHELL.
  echo $FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO

  echo

  echo "Running fish in $HOSTNAME_SUMMARY."
  
  uptime

  echo
  
  if [ "$PWD" = "$HOME" ]

    uname -a

    if command -v neofetch > /dev/null 2>&1
      if command -v neofetch-cached > /dev/null 2>&1
        neofetch-cached
      else
        neofetch
      end
    else if command -v screenfetch > /dev/null 2>&1
      if command -v screenfetch-cached > /dev/null 2>&1
        screenfetch-cached
      else
        screenfetch
      end
    end
    
    echo

    # echo (curl https://raw.githubusercontent.com/asdf-vm/asdf/master/ballad-of-asdf.md)
  else
    true
    fish_prompt
    echo
    if command -v exa > /dev/null 2>&1
      exa "$PWD"
    else
      ls "$PWD"
    end
  end
  
  echo

  if command -v fortune > /dev/null 2>&1
    fortune
  else
    echo FOTD: Create something beautiful.
  end
end
