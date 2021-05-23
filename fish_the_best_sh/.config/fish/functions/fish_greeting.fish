function fish_greeting
  echo Welcome to $FISH_LOGO, the FRIENDLY INTERACTIVE SHELL.
  echo Type `sl` for instructions on how to use $FISH_LOGO.
  echo $FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO$FISH_LOGO
  echo
  
  if [ "$PWD" = "$HOME" ]

    uname -a

    if type screenfetch-cached > /dev/null 2>&1
      screenfetch-cached
    else if type screenfetch > /dev/null 2>&1
      screenfetch
    end
    
    echo

    # echo (curl https://raw.githubusercontent.com/asdf-vm/asdf/master/ballad-of-asdf.md)
  else
    echo (hostname) '>' ls "$PWD"
    if type exa > /dev/null 2>&1
      exa "$PWD"
    else
      ls "$PWD"
    end
  end
  
  echo

  uptime

  echo
  
  if type fortune > /dev/null 2>&1
    fortune
  else
    echo FOTD: Create something beautiful.
  end
end
