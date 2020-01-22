function fish_greeting
	echo Welcome to fish, the FRIENDLY INTERACTIVE SHELL. 🐠
  echo Type `sl` for instructions on how to use fish. 🐠
  echo 🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠🐠
  echo
  echo
  echo
  
  if [ "$PWD" = "$HOME" ]
    screenfetch
    
    echo
    echo
    
    echo (curl https://raw.githubusercontent.com/asdf-vm/asdf/master/ballad-of-asdf.md)
    
    echo
    echo
  end
  
  echo (hostname) '>' ls "$PWD"
  if type exa > /dev/null 2>>&1
    exa "$PWD"
  else
    ls "$PWD"
  end
  
  echo
  echo
  
  if type fortune > /dev/null 2>>&1
    fortune
  else
    echo FOTD: Keep your house clean.
  end
end
