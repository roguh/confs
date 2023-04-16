alias v vim

if status is-interactive
  echo Mater artium necessitas.

  if command -v starship > /dev/null
    starship init fish | source
  end
end
