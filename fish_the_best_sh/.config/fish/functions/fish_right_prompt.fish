function fish_right_prompt
  if command -v kubectx > /dev/null && ! string match -e '/src' "$PWD" > /dev/null
    set_color -b normal
    set_color normal
    set_color --italics 8df

    echo ⎈ (kubectx -c)

    set_color -b normal
    set_color normal
  end
end
