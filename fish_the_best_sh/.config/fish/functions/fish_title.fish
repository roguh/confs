function title_get_project
  git rev-parse 2> /dev/null
  if test "$status" = 0
    printf "%s |" (basename (git rev-parse --show-toplevel 2> /dev/null) "")
  end
end

function fish_title_info
  printf "Fish Shell pid=%s" $fish_pid
end

function fish_title
  printf "🐟 \$ %s %s %s" (title_get_project) (pwd) (fish_title_info)
end
