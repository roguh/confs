function title_get_project
  git rev-parse 2> /dev/null
  if test "$status" = 0
    printf "%s |" (basename (git rev-parse --show-toplevel 2> /dev/null) "")
  end
  printf " "
end

function ssh_info
  if ! test "$SSH_TTY" = ""
    printf "%s" "ssh " (hostname)
  end
  printf " "
end

function fish_title_info
  printf "Fish Shell pid=%s" $fish_pid
end

function fish_title
  printf "🐟 %s\$ %s %s %s" (ssh_info) (title_get_project) (pwd) (fish_title_info)
end
