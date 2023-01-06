function title_get_project
  set PROJECTNAME (projectname.sh)
  if test "$status" = 0 && test "$PROJECTNAME" != ""
    printf "$PROJECTNAME |"
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
  printf "pid=%s" $fish_pid
end

function fish_title
  printf "🐟 %s %s%s %s" (ssh_info) (title_get_project) (pwd) (fish_title_info)
end
