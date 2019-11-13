function title_get_project
  git rev-parse 2> /dev/null
  if test "$status" = 0
    printf "%s |" (basename (git rev-parse --show-toplevel 2> /dev/null) "")
  end
end

function title_trimdir
  if type trimdir.py > /dev/null 2>&1
    trimdir.py (pwd)
  else
    pwd
  end
end

function fish_title
  printf "🐟 \$ %s %s" (title_get_project) (title_trimdir)
end
