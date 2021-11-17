# Fish git prompt
set __fish_git_prompt_showdirtystate "yes"
set __fish_git_prompt_showstashstate "yes"
set __fish_git_prompt_showuntrackedfiles "yes"
set __fish_git_prompt_showupstream "yes"
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

# Status Chars
set __fish_git_prompt_char_dirtystate "*"
set __fish_git_prompt_char_stagedstate ">"
set __fish_git_prompt_char_untrackedfiles "!"
set __fish_git_prompt_char_stashstate "s"
set __fish_git_prompt_char_upstream_ahead "+"
set __fish_git_prompt_char_upstream_behind "-"


function fish_prompt
    set last_status $status

    if [ (whoami) = "root" ]
        set end \#
    else
        set end \$
    end

    set status_message ""
    if test ! $last_status -eq 0
        set status_message " [ $last_status ]"
    end

    function clean
        set_color -b normal
        set_color brwhite
    end

    function clean_and_space
      clean
      printf " "
    end

    set SHELL_TYPE ([ -n "$SSH_CLIENT" ] && echo SSH || echo)
    switch $hostname$SHELL_TYPE
      case '*T580*'
        set USER_AND_HOST_COLOR brcyan
      case '*flex*'
        set USER_AND_HOST_COLOR bryellow
      case 'raspberrypi' '*SSH*'
        set USER_AND_HOST_COLOR brred
      case '*'
        set USER_AND_HOST_COLOR brwhite
    end

    set_color --italics -b $USER_AND_HOST_COLOR
    set_color black
    printf "%s" (whoami)@(hostname)

    if [ -n "$SHELL_TYPE" ]
      printf " %s" "$SHELL_TYPE"
    end

    clean_and_space
    set_color --dim
    printf "%s" (date +%H:%M:%S)

    clean_and_space
    set_color --bold
    printf "%s" (prompt_pwd)

    clean
    printf "%s" (__fish_git_prompt)

    clean
    printf "%s" $status_message

    clean_and_space
    printf "%s" $end

    clean_and_space
end
