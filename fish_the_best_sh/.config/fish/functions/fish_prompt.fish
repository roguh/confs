# Fish git prompt
set __fish_git_prompt_showdirtystate "yes"
set __fish_git_prompt_showstashstate "yes"
set __fish_git_prompt_showuntrackedfiles "yes"
set __fish_git_prompt_showupstream "yes"
set __fish_git_prompt_color_branch bryellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind brred

# Status Chars
set __fish_git_prompt_char_dirtystate "*"
set __fish_git_prompt_char_stagedstate ">"
set __fish_git_prompt_char_untrackedfiles "!"
set __fish_git_prompt_char_stashstate "s"
set __fish_git_prompt_char_upstream_ahead "+"
set __fish_git_prompt_char_upstream_behind "-"

function clean
    set_color -b normal
    set_color brwhite
end

function fish_prompt
    set last_status $status

    if [ "$USER" = "root" ]
        set end \#
    else
        set end \$
    end

    set status_message ""
    if test ! $last_status -eq 0
        set status_message "[ $last_status ] "
    end

    set_color --italics -b $USER_AND_HOST_COLOR
    set_color black
    printf "%s" "$USER@$hostname$SHELL_TYPE"

    clean
    set_color --dim
    printf " %s" (date +%H:%M:%S)

    clean
    set_color --bold
    printf " %s%s%s%s " (prompt_pwd) (__fish_git_prompt ; echo ' ') $status_message $end
end
