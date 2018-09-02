# Fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '+'
set __fish_git_prompt_char_upstream_behind '-'


function fish_prompt
    set last_status $status 

    if [ (whoami) = "root" ]
        set end "# "
    else
        set end "\$ "
    end

    set st ""
    if [ $last_status -ne 0 ]
        set st "[$last_status] "
    end

    function clean_space
        set_color -b normal
        set_color normal
        printf ' '
    end

    set_color --italics -b white
    set_color black
    printf '%s' (whoami)@(hostname) 

    clean_space

    set_color --dim
    printf '%s' (date +%H:%M:%S) 

    clean_space

    set_color --bold
    printf '%s' (prompt_pwd)

    clean_space
    printf '%s' (__fish_git_prompt) $st$end
end
