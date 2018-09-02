function load_theme
    set theme $HOME/.cache/wal/sequences
    if test -e $theme
        cat $theme
    end
end

