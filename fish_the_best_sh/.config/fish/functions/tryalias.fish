function tryalias
    # Make sure command is valid
    # If so, set the alias
    for cmd in $argv[2..-2]
        if type string > /dev/null
            set binname (string split " " $cmd)[1]
        else if type string_split.py > /dev/null
            set binname (string_split.py " " $cmd 0)
        else
            set binname $cmd
        end
        if command -v $binname > /dev/null
            alias $argv[1]="$cmd"
            return 0
        end
    end

    # Define as last element if all others failed
    alias $argv[1]=$argv[-1]
end

