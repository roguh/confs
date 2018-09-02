function tryalias
    # Make sure command is valid
    # If so, set the alias
    for cmd in $argv[2..-2]
        if command -v (string split " " $cmd)[1] > /dev/null
            alias $argv[1]="$cmd"
            return 0
        end
    end

    # Define as last element if all others failed
    alias $argv[1]=$argv[-1]
end

