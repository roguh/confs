alias tryalias "alias \!:1 \!:2"
set ALIASES = $HOME/.aliases
if ( -f $ALIASES && -o $ALIASES ) source $ALIASES 
