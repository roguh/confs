#!/bin/sh
# THEME=base16-greenscreen
# THEME=base16-zenburn

# warm
# THEME=sexy-sweetlove

# cool
# THEME=sexy-navy-and-ivory
THEME=base16-atelier-sulphurpool
# THEME=sexy-gotham
## metallic
# THEME=base16-black-metal-nile
# THEME=base16-black-metal-burzum
wal --saturate 0.2 -o theme-post.sh --theme ${THEME:-random}
