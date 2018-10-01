#!/bin/sh
# THEME=base16-greenscreen
# THEME=base16-zenburn

# warm
# THEME=sexy-sweetlove
# THEME=base16-embers
# THEME=dkeg-novmbr
# THEME=dkeg-bark
## metallic
# THEME=base16-black-metal-nile
# THEME=dkeg-urban

# cool
THEME=base16-flat
# THEME=base16-atelier-lakeside
# THEME=hybrid-material
# THEME=sexy-navy-and-ivory
# THEME=base16-atelier-sulphurpool
# THEME=sexy-gotham
# THEME=sexy-jasonwryan
# THEME=dkeg-prevail
## metallic
# THEME=base16-black-metal-burzum
# THEME=dkeg-blumune
wal --saturate 0.2 -o theme-post.sh --theme ${THEME:-random}
