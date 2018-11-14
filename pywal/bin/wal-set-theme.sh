#!/bin/sh
# THEME=base16-greenscreen
# THEME=base16-zenburn

# light
# THEME=base16-atelier-savanna
# THEME= base16-github

# warm
# THEME=tempus_autumn #good
# THEME=sexy-sweetlove
# THEME=base16-embers
# THEME=dkeg-novmbr
# THEME=dkeg-bark
# THEME=dkeg-simplicity
## metallic
# THEME=base16-black-metal-nile
# THEME=dkeg-urban

# cool
# THEME="$HOME/sync/technical/roguh-base16-flat.json"
# THEME=dkeg-skigh #good with saturate=0.8
# THEME=sexy-thwump # pale
# THEME=base16-oceanicnext
# THEME=sexy-vacuous2
# THEME=base16-flat
# THEME=base16-onedark
# THEME=base16-atelier-lakeside
# THEME=hybrid-material
# THEME=sexy-navy-and-ivory
# THEME=base16-atelier-sulphurpool
# THEME=sexy-gotham
# THEME=sexy-jasonwryan
# THEME=dkeg-prevail
## metallic
THEME=base16-black-metal-burzum
# THEME=dkeg-blumune
# THEME=dkeg-owl #good
# generate oomox theme, also run theme-post.sh
wal -g --saturate 1.0 -o theme-post.sh --theme ${THEME:-random}
