#!/bin/bash

# https://bbs.archlinux.org/viewtopic.php?id=34832

# pacman -Q | awk '{ print $1 }' > installed.tmp
# for i in $(pacman -Qm | awk '{ print $1 }'); do
#     cat installed.tmp | sed "s/^$i\$//;" > installed.tmp.1
#     mv installed.tmp.1 installed.tmp
# done
# pacman -S --noconfirm `cat installed.tmp`
# rm installed.tmp

# i think that'll do the trick... just make sure you backup your /etc before hand, it might overwrite stuff.
# \\ archlinux on a XPS M1530 //
# kano
# From: Michigan
# Registered: 2007-05-04
# Posts: 185






# kano wrote:
# 
#     #!/bin/bash
# 
#     pacman -Q | awk '{ print $1 }' > installed.tmp
#     for i in $(pacman -Qm | awk '{ print $1 }'); do
#         cat installed.tmp | sed "s/^$i\$//;" > installed.tmp.1
#         mv installed.tmp.1 installed.tmp
#     done
#     pacman -S --noconfirm `cat installed.tmp`
#     rm installed.tmp
# 
# That could be simplified with the 'comm' command:
# 
# #!/bin/bash
# 
# pacman -Q | awk '{ print $1 }' | sort > /tmp/pacman-installed.tmp
# pacman -Qm | awk '{ print $1 }' | sort | comm -3 /tmp/pacman-installed.tmp - > /tmp/pacman-to-reinstall.tmp
# 
# pacman -S --noconfirm $(cat /tmp/pacman-to-reinstall.tmp)
# rm /tmp/pacman-{to-reinstall,installed}.tmp
# 
# That should do the same thing... I think.
# 
# Last edited by Cerebral (2007-07-04 01:51:10)
# 






# response to this four year old thread:
# pacman -S $(pacman -Qeq) --noconfirm


# unbased mod:
# 
# Moderator comment:
# swiftscythe,
# You do realize you are responding to a four year old thread?  Don't worry, we have all done it -- just remember the policy though.
# Closing
# 
