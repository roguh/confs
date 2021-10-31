#!/bin/bash
read -rn1 MOUSE_CHAR < /dev/input/mice
echo $MOUSE_CHAR
