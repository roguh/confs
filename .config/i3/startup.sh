#!/bin/sh
# Start dropbox and emacs, if they exist.
{ type dropbox >/dev/null 2>&1 && dropbox start& } || echo >&2 "dropbox not found"
{ type emacs >/dev/null 2>&1 && emacs --daemon & } || echo >&2 "emacs not found"
