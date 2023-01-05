#!/usr/bin/env python3
"""
1. read stdin
2. split by '/'
3. replace all parent directory names with their 1st or 1st and 2nd characters
"""

import sys
from os.path import expanduser, join, normcase, normpath, split
from string import punctuation as punc


def shorten_name(dirname):
    # type: (str) -> str
    return dirname[0:2] if dirname[0:1] in punc else dirname[0:1]


def trimdir(input_path):
    # type: (str) -> str

    normalized_path = normcase(normpath(input_path))

    # Replace home dir with ~
    home = expanduser("~")
    if normalized_path.startswith(home):
        normalized_path = normalized_path.replace(home, "~")

    # Shorten each component of the normalized_path
    dirname, basename = split(normalized_path)
    simplified_path = basename

    while dirname not in ["", "/"]:
        dirname, basename = split(dirname)
        simplified_path = join(shorten_name(basename), simplified_path)

    # Add leading / if it was removed
    if normalized_path.startswith("/"):
        simplified_path = "/" + simplified_path

    return simplified_path


if __name__ == "__main__":
    if len(sys.argv) > 1:
        for p in sys.argv[1:]:
            print(trimdir(p))
    else:
        print(trimdir(sys.stdin.read().rstrip()))
