#!/usr/bin/env python3

# 1. read stdin
# 2. split by '/'
# 3. replace all parent directory names with their 1st or 1st and 2nd characters
import sys
from string import punctuation as punc
from os.path import join, split, expanduser, normpath

def simp(s):
    return s[0:2] if s[0:1] in punc else s[0:1]

def simplify_path(input_path=None):
    if input_path is None:
        input_path = sys.stdin.read().rstrip()

    path = normpath(input_path)
    home = expanduser('~')
    if path.startswith(home):
        path = path.replace(home, '~')
    p = split(path)
    r = p[1]

    while p[0] not in ['', '/']:
        p = split(p[0])
        r = join(simp(p[1]), r)

    if path.startswith('/'):
        r = '/' + r

    return r

if len(sys.argv) > 1:
    for p in sys.argv[1:]:
        print(simplify_path(p))
else:
    print(simplify_path())
