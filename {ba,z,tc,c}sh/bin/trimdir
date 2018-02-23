#!/usr/bin/env python3

# 1. read stdin
# 2. split by '/'
# 3. replace all parent directory names with their 1st or 1st and 2nd characters
import sys
from string import punctuation as punc
from os.path import join, split


def simp(s):
    return s[0:2] if s[0:1] in punc else s[0:1]


p = split(sys.stdin.read())
r = p[1]

while p[0] not in ['', '/']:
    p = split(p[0])
    r = join(simp(p[1]), r)

print(r)
