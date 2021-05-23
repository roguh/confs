#!/usr/bin/env python3
import sys

def help():
    return "USAGE: {} split_string string_to_split index".format(sys.argv[0])

if len(sys.argv) != 4:
    raise Exception(help())

def main():
    split_string, string_to_split, index = sys.argv[1:4]
    index = int(index)
    print(string_to_split.split(split_string)[index])

if __name__ == "__main__":
    main()


