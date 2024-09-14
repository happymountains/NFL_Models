import sys

def off_convert(old):
    print(0.06*float(old) - 0.02)

def def_convert(old):
    print(-0.07*float(old) + 0.01)

def_convert(sys.argv[1])