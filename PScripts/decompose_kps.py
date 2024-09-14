import sys

def decompose(total, spread, kps):
    tt = (total / 2) + (spread / 2)
    xps = round((tt - kps) / 6, 1)
    fgs = round((kps - 0.95 * xps) / 3, 1)

    print(xps, fgs)

decompose(float(sys.argv[1]), float(sys.argv[2]), float(sys.argv[3]))