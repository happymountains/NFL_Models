for i in range(18):
    j = i
    total = 0.20
    interval = 0.20
    while j > 0:
        interval = interval * 0.9835
        total = total + interval
        j = j - 1

    print(interval)