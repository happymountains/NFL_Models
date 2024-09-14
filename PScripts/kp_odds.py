import sys

def juice_ml(ml):
    line = ml - 15
    if 0 < line < 100:
        return -100 - (100 - line)
    elif 0 > line > -100:
        return 100 - (100 - line)
    elif line == -100 or line == 100:
        return 100
    else:
        return line

def percent_to_line(pct):
    if pct > 0.5:
        return (pct / (1 - pct)) * -100
    else:
        return ((1 - pct) / pct) * 100

def score_to_line(my_num, their_num):
    h_odds = (my_num ** 2 / (my_num ** 2 + their_num ** 2))
    a_odds = (their_num ** 2 / (my_num ** 2 + their_num ** 2))

    h_line = juice_ml(int(percent_to_line(h_odds)))
    a_line = juice_ml(int(percent_to_line(a_odds)))

    return(h_line, a_line)

def test(fg, td):
    return(3 * fg + 0.95 * 7 * td)

# print(test(float(sys.argv[1]), float(sys.argv[2])))

print(score_to_line(float(sys.argv[1]), float(sys.argv[2])))