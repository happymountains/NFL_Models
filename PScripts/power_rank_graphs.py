import csv
import math
import os

from labellines import labelLines
import matplotlib.pyplot as plot
import numpy as np

TEAM = 0
PR = 1
PYTH = 2
OFF = 3
DEF = 4
QB = 5

GRAPH_LABELS = {
    0: "Power Rating",
    1: "S.O.V",
    2: "Off",
    3: "Def",
    4: "QB",
    5: "Total Grading",
    6: "Offensive Grade",
    7: "Pass Blocking",
    8: "Receiving",
    9: "Rushing Grade",
    10: "Run Blocking",
    11: "Defensive Grade",
    12: "Run Defense",
    13: "Tackling",
    14: "Pass Rush",
    15: "Coverage"
}

GRAPH_FILENAMES = {
    0: "pr",
    1: "pyth",
    2: "off",
    3: "def",
    4: "qb",
    5: "all-grade",
    6: "off-grade",
    7: "pass-block",
    8: "recv",
    9: "rush-grade",
    10: "run-block",
    11: "def-grade",
    12: "run-def",
    13: "tack",
    14: "prsh",
    15: "cov"
}

directory = "aEPA_Tests/season/overall/"


def get_num_weeks(year):
    if year < 2021:
        return 17
    else:
        return 18

def graph_history(year, history):
    for key in history:
        graph_team(key, year, history[key])

    for i in range(0, len(GRAPH_FILENAMES)):
        x, y, teams = graph_league(year, history, i)
        plot.figure(figsize=(12, 12))
        for index, points in enumerate(y):
            plot.plot(x, points, label=teams[index])
    
        mins = []
        maxs = []
        for thing in y:
            mins.append(min(thing))
            maxs.append(max(thing))
        high = max(maxs)
        low = min(mins)

        labelLines(plot.gca().get_lines(), zorder=2.5)
        plot.yticks(np.arange(low, high, 0.3))
        plot.ylabel("Power Rating")
        plot.xticks(x)
        plot.xlabel("Week")
        plot.title(f"{GRAPH_LABELS[i]} Power Ratings")
        plot.tight_layout()
        plot.savefig(f"aEPA_Tests/season/pr_graphs/{year}/{GRAPH_FILENAMES[i]}.png")
        plot.close()

def graph_league(year, league_data, field):
    x = [i for i in range(1, len(league_data["ARI"][field]) + 1)]
    y = []
    teams = []
    for team in league_data:
        y.append(league_data[team][field])
        teams.append(team)

    return(x, y, teams)


def graph_team(team_name, year, team_data):
    x = [i for i in range(1, len(team_data[0]) + 1)]
    y = []
    for i in range(0, len(GRAPH_FILENAMES)):
        y.append([])
        for value in team_data[i]:
            y[i].append(value)

    plot.figure(figsize=(12, 8))
    for i in range(0, 5):
        plot.plot(x, y[i], label=GRAPH_LABELS[i])
        
    
    mins = []
    maxs = []
    for thing in y:
        mins.append(min(thing))
        maxs.append(max(thing))
    high = max(maxs)
    low = min(mins)

    labelLines(plot.gca().get_lines(), zorder=2.5)
    plot.legend(bbox_to_anchor=(1.05, 1.0), loc='upper left')
    plot.xticks(x)
    plot.yticks(np.arange(math.floor(low), math.ceil(high), 0.4))
    plot.ylabel("Power Rating")
    plot.xlabel("Week")
    plot.title("Constituent Power Ratings")
    plot.tight_layout()
    plot.savefig(f"aEPA_Tests/season/pr_graphs/constituents/{year}/{team_name}_constituents_graph.png")
    plot.close()

def parse_pr_file(path, year_history):
    history = year_history
    with open(path, "r") as file:
        pr_reader = csv.reader(file, delimiter=",")
        for index, row in enumerate(pr_reader):
            if index == 0: continue

            team = row[TEAM]
            if team not in history:
                history[team] = [[] for _ in range(len(GRAPH_FILENAMES))]

            for i in range(1, len(GRAPH_FILENAMES) + 1):
                history[team][i - 1].append(float(row[i]))

    return history

for year in range(2007, 2024):
    print(f"Graphing {year}...")
    year_history = {}
    year_folder = directory + str(year)
    for week in range(1, get_num_weeks(year) + 1):
        filename = f"Power_{year}_week_{week}.csv"
        path = os.path.join(year_folder, filename)
        if os.path.isfile(path):
            year_history = parse_pr_file(path, year_history)

    graph_history(year, year_history)