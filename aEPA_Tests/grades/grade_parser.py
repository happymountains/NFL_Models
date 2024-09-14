import csv
import os
import sys

import numpy as np
import pyperclip


OFF = 7
PASS = 8
PBLK = 9
RECV = 10
RUN = 11
RBLK = 12
DEF = 13
RDEF = 14
TACK = 15
PRSH = 16
COV = 17

GRADE_CONSTANTS = [
    OFF,
    PASS,
    PBLK,
    RECV,
    RUN,
    RBLK,
    DEF,
    RDEF,
    TACK,
    PRSH,
    COV
]

OFF_STR = "off"
PASS_STR = "qb"
PBLK_STR = "pblk"
RECV_STR = "recv"
RUN_STR = "run"
RBLK_STR = "rblk"
DEF_STR = "def"
RDEF_STR = "rdef"
TACK_STR = "tack"
PRSH_STR = "prsh"
COV_STR = "cov"

GRADE_STRINGS = [
    OFF_STR,
    PASS_STR,
    PBLK_STR,
    RECV_STR,
    RUN_STR,
    RBLK_STR,
    DEF_STR,
    RDEF_STR,
    TACK_STR,
    PRSH_STR,
    COV_STR
]

TEAM_CONVERSION = {
    "ARZ": "ARI",
    "BLT": "BAL",
    "CLV": "CLE",
    "HST": "HOU",
    "OAK": "LV",
    "SD": "LAC",
    "SL": "LA",
    "STL": "LA"
}

off_grades = []
pass_grades = []
pblk_grades = []
recv_grades = []
run_grades = []
rblk_grades = []
def_grades = []
rdef_grades = []
tack_grades = []
prsh_grades = []
cov_grades = []

grade_lists = [
    off_grades,
    pass_grades,
    pblk_grades,
    recv_grades,
    run_grades,
    rblk_grades,
    def_grades,
    rdef_grades,
    tack_grades,
    prsh_grades,
    cov_grades
]

def write_file(data, type, year, team):
    if team in TEAM_CONVERSION:
        team = TEAM_CONVERSION[team]

    path = f"{type}/{year}"
    filename = f"{team}_{type}_{year}.csv"
    total = f"{path}/{filename}"
    if not os.path.isdir(path):
        os.makedirs(path)

    with open(total, "w") as file:
        file.write("Grade\n")
        for grade in data:
            file.write(str(grade))
            file.write("\n")

def parse_team_grades(year):
    input_vals = []
    for line in sys.stdin:
        if line == "\n":
            break
        else:
            input_vals.append(line.strip())

    FORBIDDEN = ["@", "WC", "DP", "CC", "SB"]

    start = 0
    bye = 1

    # starting at 2 to skip the header
    team = input_vals[0].split("-")[0].strip()
    for i in range(2, len(input_vals)):
        cell = input_vals[i]
        if cell.isnumeric():
            if int(cell) == bye:
                bye += 1
        elif cell in FORBIDDEN:
            continue
        else:
            start = i
            break

    relevant_info = input_vals[start:]
    num_games = len(relevant_info) // 20

    for i in range(num_games):
        for j, column in enumerate(GRADE_CONSTANTS):
            grade_lists[j].append(relevant_info[i * 20 + column])

    for i, column in enumerate(grade_lists):
        curr_list = grade_lists[i]
        # bye is 1-based, turn it to 0-based
        curr_list.insert(bye - 1, -1)
        write_file(curr_list, GRADE_STRINGS[i], year, team)

    print(f"Scraped {team}.")

parse_team_grades(2024)

# for year in range(2011, 2023):
#     for category in GRADE_STRINGS:
#         if category == "qb":
#             continue
#         basedir = f"{category}/{year}"
#         for file in os.listdir(basedir):
#             team_name = file.split("_")[0]
#             if team_name in TEAM_CONVERSION:
#                 new_filename = f"{TEAM_CONVERSION[team_name]}_{'_'.join(file.split('_')[1:])}"
#                 os.rename(f"{basedir}/{file}", f"{basedir}/{new_filename}")