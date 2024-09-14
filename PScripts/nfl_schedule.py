import requests 
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By


ESPN_ACRONYM = {
    "LAC": "LAC",
    "LAR": "LA",
    "NYG": "NYG",
    "NYJ": "NYJ",
    "SD": "LAC",
    "STL": "LA"
}

ESPN_MAP = {
    "Arizona": "ARI",
    "Atlanta": "ATL",
    "Baltimore": "BAL",
    "Buffalo": "BUF",
    "Carolina": "CAR",
    "Chicago": "CHI",
    "Cincinnati": "CIN",
    "Cleveland": "CLE",
    "Dallas": "DAL",
    "Denver": "DEN",
    "Detroit": "DET",
    "Green Bay": "GB",
    "Houston": "HOU",
    "Indianapolis": "IND",
    "Jacksonville": "JAX",
    "Kansas City": "KC",
    "los-angeles-chargers": "LAC",
    "los-angeles-rams": "LA",
    "Las Vegas": "LV",
    "Miami": "MIA",
    "Minnesota": "MIN",
    "new-york-giants": "NYG",
    "new-york-jets": "NYJ",
    "New England": "NE",
    "New Orleans": "NO",
    "Oakland": "LV",
    "Philadelphia": "PHI",
    "Pittsburgh": "PIT",
    "San Diego": "LAC",
    "San Francisco": "SF",
    "Seattle": "SEA",
    "St. Louis": "LA",
    "Tampa Bay": "TB",
    "Tennessee": "TEN",
    "Washington": "WAS"
}

TEAM_MAP = {
    "Arizona": "ARI",
    "Atlanta": "ATL",
    "Baltimore": "BAL",
    "Buffalo": "BUF",
    "Carolina": "CAR",
    "Chicago": "CHI",
    "Cincinnati": "CIN",
    "Cleveland": "CLE",
    "Dallas": "DAL",
    "Denver": "DEN",
    "Detroit": "DET",
    "Green Bay": "GB",
    "Houston": "HOU",
    "Indianapolis": "IND",
    "Jacksonville": "JAX",
    "Kansas City": "KC",
    "L.A. Chargers": "LAC",
    "L.A. Rams": "LA",
    "Las Vegas": "LV",
    "Miami": "MIA",
    "Minnesota": "MIN",
    "N.Y. Giants": "NYG",
    "N.Y. Jets": "NYJ",
    "New England": "NE",
    "New Orleans": "NO",
    "Philadelphia": "PHI",
    "Pittsburgh": "PIT",
    "San Francisco": "SF",
    "Seattle": "SEA",
    "Tampa Bay": "TB",
    "Tennessee": "TEN",
    "Washington": "WAS"
}

chrome_options = Options()
chrome_options.headless = True
chrome_options.add_argument("--headless")
chrome_options.add_experimental_option("excludeSwitches", ["enable-logging"])

PATH = r"C:\Users\frobe\Downloads\chromedriver-win64\chromedriver.exe"
driver = webdriver.Chrome(options=chrome_options)
driver.implicitly_wait(2)

def get_byes_espn(year, num_weeks):
    team_byes = {}
    for i in range(1, int(num_weeks)):
        url = f'https://www.espn.com/nfl/schedule/_/week/{int(i)}/year/{int(year)}/seasontype/2'
        driver.get(url)
        byes = driver.find_elements(By.CLASS_NAME, "byeteams")
        if len(byes) == 0:
            continue

        bye_teams = byes[0].find_elements(By.CLASS_NAME, "Table__Team")
        for team in bye_teams:
            if team.text == "New York" or team.text == "Los Angeles":
                src = team.find_elements(By.CLASS_NAME, "Image")[0].get_attribute("src")
                team_byes[ESPN_ACRONYM[src.split("/")[-1].split(".")[0].upper()]] = i
            else:
                team_byes[ESPN_MAP[team.text]] = i

    for team, abbrev in ESPN_MAP.items():
        if abbrev not in team_byes:
            team_byes[abbrev] = 1

    return team_byes

def get_byes(year, num_weeks):
    team_byes = {}
    for i in range(1, int(num_weeks)):
        url = f'https://www.cbssports.com/nfl/schedule/{int(year)}/regular/{int(i)}/'
        page = requests.get(url)
        soup = BeautifulSoup(page.content, "html.parser")

        notes = soup.find_all("li", class_="TableBase-notesItem")
        notes_text = notes[0].get_text().strip()
        if notes_text[0:3] == "Bye":
            team_list = notes_text[5:].split(",")
            for team in team_list:
                team_byes[TEAM_MAP[team.strip()]] = i

    for team, abbrev in TEAM_MAP.items():
        if abbrev not in team_byes:
            team_byes[abbrev] = 1

    return team_byes

def kill_driver():
    driver.quit()

def scrape_schedule(year, week, num_weeks = 18):
    season_type = "regular"
    if week > num_weeks:
        season_type = "postseason"

    if week == (num_weeks + 4):
        week = num_weeks + 5

    url = f'https://www.cbssports.com/nfl/schedule/{int(year)}/{season_type}/{int(week)}/'
    page = requests.get(url)
    soup = BeautifulSoup(page.content, "html.parser")

    team_elements = soup.find_all("span", class_="TeamName")
    teams = []
    for element in team_elements:
        city = element.findChildren("a", recursive=False)[0].get_text()
        teams.append(TEAM_MAP[city])

    matchups = []
    for i in range(0, len(teams), 2):
        matchups.append([teams[i], teams[i + 1]])

    return matchups

def scrape_schedule_espn(year, week, num_weeks = 18):
    season_type = 2
    if week > num_weeks:
        season_type = 3

    if week == (num_weeks + 4):
        week = num_weeks + 5
    
    url = f'https://www.espn.com/nfl/schedule/_/week/{int(week)}/year/{int(year)}/seasontype/{season_type}'
    driver.get(url)
    schedule_block = driver.find_elements(By.CLASS_NAME, "ScheduleTables")
    teams = []
    for block in schedule_block:
        for element in block.find_elements(By.CLASS_NAME, "Table__Team"):
            if element.text == "New York":
                href = element.find_elements(By.CLASS_NAME, "AnchorLink")[0].get_attribute("href")
                teams.append(ESPN_MAP[href.split("/")[-1]])
            elif element.text == "Los Angeles":
                href = element.find_elements(By.CLASS_NAME, "AnchorLink")[0].get_attribute("href")
                teams.append(ESPN_MAP[href.split("/")[-1]])
            else:
                teams.append(ESPN_MAP[element.text])

    matchups = []
    for i in range(0, len(teams), 2):
        matchups.append([teams[i], teams[i + 1]])

    return matchups