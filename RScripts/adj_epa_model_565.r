library(stringr)
library(hash)
library(nflreadr)
library(reticulate)

setwd(str_interp("G:\\My Drive\\Programming Projects\\NFL\\"))
source("RScripts\\stats.r")

use_python(
  "C:\\Users\\frobe\\AppData\\Local\\Programs\\Python\\Python312\\python.exe",
  required = T
)
py_config()
source_python("PScripts\\nfl_schedule.py")

get_pr <- function(years_data, teams, season, week, matchups) {
  now_data <- years_data[[toString(season)]]
  last_data <- years_data[[toString(season - 1)]]

  num_weeks <- get_num_weeks(season)
  last_weeks <- get_num_weeks(season - 1) - 1

  byes <- hash()
  off_rankings <- hash()
  off_noadj <- hash()
  off_adjust <- hash()
  def_rankings <- hash()
  def_noadj <- hash()
  def_adjust <- hash()
  pyth_rankings <- hash()
  pyth_team_adj <- hash()
  for (team in teams) {
    off_rankings[[team]] <- 0
    def_rankings[[team]] <- 0
    pyth_rankings[[team]] <- 0
    pyth_team_adj[[team]] <- 0
    byes[[team]] <- 0
  }

  i <- week
  if (i == 1) {
    print("Yes it's 1")
    print(str_interp(
      "aEPA_Tests/season/offense/${season - 1}/Off_Ranking_${season - 1}_${last_weeks}.csv"))
    now_offense <- read.csv(str_interp(
      "aEPA_Tests/season/offense/${season - 1}/Off_Ranking_${season - 1}_${last_weeks}.csv"
    ))
    now_defense <- read.csv(str_interp(
      "aEPA_Tests/season/defense/${season - 1}/Def_Ranking_${season - 1}_${last_weeks}.csv"
    ))
  } else {
    now_offense <- read.csv(str_interp(
      "aEPA_Tests/season/offense/${season}/Off_Ranking_${season}_${i - 1}.csv"
    ))
    now_defense <- read.csv(str_interp(
      "aEPA_Tests/season/defense/${season}/Def_Ranking_${season}_${i - 1}.csv"
    )) # nolint: line_length_linter.
  }

  print(str_interp("--- Getting Schedule for ${season} week ${week} ---"))
  now_weight <- sides_weight(i)
  last_weight <- (1 - now_weight)

  print(str_interp("--- Calculating games for week ${i} ---"))

  print(str_interp("--- Getting Defensive Pass EPA values ---"))
  dpepa_hash <- get_dpepa_values(
    teams, now_data, last_data,
    i, num_weeks, now_weight, last_weight
  )
  print(str_interp("--- Getting Defensive Rush EPA values ---"))
  drepa_hash <- get_drepa_values(
    teams, now_data, last_data,
    i, num_weeks, now_weight, last_weight
  )
  print(str_interp("--- Getting DPSR ---"))
  dpsr_hash <- get_dpsr_values(
    teams, now_data, last_data,
    i, num_weeks, now_weight, last_weight
  )
  print(str_interp("--- Getting DRSR ---"))
  drsr_hash <- get_drsr_values(
    teams, now_data, last_data,
    i, num_weeks, now_weight, last_weight
  )

  print(str_interp("--- Getting Offensive Pass EPA values ---"))
  opepa_hash <- get_opepa_values(
    teams, now_data, last_data,
    i, num_weeks, now_weight, last_weight
  )
  print(str_interp("--- Getting Offensive Rush EPA values ---"))
  orepa_hash <- get_orepa_values(
    teams, now_data, last_data,
    i, num_weeks, now_weight, last_weight
  )
  print(str_interp("--- Getting Offensive Pass Success Rate values ---"))
  opsr_hash <- get_opsr_values(
    teams, now_data, last_data,
    i, num_weeks, now_weight, last_weight
  )
  print(str_interp("--- Getting Offensive Run Success Rate values ---"))
  orsr_hash <- get_orsr_values(
    teams, now_data, last_data,
    i, num_weeks, now_weight, last_weight
  )

  print("--- Loading pythagorean values ---")
  pyth_vals <- get_pyth_values(
    teams, now_data,
    last_data, i, last_weeks, now_weight, last_weight, byes
  )

  for (team in teams) {
    log <- FALSE # team == "BAL"
    def_result <- get_adjusted_def_game(
      now_data, team, season,
      i, num_weeks, dpepa_hash, drepa_hash, dpsr_hash, drsr_hash
    )
    off_result <- get_adjusted_off_game(
      now_data, team, season,
      i, num_weeks, opepa_hash, orepa_hash, opsr_hash, orsr_hash
    )
    if (def_result == -1) {
      byes[[team]] <- 1
    } else {
      other_team <- ""
      for (index in 1:length(matchups)) {
        matchup <- matchups[[index]]
        if (team %in% matchup) {
          if (team == matchup[1]) {
            other_team <- matchup[2]
          } else if (team == matchup[2]) {
            other_team <- matchup[1]
          }
        }
      }

      last_off <- now_offense[
        now_offense$Team == team, "Off.Power"
      ][[1]][[1]]
      if (team == "KC") {
        print(last_off)
      }
      last_off_noadj <- now_offense[
        now_offense$Team == team, "Unadjusted"
      ][[1]][[1]]
      last_def <- now_defense[
        now_defense$Team == team, "Def.Power"
      ][[1]][[1]]
      last_def_noadj <- now_defense[
        now_defense$Team == team, "Unadjusted"
      ][[1]][[1]]
      last_pyth_adj <- now_offense[
        now_offense$Team == team, "PythAdj"
      ][[1]][[1]]

      if (other_team != "") {
        team_pyth <- pyth_vals[[team]]
        other_pyth <- pyth_vals[[other_team]]

        off_adj <- off_result - OFF_ADJ * as.double(now_defense[
          now_defense$Team == other_team, "Def.Power"
        ][[1]][[1]])
        def_adj <- def_result - DEF_ADJ * as.double(now_offense[
          now_offense$Team == other_team, "Off.Power"
        ][[1]][[1]])

        # opponent pyth is based on 1 season of data,
        # n - 1 weeks' weight to previous data, 1 week's weight
        # to new data
        pyth_team_adj[[team]] <- round((last_pyth_adj * (num_weeks - 1) +
          other_pyth) / num_weeks, 3)

        off_rankings[[team]] <- ((off_adj * now_weight) +
          (last_off * (1 - now_weight)))
        off_noadj[[team]] <- ((off_result * now_weight) +
          (last_off_noadj * (1 - now_weight)))
        def_rankings[[team]] <- ((def_adj * now_weight) +
          (last_def * (1 - now_weight)))
        def_noadj[[team]] <- ((def_result * now_weight) +
          (last_def_noadj * (1 - now_weight)))

        if (log) {
          print(str_interp(
            "${round(off_adj, 3)} x ${round(now_weight, 3)} + ${last_off} x ${1 - now_weight} x 10 = ${round(off_rankings[[team]], 3)}"
          ))
          print(str_interp(
            "${round(def_adj, 3)} x ${round(now_weight, 3)} + ${last_def} x ${1 - now_weight} x 10 = ${round(def_rankings[[team]], 3)}"
          ))
        }

        pyth_rankings[[team]] <- pyth_vals[[team]]
      } else {
        off_rankings[[team]] <- last_off
        off_noadj[[team]] <- last_off_noadj
        def_rankings[[team]] <- last_def
        def_noadj[[team]] <- last_def_noadj
        pyth_rankings[[team]] <- pyth_vals[[team]]
        pyth_team_adj[[team]] <- last_pyth_adj
      }
    }
  }

  teams <- keys(def_rankings)
  def_ranking_vec <- vector(, 32)
  def_unadjusted <- vector(, 32)
  diff_vec <- vector(, 32)
  dpepa_vec <- vector(, 32)
  drepa_vec <- vector(, 32)
  dpsr_vec <- vector(, 32)
  drsr_vec <- vector(, 32)
  pyth_vec <- vector(, 32)
  pyth_adj_vec <- vector(, 32)

  for (j in 1:length(teams)) {
    team <- teams[j]
    dpepa_vec[j] <- round(dpepa_hash[[team]], 3)
    drepa_vec[j] <- round(drepa_hash[[team]], 3)
    dpsr_vec[j] <- round(dpsr_hash[[team]], 3)
    drsr_vec[j] <- round(drsr_hash[[team]], 3)
    pyth_vec[j] <- round(pyth_rankings[[team]], 3)
    pyth_adj_vec[j] <- round(pyth_team_adj[[team]], 3)
    def_ranking_vec[j] <- round(as.double(def_rankings[[team]]), 3)

    if (is.null(def_noadj[[team]])) {
      def_noadj[[team]] <- last_def_noadj
    } else {
      def_unadjusted[j] <- round(def_noadj[[team]], 3)
    }

    diff_vec[j] <- round(as.double(def_unadjusted[[j]]) - as.double(def_ranking_vec[[j]]), 3)
  }

  final_frame <- data.frame(
    teams, def_ranking_vec, def_unadjusted, diff_vec, dpepa_vec, drepa_vec,
    dpsr_vec, drsr_vec, pyth_vec, pyth_adj_vec
  )
  colnames(final_frame) <- c(
    "Team", "Def Power", "Unadjusted", "Diff", "DPEPA", "DREPA", "DPSR", "DRSR", "Pyth", "PythAdj"
  )

  write.csv(final_frame[order(def_ranking_vec), ], str_interp(
    "aEPA_Tests/season/defense/${season}/Def_Ranking_${season}_${i}.csv"
  ),
  row.names = FALSE, quote = FALSE
  )

  off_ranking_vec <- vector(, 32)
  off_unadjusted <- vector(, 32)
  diff_vec <- vector(, 32)
  opepa_vec <- vector(, 32)
  orepa_vec <- vector(, 32)
  opsr_vec <- vector(, 32)
  orsr_vec <- vector(, 32)

  for (j in 1:length(teams)) {
    team <- teams[j]
    off_ranking_vec[j] <- round(off_rankings[[team]], 3)
    opepa_vec[j] <- round(opepa_hash[[team]], 3)
    orepa_vec[j] <- round(orepa_hash[[team]], 3)
    opsr_vec[j] <- round(opsr_hash[[team]], 3)
    orsr_vec[j] <- round(orsr_hash[[team]], 3)

    if (is.null(off_noadj[[team]])) {
      off_noadj[[team]] <- last_off_noadj
    } else {
      off_unadjusted[j] <- round(off_noadj[[team]], 3)
    }

    diff_vec[j] <- round(as.double(off_unadjusted[j]) - as.double(off_ranking_vec[[j]]), 3)
  }

  final_frame <- data.frame(
    teams, off_ranking_vec, off_unadjusted, diff_vec, opepa_vec,
    orepa_vec, opsr_vec, orsr_vec, pyth_vec, pyth_adj_vec
  )
  colnames(final_frame) <- c(
    "Team", "Off Power", "Unadjusted", "Diff", "OPEPA", "OREPA", "OPSR", "ORSR", "Pyth", "PythAdj"
  )

  write.csv(final_frame[rev(order(off_ranking_vec)), ], str_interp(
    "aEPA_Tests/season/offense/${season}/Off_Ranking_${season}_${i}.csv"
  ),
  row.names = FALSE, quote = FALSE
  )
}

roll_up_grade <- function(data, week) {
  i <- week
  total_grade <- 0
  bye <- 0
  while (i > (week - (RECENT_INTERVAL + bye))) {
    if (i < 1) {
      break
    }

    grade <- data$Grade[i]
    if (is.na(grade)) {
      return(-1)
    }

    if (grade == -1) {
      bye <- 1
    } else {
      total_grade <- total_grade + grade
    }

    i <- i - 1
  }

  j <- i
  old_grade <- 0
  bye_two <- 0
  while (j > 0) {
    grade <- data$Grade[j]
    if (grade == -1) {
      bye_two <- 1
    } else {
      old_grade <- old_grade + grade
    }

    j <- j - 1
  }

  total_grade <- round(total_grade / (week - (i + bye)), 1)
  if (old_grade == 0) {
    weight <- 0
  } else {
    old_grade <- round(old_grade / (i - bye_two), 1)
    weight <- RECENT_GRADE_WEIGHT
  }


  final_grade <- round(old_grade * weight + total_grade * (1 - weight), 1)
  return(final_grade)
}

roll_up_year_grade <- function(data, week) {
  i <- week
  total_grade <- 0
  bye <- 0
  while (i > 0) {
    if (i < 1) {
      break
    }

    grade <- data$Grade[i]
    if (is.na(grade)) {
      if (!is.na(data$Grade[1])) {
        i <- i - 1
        week <- week - 1
        next
      } else {
        return(-1)
      }
    }

    if (grade == -1) {
      bye <- 1
    } else {
      total_grade <- total_grade + grade
    }

    i <- i - 1
  }

  j <- i
  old_grade <- 0
  bye_two <- 0
  while (j > 0) {
    grade <- data$Grade[j]
    if (grade == -1) {
      bye_two <- 1
    } else {
      old_grade <- old_grade + grade
    }

    j <- j - 1
  }

  total_grade <- round(total_grade / (week - (i + bye)), 1)
  if (old_grade == 0) {
    weight <- 0
  } else {
    old_grade <- round(old_grade / (i - bye_two), 1)
    weight <- RECENT_GRADE_WEIGHT
  }


  final_grade <- round(old_grade * weight + total_grade * (1 - weight), 1)
  return(final_grade)
}

get_grade <- function(year, week, category) {
  grades_frame <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(grades_frame) <- c("Team", "CurrentGrade", "LastGrade")

  for (i in 1:length(teams)) {
    team <- teams[i]
    last_data <- read.csv(
      str_interp(
        "aEPA_Tests/grades/${category}/${year - 1}/${team}_${category}_${year - 1}.csv"
      ),
      strip.white = TRUE
    )

    last_grade <- roll_up_year_grade(last_data, get_num_weeks(year - 2))
    tryCatch(
      {
        now_data <- read.csv(
          str_interp(
            "aEPA_Tests/grades/${category}/${year}/${team}_${category}_${year}.csv"
          ),
          strip.white = TRUE
        )

        current_grade <- roll_up_grade(now_data, week)
      },
      error = function(cond) {
        current_grade <- last_grade
      },
      warning = function(cond) {},
      finally = {}
    )

    if (current_grade == -1) {
      current_grade <- last_grade
    }

    grades_frame[i, ] <- c(team, current_grade, last_grade)
  }

  return(grades_frame)
}

get_qb_value_nograde <- function(team, now_data, last_data, year, week, num_weeks) {
  PCT_TWO <- 0.69
  PCT_THREE <- 0.31

  last_vals <- fetch_season_cpoe_vs_epa(last_data, num_weeks)

  qb_weight <- 0
  recent_vals <- fetch_range_cpoe_vs_epa(now_data, week)
  season_vals <- fetch_season_cpoe_vs_epa(now_data, week)
  qb_vals <- hash()
  for (team in teams) {
    team_last <- last_vals[which(last_vals$posteam == team), ]
    team_recent <- recent_vals[which(recent_vals$posteam == team), ]
    team_season <- season_vals[which(season_vals$posteam == team), ]

    last_qb <- round(team_last$epa * PCT_TWO +
      (team_last$cpoe * CPOE_REDUCTION) * PCT_THREE, digits = 3)

    qb_weight <- grade_cat_weight(week)
    if (NEW_QB[[team]] == TRUE) {
      qb_weight <- qb_weight + 0.06
    }

    if (qb_weight > 1) {
      qb_weight <- 1
    }

    if (nrow(team_recent) == 0) {
      now_cpoe <- team_season$cpoe * CPOE_REDUCTION
      now_epa <- team_season$epa
    } else {
      now_cpoe <- (team_season$cpoe * (1 - QB_RECENT_WEIGHT) +
        team_recent$cpoe * (QB_RECENT_WEIGHT)) * CPOE_REDUCTION
      now_epa <- team_season$epa * (1 - QB_RECENT_WEIGHT) + team_recent$epa * (recent_weight)
    }

    now_qb <- round(now_epa * PCT_TWO + now_cpoe * PCT_THREE, digits = 3)
    if (identical(now_qb, numeric(0))) {
      now_qb <- last_qb
    }

    final <- round(weigh_two_data(
      now_qb, last_qb, qb_weight, 1 - qb_weight
    ), digits = 3)
    qb_vals[team] <- final
  }

  return(qb_vals)
}

get_category_value <- function(year, week, category, week_weight, grade_weight) {
  temp_list <- list()
  temp_vals <- vector("double", length = 32)
  i <- 1

  for (team in teams) {
    last_data <- read.csv(
      str_interp(
        "aEPA_Tests/grades/${category}/${year - 1}/${team}_${category}_${year - 1}.csv"
      ),
      strip.white = TRUE
    )

    last_grade <- roll_up_year_grade(last_data, get_num_weeks(year - 2)) * GRADE_REDUCTION
    tryCatch(
      {
        now_data <- read.csv(
          str_interp(
            "aEPA_Tests/grades/${category}/${year}/${team}_${category}_${year}.csv"
          ),
          strip.white = TRUE
        )

        new_grade <- roll_up_grade(now_data, week) * GRADE_REDUCTION
      },
      error = function(cond) {
        new_grade <- last_grade
      },
      warning = function(cond) {},
      finally = {}
    )


    last_weight <- 1 - week_weight
    final <- round(weigh_two_data(new_grade, last_grade, week_weight, last_weight), digits = 3)
    temp_list[[i]] <- c(team, final)
    temp_vals[i] <- final
    i <- i + 1
  }

  # transform to point value
  temp_vals <- sort(temp_vals)
  median <- (as.double(temp_vals[16]) +
    as.double(temp_vals[17])) / 2

  final_hash <- hash()
  for (i in 1:length(temp_list)) {
    transfer <- temp_list[i]
    team <- transfer[[1]][[1]]
    grade <- transfer[[1]][[2]]
    transformed <- round(GRADE_MULT * (as.double(grade) - median) * grade_weight, 2)
    final_hash[team] <- transformed
  }

  return(final_hash)
}

get_qb_value <- function(
    team, now_data, last_data, year, week, num_weeks,
    grades) {
  last_vals <- fetch_season_cpoe_vs_epa(last_data, num_weeks)
  qb_weight <- 0
  recent_vals <- fetch_range_cpoe_vs_epa(now_data, week)
  season_vals <- fetch_season_cpoe_vs_epa(now_data, week)
  qb_vals <- hash()
  for (team in teams) {
    team_grade <- as.double(grades[grades$Team == team, "LastGrade"]) * QB_GRADE_REDUCTION
    team_last <- last_vals[which(last_vals$posteam == team), ]
    team_recent <- recent_vals[which(recent_vals$posteam == team), ]
    team_season <- season_vals[which(season_vals$posteam == team), ]

    last_qb <- round(team_grade * PFF_WEIGHT + team_last$epa * EPA_WEIGHT +
      (team_last$cpoe * CPOE_REDUCTION) * CPOE_WEIGHT, digits = 3)

    # this 1.2 can be tweaked
    qb_weight <- grade_cat_weight(week)
    if (NEW_QB[[team]] == TRUE) {
      qb_weight <- qb_weight + NEW_QB_WEIGHT
    }

    if (qb_weight > 1) {
      qb_weight <- 1
    }

    new_grade <- as.double(grades[grades$Team == team, "CurrentGrade"]) * QB_GRADE_REDUCTION
    if (nrow(team_recent) == 0) {
      now_cpoe <- team_season$cpoe * CPOE_REDUCTION
      now_epa <- team_season$epa
    } else {
      now_cpoe <- (team_season$cpoe * (1 - QB_RECENT_WEIGHT) +
        team_recent$cpoe * (QB_RECENT_WEIGHT)) * CPOE_REDUCTION
      now_epa <- team_season$epa * (1 - QB_RECENT_WEIGHT) + team_recent$epa * (QB_RECENT_WEIGHT)
    }


    now_qb <- round(new_grade * PFF_WEIGHT + now_epa * EPA_WEIGHT +
      now_cpoe * CPOE_WEIGHT, digits = 3)

    if (identical(now_qb, numeric(0))) {
      now_qb <- last_qb
    }

    final <- round(weigh_two_data(
      now_qb, last_qb, qb_weight, 1 - qb_weight
    ), digits = 3)
    qb_vals[team] <- final
  }

  return(qb_vals)
}

clear_teams <- function(byes) {
  new_hash <- hash()
  for (team in teams) {
    new_hash[[team]] <- 0
  }

  return(new_hash)
}

get_pyth_values <- function(
    teams, now_data, last_data, week,
    num_weeks, now_weight, last_weight, byes) {
  pyth_values <- hash()

  pyth_now_dict <- team_pythagorean_projection(now_data, week, byes)
  wp_now_dict <- team_win_percentage(now_data, week)
  byes <- clear_teams(byes)
  pyth_last_dict <- team_pythagorean_projection(last_data, num_weeks, byes)
  wp_last_dict <- team_win_percentage(last_data, num_weeks)

  for (team in teams) {
    week_modifier <- 0
    if (!is.na(byes[[team]])) {
      if (byes[[team]] <= week) {
        week_modifier <- 0
      }
    }

    if (!(week == 1 & byes[[team]] == 1)) {
      pyth_now <- pyth_now_dict[[team]] / (week - week_modifier)
      wp_now <- wp_now_dict[[team]]
      pyth_last <- pyth_last_dict[[team]] / (num_weeks - week_modifier)
      wp_last <- wp_last_dict[[team]]
      if (identical(pyth_now, numeric(0))) {
        pyth_now <- pyth_last
        wp_now <- wp_last
      }

      if (is.na(pyth_now)) {
        pyth_now <- pyth_last
        wp_now <- wp_last
      }

      weighed_pyth <- weigh_two_data(
        pyth_now, pyth_last, now_weight,
        last_weight
      )
      weighed_wp <- weigh_two_data(
        wp_now, wp_last, now_weight,
        last_weight
      )
      pyth_values[[team]] <- round(
        weighed_pyth * PYTH_WEIGHT + weighed_wp * WP_WEIGHT, 4
      )
    }
  }

  return(pyth_values)
}

get_dpepa_values <- function(
    teams, now_data, last_data, week,
    num_weeks, now_weight, last_weight) {
  dpepa_values <- hash()
  dpepa_now_df <- fetch_def_pass_epa(now_data, week)
  dpepa_last_df <- fetch_def_pass_epa(last_data, num_weeks)
  dpepa_recent_df <- fetch_def_pass_epa_recent(now_data, week)
  for (team in teams) {
    dpepa_now <- as.double(dpepa_now_df[
      dpepa_now_df$defteam == team, "epa_per_play"
    ])
    dpepa_last <- as.double(dpepa_last_df[
      dpepa_last_df$defteam == team, "epa_per_play"
    ])

    if (is.null(dpepa_now)) {
      dpepa_now <- dpepa_last
    } else if (week > RECENT_INTERVAL) {
      dpepa_recent <- as.double(dpepa_recent_df[
        dpepa_recent_df$defteam == team, "epa_per_play"
      ])
      recent_weight <- RECENT_WEIGHT
      dpepa_now <- dpepa_recent * recent_weight + dpepa_now * (1 - recent_weight)
    }

    weighed_dpepa <- weigh_two_data(
      dpepa_now, dpepa_last, now_weight,
      last_weight
    )
    dpepa_values[[team]] <- round(weighed_dpepa, 4)
  }

  return(dpepa_values)
}

get_drepa_values <- function(
    teams, now_data, last_data, week,
    num_weeks, now_weight, last_weight) {
  drepa_values <- hash()
  drepa_now_df <- fetch_def_run_epa(now_data, week)
  drepa_last_df <- fetch_def_run_epa(last_data, num_weeks)
  drepa_recent_df <- fetch_def_run_epa_recent(now_data, week)
  for (team in teams) {
    drepa_now <- as.double(drepa_now_df[
      drepa_now_df$defteam == team, "epa_per_play"
    ])
    drepa_last <- as.double(drepa_last_df[
      drepa_last_df$defteam == team, "epa_per_play"
    ])

    if (is.null(drepa_now)) {
      drepa_now <- drepa_last
    } else if (week > RECENT_INTERVAL) {
      drepa_recent <- as.double(drepa_recent_df[
        drepa_recent_df$defteam == team, "epa_per_play"
      ])
      recent_weight <- RECENT_WEIGHT
      drepa_now <- drepa_recent * recent_weight + drepa_now * (1 - recent_weight)
    }

    weighed_drepa <- weigh_two_data(
      drepa_now, drepa_last, now_weight,
      last_weight
    )
    drepa_values[[team]] <- round(weighed_drepa, 4)
  }

  return(drepa_values)
}

get_dpsr_values <- function(
    teams, now_data, last_data, week,
    num_weeks, now_weight, last_weight) {
  dpsr_values <- hash()
  dpsr_now_df <- fetch_def_pass_success_rate(now_data, week)
  dpsr_last_df <- fetch_def_pass_success_rate(last_data, num_weeks)
  dpsr_recent_df <- fetch_def_pass_success_rate_recent(now_data, week)
  for (team in teams) {
    dpsr_now <- as.double(dpsr_now_df[
      dpsr_now_df$defteam == team, "def_success"
    ])
    dpsr_last <- as.double(dpsr_last_df[
      dpsr_last_df$defteam == team, "def_success"
    ])

    if (is.na(dpsr_now)) {
      dpsr_now <- dpsr_last
    } else if (week > RECENT_INTERVAL) {
      dpsr_recent <- as.double(dpsr_recent_df[
        dpsr_recent_df$defteam == team, "def_success"
      ])
      recent_weight <- RECENT_WEIGHT
      dpsr_now <- dpsr_recent * recent_weight + dpsr_now * (1 - recent_weight)
    }

    weighed_dpsr <- weigh_two_data(
      dpsr_now, dpsr_last, now_weight,
      last_weight
    )

    dpsr_values[[team]] <- weighed_dpsr
  }

  return(dpsr_values)
}

get_drsr_values <- function(
    teams, now_data, last_data, week,
    num_weeks, now_weight, last_weight) {
  drsr_values <- hash()
  drsr_now_df <- fetch_def_run_success_rate(now_data, week)
  drsr_last_df <- fetch_def_run_success_rate(last_data, num_weeks)
  drsr_recent_df <- fetch_def_run_success_rate(now_data, week)
  for (team in teams) {
    drsr_now <- as.double(drsr_now_df[
      drsr_now_df$defteam == team, "success_rate"
    ])
    drsr_last <- as.double(drsr_last_df[
      drsr_last_df$defteam == team, "success_rate"
    ])

    if (is.na(drsr_now)) {
      drsr_now <- drsr_last
    } else if (week > RECENT_INTERVAL) {
      drsr_recent <- as.double(drsr_recent_df[
        drsr_recent_df$defteam == team, "success_rate"
      ])
      recent_weight <- RECENT_WEIGHT
      drsr_now <- drsr_recent * recent_weight + drsr_now * (1 - recent_weight)
    }

    weighed_drsr <- weigh_two_data(
      drsr_now, drsr_last, now_weight,
      last_weight
    )

    drsr_values[[team]] <- weighed_drsr
  }

  return(drsr_values)
}

get_adjusted_def_game <- function(now_data, team, season, week, num_weeks,
                                  dpepa_values, drepa_values, dpsr_values, drsr_values) {
  result <- tryCatch(
    {
      weighed_dpepa <- dpepa_values[[team]]
      weighed_drepa <- drepa_values[[team]]
      weighed_dpsr <- dpsr_values[[team]]
      weighed_drsr <- drsr_values[[team]]

      result <- DPEPA * weighed_dpepa + DREPA * weighed_drepa +
        DPSR * (weighed_dpsr - AVG_DPSR) + DRSR * (weighed_drsr - AVG_DRSR)

      if (is.na(result)) {
        return(-1)
      } else {
        return(result)
      }
    },
    error = function(cond) {
      print(cond)
      return(-1)
    },
    warning = function(cond) {},
    finally = {}
  )

  return(result)
}

get_opepa_values <- function(
    teams, now_data, last_data, week,
    num_weeks, now_weight, last_weight) {
  opepa_values <- hash()
  opepa_now_df <- fetch_pass_epa(now_data, week)
  opepa_last_df <- fetch_pass_epa(last_data, num_weeks)
  opepa_recent_df <- fetch_pass_epa_recent(now_data, week)
  for (team in teams) {
    opepa_now <- as.double(opepa_now_df[
      opepa_now_df$posteam == team, "epa_per_play"
    ])
    opepa_last <- as.double(opepa_last_df[
      opepa_last_df$posteam == team, "epa_per_play"
    ])

    if (NEW_QB[[team]] == TRUE) {
      now_weight <- now_weight + NEW_QB_WEIGHT
    }

    if (is.na(opepa_now)) {
      opepa_now <- opepa_last
    } else if (week > RECENT_INTERVAL) {
      opepa_recent <- as.double(opepa_recent_df[
        opepa_recent_df$posteam == team, "epa_per_play"
      ])
      recent_weight <- RECENT_WEIGHT
      opepa_now <- opepa_recent * recent_weight + opepa_now * (1 - recent_weight)
    }

    weighed_opepa <- weigh_two_data(
      opepa_now, opepa_last,
      now_weight, last_weight
    )

    opepa_values[[team]] <- weighed_opepa
  }

  return(opepa_values)
}

get_orepa_values <- function(
    teams, now_data, last_data, week,
    num_weeks, now_weight, last_weight) {
  orepa_values <- hash()
  orepa_now_df <- fetch_run_epa(now_data, week)
  orepa_last_df <- fetch_run_epa(last_data, num_weeks)
  orepa_recent_df <- fetch_run_epa_recent(now_data, week)
  for (team in teams) {
    orepa_now <- as.double(orepa_now_df[
      orepa_now_df$posteam == team, "epa_per_play"
    ])
    orepa_last <- as.double(orepa_last_df[
      orepa_last_df$posteam == team, "epa_per_play"
    ])

    if (is.na(orepa_now)) {
      orepa_now <- orepa_last
    } else if (week > RECENT_INTERVAL) {
      orepa_recent <- as.double(orepa_recent_df[
        orepa_recent_df$posteam == team, "epa_per_play"
      ])
      recent_weight <- RECENT_WEIGHT
      orepa_now <- orepa_recent * recent_weight + orepa_now * (1 - recent_weight)
    }

    weighed_orepa <- weigh_two_data(
      orepa_now, orepa_last,
      now_weight, last_weight
    )

    orepa_values[[team]] <- weighed_orepa
  }

  return(orepa_values)
}

get_opsr_values <- function(
    teams, now_data, last_data, week,
    num_weeks, now_weight, last_weight) {
  opsr_values <- hash()
  opsr_now_df <- fetch_pass_success_rate(now_data, week)
  opsr_last_df <- fetch_pass_success_rate(last_data, num_weeks)
  opsr_recent_df <- fetch_pass_success_rate_recent(now_data, week)
  for (team in teams) {
    opsr_now <- as.double(opsr_now_df[
      opsr_now_df$posteam == team, "success_rate"
    ])
    opsr_last <- as.double(opsr_last_df[
      opsr_last_df$posteam == team, "success_rate"
    ])

    if (NEW_QB[[team]] == TRUE) {
      now_weight <- now_weight + NEW_QB_WEIGHT
    }

    if (is.na(opsr_now)) {
      opsr_now <- opsr_last
    } else if (week > RECENT_INTERVAL) {
      opsr_recent <- as.double(opsr_recent_df[
        opsr_recent_df$posteam == team, "success_rate"
      ])
      recent_weight <- RECENT_WEIGHT
      opsr_now <- opsr_recent * recent_weight + opsr_now * (1 - recent_weight)
    }

    weighed_opsr <- weigh_two_data(
      opsr_now, opsr_last, now_weight,
      last_weight
    )

    opsr_values[[team]] <- weighed_opsr
  }

  return(opsr_values)
}

get_orsr_values <- function(
    teams, now_data, last_data, week,
    num_weeks, now_weight, last_weight) {
  orsr_values <- hash()
  orsr_now_df <- fetch_run_success_rate(now_data, week)
  orsr_last_df <- fetch_run_success_rate(last_data, num_weeks)
  orsr_recent_df <- fetch_run_success_rate_recent(now_data, week)
  for (team in teams) {
    orsr_now <- as.double(orsr_now_df[
      orsr_now_df$posteam == team, "success_rate"
    ])
    orsr_last <- as.double(orsr_last_df[
      orsr_last_df$posteam == team, "success_rate"
    ])

    if (is.na(orsr_now)) {
      orsr_now <- orsr_last
    } else if (week > RECENT_INTERVAL) {
      orsr_recent <- as.double(orsr_recent_df[
        orsr_recent_df$posteam == team, "success_rate"
      ])
      recent_weight <- RECENT_WEIGHT
      orsr_now <- orsr_recent * recent_weight + orsr_now * (1 - recent_weight)
    }

    weighed_orsr <- weigh_two_data(
      orsr_now, orsr_last, now_weight,
      last_weight
    )

    orsr_values[[team]] <- weighed_orsr
  }

  return(orsr_values)
}

get_adjusted_off_game <- function(now_data, team, season, week, num_weeks,
                                  opepa_values, orepa_values, opsr_values, orsr_values, report_frame) {
  result <- tryCatch(
    {
      weighed_opepa <- opepa_values[[team]]
      weighed_orepa <- orepa_values[[team]]
      weighed_opsr <- opsr_values[[team]]
      weighed_orsr <- orsr_values[[team]]

      result <- OPEPA * weighed_opepa + OREPA * weighed_orepa +
        OPSR * (weighed_opsr - AVG_OPSR) + ORSR * (weighed_orsr - AVG_ORSR)

      if (is.na(result)) {
        return(-1)
      } else {
        return(result)
      }

      return(result)
    },
    error = function(cond) {
      print(cond)
      return(-1)
    },
    warning = function(cond) {},
    finally = {}
  )

  return(result)
}

weigh_data <- function(field_now, field_last, field_old, now_weight,
                       last_weight, old_weight) {
  return(field_now * now_weight + field_last * last_weight +
    field_old * old_weight)
}

write_power_rank_files <- function(year, years_data, week, byes) {
  print(str_interp("--- Writing ${year} PowerRating ---"))
  report_frame <- data.frame(matrix(ncol = 17, nrow = 0))
  colnames(report_frame) <- c(
    "Team", "PowerRating", "Pyth", "Off", "Def", "QB", "AllGrade", "OFF", "PBLK", "RCV", "RUN", "RBLK", "DEF", "RDEF", "TACK", "PRSH", "COV"
  )

  last_year <- year - 1
  num_weeks <- get_num_weeks(year)
  last_weeks <- get_num_weeks(last_year)

  j <- week

  print(str_interp("Week ${j}..."))
  now_offense <- read.csv(str_interp(
    "aEPA_Tests/season/offense/${year}/Off_Ranking_${year}_${j}.csv"
  ))
  now_defense <- read.csv(str_interp(
    "aEPA_Tests/season/defense/${year}/Def_Ranking_${year}_${j}.csv"
  ))
  last_offense <- read.csv(str_interp(
    "aEPA_Tests/season/offense/${last_year}/Off_Ranking_${last_year}_${last_weeks - 1}.csv"
  ))
  last_defense <- read.csv(str_interp(
    "aEPA_Tests/season/defense/${last_year}/Def_Ranking_${last_year}_${last_weeks - 1}.csv"
  ))

  if (BACKTESTING) {
    grades <- get_grade(year, j, PASS_STR)
    qb_vals <- get_qb_value(
      team, years_data[[toString(year)]],
      years_data[[toString(last_year)]], year, j, last_weeks,
      grades
    )
  } else {
    qb_vals <- read.csv(
      str_interp("aEPA_Tests/season/qb/${QB_FILE_YEAR}_qb.csv"))
  }

  off_vals <- get_category_value(
    year, j, OFF_STR, test_weight(j), OFF_GRADE_PCT)
  pblk_vals <- get_category_value(year, j, PBLK_STR, test_weight(j), PBLK_PCT)
  recv_vals <- get_category_value(year, j, RECV_STR, test_weight(j), RECV_PCT)
  run_vals <- get_category_value(year, j, RUN_STR, test_weight(j), RUN_PCT)
  rblk_vals <- get_category_value(year, j, RBLK_STR, test_weight(j), RBLK_PCT)
  def_vals <- get_category_value(
    year, j, DEF_STR, test_weight(j), DEF_GRADE_PCT)
  rdef_vals <- get_category_value(year, j, RDEF_STR, test_weight(j), RDEF_PCT)
  tack_vals <- get_category_value(year, j, TACK_STR, test_weight(j), TACK_PCT)
  prsh_vals <- get_category_value(year, j, PRSH_STR, test_weight(j), PRSH_PCT)
  cov_vals <- get_category_value(year, j, COV_STR, test_weight(j), COV_PCT)

  for (k in 1:length(teams)) {
    team <- teams[k]
    if (BACKTESTING) {
      qb <- qb_vals[[team]]
      qb <- resize_qb_val(qb)
    } else {
      qb <- qb_vals[qb_vals$Team == team, "Composite"][[1]][[1]]
      qb <- resize_qb_comp(qb)
    }

    now_offense_value <- now_offense[
      now_offense$Team == team, "Off.Power"
    ][[1]][[1]]
    now_defense_value <- now_defense[
      now_defense$Team == team, "Def.Power"
    ][[1]][[1]]
    now_pyth_value <- now_offense[
      now_offense$Team == team, "Pyth"
    ][[1]][[1]]
    now_pyth_value <- now_pyth_value * 0.7 + now_offense[
      now_offense$Team == team, "PythAdj"
    ][[1]][[1]] * 0.3
    all_grade <- round(off_vals[[team]] +
      pblk_vals[[team]] + recv_vals[[team]] + run_vals[[team]] + rblk_vals[[team]] +
      def_vals[[team]] + rdef_vals[[team]] + tack_vals[[team]] +
      prsh_vals[[team]] + cov_vals[[team]], 2)

    now_offense_value <- resize_off(now_offense_value)
    now_defense_value <- resize_def(now_defense_value)
    now_pyth_value <- resize_pyth(now_pyth_value)
    all_grade <- resize_grade(all_grade)

    offense <- OFF_QB_RATIO * now_offense_value + (1 - OFF_QB_RATIO) * qb
    defense <- now_defense_value
    overall <- now_pyth_value * PYTH_PCT + offense * OFF_QB_PCT +
      defense * DEF_PCT + all_grade * GRADE_PCT

    row <- c(
      team, round(overall, 3), round(now_pyth_value, 3), round(offense, 3),
      round(defense, 3), qb, all_grade, off_vals[[team]], pblk_vals[[team]], recv_vals[[team]],
      run_vals[[team]], rblk_vals[[team]], def_vals[[team]], rdef_vals[[team]],
      tack_vals[[team]], prsh_vals[[team]], cov_vals[[team]]
    )
    report_frame[k, ] <- row
  }

  report_frame <- report_frame[rev(order(as.double(report_frame$Pyth))), ]
  pyth_med <- (as.double(report_frame$Pyth[16]) +
    as.double(report_frame$Pyth[17])) / 2
  report_frame <- report_frame[rev(order(as.double(report_frame$Off))), ]
  off_med <- (as.double(report_frame$Off[16]) +
    as.double(report_frame$Off[17])) / 2
  report_frame <- report_frame[order(as.double(report_frame$Def)), ]
  def_med <- (as.double(report_frame$Def[16]) +
    as.double(report_frame$Def[17])) / 2
  report_frame <- report_frame[rev(order(as.double(report_frame$QB))), ]
  qb_med <- (as.double(report_frame$QB[16]) +
    as.double(report_frame$QB[17])) / 2
  report_frame <- report_frame[rev(order(as.double(report_frame$PowerRating))), ]
  pr_med <- (as.double(report_frame$PowerRating[16]) +
    as.double(report_frame$PowerRating[17])) / 2

  for (index in 1:length(report_frame$PowerRating)) {
    team <- report_frame$Team[index]

    report_frame$PowerRating[index] <- round(PR_MULT *
      (as.double(report_frame$PowerRating[index]) - pr_med), 1)
    report_frame$Pyth[index] <- round(
      (as.double(report_frame$Pyth[index]) - pyth_med), 1)
    report_frame$Off[index] <- round(
      (as.double(report_frame$Off[index]) - off_med), 1)
    report_frame$Def[index] <- round(
      (as.double(report_frame$Def[index]) - def_med), 1)
    report_frame$QB[index] <- round(
      (as.double(report_frame$QB[index]) - qb_med), 1)
  }

  report_frame <- report_frame[rev(order(as.double(report_frame$PowerRating))), ]
  pr_med <- (as.double(report_frame$PowerRating[16]) +
    as.double(report_frame$PowerRating[17])) / 2
  for (index in 1:length(report_frame$PowerRating)) {
    report_frame$PowerRating[index] <- round(
      as.double(report_frame$PowerRating[index]) - pr_med, 2
    )
  }

  report_frame <- report_frame[rev(order(as.double(report_frame$PowerRating))), ]
  write.csv(report_frame, str_interp(
    "aEPA_Tests/season/overall/${year}/Power_${year}_week_${j}.csv"
  ),
  row.names = FALSE, quote = FALSE
  )

  write_totals(year, years_data, week)
}

write_totals <- function(year, years_data, week) {
  total_frame <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(total_frame) <- c("Team", "OffTotal", "DefTotal", "QBTotal", "TeamTotal")
  power <- read.csv(str_interp(
    "aEPA_Tests/season/overall/${year}/Power_${year}_week_${week}.csv"
  ))

  points <- round(get_average_total(years_data, year, week) / 2 - 1, 2)
  for (index in 1:length(power$PowerRating)) {
    team <- power$Team[index]
    offense <- as.double(power$Off[index])
    defense <- as.double(power$Def[index])
    qb <- as.double(power$QB[index])

    off_total <- (points + O_POINTS * offense)
    def_total <- (points - D_POINTS * defense)
    qb_total <- qb
    team_total <- get_team_scoring(years_data, team, year, week, points)
    row <- c(team, round(off_total, 1), round(def_total, 1), round(qb_total, 2), round(team_total, 1))
    total_frame[index, ] <- row
  }

  write.csv(total_frame, str_interp(
    "aEPA_Tests/season/total_rating/${year}/Total_${year}_${week}.csv"
  ),
  row.names = FALSE, quote = FALSE
  )
}

get_lines <- function(season, week, matchups) {
  print(str_interp("Getting Spreads for ${season} week ${week}..."))
  lines_frame <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(lines_frame) <- c("Away", "Line", "Home")

  file_week <- week - 1
  file_season <- season
  if (file_week == 0) {
    file_season <- season - 1
    file_week <- get_num_weeks(file_season) - 1
  }

  epa_frame <- read.csv(str_interp(
    "aEPA_Tests/season/overall/${file_season}/Power_${file_season}_week_${file_week}.csv"
  ))

  for (i in 1:length(matchups)) {
    matchup <- matchups[[i]]
    split <- strsplit(matchup, " ")
    away <- split[[1]]
    home <- split[[2]]

    away_rating <- as.double(epa_frame[
      epa_frame$Team == away, "PowerRating"
    ][[1]][[1]])
    home_rating <- as.double(epa_frame[
      epa_frame$Team == home, "PowerRating"
    ][[1]][[1]])

    diff <- abs(away_rating - home_rating)
    if (away_rating > home_rating) {
      diff <- round(diff * -1, 3)
    }

    diff <- round(diff + 1.4, 1)
    lines_frame[i, ] <- c(away, diff, home)
  }

  write.csv(lines_frame,
    str_interp("aEPA_Tests/season/picks/${season}/Week_${week}_sides.csv"),
    row.names = FALSE, quote = FALSE
  )
}

get_matchup_line <- function(season, week, home, away) {
  lines_frame <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(lines_frame) <- c("Away", "Line", "Home")

  file_week <- week - 1
  file_season <- season
  if (file_week == 0) {
    file_season <- season - 1
    file_week <- get_num_weeks(file_season)
  }

  file <- str_interp(
    "aEPA_Tests/season/overall/${file_season}/Power_${file_season}_week_${file_week}.csv"
  )
  epa_frame <- read.csv(str_interp(
    "aEPA_Tests/season/overall/${file_season}/Power_${file_season}_week_${file_week}.csv"
  ))

  away_rating <- as.double(epa_frame[
    epa_frame$Team == away, "PowerRating"
  ][[1]][[1]])
  home_rating <- as.double(epa_frame[
    epa_frame$Team == home, "PowerRating"
  ][[1]][[1]])

  diff <- abs(away_rating - home_rating)
  if (away_rating > home_rating) {
    diff <- round(diff * -1, 3)
  }

  diff <- diff + 1.4
  print(str_interp(
    "${away} ${diff} ${home}"
  ))
}

get_schedule <- function(season, week) {
  if (season > 2015) {
    return(scrape_schedule(season, week, get_num_weeks(season)))
  } else {
    return(scrape_schedule_espn(season, week, get_num_weeks(season)))
  }
}

get_team_byes <- function(season) {
  if (season > 2015) {
    return(get_byes(season, get_num_weeks(season)))
  } else {
    return(get_byes_espn(season, get_num_weeks(season)))
  }
}

sides_weight <- function(x, num_weeks) {
  # https://mycurvefit.com/
  # y = ax^b

  val <- round(1.15 - 1.1 * exp(1)^(-0.1 * x), 3) # 56.2
  if (val > 1) {
    val <- 1
  }

  return(val)
}

get_average_ppg <- function(season, week, teams) {
  WEEKS_SUMMED <- 20
  num_weeks <- get_num_weeks(season)

  current_year <- season - 1
  curr_year_weeks <- WEEKS_SUMMED - week
  if (WEEKS_SUMMED - week > num_weeks) {
    current_year <- season - 2
    curr_year_weeks <- WEEKS_SUMMED - num_weeks - week
  }

  num_weeks <- get_num_weeks(current_year)
  current_week <- num_weeks - curr_year_weeks

  years_data <- hash()
  for (i in current_year:season) {
    print(str_interp("... Loading ${i} data ..."))
    years_data[i] <- load_pbp(i)
  }

  total <- 0
  print("Processing PPG...")
  for (i in 0:WEEKS_SUMMED) {
    current_week <- current_week + 1
    if (current_week > num_weeks) {
      current_week <- i %% num_weeks
      num_weeks <- get_num_weeks(current_week)
    }

    total <- total + get_week_average_points(years_data[[
      toString(current_year)
    ]], current_year, current_week, teams)
  }

  return(round(total / WEEKS_SUMMED, 2))
}

get_totals <- function(season, week, matchups) {
  print(str_interp("Getting Totals for ${season} week ${week}..."))

  total_frame <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(total_frame) <- c("Away", "AwayTotal", "Home", "HomeTotal", "Total", "Spread")

  file_week <- week - 1
  file_season <- season
  if (file_week == 0) {
    file_season <- season - 1
    file_week <- get_num_weeks(file_season) - 1
  }

  totals_file <- read.csv(str_interp(
    "aEPA_Tests/season/total_rating/${file_season}/Total_${file_season}_${file_week}.csv"
  ))
  for (i in 1:length(matchups)) {
    matchup <- matchups[[i]]
    split <- strsplit(matchup, " ")
    away <- split[[1]]
    home <- split[[2]]

    away_o <- totals_file[totals_file$Team == away, "OffTotal"][[1]][[1]]
    away_d <- totals_file[totals_file$Team == away, "DefTotal"][[1]][[1]]
    away_q <- totals_file[totals_file$Team == home, "QBTotal"][[1]][[1]]
    away_t <- totals_file[totals_file$Team == away, "TeamTotal"][[1]][[1]]
    home_o <- totals_file[totals_file$Team == home, "OffTotal"][[1]][[1]]
    home_d <- totals_file[totals_file$Team == home, "DefTotal"][[1]][[1]]
    home_q <- totals_file[totals_file$Team == home, "QBTotal"][[1]][[1]]
    home_t <- totals_file[totals_file$Team == home, "TeamTotal"][[1]][[1]]

    away_half <- round(O_WEIGHT * away_o + D_WEIGHT * home_d + 
      away_q * Q_WEIGHT + (0.61 * away_t + 0.39 * home_t) * T_WEIGHT, 1)
    home_half <- round(O_WEIGHT * home_o + D_WEIGHT * away_d +
      home_q * Q_WEIGHT + (0.61 * home_t + 0.39 * away_t) * T_WEIGHT, 1)

    total <- round(away_half + home_half, 1)
    total_frame[nrow(total_frame) + 1, ] <- c(away, away_half, home,
      home_half, total, round(away_half - home_half, 1))
  }

  write.csv(total_frame, str_interp(
    "aEPA_Tests/season/totals/${season}/Total_${season}_${week}.csv"
  ),
  row.names = FALSE, quote = FALSE
  )
}

unit_weight <- function(x) {
  if (x >= 1.3 & x <= 2.1) {
    val <- 2
  } else if (x > 2.1 & x <= 2.6) {
    val <- 0.5
  } else if (x > 2.6 & x <= 3.2) {
    val <- 2
  } else if (x > 3.2 & x <= 5) {
    val <- 1.5
  } else {
    val <- 1
  }
}

get_game_sides <- function(years_data) {
  # the difference between my line and their line that indicates the
  # profit break-even point
  VALUE <- 1.3
  wins <- 0
  winnings <- 0
  losses <- 0

  report_frame <- data.frame(matrix(ncol = 12, nrow = 0))
  colnames(report_frame) <- c(
    "Season", "Week", "Away", "Home", "Result",
    "Away Score", "Home Score", "My Spread", "Spread", "Direction",
    "Difference", "Record"
  )
  num_rows <- 0
  num_less <- 0
  num_in <- 0
  num_greater <- 0
  total_bet <- 0
  for (season in 2020:2023) {
    print(stringr::str_interp("Picking for ${season}..."))
    num_weeks <- get_num_weeks(season)
    for (week in 1:num_weeks) {
      if (season == CURRENT_YEAR & week == CURRENT_WEEK + 1) {
        break
      }

      games <- fetch_week_games(years_data[[toString(season)]], week)
      picks <- read.csv(stringr::str_interp(
        "aEPA_Tests/season/picks/${season}/Week_${week}_sides.csv"
      ))

      for (i in 1:nrow(picks)) {
        num_rows <- num_rows + 1
        row <- picks[i, ]
        if (nrow(games[games$away == row[[1]], ]) == 0) {
          next
        }

        h_score <- games[games$away == row[[1]], "h_score"][[1]]
        a_score <- games[games$away == row[[1]], "a_score"][[1]]
        total <- games[games$away == row[[1]], "total"][[1]]
        spread <- games[games$away == row[[1]], "spread"][[1]]
        away_team <- games[games$away == row[[1]], "away"][[1]]
        home_team <- games[games$away == row[[1]], "home"][[1]]
        my_spread <- row[[2]]

        result <- ""
        direction <- ""
        difference <- 0
        if (is.na(my_spread)) {
          next
        }

        score_diff <- abs(my_spread - spread)
        if (score_diff >= VALUE & score_diff < 5.3) {
          result <- tryCatch(
            {
              game_result <- ""
              difference <- (a_score - h_score) * -1
              units <- round(unit_weight(score_diff), 2)
              total_bet <- total_bet + units
              if (my_spread > spread) {
                if (difference > spread) {
                  game_result <- units
                  direction <- "A"
                  wins <- wins + 1
                  winnings <- winnings + units
                } else if (difference < spread) {
                  game_result <- -1 * units
                  direction <- "A"
                  losses <- losses + 1
                  winnings <- winnings - units
                } else {
                  game_result <- "P"
                  direction <- "N"
                }
              } else if (my_spread <= spread) {
                difference <- (a_score - h_score) * -1
                if (difference < spread) {
                  game_result <- units
                  direction <- "H"
                  wins <- wins + 1
                  winnings <- winnings + units
                } else if (difference > spread) {
                  game_result <- -1 * units
                  direction <- "H"
                  losses <- losses + 1
                  winnings <- winnings - units
                } else {
                  game_result <- "P"
                  direction <- "N"
                }
              }

              report_frame[num_rows, ] <- c(
                season, week, away_team,
                home_team, game_result, a_score, h_score, my_spread,
                spread, direction, difference, ""
              )

              game_result <- game_result
            },
            error = function(cond) {
              print("the one postponed game")
            },
            warning = function(cond) {},
            finally = {}
          )
        }
      }
    }
  }

  w_l <- str_interp("${wins} - ${losses}")
  wp <- str_interp(
    "${round(wins / (wins + losses), 3) * 100}%"
  )
  profit <- str_interp("$${wins * 10 - losses * 11}")
  print(str_interp("Record: ${w_l}"))
  print(str_interp("Win Percentage: ${wp}"))
  print(str_interp("ROI: ${round(winnings / total_bet * 100, 1)}%"))
  print(str_interp("Total Profit: $${round(winnings * 10, 2)}"))

  report_frame[nrow(report_frame) + 1, ] <- c(
    "", "", "", "", "", "", "", "",
    "", w_l, wp, profit
  )
  write.csv(report_frame, str_interp(
    "aEPA_Tests/spread_report_${VALUE}_value.csv"
  ),
  row.names = FALSE, quote = FALSE
  )
}

get_game_sides_sweet <- function(years_data) {
  # the difference between my line and their line that indicates the
  # profit break-even point
  wins <- 0
  winnings <- 0
  losses <- 0
  total_bet <- 0

  report_frame <- data.frame(matrix(ncol = 12, nrow = 0))
  colnames(report_frame) <- c(
    "Season", "Week", "Away", "Home", "Result",
    "Away Score", "Home Score", "My Spread", "Spread", "Direction",
    "Difference", "Record"
  )
  num_rows <- 0
  num_less <- 0
  num_in <- 0
  num_greater <- 0
  for (season in 2019:2023) {
    num_weeks <- get_num_weeks(season)
    for (week in 1:num_weeks) {
      if (season == CURRENT_YEAR & week == CURRENT_WEEK + 1) {
        break
      }

      games <- fetch_week_games(years_data[[toString(season)]], week)
      picks <- read.csv(stringr::str_interp(
        "aEPA_Tests/season/picks/${season}/Week_${week}_sides.csv"
      ))

      for (i in 1:nrow(picks)) {
        num_rows <- num_rows + 1
        row <- picks[i, ]
        if (nrow(games[games$away == row[[1]], ]) == 0) {
          next
        }

        h_score <- games[games$away == row[[1]], "h_score"][[1]]
        a_score <- games[games$away == row[[1]], "a_score"][[1]]
        total <- games[games$away == row[[1]], "total"][[1]]
        spread <- games[games$away == row[[1]], "spread"][[1]]
        away_team <- games[games$away == row[[1]], "away"][[1]]
        home_team <- games[games$away == row[[1]], "home"][[1]]
        my_spread <- row[[2]]

        result <- ""
        direction <- ""
        difference <- 0
        if (is.na(my_spread)) {
          next
        }

        score_diff <- abs(my_spread - spread)
        if (score_diff >= 1.3 & score_diff <= 2.1) {
          result <- tryCatch(
            {
              game_result <- ""
              difference <- (a_score - h_score) * -1
              units <- round(unit_weight(score_diff), 2)
              total_bet <- total_bet + units
              if (my_spread > spread) {
                if (difference > spread) {
                  game_result <- units
                  direction <- "A"
                  wins <- wins + 1
                  winnings <- winnings + units
                } else if (difference < spread) {
                  game_result <- -1 * units
                  direction <- "A"
                  losses <- losses + 1
                  winnings <- winnings - units
                } else {
                  game_result <- "P"
                  direction <- "N"
                }
              } else if (my_spread <= spread) {
                difference <- (a_score - h_score) * -1
                if (difference < spread) {
                  game_result <- units
                  direction <- "H"
                  wins <- wins + 1
                  winnings <- winnings + units
                } else if (difference > spread) {
                  game_result <- -1 * units
                  direction <- "H"
                  losses <- losses + 1
                  winnings <- winnings - units
                } else {
                  game_result <- "P"
                  direction <- "N"
                }
              }

              report_frame[num_rows, ] <- c(
                season, week, away_team,
                home_team, game_result, a_score, h_score, my_spread,
                spread, direction, difference, ""
              )

              game_result <- game_result
            },
            error = function(cond) {
              print("the one postponed game")
            },
            warning = function(cond) {},
            finally = {}
          )
        }
      }
    }
  }

  w_l <- str_interp("${wins} - ${losses}")
  wp <- str_interp(
    "${round(wins / (wins + losses), 3) * 100}%"
  )
  profit <- str_interp("$${wins * 10 - losses * 11}")
  print("Sweet Spot:")
  print(w_l)
  print(str_interp("ROI: ${round(winnings / total_bet * 100, 1)}%"))
  print(str_interp("Win Percentage: ${wp}"))
  print(str_interp("Total Profit: $${round(winnings * 10, 2)}"))
}

get_total_results <- function(years_data) {
  # the difference between my line and their line that indicates the
  # profit break-even point
  VALUE <- 1.6
  wins <- 0
  losses <- 0
  winnings <- 0

  report_frame <- data.frame(matrix(ncol = 10, nrow = 0))
  colnames(report_frame) <- c(
    "Season", "Week", "Away", "Home", "Result",
    "My Total", "Total", "Direction", "Difference", "Record"
  )
  num_rows <- 0
  num_less <- 0
  num_in <- 0
  num_greater <- 0
  total_bet <- 0
  for (season in 2020:2023) {
    print(stringr::str_interp("Picking for ${season}..."))
    num_weeks <- get_num_weeks(season)
    for (week in 1:num_weeks) {
      if (season == CURRENT_YEAR & week == CURRENT_WEEK + 1) {
        break
      }

      games <- fetch_week_games(years_data[[toString(season)]], week)
      picks <- read.csv(stringr::str_interp(
        "aEPA_Tests/season/totals/${season}/Total_${season}_${week}.csv"
      ))

      for (i in 1:nrow(picks)) {
        num_rows <- num_rows + 1
        row <- picks[i, ]
        split <- strsplit(row[[1]], " ")
        if (nrow(games[games$away == split[[1]], ]) == 0) {
          next
        }

        total <- games[games$away == split[[1]], "total"][[1]]
        their_line <- games[games$away == split[[1]], "line"][[1]]
        away_team <- games[games$away == split[[1]], "away"][[1]]
        home_team <- games[games$away == split[[1]], "home"][[1]]
        my_total <- row[[2]]

        result <- ""
        direction <- ""
        difference <- 0
        if (is.na(my_total)) {
          next
        }

        score_diff <- abs(my_total - their_line)
        if (score_diff >= VALUE) {
          result <- tryCatch(
            {
              game_result <- ""
              difference <- round(total - their_line, 1)
              units <- round(unit_weight(score_diff), 2)
              total_bet <- total_bet + units
              if (my_total > their_line) {
                if (difference > 0) {
                  game_result <- units
                  direction <- "O"
                  wins <- wins + 1
                  winnings <- winnings + units
                } else if (difference < 0) {
                  game_result <- -1 * units
                  direction <- "O"
                  losses <- losses + 1
                  winnings <- winnings - units
                } else {
                  game_result <- "P"
                  direction <- "N"
                }
              } else if (my_total <= their_line) {
                if (difference < 0) {
                  game_result <- units
                  direction <- "U"
                  wins <- wins + 1
                  winnings <- winnings + units
                } else if (difference > 0) {
                  game_result <- -1 * units
                  direction <- "U"
                  losses <- losses + 1
                  winnings <- winnings - units
                } else {
                  game_result <- "P"
                  direction <- "N"
                }
              }

              report_frame[num_rows, ] <- c(
                season, week, away_team,
                home_team, game_result, my_total,
                total, direction, difference, ""
              )

              game_result <- game_result
            },
            error = function(cond) {
              print("the one postponed game")
              print(cond)
            },
            warning = function(cond) {},
            finally = {}
          )
        }
      }
    }
  }

  w_l <- str_interp("${wins} - ${losses}")
  wp <- str_interp(
    "${round(wins / (wins + losses), 3) * 100}%"
  )
  profit <- str_interp("$${wins * 10 - losses * 11}")
  print(str_interp("Record: ${w_l}"))
  print(str_interp("Win Percentage: ${wp}"))
  print(str_interp("ROI: ${round(winnings / total_bet * 100, 1)}%"))
  print(str_interp("Total Profit: $${round(winnings * 10, 2)}"))

  report_frame[nrow(report_frame) + 1, ] <- c(
    "", "", "", "", "", "",
    "", w_l, wp, profit
  )
  write.csv(report_frame, str_interp(
    "aEPA_Tests/total_report_${VALUE}_value.csv"
  ),
  row.names = FALSE, quote = FALSE
  )
}

round_to_half <- function(value) {
  return(as.integer(value * 2) / 2)
}

get_team_scoring <- function(years_data, team, year, nfl_week, average) {
  this_year <- 0
  average <- get_average_total(years_data, year, nfl_week)
  this_weight <- nfl_week / 9
  if (this_weight > 1) {
    this_weight <- 1
  }

  now_average <- years_data[[toString(year)]] %>%
    filter(week <= nfl_week & posteam == team) %>%
    select(game_id, total, posteam_type, away_score, home_score) %>%
    group_by(game_id) %>%
    slice(n())

  total_score <- 0
  avg_matrix <- as.matrix(now_average)
  for (i in seq(1:nrow(avg_matrix))) {
    row <- avg_matrix[i, ]
    if (row[[3]] == "home") {
      week_score <- as.numeric(row[[5]])
    } else {
      week_score <- as.numeric(row[[4]])
    }

    total_score <- total_score + week_score
  }

  this_year <- total_score / nfl_week
  return(this_year * this_weight + (average * (1 - this_weight) / 2))
}

get_average_total <- function(years_data, year, nfl_week) {
  num_seasons <- 2
  total <- 0
  if (nfl_week > 1) {
    now_average <- years_data[[toString(year)]] %>%
      filter(week <= nfl_week) %>%
      select(game_id, total) %>%
      group_by(game_id) %>%
      slice(n())
    total <- mean(now_average$total)
    num_seasons <- num_seasons + 1
  }

  old_average <- years_data[[toString(year - 2)]] %>%
    filter(week <= get_num_weeks(year - 2)) %>%
    select(game_id, total) %>%
    group_by(game_id) %>%
    slice(n())
  total <- total + mean(old_average$total)

  last_average <- years_data[[toString(year - 1)]] %>%
    filter(week <= get_num_weeks(year - 1)) %>%
    select(game_id, total) %>%
    group_by(game_id) %>%
    slice(n())
  total <- total + mean(last_average$total)

  return(round_to_half(total / num_seasons))
}

teams <- c(
  "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL",
  "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LV", "LAC", "LA", "MIA",
  "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN",
  "WAS"
)

NEW_QB <- hash()
for (team in teams) {
  NEW_QB[team] <- FALSE
}

OFF_STR <- "off"
PASS_STR <- "qb"
PBLK_STR <- "pblk"
RECV_STR <- "recv"
RUN_STR <- "run"
RBLK_STR <- "rblk"
DEF_STR <- "def"
RDEF_STR <- "rdef"
TACK_STR <- "tack"
PRSH_STR <- "prsh"
COV_STR <- "cov"

# Power Ranking Modifiers
PR_MULT <- 1.8
GRADE_MULT <- 80
PYTH_PCT <- 0.1
DEF_PCT <- 0.36
GRADE_PCT <- 0.04
OFF_QB_PCT <- 0.5
OFF_QB_RATIO <- 0.6
OFF_GRADE_PCT <- 0.17
PBLK_PCT <- 0.15
RECV_PCT <- 0.08
RUN_PCT <- 0.14
RBLK_PCT <- 0.13
DEF_GRADE_PCT <- 0.05
RDEF_PCT <- 0.03
TACK_PCT <- 0.13
PRSH_PCT <- 0.19
COV_PCT <- 0.17
RECENT_WEIGHT <- 0.13
RECENT_GRADE_WEIGHT <- 0.17
RECENT_INTERVAL <- 5
PYTH_WEIGHT <- 0.69
WP_WEIGHT <- 0.31
GRADE_REDUCTION <- 0.0025

OFF_ADJ <- 0.14
DEF_ADJ <- 0.3

# Def Game Adjs
DPEPA <- 0.5
DREPA <- 0.07
DPSR <- 1.06
DRSR <- 0.3
AVG_DPSR <- 0.455
AVG_DRSR <- 0.415

# Off Game Adjs
OPEPA <- 0.35
OREPA <- 0.04
OPSR <- 1.69
ORSR <- 1.27
AVG_OPSR <- 0.445
AVG_ORSR <- 0.4

# QB Stats
PFF_WEIGHT <- 0.94
EPA_WEIGHT <- 0.0
CPOE_WEIGHT <- 0.06
CPOE_REDUCTION <- 0.025
QB_RECENT_WEIGHT <- 0.18
QB_GRADE_REDUCTION <- 0.008

# Totals
T_WEIGHT <- 0.5
O_D_RATIO <- 0.61 / 0.39
D_WEIGHT <- (1 - T_WEIGHT) / (1 + O_D_RATIO)
O_WEIGHT <- D_WEIGHT * O_D_RATIO
Q_WEIGHT <- 0.4
O_POINTS <- 2.2
D_POINTS <- 1.6

NEW_QB_WEIGHT <- 0.07

grade_cat_weight <- function(x) {
  return(0.21 * x^0.63)
}

test_weight <- function(x) {
  return(0.18 * x^0.65)
}

###############################################################
# Make sure to update EPA/Total data before running new lines #
###############################################################

years_data <- hash()
for (i in 2021:2024) {
  print(str_interp("... Loading ${i} data ..."))
  years_data[i] <- nflreadr::load_pbp(i)
}

# matchups <- get_schedule(2024, 1)
# fg_prediction(2024, years_data, 1)
# fgs <- as.double(get_average_fgs(years_data[[toString(2024)]], 2024)$makes)
# tds <- as.double(get_average_tds(years_data[[toString(2024)]], 2024)$touchdowns)
# results <- compose_fg_totals(2024, a_week, matchups, fgs, tds)
# avg_fg <- avg_fg + results[[1]]
# avg_td <- avg_td + results[[2]]

# for (year in 2020:2023) {
#     num_weeks <- get_num_weeks(year)
#     avg_fg <- 0
#     avg_td <- 0
#     fgs <- as.double(get_average_fgs(years_data[[toString(year)]], year)$makes)
#     tds <- as.double(get_average_tds(years_data[[toString(year)]], year)$touchdowns)
#     print(str_interp("Running ${year}..."))
#     for (a_week in 1:num_weeks) {
#         if (year == 2017 && a_week == 1) {
#             next
#         }

#         matchups <- get_schedule(year, a_week)
#         fg_prediction(year, years_data, a_week)
#         results <- compose_fg_totals(year, a_week, matchups, fgs, tds)
#         avg_fg <- avg_fg + results[[1]]
#         avg_td <- avg_td + results[[2]]
#         # get_pr(years_data, teams, year, a_week, matchups)
#         # write_power_rank_files(year, years_data, a_week,
#         #     get_team_byes(year))
#         # get_lines(year, a_week, matchups)
#         # get_totals(year, a_week, matchups)
#     }

#     print("FG Comparison:")
#     print(fgs)
#     avg_fg <- round(avg_fg / num_weeks, 2)
#     print(avg_fg)
#     print("TD Comparison:")
#     print(tds)
#     avg_td <- round(avg_td / num_weeks, 2)
#     print(avg_td)
# }

# get_game_sides(years_data)
# get_game_sides_sweet(years_data)

# get_total_results(years_data)

CURRENT_YEAR <- 2024
QB_FILE_YEAR <- CURRENT_YEAR
# this is the week for which to generate odds
CURRENT_WEEK <- 2
BACKTESTING <- FALSE

NEW_QB["MIN"] <- TRUE
NEW_QB["DEN"] <- TRUE
NEW_QB["NE"] <- TRUE
NEW_QB["WAS"] <- TRUE
NEW_QB["NYJ"] <- TRUE
NEW_QB["PIT"] <- TRUE
NEW_QB["CHI"] <- TRUE
NEW_QB["ATL"] <- TRUE
NEW_QB["ARI"] <- TRUE
NEW_QB["CIN"] <- TRUE

# Ctrl + Shift + S to execute

# WEEKLY UPDATES
# MAKE SURE YOU'RE USING THE GRADE FOR THESE
# matchups <- get_schedule(CURRENT_YEAR, CURRENT_WEEK - 1)
# # get_pr(years_data, teams, CURRENT_YEAR, CURRENT_WEEK - 1, matchups)
# write_power_rank_files(
#   CURRENT_YEAR, years_data, CURRENT_WEEK - 1,
#   get_team_byes(year)
# )
matchups <- get_schedule(CURRENT_YEAR, CURRENT_WEEK)
# get_lines(CURRENT_YEAR, CURRENT_WEEK, matchups)
# get_totals(CURRENT_YEAR, CURRENT_WEEK, matchups)

# if doing this for a new year, do CURRENT_YEAR - 1, 18
fgs <- round(
  as.double(meld_average_fgs(years_data, CURRENT_YEAR, CURRENT_WEEK - 1)), 2)
tds <- round(
  as.double(meld_average_tds(years_data, CURRENT_YEAR, CURRENT_WEEK - 1)), 2)

fg_prediction(CURRENT_YEAR, years_data, CURRENT_WEEK)
results <- compose_fg_totals(CURRENT_YEAR, CURRENT_WEEK, matchups, fgs, tds)
print(fgs)
print(results[[1]])
print(tds)
print(results[[2]])


# fg_prediction(CURRENT_YEAR, years_data, 1)

# year <- 2023
# num_weeks <- get_num_weeks(year)
# for (a_week in 2:18) {
#     fgs <- round(
#       as.double(meld_average_fgs(years_data, year, a_week - 1)), 2)
#     tds <- round(
#       as.double(meld_average_tds(years_data, year, a_week - 1)), 2)

#     fg_prediction(year, years_data, a_week)
#     results <- compose_fg_totals(year, a_week, matchups, fgs, tds)
#     print(fgs)
#     print(results[[1]])
#     print(tds)
#     print(results[[2]])
#     print("")
# }

kill_driver()
