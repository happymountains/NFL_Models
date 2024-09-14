source("stats.r")
source("adj_epa_model.r")

team_spread_win_percentage <- function(data, num_weeks) {
    wins_dict <- hash()
    losses_dict <- hash()
    wp_dict <- hash()
    games_dict <- hash()

    for (i in 1:num_weeks) {
        filtered_data <- data  %>%
            filter(week == i)  %>%
            select(game_id, home_team, home_score, away_team,
                away_score, spread_line)  %>%
            group_by(game_id)  %>%
            slice(n())

        num_rows <- nrow(filtered_data)
        game_list <- split(filtered_data, 1:num_rows)
        for (game in game_list) {
            home <- game$home_team
            away <- game$away_team
            difference <- game$home_score - game$away_score
            spread <- game$spread_line

            if (!has.key(home, wins_dict)) {
                wins_dict[[home]] <- 0
                losses_dict[[home]] <- 0
                games_dict[[home]] <- 0
            }

            if (!has.key(away, wins_dict)) {
                wins_dict[[away]] <- 0
                losses_dict[[away]] <- 0
                games_dict[[away]] <- 0
            }

            if (has.key(home, wins_dict)) {
                if (difference - spread > 0) {
                    wins_dict[[home]] <- wins_dict[[home]] + 1
                    losses_dict[[away]] <- losses_dict[[away]] + 1
                } else if (difference - spread < 0) {
                    losses_dict[[home]] <- losses_dict[[home]] + 1
                    wins_dict[[away]] <- wins_dict[[away]] + 1
                } else {
                    wins_dict[[home]] <- wins_dict[[home]] + 0.5
                    losses_dict[[home]] <- losses_dict[[home]] + 0.5
                    wins_dict[[away]] <- wins_dict[[away]] + 0.5
                    losses_dict[[away]] <- losses_dict[[away]] + 0.5
                }

                games_dict[[home]] <- games_dict[[home]] + 1
                games_dict[[away]] <- games_dict[[away]] + 1
            }
        }
    }

    for (key in keys(wins_dict)) {
        wp_dict[[key]] <- round((wins_dict[[key]] / games_dict[[key]]), 3)
    }

    return(wp_dict)
}

team_total_win_percentage <- function(data, num_weeks) {
    wins_dict <- hash()
    losses_dict <- hash()
    wp_dict <- hash()
    games_dict <- hash()

    for (i in 1:num_weeks) {
        filtered_data <- data  %>%
            filter(week == i)  %>%
            select(game_id, home_team, home_score, away_team,
                away_score, total_line)  %>%
            group_by(game_id)  %>%
            slice(n())

        num_rows <- nrow(filtered_data)
        game_list <- split(filtered_data, 1:num_rows)
        for (game in game_list) {
            home <- game$home_team
            away <- game$away_team
            score <- game$home_score + game$away_score
            total <- game$total_line

            if (!has.key(home, wins_dict)) {
                wins_dict[[home]] <- 0
                losses_dict[[home]] <- 0
                games_dict[[home]] <- 0
            }

            if (!has.key(away, wins_dict)) {
                wins_dict[[away]] <- 0
                losses_dict[[away]] <- 0
                games_dict[[away]] <- 0
            }

            if (has.key(home, wins_dict)) {
                if (score - total > 0) {
                    wins_dict[[home]] <- wins_dict[[home]] + 1
                    wins_dict[[away]] <- wins_dict[[away]] + 1
                } else if (score - total < 0) {
                    losses_dict[[home]] <- losses_dict[[home]] + 1
                    losses_dict[[away]] <- losses_dict[[away]] + 1
                } else {
                    wins_dict[[home]] <- wins_dict[[home]] + 0.5
                    losses_dict[[home]] <- losses_dict[[home]] + 0.5
                    wins_dict[[away]] <- wins_dict[[away]] + 0.5
                    losses_dict[[away]] <- losses_dict[[away]] + 0.5
                }

                games_dict[[home]] <- games_dict[[home]] + 1
                games_dict[[away]] <- games_dict[[away]] + 1
            }
        }
    }

    for (key in keys(wins_dict)) {
        wp_dict[[key]] <- round((wins_dict[[key]] / games_dict[[key]]), 3)
    }

    return(wp_dict)
}

spread_vs_pyth <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    pyth <- team_pythagorean_projection(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)

    teams <- keys(pyth)
    pyth_values <- vector(, 32)
    spread_values <- vector(, 32)

    for (i in 1:length(teams)) {
        team <- teams[i]
        pyth_values[i] <- pyth[[team]]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(team, pyth_values, spread_values)
    ggplot(graph_df, aes(x = pyth_values,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_pyth <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    pyth <- team_pythagorean_projection(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)

    teams <- keys(pyth)
    pyth_values <- vector(, 32)
    total_values <- vector(, 32)

    for (i in 1:length(teams)) {
        team <- teams[i]
        pyth_values[i] <- pyth[[team]]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(team, pyth_values, total_values)
    ggplot(graph_df, aes(x = pyth_values,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_sos <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    sos <- team_sos(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)
    teams <- keys(sos)
    sos_values <- vector(, 32)
    spread_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        sos_values[i] <- sos[[team]]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(team, sos_values, spread_values)
    ggplot(graph_df, aes(x = sos_values,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_sos <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    sos <- team_sos(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(sos)
    sos_values <- vector(, 32)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        sos_values[i] <- sos[[team]]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(team, sos_values, total_values)
    ggplot(graph_df, aes(x = sos_values,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_def_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    depa <- fetch_def_epa(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)
    teams <- keys(spread)
    spread_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(teams, depa$epa_per_play, spread_values)
    ggplot(graph_df, aes(x = depa$epa_per_play,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_def_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    depa <- fetch_def_epa(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, depa$epa_per_play, total_values)
    ggplot(graph_df, aes(x = depa$epa_per_play,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

pyth_vs_def_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    depa <- fetch_def_epa(data, num_weeks)
    total <- team_pythagorean_projection(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, depa$epa_per_play, total_values)
    ggplot(graph_df, aes(x = depa$epa_per_play,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_def_pass_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    dpepa <- fetch_def_pass_epa(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)
    teams <- keys(spread)
    spread_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(teams, dpepa$epa_per_play, spread_values)
    ggplot(graph_df, aes(x = dpepa$epa_per_play,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

pyth_vs_def_pass_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    dpepa <- fetch_def_pass_epa(data, num_weeks)
    pyth <- team_pythagorean_projection(data, num_weeks)
    teams <- keys(pyth)
    pyth_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        pyth_values[i] <- pyth[[team]]
    }

    graph_df <- data.frame(teams, dpepa$epa_per_play, pyth_values)
    ggplot(graph_df, aes(x = dpepa$epa_per_play,
            y = pyth_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_def_run_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    drepa <- fetch_def_run_epa(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)
    teams <- keys(spread)
    spread_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(teams, drepa$epa_per_play, spread_values)
    ggplot(graph_df, aes(x = drepa$epa_per_play,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_def_run_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    drepa <- fetch_def_run_epa(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, drepa$epa_per_play, total_values)
    ggplot(graph_df, aes(x = drepa$epa_per_play,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

pyth_vs_def_run_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    drepa <- fetch_def_run_epa(data, num_weeks)
    total <- team_pythagorean_projection(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, drepa$epa_per_play, total_values)
    ggplot(graph_df, aes(x = drepa$epa_per_play,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_def_pass_success_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    dpsr <- fetch_def_pass_success_rate(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)
    teams <- keys(spread)
    spread_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(teams, dpsr$success_rate, spread_values)
    ggplot(graph_df, aes(x = dpsr$success_rate,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_def_pass_success_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    dpsr <- fetch_def_pass_success_rate(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, dpsr$success_rate, total_values)
    ggplot(graph_df, aes(x = dpsr$success_rate,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

pyth_vs_def_pass_success_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    dpsr <- fetch_def_pass_success_rate(data, num_weeks)
    total <- team_pythagorean_projection(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, dpsr$success_rate, total_values)
    ggplot(graph_df, aes(x = dpsr$success_rate,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_def_run_success_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    dpsr <- fetch_def_run_success_rate(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)
    teams <- keys(spread)
    spread_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(teams, dpsr$success_rate, spread_values)
    ggplot(graph_df, aes(x = dpsr$success_rate,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_def_run_success_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    dpsr <- fetch_def_run_success_rate(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, dpsr$success_rate, total_values)
    ggplot(graph_df, aes(x = dpsr$success_rate,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    oepa <- fetch_team_epa(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)
    teams <- keys(spread)
    spread_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(teams, oepa$epa_per_play, spread_values)
    ggplot(graph_df, aes(x = oepa$epa_per_play,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    oepa <- fetch_team_epa(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, oepa$epa_per_play, total_values)
    ggplot(graph_df, aes(x = oepa$epa_per_play,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_pass_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    opepa <- fetch_pass_epa(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)
    teams <- keys(spread)
    spread_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(teams, opepa$epa_per_play, spread_values)
    ggplot(graph_df, aes(x = opepa$epa_per_play,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_pass_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    opepa <- fetch_pass_epa(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, opepa$epa_per_play, total_values)
    ggplot(graph_df, aes(x = opepa$epa_per_play,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_run_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    orepa <- fetch_run_epa(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)
    teams <- keys(spread)
    spread_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(teams, orepa$epa_per_play, spread_values)
    ggplot(graph_df, aes(x = orepa$epa_per_play,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_run_epa <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    orepa <- fetch_run_epa(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, orepa$epa_per_play, total_values)
    ggplot(graph_df, aes(x = orepa$epa_per_play,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_pass_success_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    psr <- fetch_pass_success_rate(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)
    teams <- keys(spread)
    spread_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(teams, psr$success_rate, spread_values)
    ggplot(graph_df, aes(x = psr$success_rate,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_pass_success_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    psr <- fetch_pass_success_rate(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, psr$success_rate, total_values)
    ggplot(graph_df, aes(x = psr$success_rate,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_run_success_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    rsr <- fetch_run_success_rate(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)
    teams <- keys(spread)
    spread_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(teams, rsr$success_rate, spread_values)
    ggplot(graph_df, aes(x = rsr$success_rate,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_run_success_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    rsr <- fetch_run_success_rate(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, rsr$success_rate, total_values)
    ggplot(graph_df, aes(x = rsr$success_rate,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_explosive_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    epr <- fetch_explosive_play_rate(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)
    teams <- keys(spread)
    spread_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(teams, epr$explosive_rate, spread_values)
    ggplot(graph_df, aes(x = epr$explosive_rate,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_explosive_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    epr <- fetch_explosive_play_rate(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, epr$explosive_rate, total_values)
    ggplot(graph_df, aes(x = epr$explosive_rate,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_success_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    success <- fetch_success_rate(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, success$success_rate, total_values)
    ggplot(graph_df, aes(x = success$success_rate,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_def_success_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    success <- fetch_def_success_rate(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, success$success_rate, total_values)
    ggplot(graph_df, aes(x = success$success_rate,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_def_explosive_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    epr <- fetch_def_explosive_play_rate(data, num_weeks)
    spread <- team_spread_win_percentage(data, num_weeks)
    teams <- keys(spread)
    spread_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        spread_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(teams, epr$explosive_rate, spread_values)
    ggplot(graph_df, aes(x = epr$explosive_rate,
            y = spread_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_def_explosive_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    epr <- fetch_def_explosive_play_rate(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, epr$explosive_rate, total_values)
    ggplot(graph_df, aes(x = epr$explosive_rate,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

spread_vs_adjusted_oepa <- function(years_data, teams, season, num_weeks) {
    adj_epa <- get_adjusted_off_epa(years_data, teams, season, num_weeks)

    data <- load_pbp(season)
    spread <- team_spread_win_percentage(data, num_weeks)

    s_teams <- keys(adj_epa)
    s_values <- vector(, 32)
    e_values <- vector(, 32)

    for (i in 1:length(s_teams)) {
        team <- s_teams[i]
        e_values[i] <- adj_epa[[team]]
        s_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(team, s_values, e_values)
    print(ggplot(graph_df, aes(x = s_values,
            y = e_values)) +
        geom_text_repel(aes(label = s_teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("SvAOE: ${season}")) +
        xlab("Spread Win %") + ylab("Opponent-Adjusted EPA"))
}

spread_vs_adjusted_depa <- function(years_data, teams, season, num_weeks) {
    adj_epa <- get_adjusted_def_epa(years_data, teams, season, num_weeks)

    data <- years_data[[toString(season)]]
    spread <- team_spread_win_percentage(data, num_weeks)
    depa <- fetch_def_epa(data, num_weeks)

    s_teams <- keys(adj_epa)
    s_values <- vector(, 32)
    e_values <- vector(, 32)

    for (i in 1:length(s_teams)) {
        team <- s_teams[i]
        e_values[i] <- adj_epa[[team]]
        s_values[i] <- spread[[team]]
    }

    graph_df <- data.frame(teams, s_values, e_values)
    print(ggplot(graph_df, aes(x = s_values,
            y = e_values)) +
        geom_text_repel(aes(label = s_teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("SvADE: ${season}")) +
        xlab("Spread Win %") + ylab("Opponent-Adjusted Defensive EPA"))
}

total_vs_aoepa <- function(data, week) {
    team_total_wp <- team_total_win_percentage(data, week)
    aoepa <- read.csv(str_interp(
        "aEPA_Tests/season/offense/AOEPA_${i}.csv"))

    t_teams <- keys(team_total_wp)
    t_values <- vector(, 32)
    for (i in 1:length(t_teams)) {
        team <- t_teams[i]
        t_values[i] <- team_total_wp[[team]]
    }

    graph_df <- data.frame(t_teams, t_values, aoepa$A_EPA)
    print(ggplot(graph_df, aes(x = t_values,
            y = aoepa$A_EPA)) +
        geom_text_repel(aes(label = t_teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("TvAOE: ${season}")) +
        xlab("Total Win %") + ylab("Opponent-Adjusted Offensive EPA"))
}

total_vs_adepa <- function(data, week) {
    team_total_wp <- team_total_win_percentage(data, week)
    adepa <- read.csv(str_interp(
        "aEPA_Tests/season/defense/ADEPA_${i}.csv"))

    t_teams <- keys(team_total_wp)
    t_values <- vector(, 32)
    for (i in 1:length(t_teams)) {
        team <- t_teams[i]
        t_values[i] <- team_total_wp[[team]]
    }

    graph_df <- data.frame(t_teams, t_values, adepa$A_EPA)
    print(ggplot(graph_df, aes(x = t_values,
            y = adepa$A_EPA)) +
        geom_text_repel(aes(label = t_teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("TvAOE: ${season}")) +
        xlab("Total Win %") + ylab("Opponent-Adjusted Defensive EPA"))
}

total_vs_tr <- function(data, week) {
    team_total_wp <- team_total_win_percentage(data, week)
    tr_frame <- read.csv(str_interp(
        "aEPA_Tests/season/totals/TR_${i}.csv"))

    t_teams <- keys(team_total_wp)
    t_values <- vector(, 32)
    for (i in 1:length(t_teams)) {
        team <- t_teams[i]
        t_values[i] <- team_total_wp[[team]]
    }

    graph_df <- data.frame(t_teams, t_values, tr_frame$tr_vec)
    print(ggplot(graph_df, aes(x = t_values,
            y = tr_frame$tr_vec)) +
        geom_text_repel(aes(label = t_teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("TvTR: ${season}")) +
        xlab("Total Win %") + ylab("Total Rating"))
}

spread_vs_aepa <- function(year, data, week) {
    team_spread_wp <- team_spread_win_percentage(data, week)
    aepa <- read.csv(str_interp(
        "aEPA_Tests/season/overall/AEPA_${year}.csv"))

    t_teams <- keys(team_spread_wp)
    t_values <- vector(, 32)
    for (i in 1:length(t_teams)) {
        team <- t_teams[i]
        t_values[i] <- team_spread_wp[[team]]
    }

    graph_df <- data.frame(t_teams, t_values, aepa$A_EPA)
    print(ggplot(graph_df, aes(x = t_values,
            y = aepa$A_EPA)) +
        geom_text_repel(aes(label = t_teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("TvAOE: ${season}")) +
        xlab("Spread Win %") + ylab("Opponent-Adjusted EPA"))
}

pyth_vs_aepa <- function(year, data, week) {
    team_pyth_wp <- team_pythagorean_wp(data, week)
    aepa <- read.csv(str_interp(
        "aEPA_Tests/season/overall/AEPA_${year}.csv"))

    t_teams <- keys(team_pyth_wp)
    t_values <- vector(, 32)
    for (i in 1:length(t_teams)) {
        team <- t_teams[i]
        t_values[i] <- team_pyth_wp[[team]]
    }

    graph_df <- data.frame(t_teams, t_values, aepa$A_EPA)
    print(ggplot(graph_df, aes(x = t_values,
            y = aepa$A_EPA)) +
        geom_text_repel(aes(label = t_teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("TvAOE: ${season}")) +
        xlab("Spread Win %") + ylab("Opponent-Adjusted EPA"))
}

total_vs_off_pace <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    pace <- team_off_pace(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, pace$plays, total_values)
    ggplot(graph_df, aes(x = pace$plays,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_def_pace <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    pace <- team_def_pace(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, pace$plays, total_values)
    ggplot(graph_df, aes(x = pace$plays,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_pass_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    pass_rate <- fetch_season_pass_rate(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, pass_rate$mean_pass, total_values)
    ggplot(graph_df, aes(x = pass_rate$mean_pass,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_run_rate <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    run_rate <- fetch_season_run_rate(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, run_rate$mean_run, total_values)
    ggplot(graph_df, aes(x = run_rate$mean_run,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_cpoe <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    team_cpoe <- fetch_team_cpoe(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, team_cpoe$cpoe, total_values)
    ggplot(graph_df, aes(x = team_cpoe$cpoe,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

total_vs_def_adot <- function(season, num_weeks) {
    data <- load_pbp(season)
    
    team_adot <- fetch_def_adot(data, num_weeks)
    total <- team_total_win_percentage(data, num_weeks)
    teams <- keys(total)
    total_values <- vector(, 32)
    for (i in 1:length(teams)) {
        team <- teams[i]
        total_values[i] <- total[[team]]
    }

    graph_df <- data.frame(teams, team_adot$ADOT, total_values)
    ggplot(graph_df, aes(x = team_adot$ADOT,
            y = total_values)) +
        geom_text_repel(aes(label = teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle(str_interp("Test: ${season}"))
}

print_pdf <- function() {
    num_weeks <- 17
    pdf(str_interp("aEPA_Tests/TestCorrelations/PvDPSR_Correlation.pdf"))
    for (year in 2006:2021) {
        if (year >= 2021) {
            num_weeks <- 18
        }

        print(year)
        print(pyth_vs_def_pass_success_rate(year, num_weeks))
    }

    dev.off()
}

print_model_pdfs <- function(years_data) {
    num_weeks <- 17
    pdf(str_interp("aEPA_Tests/TestCorrelations/PvDEPA_Correlation.pdf"))
    for (year in 2006:2021) {
        if (year >= 2021) {
            num_weeks <- 18
        }

        pyth_vs_def_epa(year, years_data[[toString(year)]], num_weeks)
    }

    dev.off()
}

teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL",
    "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LV", "LAC", "LA", "MIA",
    "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN",
    "WAS")

# years_data <- hash()
# for (i in 2006:2021) {
#     print(str_interp("... Loading ${i} data ..."))
#     years_data[i] <- load_pbp(i)
# }

# print_model_pdfs(years_data)
print_pdf()