library(dplyr)
library(ggimage)
library(ggpubr)
library(ggrepel)
library(hash)
library(miscTools)
library(nflfastR)
library(nflreadr)
library(stringr)

# setwd(str_interp("${dirname(getwd())}\\Programming Projects\\NFL"))

options(scipen = 9999)
options(dplyr.summarise.inform = FALSE)

WP_LOW <- 0.05
WP_HIGH <- 0.95
RECENT_INTERVAL <- 5

pass_filter <- function(data) {
    if (is.null(data)) {
        return(NULL)
    }
    
    data1 <- data %>%
        filter(down <= 4 & qtr <= 2) %>%
        filter(pass == 1 & play_type != "no_play")
    
    data2 <- data %>%
        filter(wp >= WP_LOW & wp <= WP_HIGH & down <= 4 &
            (qtr == 3 | qtr == 4)) %>%
        filter(pass == 1 & play_type != "no_play")

    return(rbind(data1, data2))
}

run_filter <- function(data) {
    if (is.null(data)) {
        return(NULL)
    }
    
    data1 <- data %>%
        filter(down <= 4 & qtr <= 2) %>%
        filter(rush == 1 & play_type != "no_play")
    
    data2 <- data %>%
        filter(wp >= WP_LOW & wp <= WP_HIGH & down <= 4 &
            (qtr == 3 | qtr == 4)) %>%
        filter(rush == 1 & play_type != "no_play")

    return(rbind(data1, data2))
}

##### Game Stats #####

fetch_game_pass_epa <- function(data, nfl_week, team) {
    filtered_data <- pass_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week == nfl_week, posteam == team) %>%
        summarize(
            plays = n(),
            value = mean(epa, na.rm = TRUE)
        )  %>%

    return(epa_per_play_data)
}

fetch_game_run_epa <- function(data, nfl_week, team) {
    filtered_data <- run_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week == nfl_week, posteam == team) %>%
        summarize(
            plays = n(),
            value = mean(epa, na.rm = TRUE)
        )  %>%

    return(epa_per_play_data)
}

fetch_game_pass_success_rate <- function(data, nfl_week, team) {
    filtered_data <- pass_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week == nfl_week, posteam == team) %>%
        summarize(
            plays = n(),
            value = mean(epa > 0, na.rm = TRUE)
        )

    return(success_rate_data)
}

fetch_game_def_pass_success_rate <- function(data, nfl_week, team) {    
    filtered_data <- pass_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week == nfl_week, defteam == team) %>%
        summarize(
            plays = n(),
            value = mean(epa > 0, na.rm = TRUE)
        )

    return(success_rate_data)
}

fetch_game_def_run_success_rate <- function(data, nfl_week, team) {    
    filtered_data <- run_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week == nfl_week, defteam == team) %>%
        summarize(
            plays = n(),
            value = mean(epa > 0, na.rm = TRUE)
        )

    return(success_rate_data)
}

fetch_game_def_pass_epa <- function(data, nfl_week, team) {
    filtered_data <- pass_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week == nfl_week, defteam == team) %>%
        summarize(
            plays = n(),
            value = mean(epa, na.rm = TRUE)
        )  %>%

    return(epa_per_play_data)
}

fetch_game_def_run_epa <- function(data, nfl_week, team) {
    filtered_data <- run_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week == nfl_week, defteam == team) %>%
        summarize(
            plays = n(),
            value = mean(epa, na.rm = TRUE)
        )  %>%

    return(epa_per_play_data)
}

##### End Game Stats #####



transparent <- function(img) {
    magick::image_fx(img, expression = "0.28*a", channel = "alpha")
}

fetch_def_explosive_play_rate <- function(data, nfl_week) {  
    filtered_data <- data %>%
        filter(down <= 4)

    explosive_play_data <- filtered_data  %>%
        filter(week <= nfl_week)  %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            explosive_plays = sum(yards_gained >= 20, na.rm = TRUE),
            explosive_rate = explosive_plays / plays
        )

    return(explosive_play_data)
}

fetch_def_week_xpr <- function(data, nfl_week) {  
    filtered_data <- data %>%
        filter(down <= 4)

    explosive_play_data <- filtered_data  %>%
        filter(week == nfl_week)  %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            explosive_plays = sum(yards_gained >= 20, na.rm = TRUE),
            explosive_rate = explosive_plays / plays
        )

    return(explosive_play_data)
}

calculate_def_explosive_play_rate <- function(data, nfl_week) {
    explosive_play_data <- fetch_def_explosive_play_rate(data, nfl_week)
    ggplot(explosive_play_data, aes(x = reorder(defteam, explosive_rate),
            y = explosive_rate)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle("Team Explosive Play Rate Allowed") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Explosive Play Rate Allowed")
}

calculate_week_def_explosive_play_rate <- function(data, nfl_week) {
    filtered_data <- data %>%
        filter(down <= 4)

    explosive_play_data <- filtered_data  %>%
        filter(week == nfl_week)  %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            explosive_plays = sum(yards_gained >= 20, na.rm = TRUE),
            explosive_rate = explosive_plays / plays
        )

    ggplot(explosive_play_data, aes(x = reorder(defteam, explosive_rate),
            y = explosive_rate)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle(str_interp("Team Explosive Play Rate Allowed for week ${nfl_week}")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Explosive Play Rate Allowed")
}

fetch_def_epa <- function(data, nfl_week) {
    if (is.null(data)) {
        return(NULL)
    }
    
    data1 <- data %>%
        filter(down <= 4 & qtr <= 2) %>%
        filter(play_type != "no_play")
    
    data2 <- data %>%
        filter(wp >= WP_LOW & wp <= WP_HIGH & down <= 4 &
            (qtr == 3 | qtr == 4)) %>%
        filter(play_type != "no_play")

    filtered_data <- rbind(data1, data2)
    epa_per_play_data <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            def_epa = round(mean(epa, na.rm = TRUE), digits = 3)
        )

    return(epa_per_play_data)
}

fetch_def_epa_recent <- function(data, nfl_week) {
    data1 <- data %>%
        filter(down <= 4 & qtr <= 2)
    data2 <- data %>%
        filter(wp >= WP_LOW & wp <= WP_HIGH & down <= 4 &
            (qtr == 3 | qtr == 4))

    filtered_data <- rbind(data1, data2)
    epa_per_play_data <- filtered_data %>%
        filter(week <= nfl_week & week > nfl_week - RECENT_INTERVAL) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            def_epa = round(mean(epa, na.rm = TRUE), digits = 3)
        )

    return(epa_per_play_data)
}

fetch_week_def_epa <- function(data, nfl_week) {
    data1 <- data %>%
        filter(down <= 4 & qtr <= 2)
    data2 <- data %>%
        filter(wp >= WP_LOW & wp <= WP_HIGH & down <= 4 &
            (qtr == 3 | qtr == 4))

    filtered_data <- rbind(data1, data2)
    epa_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        select(posteam, defteam, epa) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE),
            offense = posteam
        ) %>%
        arrange(-epa_per_play)

    return(epa_data)
}

calculate_def_epa <- function(data, nfl_week) {
    epa_per_play_data <- fetch_def_epa(data, nfl_week)
    ggplot(epa_per_play_data, aes(x = reorder(defteam, def_epa),
            y = def_epa)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle("Team EPA/Play Allowed") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("EPA Allowed")
}

calculate_def_epa_week <- function(data, nfl_week) {
    filtered_data <- data  %>%
        filter(down <= 4)

    epa_per_play_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE)
        )

    ggplot(epa_per_play_data, aes(x = reorder(defteam, epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle("Team EPA/Play Allowed This Week") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("EPA Allowed")
}

fetch_def_epa_quarter <- function(data, nfl_week, quarter) {
    data1 <- data %>%
        filter(down <= 4 & qtr <= 2)
    data2 <- data %>%
        filter(wp >= WP_LOW & wp <= WP_HIGH & down <= 4 &
            (qtr == 3 | qtr == 4))

    filtered_data <- rbind(data1, data2)
    epa_per_play_data <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            def_epa = round(mean(epa, na.rm = TRUE), digits = 3)
        )

    return(epa_per_play_data)
}

calculate_def_epa_q1 <- function(data, nfl_week) {
    epa_per_play_data <- fetch_def_epa_quarter(data, nfl_week, 1)
    ggplot(epa_per_play_data, aes(x = reorder(defteam, def_epa),
            y = def_epa)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle("Team EPA/Play Allowed Q1") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("EPA Allowed")
}

calculate_def_epa_q2 <- function(data, nfl_week) {
    epa_per_play_data <- fetch_def_epa_quarter(data, nfl_week, 2)
    ggplot(epa_per_play_data, aes(x = reorder(defteam, def_epa),
            y = def_epa)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle("Team EPA/Play Allowed Q2") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("EPA Allowed")
}

calculate_def_epa_q3 <- function(data, nfl_week) {
    epa_per_play_data <- fetch_def_epa_quarter(data, nfl_week, 3)
    ggplot(epa_per_play_data, aes(x = reorder(defteam, def_epa),
            y = def_epa)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle("Team EPA/Play Allowed Q3") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("EPA Allowed")
}

calculate_def_epa_q4 <- function(data, nfl_week) {
    epa_per_play_data <- fetch_def_epa_quarter(data, nfl_week, 4)
    ggplot(epa_per_play_data, aes(x = reorder(defteam, def_epa),
            y = def_epa)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle("Team EPA/Play Allowed Q4") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("EPA Allowed")
}

fetch_def_pass_success_rate <- function(data, nfl_week) {
    if (is.null(data)) {
        return(NULL)
    }
    
    filtered_data <- pass_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            def_success = mean(epa > 0, na.rm = TRUE)
        )

    return(success_rate_data)
}

fetch_def_pass_success_rate_recent <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week <= nfl_week & week > nfl_week - RECENT_INTERVAL) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            def_success = mean(epa > 0, na.rm = TRUE)
        )

    return(success_rate_data)
}

fetch_def_success_rate <- function(data, nfl_week) {
    data1 <- data %>%
        filter(down <= 4 & qtr <= 2)
    data2 <- data %>%
        filter(wp >= WP_LOW & wp <= WP_HIGH & down <= 4 &
            (qtr == 3 | qtr == 4))

    filtered_data <- rbind(data1, data2)
    success_rate_data <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            def_success = round(mean(epa > 0, na.rm = TRUE), digits = 3)
        )

    return(success_rate_data)
}

combine_off_fields <- function(this, last, this_weight, fieldname) {
    teams <- this[[1]]
    for (i in 1:length(teams)) {
        team <- teams[[i]]
        this_val <- this[this$posteam == team, fieldname]
        last_val <- last[last$posteam == team, fieldname]
        this[this$posteam == team, fieldname] <- this_val * this_weight + last_val * (1 - this_weight)
    }

    return(this)
}

combine_def_fields <- function(this, last, this_weight, fieldname) {
    this <- this[-c(33), ]
    last <- last[-c(33), ]
    teams <- this[[1]]

    for (i in 1:length(teams)) {
        team <- teams[[i]]
        this_val <- this[this$defteam == team, fieldname]
        last_val <- last[last$defteam == team, fieldname]
        this[this$defteam == team, fieldname] <- this_val * this_weight + last_val * (1 - this_weight)
    }

    return(this)
}

combine_fg_fields <- function(this, last, this_weight) {
    this <- this[-c(33), ]
    last <- last[-c(33), ]
    teams <- this[[1]]

    for (i in 1:length(teams)) {
        team <- teams[[i]]
        
        this_val <- head(this[this$posteam == team, "field_goals"], 1)
        last_val <- head(last[last$posteam == team, "field_goals"], 1)
        this[this$posteam == team, "field_goals"] <- this_val * this_weight + last_val * (1 - this_weight)
        
        this_val <- head(this[this$posteam == team, "field_goal_makes"], 1)
        last_val <- head(last[last$posteam == team, "field_goal_makes"], 1)
        this[this$posteam == team, "field_goal_makes"] <- this_val * this_weight + last_val * (1 - this_weight)
        
        this_val <- head(this[this$posteam == team, "pct"], 1)
        last_val <- head(last[last$posteam == team, "pct"], 1)
        this[this$posteam == team, "pct"] <- this_val * this_weight + last_val * (1 - this_weight)
    }

    return(this)
}

fg_prediction <- function(year, data, week) {
    if (week == 1) {
        this_year <- year - 1
        last_year <- year - 2
        nfl_week <- get_num_weeks(year) - 1
        
    } else {
        this_year <- year
        last_year <- year - 1
        nfl_week <- week - 1
    }

    this_data <- data[[toString(this_year)]]
    last_data <- data[[toString(last_year)]]
    weight <- get_week_weight(nfl_week)
    if (weight > 1) {
        weight <- 1
    }

    off_data <- fetch_team_success_rate(this_data, nfl_week)
    def_data <- fetch_def_success_rate(this_data, nfl_week)
    explosive_data <- fetch_explosive_play_rate(this_data, nfl_week)
    off_epa <- fetch_team_epa(this_data, nfl_week)
    def_epa <- fetch_def_epa(this_data, nfl_week)
    field_goals <- fetch_field_goal_attempts(this_data, nfl_week)
    field_goals$pct[is.na(field_goals$pct)] <- 0
    def_field_goals <- fetch_def_field_goal_attempts(this_data, nfl_week)

    last_off_data <- fetch_team_success_rate(last_data, get_num_weeks(last_year) - 1)
    last_def_data <- fetch_def_success_rate(last_data, get_num_weeks(last_year) - 1)
    last_explosive_data <- fetch_explosive_play_rate(last_data, get_num_weeks(last_year) - 1)
    last_off_epa <- fetch_team_epa(last_data, get_num_weeks(last_year) - 1)
    last_def_epa <- fetch_def_epa(last_data, get_num_weeks(last_year) - 1)
    last_field_goals <- fetch_field_goal_attempts(last_data, get_num_weeks(last_year) - 1)
    last_field_goals$pct[is.na(field_goals$pct)] <- 0
    last_def_field_goals <- fetch_def_field_goal_attempts(last_data, get_num_weeks(last_year) - 1)

    off_data <- combine_off_fields(off_data, last_off_data, weight, "success_rate")
    def_data <- combine_def_fields(def_data, last_def_data, weight, "def_success")
    explosive_data <- combine_off_fields(explosive_data, last_explosive_data, weight, "explosive_rate")
    off_epa <- combine_off_fields(off_epa, last_off_epa, weight, "epa_per_play")
    def_epa <- combine_def_fields(def_epa, last_def_epa, weight, "def_epa")
    field_goals <- combine_fg_fields(field_goals, last_field_goals, weight)
    def_field_goals <- combine_def_fields(def_field_goals, last_def_field_goals, weight, "def_field_goals")

    data <- merge(off_data, def_data, by.x = "posteam", by.y = "defteam")
    data <- merge(data, explosive_data, by = "posteam") 
    data <- merge(data, off_epa, by = "posteam") 
    data <- merge(data, def_epa, by.x = "posteam", by.y = "defteam")
    data <- merge(data, field_goals, by = "posteam")
    data <- merge(data, def_field_goals, by.x = "posteam", by.y = "defteam")  %>%
        select(posteam, success_rate, def_success, explosive_rate,
            epa_per_play, def_epa, field_goals, pct, def_field_goals)

    data$pct[is.na(data$pct)] <- 0

    league <- c("League",
        round(median(data$success_rate), digits = 3),
        round(median(data$def_success), digits = 3),
        round(median(data$explosive_rate), digits = 3),
        round(median(data$epa_per_play), digits = 3),
        round(median(data$def_epa), digits = 3),
        round(median(data$field_goals), digits = 1),
        round(median(data$pct), digits = 3),
        round(median(data$def_field_goals), digits = 1))
    extremes <- c("Extremes",
        max(data$success_rate) - min(data$success_rate),
        max(data$def_success) - min(data$def_success),
        max(data$explosive_rate) - min(data$explosive_rate),
        max(data$epa_per_play) - min(data$epa_per_play),
        max(data$def_epa) - min(data$def_epa),
        max(data$field_goals) - min(data$field_goals),
        max(data$pct) - min(data$pct),
        max(data$def_field_goals) - min(data$def_field_goals))

    data[nrow(data) + 1,] = league
    data[nrow(data) + 1,] = extremes
    write.csv(data, "raw_fg_stats.csv", row.names = FALSE, quote = FALSE)

    fg_data <- data.frame(matrix(ncol = 13, nrow = 0))
    colnames(fg_data) <- c("Team", "Off. Succ", "Off. EPA", "Off", "Def. Succ", "Def. EPA", "Def", "Explosive", "Off FGs",
        "Off FG PCT", "Def FGs", "Off FG Chance", "Def FG Chance")

    for (i in 2:nrow(data) - 2) {
        team <- data[i, "posteam"]
        exponent <- round(as.double(data[i, "success_rate"]) -
            as.double(data[nrow(data) - 1, "success_rate"]), digits = 3)
        o_succ <- round(logistic_function(200, 11, exponent, 0, 200))
        exponent <- round(as.double(data[i, "def_success"]) -
            as.double(data[nrow(data) - 1, "def_success"]), digits = 3)
        d_succ <- round(logistic_function(200, 10, exponent, 0, 200))
        exponent <- round(as.double(data[i, "explosive_rate"]) -
            as.double(data[nrow(data) - 1, "explosive_rate"]), digits = 3)
        explosive <- round(logistic_function(200, 12, exponent, 0, 200))
        exponent <- round(as.double(data[i, "epa_per_play"]) -
            as.double(data[nrow(data) - 1, "epa_per_play"]), digits = 3)
        o_epa <- round(logistic_function(200, 5.5, exponent, 0, 200))
        exponent <- round(as.double(data[i, "def_epa"]) -
            as.double(data[nrow(data) - 1, "def_epa"]), digits = 3)
        d_epa <- round(logistic_function(200, 5, exponent, 0, 200))
        exponent <- round(as.double(data[i, "field_goals"]) -
            as.double(data[nrow(data) - 1, "field_goals"]), digits = 1)
        fg <- round(logistic_function(200, .75, exponent, 0, 200))
        exponent <- round(as.double(data[i, "def_field_goals"]) -
            as.double(data[nrow(data) - 1, "def_field_goals"]), digits = 1)
        fg_d <- round(logistic_function(200, .04, exponent, 0, 200))
        o_pct <- round(as.double(data[i, "pct"]), 3)

        off <- round((o_epa * 0.85 + (200 - o_succ) * 0.15))
        def <- round((d_epa * 0.85 + (200 - d_succ) * 0.15))
        exp <- explosive / 100
        o_fg <- round(0.6 * fg + 0.4 * (o_epa * 0.5 + (200 - o_succ) * 0.5) * exp * o_pct, digits = 2)
        d_fg <- round(0.4 * fg_d + 0.6 * (d_epa * 0.5 + (200 - d_succ) * 0.5))

        fg_data <- as.data.frame(insertRow(as.matrix(fg_data), nrow(fg_data) + 1,
            c(team, o_succ, o_epa, off, d_succ, d_epa, def, explosive, fg, o_pct,
                fg_d, as.double(o_fg), as.double(d_fg))))
    }

    off_fg <- fg_data[["Off FG Chance"]]
    new_off_fg <- c()
    median <- median(as.numeric(as.character(off_fg)))
    for (value in off_fg) {
        value <- as.double(value)
        exponent <- value - median
        value <- round(logistic_function(200, 0.028, exponent, 0, 200))
        new_off_fg <- append(new_off_fg, value)
    }

    fg_data[["Off FG Chance"]] <- new_off_fg
    write.csv(fg_data, str_interp(
        "FGs/${year}/${year}_week_${week}_fgs.csv"), row.names = FALSE, quote = FALSE)
}

compose_fg_totals <- function(year, week, matchups, avg_fgs, avg_tds) {
    fg_data <- read.csv(str_interp("FGs/${year}/${year}_week_${week}_fgs.csv"))
    teams <- c()
    fgs <- c()
    tds <- c()
    kps <- c()
    fgmult <- c()
    xpmult <- c()
    tt <- c()
    total <- c()
    spread <- c()

    for (index in 1:length(matchups)) {
        matchup <- matchups[[index]]
        teams <- append(teams, matchup[[1]])
        teams <- append(teams, matchup[[2]])

        team1_vals <- fg_data[fg_data$Team == matchup[[1]],]
        team2_vals <- fg_data[fg_data$Team == matchup[[2]],]

        team1_fg <- round(avg_fgs * (as.double(team1_vals[["Off.FG.Chance"]]) / 100) *
            (as.double(team2_vals[["Def.FG.Chance"]]) / 100), 1)
        team1_fgmult <- round((as.double(team1_vals[["Off.FG.Chance"]]) / 100) *
            (as.double(team2_vals[["Def.FG.Chance"]]) / 100) * 100)
        team1_xp <- round(avg_tds * (as.double(team1_vals[["Off"]]) / 100) * (as.double(team2_vals[["Def"]] / 100))
            * (as.double(team1_vals[["Explosive"]]) / 100) * 0.95, 1)
        team1_xpmult <- round((as.double(team1_vals[["Off"]]) / 100) * (as.double(team2_vals[["Def"]] / 100))
            * (as.double(team1_vals[["Explosive"]]) / 100) * 95)
        
        if (matchup[[1]] == "LAC") {
            print(team1_fg)
            print(team1_fgmult)
        }

        team2_fg <- round(avg_fgs * (as.double(team2_vals[["Off.FG.Chance"]]) / 100) * 
            (as.double(team1_vals[["Def.FG.Chance"]]) / 100), 1)
        team2_fgmult <- round((as.double(team2_vals[["Off.FG.Chance"]]) / 100) * 
            (as.double(team1_vals[["Def.FG.Chance"]]) / 100) * 100)
        team2_xp <- round(avg_tds * (as.double(team2_vals[["Off"]] / 100)) * (as.double(team1_vals[["Def"]] / 100))
            * (as.double(team2_vals[["Explosive"]]) / 100) * 0.95, 1)
        team2_xpmult <- round((as.double(team2_vals[["Off"]] / 100)) * (as.double(team1_vals[["Def"]] / 100))
            * (as.double(team2_vals[["Explosive"]]) / 100) * 95)

        team1_kp <- 3 * team1_fg + team1_xp
        team2_kp <- 3 * team2_fg + team2_xp

        t1t <- round(team1_fg * 3 + team1_xp * 7, 1)
        t2t <- round(team2_fg * 3 + team2_xp * 7, 1)

        fgs <- append(fgs, team1_fg)
        fgs <- append(fgs, team2_fg)
        tds <- append(tds, team1_xp)
        tds <- append(tds, team2_xp)
        kps <- append(kps, team1_kp)
        kps <- append(kps, team2_kp)
        fgmult <- append(fgmult, team1_fgmult)
        fgmult <- append(fgmult, team2_fgmult)
        xpmult <- append(xpmult, team1_xpmult)
        xpmult <- append(xpmult, team2_xpmult)
        tt <- append(tt, t1t)
        tt <- append(tt, t2t)
        total <- append(total, t1t+t2t)
        total <- append(total, t1t+t2t)
        spread <- append(spread, round(-1 * (t1t-t2t), 1))
        spread <- append(spread, round(-1 * (t2t-t1t), 1))
    }

    frame <- data.frame(team = teams)
    frame$KP <- kps
    frame$FG <- fgs
    frame$TD <- tds
    frame$Points <- tt
    frame$Total <- total
    frame$Spread <- spread
    frame$FGMult <- fgmult
    frame$XPMult <- xpmult
    write.csv(frame, str_interp("FGs/Lines/${year}/${year}_week_${week}_lines.csv"), row.names = FALSE, quote = FALSE)
    return(c(round(mean(frame$FG), 2), round(mean(frame$TD), 2)))
}

fetch_field_goal_attempts <- function(data, nfl_week) {
    field_goals <- data %>%
        filter(week <= nfl_week) %>%
        group_by(posteam) %>%
        summarize(
            field_goals = round(sum(field_goal_attempt, na.rm = TRUE) / nfl_week, 2),
            field_goal_makes = round((sum(field_goal_result == "made", na.rm = TRUE) / nfl_week), 2),
            pct = round(field_goal_makes / field_goals, 3)
        )

    return(field_goals)
}

fetch_def_field_goal_attempts <- function(data, nfl_week) {
    field_goals <- data %>%
        filter(week <= nfl_week) %>%
        group_by(defteam) %>%
        summarize(
            def_field_goals = sum(field_goal_attempt, na.rm = TRUE)
        )

    return(field_goals)
}

logistic_function <- function(numerator, multiplier, exponent, offset, addition) {
    return(-(numerator / (1 + exp(multiplier * (exponent - offset)))) + addition) 
}

get_num_weeks <- function(season) {
    if (season < 2021) {
        return(17)
    } else {
        return (18)
    }
}

weigh_two_data <- function(field_now, field_last, now_weight,
        last_weight) {
    result <- tryCatch({
            updated_now <- now_weight
            updated_last <- 1 - updated_now
            if (updated_now > 1) {
                updated_now <- 1
                updated_last <- 0
            }

            return(field_now * updated_now + field_last * updated_last)
        },
        error = function(cond) {
            return(field_now * now_weight + field_last * last_weight)
        },
        warning = function(cond) {},
        finally = {}
    )
}

calculate_def_pass_success_rate <- function(data, nfl_week) {
    success_rate_data <- fetch_def_pass_success_rate(data, nfl_week)
    ggplot(success_rate_data, aes(x = reorder(defteam, def_success),
            y = def_success)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle("Team Passing Success Rate Allowed") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Passing Success Rate Allowed")
}

fetch_def_pass_epa <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE)
        )

    return(epa_per_play_data)
}

fetch_def_pass_epa_recent <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week <= nfl_week & week > nfl_week - RECENT_INTERVAL) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = round(mean(epa, na.rm = TRUE), digits = 3)
        )

    return(epa_per_play_data)
}

calculate_def_pass_epa <- function(data, nfl_week) {
    epa_per_play_data <- fetch_def_pass_epa(data, nfl_week)
    ggplot(epa_per_play_data, aes(x = reorder(defteam, epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle("Team Passing EPA/Play Allowed") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Pass EPA Allowed")
}

calculate_def_pass_epa_week <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE)
        )

    ggplot(epa_per_play_data, aes(x = reorder(defteam, epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle("Team Passing EPA/Play Allowed This Week") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Pass EPA Allowed")
}

calculate_week_def_pass_success_rate <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            success_rate = mean(epa > 0, na.rm = TRUE)
        )

    ggplot(success_rate_data, aes(x = reorder(defteam, success_rate),
            y = success_rate)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle(str_interp("Team Passing Success Rate Allowed for week ${nfl_week}")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Passing Success Rate Allowed")
}

fetch_def_run_epa <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE)
        )

    return(epa_per_play_data)
}

fetch_def_run_epa_recent <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week <= nfl_week & week > nfl_week - RECENT_INTERVAL) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = round(mean(epa, na.rm = TRUE), digits = 3)
        )

    return(epa_per_play_data)
}

calculate_def_run_epa <- function(data, nfl_week) {
    epa_per_play_data <- fetch_def_run_epa(data, nfl_week)
    ggplot(epa_per_play_data, aes(x = reorder(defteam, epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle("Team Rushing EPA/Play Allowed") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Run EPA Allowed")
}

calculate_def_run_epa_week <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE)
        )

    ggplot(epa_per_play_data, aes(x = reorder(defteam, epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle("Team Rushing EPA/Play Allowed This Week") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Run EPA Allowed")
}

fetch_def_run_success_rate <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            success_rate = mean(epa > 0, na.rm = TRUE),
            plays = n()
        )  %>%
        arrange(-success_rate)

    return(success_rate_data)
}

calculate_def_run_success_rate <- function(data, nfl_week) {
    success_rate_data <- fetch_def_run_success_rate(data, nfl_week)
    ggplot(success_rate_data, aes(x = reorder(defteam, success_rate),
            y = success_rate)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle("Team Rushing Success Rate Allowed") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Rushing Success Rate Allowed")
}

calculate_week_def_run_success_rate <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            success_rate = mean(epa > 0, na.rm = TRUE),
            plays = n()
        )  %>%
        arrange(-success_rate)

    ggplot(success_rate_data, aes(x = reorder(defteam, success_rate),
            y = success_rate)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle(str_interp("Team Rushing Success Rate Allowed for week ${nfl_week}")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Rushing Success Rate Allowed")
}

calculate_epa_vs_rb_targets <- function(roster, games) {
    decoded <- games  %>%
        nflfastR::decode_player_ids() %>%
        select(posteam, receiver, receiver_id, epa)

    joined <- decoded  %>%
        filter(!is.na(receiver_id))  %>%
        select(posteam, receiver, receiver_id, epa)  %>%
        left_join(roster, by = c("receiver_id" = "gsis_id"))

    epa_vs_rb_targets <- joined  %>%
        group_by(posteam)  %>%
        summarize(
            target_rate = mean(ifelse(position == "RB", 1, 0)),
            epa_per_play = mean(epa, na.rm = TRUE)
        )  %>%
        arrange(-epa_per_play)  %>%
        select(posteam, target_rate, epa_per_play)

    ggplot(epa_vs_rb_targets, aes(x = target_rate, y = epa_per_play)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        geom_hline(yintercept = mean(
            epa_vs_rb_targets$epa_per_play), color = "red",
                linetype = "dashed", alpha = 0.5) +
        geom_vline(xintercept =  mean(
            epa_vs_rb_targets$target_rate), color = "red", linetype = "dashed",
                alpha = 0.5) +
        ggtitle("Season EPA vs RB Target % by Team") +
        xlab("RB Target Rate") + ylab("EPA")
}

fetch_explosive_play_rate <- function(data, nfl_week) {
    filtered_data <- data %>%
        filter(down <= 4)

    explosive_play_data <- filtered_data  %>%
        filter(week <= nfl_week)  %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            explosive_plays = sum(yards_gained >= 20, na.rm = TRUE),
            explosive_rate = round(explosive_plays / plays, digits = 3)
        )

    return(explosive_play_data)
}

fetch_week_xpr <- function(data, nfl_week, team) {
    return(data %>%
        filter(down <= 4, qtr <= 4,
            posteam == team & week == nfl_week) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            explosive_plays = sum(yards_gained >= 20, na.rm = TRUE),
            explosive_rate = explosive_plays / plays
        ))
}

calculate_explosive_play_rate <- function(data, nfl_week) {
    explosive_play_data <- fetch_explosive_play_rate(data, nfl_week)
    ggplot(explosive_play_data, aes(x = reorder(posteam, -explosive_rate),
            y = explosive_rate)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle("Team Explosive Play Rate") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Explosive Play Rate")
}

calculate_week_explosive_play_rate <- function(data, nfl_week) {
    filtered_data <- data %>%
        filter(down <= 4)

    explosive_play_data <- filtered_data  %>%
        filter(week == nfl_week)  %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            explosive_plays = sum(yards_gained >= 20, na.rm = TRUE),
            explosive_rate = explosive_plays / plays
        )

    ggplot(explosive_play_data, aes(x = reorder(posteam, -explosive_rate),
            y = explosive_rate)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle(str_interp("Team Explosive Play Rate for week ${nfl_week}")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Explosive Play Rate")
}

fetch_pass_success_rate <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            success_rate = mean(epa > 0, na.rm = TRUE)
        )

    return(success_rate_data)
}

fetch_pass_success_rate_recent <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week <= nfl_week & week > nfl_week - RECENT_INTERVAL) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            success_rate = mean(epa > 0, na.rm = TRUE)
        )

    return(success_rate_data)
}

calculate_pass_success_rate <- function(data, nfl_week) {
    success_rate_data <- fetch_pass_success_rate(data, nfl_week)
    ggplot(success_rate_data, aes(x = reorder(posteam, -success_rate),
            y = success_rate)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle("Team Passing Success Rate") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Pass Success Rate")
}

calculate_week_pass_success_rate <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            success_rate = mean(epa > 0, na.rm = TRUE)
        )

    ggplot(success_rate_data, aes(x = reorder(posteam, -success_rate),
            y = success_rate)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle(str_interp("Team Passing Success Rate for week ${nfl_week}")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Pass Success Rate")
}

fetch_pass_epa <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE)
        )  %>%
        arrange(-epa_per_play)

    return(epa_per_play_data)
}

fetch_pass_epa_recent <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week <= nfl_week & week > nfl_week - RECENT_INTERVAL) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE)
        )  %>%
        arrange(-epa_per_play)

    return(epa_per_play_data)
}

fetch_pass_week_epa <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE)
        )  %>%
        arrange(-epa_per_play)

    return(epa_per_play_data)
}

calculate_pass_epa <- function(data, nfl_week) {
    epa_per_play_data <- fetch_pass_epa(data, nfl_week)
    ggplot(epa_per_play_data, aes(x = reorder(posteam, -epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle("Team Passing EPA/Play") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Pass EPA")
}

calculate_pass_epa_week <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE)
        )

    ggplot(epa_per_play_data, aes(x = reorder(posteam, -epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle("Team Passing EPA/Play This Week") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Pass EPA")
}

calculate_pass_success_vs_epa <- function(data, nfl_week) {
    success_rate_data <- pass_filter(data) %>%
        filter(week <= nfl_week, play_type != "no_play") %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            pass_plays = sum(pass == 1),
            success_rate = sum(pass == 1 & epa > 0, na.rm = TRUE) / pass_plays,
            epa_per_play = mean(epa, na.rm = TRUE)
        )  %>%
        arrange(-success_rate)

    ggplot(success_rate_data, aes(x = epa_per_play, y = success_rate)) +
        geom_text_repel(aes(label = posteam)) +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        geom_point() +
        ggtitle("Season Passing Success Rate vs EPA/Play") +
        xlab("EPA") + ylab("Pass Success Rate")
}

fetch_run_success_rate <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            success_rate = mean(epa > 0, na.rm = TRUE)
        )

    return(success_rate_data)
}

fetch_run_success_rate_recent <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week <= nfl_week & week > nfl_week - RECENT_INTERVAL) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            success_rate = mean(epa > 0, na.rm = TRUE)
        )

    return(success_rate_data)
}

calculate_run_success_rate <- function(data, nfl_week) {
    success_rate_data <- fetch_run_success_rate(data, nfl_week)
    ggplot(success_rate_data, aes(x = reorder(posteam, -success_rate),
            y = success_rate)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle("Team Rushing Success Rate") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Run Success Rate")
}

calculate_week_run_success_rate <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week == nfl_week, down <= 4) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            rush_plays = sum(rush == 1),
            success_rate = sum(rush == 1 & epa > 0, na.rm = TRUE) / rush_plays,
        )  %>%
        arrange(-success_rate)

    ggplot(success_rate_data, aes(x = reorder(posteam, -success_rate),
            y = success_rate)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle(str_interp("Team Rushing Success Rate for week ${nfl_week}")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Run Success Rate")
}

calculate_team_success_rate <- function(data, nfl_week) {
    success_rate_data <- fetch_team_success_rate(data, nfl_week)
    ggplot(success_rate_data, aes(x = reorder(posteam, -success_rate),
            y = success_rate)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle(str_interp("Team Success Rate")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Success Rate")
}

fetch_team_success_rate <- function(data, nfl_week) {
    data1 <- data %>%
        filter(down <= 4 & qtr <= 2)
    data2 <- data %>%
        filter(wp >= WP_LOW & wp <= WP_HIGH & down <= 4 &
            (qtr == 3 | qtr == 4))

    filtered_data <- rbind(data1, data2)
    success_rate_data <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(posteam)  %>%
        summarize(
            success_rate = round(mean(epa > 0, na.rm = TRUE), 3)
        )  %>%
        arrange(-success_rate)

    return(success_rate_data)
}

calculate_week_team_success_rate <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week == nfl_week, down <= 4) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            success_rate = mean(epa > 0, na.rm = TRUE)
        )  %>%
        arrange(-success_rate)

    ggplot(success_rate_data, aes(x = reorder(posteam, -success_rate),
            y = success_rate)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle(str_interp("Team Success Rate")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Success Rate")
}

calculate_def_success_rate <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    success_rate_data <- filtered_data %>%
        filter(week <= nfl_week, down <= 4) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            success_rate = mean(epa > 0, na.rm = TRUE)
        )  %>%
        arrange(success_rate)

    ggplot(success_rate_data, aes(x = reorder(defteam, -success_rate),
            y = success_rate)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle(str_interp("Team Success Rate Allowed")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Success Rate")
}

calculate_week_def_success_rate <- function(data, nfl_week) {
    data1 <- data %>%
        filter(down <= 4 & qtr <= 2)
    data2 <- data %>%
        filter(wp >= WP_LOW & wp <= WP_HIGH & down <= 4 &
            (qtr == 3 | qtr == 4))

    filtered_data <- rbind(data1, data2)
    success_rate_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            plays = n(),
            success_rate = mean(epa > 0, na.rm = TRUE)
        )  %>%
        arrange(success_rate)

    ggplot(success_rate_data, aes(x = reorder(defteam, -success_rate),
            y = success_rate)) +
        geom_text_repel(aes(label = defteam)) +
        geom_point() +
        ggtitle(str_interp("Team Success Rate Allowed")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Success Rate")
}

fetch_run_epa <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE)
        )  %>%
        arrange(-epa_per_play)

    return(epa_per_play_data)
}

fetch_run_epa_recent <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week <= nfl_week & week > nfl_week - RECENT_INTERVAL) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE)
        )  %>%
        arrange(-epa_per_play)

    return(epa_per_play_data)
}

fetch_run_week_epa <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE),
            defense = defteam
        )  %>%
        arrange(-epa_per_play)

    return(epa_per_play_data)
}

calculate_run_epa <- function(data, nfl_week) {
    epa_per_play_data <- fetch_run_epa(data, nfl_week)
    ggplot(epa_per_play_data, aes(x = reorder(posteam, -epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle("Team Rushing EPA/Play") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Run EPA")
}

calculate_run_epa_week <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    epa_per_play_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE)
        )

    ggplot(epa_per_play_data, aes(x = reorder(posteam, -epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle("Team Rushing EPA/Play This Week") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Run EPA")
}

fetch_team_epa <- function(data, nfl_week) {
    data1 <- data %>%
        filter(down <= 4 & qtr <= 2)
    data2 <- data %>%
        filter(wp >= WP_LOW & wp <= WP_HIGH & down <= 4 &
            (qtr == 3 | qtr == 4))

    filtered_data <- rbind(data1, data2)
    epa_data <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = round(mean(epa, na.rm = TRUE), digits = 3)
        )  %>%
        arrange(-epa_per_play)

    return(epa_data)
}

fetch_team_week_epa <- function(data, nfl_week) {
    data1 <- data %>%
        filter(down <= 4 & qtr <= 2)
    data2 <- data %>%
        filter(wp >= WP_LOW & wp <= WP_HIGH & down <= 4 &
            (qtr == 3 | qtr == 4))

    filtered_data <- rbind(data1, data2)
    epa_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        select(posteam, defteam, epa) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = mean(epa, na.rm = TRUE),
            defense = defteam
        ) %>%
        arrange(-epa_per_play)

    return(epa_data)
}

calculate_team_epa <- function(data, nfl_week) {
    epa_data <- fetch_team_epa(data, nfl_week)
    ggplot(epa_data, aes(x = reorder(posteam, -epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle(str_interp("Team EPA/Play")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("EPA")
}

fetch_team_epa_quarter <- function(data, nfl_week, quarter) {
    epa_data <- data %>%
        filter(wp > .08 & wp < .92 & down <= 4 & qtr == quarter) %>%
        filter(week <= nfl_week, down <= 4) %>%
        select(posteam, defteam, epa) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            epa_per_play = round(mean(epa, na.rm = TRUE), 3)
        ) %>%
        arrange(-epa_per_play)

    return(epa_data)
}

fetch_team_succ_quarter <- function(data, nfl_week, quarter) {
    epa_data <- data %>%
        filter(wp > .08 & wp < .92 & down <= 4 & qtr == quarter) %>%
        filter(week <= nfl_week, down <= 4) %>%
        select(posteam, defteam, epa) %>%
        group_by(posteam)  %>%
        summarize(
            plays = n(),
            success_rate = round(mean(epa > 0, na.rm = TRUE), 3)
        ) %>%
        arrange(-success_rate)

    return(epa_data)
}

calculate_team_epa_q1 <- function(data, nfl_week) {
    epa_data <- fetch_team_epa_quarter(data, nfl_week, 1)
    ggplot(epa_data, aes(x = reorder(posteam, -epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle(str_interp("Team EPA/Play in Q1")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("EPA")
}

calculate_team_epa_q2 <- function(data, nfl_week) {
    epa_data <- fetch_team_epa_quarter(data, nfl_week, 2)
    ggplot(epa_data, aes(x = reorder(posteam, -epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle(str_interp("Team EPA/Play in Q2")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("EPA")
}

calculate_team_epa_q3 <- function(data, nfl_week) {
    epa_data <- fetch_team_epa_quarter(data, nfl_week, 3)
    ggplot(epa_data, aes(x = reorder(posteam, -epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle(str_interp("Team EPA/Play in Q3")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("EPA")
}

calculate_team_epa_q4 <- function(data, nfl_week) {
    epa_data <- fetch_team_epa_quarter(data, nfl_week, 4)
    ggplot(epa_data, aes(x = reorder(posteam, -epa_per_play),
            y = epa_per_play)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle(str_interp("Team EPA/Play in Q4")) +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("EPA")
}

fetch_def_adot <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_adot <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(defteam)  %>%
        summarize(
            ADOT = round(mean(air_yards, na.rm = TRUE), 3),
            plays = n()
        )

    return(qb_adot)
}

calculate_season_adot <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_adot <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(passer)  %>%
        summarize(
            ADOT = round(mean(air_yards, na.rm = TRUE), 3),
            team = last(posteam),
            plays = n()
        )  %>%
        arrange(-ADOT)  %>%
        filter(plays > nfl_week * ATTEMPT_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(qb_adot, aes(x = plays, y = ADOT)) +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = qb_adot$ADOT / 210, asp = 7 / 7) +
        geom_text_repel(aes(label = passer)) +
        ggtitle("Season ADOT for QBs") +
        xlab("Num. Attempts") + ylab("Average Depth of Target")
}

calculate_season_adot_vs_epa <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_adot <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(passer)  %>%
        summarize(
            ADOT = round(mean(air_yards, na.rm = TRUE), 3), plays = n(),
            EPA_per_play = round(mean(qb_epa), 3),
            team = last(posteam)
        )  %>%
        arrange(-ADOT)  %>%
        filter(plays > nfl_week * ATTEMPT_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(qb_adot, aes(x = EPA_per_play, y = ADOT)) +
        geom_text_repel(aes(label = passer)) +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = qb_adot$ADOT / 210, asp = 7 / 7) +
        geom_hline(yintercept = mean(
            qb_adot$ADOT), color = "red", linetype = "dashed", alpha = 0.5) +
        geom_vline(xintercept =  mean(
            qb_adot$EPA_per_play), color = "red", linetype = "dashed",
                alpha = 0.5) +
        ggtitle("Season ADOT vs EPA for QBs") +
        xlab("Num. Attempts") + ylab("Average Depth of Target")
}

calculate_season_adot_vs_ypa <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_adot <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(passer)  %>%
        summarize(
            ADOT = round(mean(air_yards, na.rm = TRUE), 3), plays = n(),
            YPA = mean(yards_gained),
            team = last(posteam)
        )  %>%
        arrange(-ADOT)  %>%
        filter(plays > nfl_week * ATTEMPT_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(qb_adot, aes(x = YPA, y = ADOT)) +
        geom_text_repel(aes(label = passer)) +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = qb_adot$ADOT / 210, asp = 7 / 7) +
        geom_hline(yintercept = mean(
            qb_adot$ADOT), color = "red", linetype = "dashed", alpha = 0.5) +
        geom_vline(xintercept =  mean(
            qb_adot$YPA), color = "red", linetype = "dashed",
                alpha = 0.5) +
        ggtitle("Season ADOT vs YPA for QBs") +
        xlab("Yards Per Attempt") + ylab("Average Depth of Target")
}

calculate_season_cpoe <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_cpoe <- filtered_data %>%
        filter(week <= nfl_week) %>%
        filter(!is.na(cpoe))  %>%
        group_by(passer) %>%
        summarize(
            plays = n(),
            cpoe = round(mean(cpoe, na.rm = TRUE), 2),
            team = last(posteam)
        )  %>%
        arrange(-cpoe)  %>%
        filter(plays > nfl_week * ATTEMPT_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(qb_cpoe, aes(x = plays, y = cpoe)) +
        geom_text_repel(aes(label = passer)) +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = abs((qb_cpoe$cpoe + 15) / 400), asp = 7 / 7) +
        ggtitle("Season CPOE") +
        xlab("Num. Attempts") + ylab("Completion Percentage Over Expectation")
}

fetch_qb_cpoe <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_cpoe <- filtered_data %>%
        filter(week <= nfl_week) %>%
        filter(!is.na(cpoe))  %>%
        group_by(passer) %>%
        summarize(
            plays = n(),
            cpoe = round(mean(cpoe, na.rm = TRUE), 2),
            team = last(posteam)
        )  %>%
        arrange(-cpoe)  %>%
        filter(plays > nfl_week * ATTEMPT_INTERVAL)

    return(qb_cpoe)
}

calculate_season_cpoe_vs_adot <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_cpoe <- filtered_data %>%
        filter(week <= nfl_week) %>%
        filter(!is.na(cpoe))  %>%
        group_by(passer) %>%
        summarize(
            plays = n(),
            cpoe = round(mean(cpoe, na.rm = TRUE), 2),
            ADOT = round(mean(air_yards, na.rm = TRUE), 3),
            team = last(posteam)
        )  %>%
        arrange(-cpoe)  %>%
        filter(plays > nfl_week * ATTEMPT_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(qb_cpoe, aes(x = ADOT, y = cpoe)) +
        geom_text_repel(aes(label = passer)) +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = abs((qb_cpoe$cpoe + 15) / 400), asp = 7 / 7) +
        ggtitle("Season CPOE vs ADOT") +
        xlab("Average Depth of Target") +
        ylab("Completion Percentage Over Expectation")
}

fetch_season_cpoe_vs_epa <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_cpoe <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(posteam) %>%
        summarize(
            plays = n(),
            cpoe = round(mean(cpoe, na.rm = TRUE), 2),
            epa = round(mean(qb_epa), 3),
            team = last(posteam)
        )  %>%
        arrange(-cpoe)

    return(qb_cpoe)
}

fetch_week_cpoe_vs_epa <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_cpoe <- filtered_data %>%
        filter(week == nfl_week) %>%
        group_by(posteam) %>%
        summarize(
            plays = n(),
            cpoe = round(mean(cpoe, na.rm = TRUE), 2),
            epa = round(mean(qb_epa, na.rm = TRUE), 3),
        )  %>%
        arrange(-cpoe)

    return(qb_cpoe)
}

fetch_range_cpoe_vs_epa <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_cpoe <- filtered_data %>%
        filter(week > nfl_week - RECENT_INTERVAL & week <= nfl_week) %>%
        group_by(posteam) %>%
        summarize(
            plays = n(),
            cpoe = round(mean(cpoe, na.rm = TRUE), 2),
            epa = round(mean(qb_epa, na.rm = TRUE), 3),
        )  %>%
        arrange(-cpoe)

    return(qb_cpoe)
}

fetch_season_cpoe_vs_epa <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_cpoe <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(posteam) %>%
        summarize(
            plays = n(),
            cpoe = round(mean(cpoe, na.rm = TRUE), 2),
            epa = round(mean(qb_epa, na.rm = TRUE), 3),
        )  %>%
        arrange(-cpoe)

    return(qb_cpoe)
}

fetch_test <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_cpoe <- filtered_data %>%
        filter(week <= nfl_week & posteam == "BAL") %>%
        group_by(posteam) %>%
        summarize(
            plays = n(),
            cpoe = round(mean(cpoe, na.rm = TRUE), 2),
            epa = round(mean(qb_epa), 3),
            def = last(defteam)
        )  %>%
        arrange(-plays)
    return(qb_cpoe)
}

calculate_season_epa_vs_cpoe <- function(data, nfl_week) {
    qb_cpoe <- fetch_test(data, nfl_week) %>%
        filter(plays > nfl_week * ATTEMPT_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(qb_cpoe, aes(x = EPA_per_play, y = cpoe)) +
        geom_text_repel(aes(label = passer)) +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = abs((qb_cpoe$cpoe + 15) / 400), asp = 7 / 7) +
        ggtitle("Season CPOE vs EPA") +
        xlab("EPA") + ylab("Completion Percentage Over Expectation")
}

calculate_season_cpoe_vs_sr <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_cpoe <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(passer) %>%
        summarize(
            plays = n(),
            cpoe = round(mean(cpoe, na.rm = TRUE), 2),
            success_rate = round(mean(qb_epa > 0), 3),
            team = last(posteam)
        )  %>%
        arrange(-cpoe)  %>%
        filter(plays > nfl_week * ATTEMPT_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(qb_cpoe, aes(x = success_rate, y = cpoe)) +
        geom_text_repel(aes(label = passer)) +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = abs((qb_cpoe$cpoe + 15) / 400), asp = 7 / 7) +
        ggtitle("Season CPOE vs Success Rate") +
        xlab("Success Rate") + ylab("Completion Percentage Over Expectation")
}

calculate_season_epa_vs_sr <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_cpoe <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(passer) %>%
        summarize(
            plays = n(),
            EPA_per_play = round(mean(qb_epa), 3),
            success_rate = round(mean(qb_epa > 0), 3),
            team = last(posteam)
        )  %>%
        arrange(-EPA_per_play)  %>%
        filter(plays > nfl_week * ATTEMPT_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(qb_cpoe, aes(x = success_rate, y = EPA_per_play)) +
        geom_text_repel(aes(label = passer)) +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = abs((qb_cpoe$EPA_per_play + 15) / 400), asp = 7 / 7) +
        ggtitle("Season EPA vs Success Rate") +
        xlab("Success Rate") + ylab("EPA")
}

fetch_early_down_pass_rate <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    pass_rate <- filtered_data %>%
        filter(down <= 2 & week <= nfl_week) %>%
        group_by(posteam) %>%
        summarize(
            plays = n(),
            passes = round(sum(ifelse(pass == 1, 1, 0), na.rm = TRUE), 3),
            mean_pass = passes / plays
        ) %>%
        arrange(-mean_pass)

    return(pass_rate)
}

fetch_season_run_rate <- function(data, nfl_week) {
    filtered_data <- run_filter(data)
    run_rate <- filtered_data %>%
        filter(week <= nfl_week & !is.na(posteam)) %>%
        group_by(posteam) %>%
        summarize(
            plays = n(),
            passes = round(sum(ifelse(rush == 1, 1, 0), na.rm = TRUE), 3),
            mean_run = passes / plays
        ) %>%
        arrange(-mean_run)

    return(run_rate)
}

calculate_season_pass_rate <- function(data, nfl_week) {
    pass_rate <- fetch_early_down_pass_rate(data, nfl_week)
    ggplot(pass_rate, aes(x = reorder(posteam, -mean_pass), y = mean_pass)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        ggtitle("Season Pass Rate on Early Downs") +
        theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
        xlab("Team") + ylab("Pass Rate")
}

calculate_early_down_pass_rate_vs_epa <- function(data, nfl_week) {
    data1 <- data %>%
        filter(down <= 4 & qtr <= 2)
    data2 <- data %>%
        filter(wp >= WP_LOW & wp <= WP_HIGH & down <= 4 &
            (qtr == 3 | qtr == 4))

    filtered_data <- rbind(data1, data2)
    pass_rate <- filtered_data %>%
        filter(down <= 2 & week <= nfl_week) %>%
        group_by(posteam) %>%
        summarize(
            plays = n(),
            passes = round(sum(ifelse(pass == 1, 1, 0), na.rm = TRUE), 3),
            mean_pass = passes / plays,
            epa_per_play = round(mean(epa), 3)
        ) %>%
        arrange(-mean_pass)

    ggplot(pass_rate, aes(x = epa_per_play, y = mean_pass)) +
        geom_text_repel(aes(label = posteam)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        geom_hline(yintercept = mean(
            pass_rate$mean_pass), color = "red",
                linetype = "dashed", alpha = 0.5) +
        geom_vline(xintercept =  mean(
            pass_rate$epa_per_play), color = "red", linetype = "dashed",
                alpha = 0.5) +
        ggtitle("Season Pass Rate on Early Downs vs EPA Per Play") +
        xlab("EPA") + ylab("Pass Rate")
}

calculate_season_qb_epa <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_epa <- filtered_data %>%
        filter(week <= nfl_week) %>%
        filter(!is.na(qb_epa))  %>%
        group_by(passer) %>%
        summarize(
            plays = n(),
            EPA_per_play = round(mean(qb_epa, na.rm = TRUE), 3),
            team = last(posteam)
        )  %>%
        arrange(-EPA_per_play)  %>%
        filter(plays > nfl_week * ATTEMPT_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(qb_epa, aes(x = plays, y = EPA_per_play)) +
        geom_text_repel(aes(label = passer)) +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = abs(qb_epa$EPA_per_play + 2) / 65, asp = 7 / 7) +
        ggtitle("Season QB EPA") +
        xlab("Num. Attempts") + ylab("EPA")
}

calculate_late_down_epa <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    third_down_epa <- filtered_data %>%
        filter(week <= nfl_week) %>%
        filter(!is.na(qb_epa)) %>%
        filter(down >= 3) %>%
        group_by(passer) %>%
        summarize(
            plays = n(),
            EPA = round(mean(qb_epa, na.rm = TRUE), 3),
            team = last(posteam)
        )  %>%
        arrange(-EPA)  %>%
        filter(plays > nfl_week)
        
    qb_epa <- filtered_data %>%
        filter(week <= nfl_week) %>%
        filter(!is.na(qb_epa))  %>%
        group_by(passer) %>%
        summarize(
            plays = n(),
            EPA_per_play = round(mean(qb_epa, na.rm = TRUE), 3),
            team = last(posteam)
        )  %>%
        arrange(-EPA_per_play)  %>%
        filter(plays > nfl_week * ATTEMPT_INTERVAL)

    frame <- data.frame(qb_epa$passer, qb_epa$EPA_per_play,
        third_down_epa$EPA[1:length(qb_epa$EPA_per_play)])
    ggplot(frame, aes(x = qb_epa$EPA_per_play, y = third_down_epa$EPA[1:length(qb_epa$EPA_per_play)])) +
        geom_text_repel(aes(label = qb_epa$passer)) +
        geom_point() +
        ggtitle("Late Down QB EPA") +
        xlab("EPA") + ylab("Late Down EPA")
}

calculate_season_receiver_adot <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    receiver_adot <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(receiver)  %>%
        summarize(
            ADOT = round(mean(air_yards), 3),
            plays = n(),
            team = last(posteam))  %>%
        arrange(-ADOT)  %>%
        filter(plays > nfl_week * TARGET_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr')) %>%
        head(25)

    ggplot(receiver_adot, aes(x = plays, y = ADOT)) +
        geom_text_repel(aes(label = receiver)) +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = receiver_adot$ADOT / 400, asp = 7 / 7) +
        ggtitle(str_interp("Receiver ADOT for Week ${nfl_week}")) +
        xlab("Num. Targets") + ylab("Average Depth of Target")
}

calculate_week_adot <- function(nfl_week, data) {
    filtered_data <- data %>%
        filter(down <= 4 & qtr <= 4) %>%
        filter(pass == 1 & play_type != "no_play")
    qb_adot <- filtered_data %>%
        filter(week == nfl_week, !is.na(air_yards))  %>%
        group_by(passer)  %>%
        summarize(
            ADOT = round(mean(air_yards, na.rm = TRUE), 3),
            plays = n(),
            team = last(posteam)
        )  %>%
        arrange(-ADOT)  %>%
        filter(plays > ATTEMPT_INTERVAL)  %>%
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(qb_adot, aes(x = plays, y = ADOT)) +
        geom_text_repel(aes(label = passer)) +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = qb_adot$ADOT / 210, asp = 7 / 7) +
        ggtitle(str_interp("QB ADOT for Week ${nfl_week}")) +
        xlab("Num. Targets") + ylab("Average Depth of Target")
}

calculate_week_qb_epa <- function(nfl_week, data) {
    filtered_data <- pass_filter(data)
    qb_data <- filtered_data %>%
        filter(week == nfl_week) %>%
        filter(!is.na(qb_epa))  %>%
        group_by(passer) %>%
        summarize(
            plays = n(),
            EPA_per_play = round(mean(qb_epa, na.rm = TRUE), 3),
            team = last(posteam)
        )  %>%
        arrange(-EPA_per_play)  %>%
        filter(plays > ATTEMPT_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(qb_data, aes(x = plays, y = EPA_per_play)) +
        geom_text_repel(aes(label = passer)) +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = abs(qb_data$EPA_per_play + 2) / 65, asp = 7 / 7) +
        ggtitle(str_interp("QB EPA for Week ${nfl_week}")) +
        xlab("Num. Attempts") + ylab("EPA")
}

calculate_week_adot_vs_ypa <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    qb_adot <- filtered_data %>%
        filter(week == nfl_week) %>%
        group_by(passer)  %>%
        summarize(
            ADOT = round(mean(air_yards, na.rm = TRUE), 3), plays = n(),
            YPA = mean(yards_gained),
            team = last(posteam)
        )  %>%
        arrange(-ADOT)  %>%
        filter(plays > nfl_week * ATTEMPT_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(qb_adot, aes(x = YPA, y = ADOT)) +
        geom_text_repel(aes(label = passer)) +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = qb_adot$ADOT / 210, asp = 7 / 7) +
        ggtitle("Season ADOT vs YPA for QBs") +
        xlab("Yards Per Attempt") + ylab("Average Depth of Target")
}

calculate_week_receiver_adot <- function(nfl_week, data) {
    filtered_data <- pass_filter(data)
    receiver_adot <- filtered_data %>%
        filter(week == nfl_week)  %>%
        group_by(receiver)  %>%
        summarize(
            ADOT = round(mean(air_yards, na.rm = TRUE), 3),
            plays = n(),
            team = last(posteam)
        )  %>%
        arrange(-ADOT)  %>%
        filter(plays > TARGET_INTERVAL)  %>%
        left_join(teams_colors_logos, by = c('team' = 'team_abbr')) %>%
        head(25)

    ggplot(receiver_adot, aes(x = plays, y = ADOT)) +
        geom_text_repel(aes(label = receiver)) +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = receiver_adot$ADOT / 500, asp = 7 / 7) +
        ggtitle(str_interp("WR ADOT for Week ${nfl_week}")) +
        xlab("Num. Targets") + ylab("Average Depth of Target")
}

early_down_pass_rate_vs_3rd_and_long <- function(data, nfl_week) {
    data1 <- data %>%
        filter(down <= 4 & qtr <= 2)
    data2 <- data %>%
        filter(wp >= WP_LOW & wp <= WP_HIGH & down <= 4 &
            (qtr == 3 | qtr == 4))

    filtered_data <- rbind(data1, data2)
    pass_rate <- filtered_data %>%
        filter(down <= 2 & week <= nfl_week) %>%
        group_by(posteam) %>%
        summarize(
            mean_pass = round(mean(pass, na.rm = TRUE), 3),
            plays = n()
        ) %>%
        arrange(-mean_pass)

    third_and_longs <- filtered_data %>%
        filter(week <= nfl_week) %>%
        group_by(posteam) %>%
        summarize(
            third_and_long = mean(if_else(ydstogo >= 6 & down == 3, 1, 0))
        ) %>%
        arrange(-third_and_long)

    combined <- merge(pass_rate, third_and_longs)

    ggplot(combined, aes(x = third_and_long, y = mean_pass)) +
        geom_text_repel(aes(label = posteam))  +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        geom_point() +
        ggtitle("Season Pass Rate vs Percentage of 3rd and Longs") +
        xlab("Third and Long Rate") + ylab("Pass Rate")
}

top_epa_plays_season <- function(data) {
    top_epa <- data  %>%
        filter(!is.na(epa))  %>%
        arrange(-epa) %>%
        select(posteam, week, down, ydstogo, epa)

    head(top_epa, 10)
}

top_epa_plays_week <- function(nfl_week, data) {
    top_epa <- data  %>%
        filter(week == nfl_week, !is.na(epa))  %>%
        arrange(-epa) %>%
        select(posteam, week, down, ydstogo, epa)

    head(top_epa, 5)
}

top_wpa_plays_season <- function(data) {
    top_wpa <- data  %>%
        filter(!is.na(wpa))  %>%
        mutate(wpa_percentage = paste(round(wpa * 100, 1), "%", sep = "")) %>%
        arrange(-wpa) %>%
        select(posteam, week, down, ydstogo, wpa_percentage)

    head(top_wpa, 10)
}

top_wpa_plays_week <- function(nfl_week, data) {
    top_wpa <- data  %>%
        filter(week == nfl_week, !is.na(wpa))  %>%
        mutate(wpa_percentage = paste(round(wpa * 100, 1), "%", sep = "")) %>%
        arrange(-wpa) %>%
        select(posteam, week, down, ydstogo, wpa_percentage)

    head(top_wpa, 5)
}

worst_epa_plays_season <- function(data) {
    bottom_epa <- data  %>%
        filter(!is.na(epa))  %>%
        arrange(epa) %>%
        select(posteam, week, down, ydstogo, epa)

    head(bottom_epa, 10)
}

worst_epa_plays_week <- function(nfl_week, data) {
    bottom_epa <- data  %>%
        filter(!is.na(epa), week == nfl_week)  %>%
        arrange(epa) %>%
        select(posteam, week, down, ydstogo, epa)

    head(bottom_epa, 5)
}

worst_wpa_plays_season <- function(data) {
    bottom_wpa <- data  %>%
        filter(!is.na(wpa))  %>%
        mutate(wpa_percentage = paste(round(wpa * 100, 1), "%", sep = "")) %>%
        arrange(wpa) %>%
        select(posteam, week, down, ydstogo, wpa_percentage)

    head(bottom_wpa, 10)
}

worst_wpa_plays_week <- function(nfl_week, data) {
    bottom_wpa <- data  %>%
        filter(!is.na(wpa), week == nfl_week)  %>%
        mutate(wpa_percentage = paste(round(wpa * 50, 1), "%", sep = "")) %>%
        arrange(wpa) %>%
        select(posteam, week, down, ydstogo, wpa_percentage)

    setDT(head(bottom_wpa, 10))
}

yac_epa_aa_season <- function(data, nfl_week) {
    filtered_data <- pass_filter(data)
    receiver_yac_epa_ax <- filtered_data  %>%
        filter(!is.na(receiver), !is.na(xyac_epa))  %>%
        group_by(receiver)  %>%
        summarize(
            plays = n(),
            YAC_EPA_AX = round(sum((yac_epa - xyac_epa) / plays, na.rm = TRUE), 3),
            team = last(posteam)
        )  %>%
        arrange(-YAC_EPA_AX) %>%
        filter(plays > WEEK * TARGET_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(head(receiver_yac_epa_ax, 25),
        aes(x = plays, y = YAC_EPA_AX)) +
        geom_text_repel(aes(label = receiver)) +
        geom_point() +
        ggtitle("YAC EPA Above Expectation for Season") +
        xlab("Num. Targets") + ylab("YAC EPA over Expectation")
}

yac_epa_aa_week <- function(nfl_week, data) {
    filtered_data <- pass_filter(data)
    receiver_yac_epa_ax <- filtered_data  %>%
        filter(week == nfl_week, !is.na(receiver))  %>%
        filter(!is.na(xyac_epa))  %>%
        group_by(receiver)  %>%
        summarize(
            plays = n(),
            YAC_EPA_AX = round(sum((yac_epa - xyac_epa) / plays, na.rm = TRUE), 3),
            team = last(posteam)
        )  %>%
        arrange(-YAC_EPA_AX) %>%
        filter(plays > TARGET_INTERVAL)  %>% 
        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

    ggplot(head(receiver_yac_epa_ax, 25),
        aes(x = plays, y = YAC_EPA_AX)) +
        geom_text_repel(aes(label = receiver)) +
        geom_point() +
        ggtitle(str_interp("YAC EPA Above Expectation for Week ${nfl_week}")) +
        xlab("Num. Targets") + ylab("YAC EPA over Expectation")
}

fetch_def_pace <- function(data, num_weeks) {
    filtered_data <- data %>%
        filter(week <= num_weeks, down <= 4) %>%
        select(defteam) %>%
        group_by(defteam) %>%
        summarize(plays = n())

    return(filtered_data[ !(is.na(filtered_data$defteam)), ])
}

fetch_off_pace <- function(data, num_weeks) {
    filtered_data <- data %>%
        filter(week <= num_weeks, down <= 4) %>%
        select(posteam) %>%
        group_by(posteam) %>%
        summarize(plays = n())

    return(filtered_data[ !(is.na(filtered_data$posteam)), ])
}

fetch_rp_ratio <- function(data, num_weeks) {
    filtered_data <- data %>%
        filter(week <= num_weeks, down <= 4) %>%
        group_by(posteam) %>%
        summarize(ratio = round(mean(pass == 1) / mean(rush == 1), 2))

    return(filtered_data[ !(is.na(filtered_data$posteam)), ])
}

fetch_week_games <- function(data, nfl_week) {
    filtered_data <- data  %>%
        filter(data$week == nfl_week)  %>%
        select(game_id, home_team, home_score, away_team,
            away_score, total_line, spread_line)  %>%
        group_by(game_id)  %>%
        slice(n()) %>%
        summarize (
            home = home_team,
            away = away_team,
            h_score = home_score,
            a_score = away_score,
            total = home_score + away_score,
            line = total_line,
            spread = spread_line,
        )

    return(distinct(filtered_data))
}

team_pythagorean_projection <- function(data, num_weeks, byes) {
    wins_dict <- hash()
    losses_dict <- hash()
    proj_dict <- hash()

    for (i in 1:num_weeks) {
        filtered_data <- data  %>%
            filter(week == i)  %>%
            select(game_id, home_team, home_score, away_team,
                away_score)  %>%
            group_by(game_id)  %>%
            slice(n())

        num_rows <- nrow(filtered_data)
        for (game in 1:nrow(filtered_data)) {
            home <- filtered_data[[game, "home_team"]]
            away <- filtered_data[[game, "away_team"]]
            home_score <- as.integer(filtered_data[[game, "home_score"]])
            away_score <- as.integer(filtered_data[[game, "away_score"]])

            if (has.key(home, wins_dict)) {
                wins_dict[[home]] <- home_score +
                    wins_dict[[home]]
                losses_dict[[home]] <- away_score +
                    losses_dict[[home]]
            } else {
                wins_dict[[home]] <- home_score
                losses_dict[[home]] <- away_score
            }

            if (has.key(away, wins_dict)) {
                wins_dict[[away]] <- away_score +
                    wins_dict[[away]]
                losses_dict[[away]] <- home_score +
                    losses_dict[[away]]
            } else {
                wins_dict[[away]] <- away_score
                losses_dict[[away]] <- home_score
            }
        }
    }

    for (team in keys(wins_dict)) {
        MAGIC_POWER <- 2.37
        week_modifier <- 0
        if (is.na(byes[[team]])) {
            byes[[team]] <- 1
        }
        if (!is.na(byes[[team]]) & byes[[team]] <= num_weeks) {
            week_modifier <- 0
        }

        proj_dict[[team]] <- round(((wins_dict[[team]] ^ MAGIC_POWER) /
            ((wins_dict[[team]] ^ MAGIC_POWER) + (losses_dict[[team]] ^ MAGIC_POWER))
            * (num_weeks - week_modifier)), 3)
    }

    return(proj_dict)
}

team_pythagorean_wp <- function(data, num_weeks) {
    projections <- team_pythagorean_projection(data, num_weeks)
    for (key in keys(projections)) {
        # subtracting one for the bye week
        projections[[key]] <- round(projections[[key]] / (num_weeks - 1), 3)
    }

    return(projections)
}

team_win_percentage <- function(data, num_weeks) {
    wins_dict <- hash()
    losses_dict <- hash()
    wp_dict <- hash()

    for (i in 1:num_weeks) {
        filtered_data <- data  %>%
            filter(week == i)  %>%
            select(game_id, home_team, home_score, away_team,
                away_score)  %>%
            group_by(game_id)  %>%
            slice(n())

        for (game in 1:nrow(filtered_data)) {
            home <- filtered_data[[game, "home_team"]]
            away <- filtered_data[[game, "away_team"]]
            home_score <- as.integer(filtered_data[[game, "home_score"]])
            away_score <- as.integer(filtered_data[[game, "away_score"]])

            if (!has.key(home, wins_dict)) {
                wins_dict[[home]] <- 0
                losses_dict[[home]] <- 0
            }

            if (!has.key(away, wins_dict)) {
                wins_dict[[away]] <- 0
                losses_dict[[away]] <- 0
            }

            if (home_score > away_score) {
                wins_dict[[home]] <- wins_dict[[home]] + 1
                losses_dict[[away]] <- losses_dict[[away]] + 1
            } else if (away_score > home_score) {
                losses_dict[[home]] <- losses_dict[[home]] + 1
                wins_dict[[away]] <- wins_dict[[away]] + 1
            } else {
                wins_dict[[home]] <- wins_dict[[home]] + 0.5
                losses_dict[[home]] <- losses_dict[[home]] + 0.5
                wins_dict[[away]] <- wins_dict[[away]] + 0.5
                losses_dict[[away]] <- losses_dict[[away]] + 0.5
            }
        }
    }

    for (key in keys(wins_dict)) {
        if (num_weeks == 1) {
            num_weeks <- 2
        } 
        
        if (losses_dict[[key]] == 0) {
            wp_dict[[key]] <- 1
        } else {
            wp_dict[[key]] <- round((wins_dict[[key]] / (num_weeks - 1)), 3)
        }
    }

    return(wp_dict)
}

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

team_sos <- function(data, weeks) {
    win_percentages <- team_win_percentage(data, weeks)

    playing <- list()
    games_played <- hash()
    sos_dict <- hash()

    for (i in 1:weeks) {
        filtered_data <- data  %>%
            filter(week == i)  %>%
            select(game_id, home_team, away_team)  %>%
            group_by(game_id)  %>%
            slice(n())

        num_rows <- nrow(filtered_data)
        game_list <- split(filtered_data, 1:num_rows)
        for (game in game_list) {
            home <- game$home_team
            away <- game$away_team

            if (!has.key(home, sos_dict)) {
                sos_dict[[home]] <- win_percentages[[away]]
                games_played[[home]] <- 1
                append(playing, home)
            } else {
                games_played[[home]] <- games_played[[home]] + 1
                sos_dict[[home]] <- sos_dict[[home]] + win_percentages[[away]]
            }

            if (!has.key(away, sos_dict)) {
                sos_dict[[away]] <- win_percentages[[home]]
                games_played[[away]] <- 1
                append(playing, away)
            } else {
                games_played[[away]] <- games_played[[away]] + 1
                sos_dict[[away]] <- sos_dict[[away]] + win_percentages[[home]]
            }
        }
    }

    results <- hash()
    for (key in keys(sos_dict)) {
        results[[key]] <- round((sos_dict[[key]] / games_played[[key]]), 3)
    }

    return(results)
}

team_wp_vs_pp <- function(data, weeks) {
    pythagoreans <- team_pythagorean_wp(data, weeks)
    win_percentages <- team_win_percentage(data, weeks)
    p_teams <- keys(pythagoreans)
    p_values <- vector(, 32)
    wp_values <- vector(, 32)

    for (i in 1:length(p_teams)) {
        team <- p_teams[i]
        p_values[i] <- pythagoreans[[team]]
        wp_values[i] <- win_percentages[[team]]
    }

    graph_df <- data.frame(p_teams, p_values, wp_values)
    ggplot(graph_df, aes(x = wp_values,
            y = p_values)) +
        geom_text_repel(aes(label = p_teams)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        geom_hline(yintercept = mean(
            graph_df$p_values), color = "red",
                linetype = "dashed", alpha = 0.4) +
        geom_vline(xintercept =  mean(
            graph_df$wp_values), color = "red", linetype = "dashed",
                alpha = 0.4) +
        ggtitle("Pythagorean Projection vs Winning Percentage")
}



### Weekly Report Functions ###

print_all_weeks <- function() {
    for (year in BEGINNING:SEASON) {
        season_data <- nflreadr::load_pbp(year)
        directory_name <- str_interp("Weekly Reports/${year}")
        dir.create(directory_name)

        num_weeks <- NUM_WEEKS
        if (year < 2021) {
            num_weeks <- NUM_WEEKS - 1
        }

        for (week in 1:num_weeks) {
            print_weekly_pdf(directory_name, year, week, season_data)
        }
    }
}

print_weekly_pdf <- function(directory_name, season, nfl_week, season_data) {
    pdf(str_interp("${directory_name}/${season}_week_${nfl_week}_report.pdf"))
    week_compilation(nfl_week, season_data)
    dev.off()
}

week_compilation <- function(nfl_week, season_data) {
    print(calculate_season_adot(season_data, nfl_week))
    print(calculate_season_receiver_adot(season_data, nfl_week))
    print(calculate_season_adot_vs_epa(season_data, nfl_week))
    print(calculate_season_adot_vs_ypa(season_data, nfl_week))
    print(calculate_season_cpoe_vs_adot(season_data, nfl_week))
    # print(calculate_season_epa_vs_cpoe(season_data, nfl_week))
    print(calculate_season_cpoe_vs_sr(season_data, nfl_week))
    print(calculate_season_epa_vs_sr(season_data, nfl_week))
    print(calculate_season_qb_epa(season_data, nfl_week))
    print(calculate_late_down_epa(season_data, nfl_week))

    print(calculate_def_pass_success_rate(season_data, nfl_week))
    print(calculate_def_run_success_rate(season_data, nfl_week))
    print(calculate_def_success_rate(season_data, nfl_week))
    print(calculate_def_explosive_play_rate(season_data, nfl_week))
    print(calculate_def_pass_epa(season_data, nfl_week))
    print(calculate_def_run_epa(season_data, nfl_week))
    print(calculate_def_epa(season_data, nfl_week))
    print(calculate_def_epa_q1(season_data, nfl_week))
    print(calculate_def_epa_q2(season_data, nfl_week))
    print(calculate_def_epa_q3(season_data, nfl_week))
    print(calculate_def_epa_q4(season_data, nfl_week))

    print(calculate_pass_success_rate(season_data, nfl_week))
    print(calculate_run_success_rate(season_data, nfl_week))
    print(calculate_team_success_rate(season_data, nfl_week))
    print(calculate_explosive_play_rate(season_data, nfl_week))
    print(calculate_pass_epa(season_data, nfl_week))
    print(calculate_run_epa(season_data, nfl_week))
    print(calculate_team_epa(season_data, nfl_week))
    print(calculate_team_epa_q1(season_data, nfl_week))
    print(calculate_team_epa_q2(season_data, nfl_week))
    print(calculate_team_epa_q3(season_data, nfl_week))
    print(calculate_team_epa_q4(season_data, nfl_week))

    print(calculate_early_down_pass_rate_vs_epa(season_data, nfl_week))
    print(yac_epa_aa_season(season_data, nfl_week))

    print(calculate_week_receiver_adot(nfl_week, season_data))
    print(calculate_week_adot(nfl_week, season_data))
    print(calculate_week_adot_vs_ypa(season_data, nfl_week))
    print(calculate_week_qb_epa(nfl_week, season_data))

    print(calculate_week_def_pass_success_rate(season_data, nfl_week))
    print(calculate_week_def_run_success_rate(season_data, nfl_week))
    print(calculate_week_def_success_rate(season_data, nfl_week))
    print(calculate_week_def_explosive_play_rate(season_data, nfl_week))
    print(calculate_def_epa_week(season_data, nfl_week))
    print(calculate_def_pass_epa_week(season_data, nfl_week))
    print(calculate_def_run_epa_week(season_data, nfl_week))

    print(calculate_week_pass_success_rate(season_data, nfl_week))
    print(calculate_week_run_success_rate(season_data, nfl_week))
    print(calculate_week_team_success_rate(season_data, nfl_week))
    print(calculate_week_explosive_play_rate(season_data, nfl_week))
    print(calculate_pass_epa_week(season_data, nfl_week))
    print(calculate_run_epa_week(season_data, nfl_week))
    # print(calculate_team_epa_week(season_data, nfl_week))

    print(yac_epa_aa_week(nfl_week, season_data))
}



### Season Graph Functions ###

print_history_pdf <- function(category_name, method, first_year) {
    pdf(str_interp("${dirname}/${category_name}.pdf"))
    season_compilation(category_name, method, first_year)
    dev.off()
}

season_compilation <- function(name, method, first_year) {
    start <- first_year
    while (start <= SEASON) {
        data <- nflreadr::load_pbp(start)
        plot.new()
        text(0.5, 0.5, str_interp("${toString(start)} ${name}"))
        print(method(data, NUM_WEEKS))
        start <- start + 1
    }
}
### Season Graph Functions ###

print_roster_history_pdf <- function(category_name, method, first_year) {
    pdf(str_interp("${dirname}/${category_name}.pdf"))
    season_roster_compilation(category_name, method, first_year)
    dev.off()
}

season_roster_compilation <- function(name, method, first_year) {
    start <- first_year
    while (start <= SEASON) {
        roster <- nflfastR::fast_scraper_roster(start)
        games <- readRDS(url(str_interp(
            "https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_${start}.rds")))

        plot.new()
        text(0.5, 0.5, str_interp("${toString(start)} ${name}"))
        print(method(roster, games))
        start <- start + 1
    }
}



# Season QB Profiles

create_all_profiles <- function() {
    for (year in BEGINNING:SEASON) {
        season_profile_dir <- str_interp("NFL/Season QB Profiles/${year}")
        dir.create(season_profile_dir)
        make_season_profiles(year, load_pbp(year), season_profile_dir)
    }
}

create_one_season_profile <- function(year) {
    season_profile_dir <- str_interp("NFL/Season QB Profiles/${year}")
    dir.create(season_profile_dir)
    make_season_profiles(year, load_pbp(year), season_profile_dir)
}

create_qb_adot_data <- function(data, qbs, i) {
    qbs <- unlist(qbs)
    print(qbs[i])
    adot_data <- data  %>%
        filter(passer == qbs[i])  %>%
        filter(!is.na(air_yards))  %>%
        group_by(week)  %>%
        summarize(
            ADOT = round(mean(air_yards), 3), plays = n()
        )  %>%
        arrange(-week)

    return(adot_data)
}

create_qb_cpoe_data <- function(data, qbs, i) {
    qbs <- unlist(qbs)
    cpoe_data <- data  %>%
        filter(passer == qbs[i])  %>%
        filter(!is.na(cpoe))  %>%
        group_by(week)  %>%
        summarize(
            CPOE = round(mean(cpoe), 3), plays = n()
        )  %>%
        arrange(-week)

    return(cpoe_data)
}

create_qb_epa_data <- function(data, qbs, i) {
    qbs <- unlist(qbs)
    epa_data <- data  %>%
        filter(passer == qbs[i])  %>%
        filter(!is.na(epa))  %>%
        group_by(week)  %>%
        summarize(
            EPA = round(mean(epa), 3), plays = n()
        )  %>%
        arrange(-week)

    return(epa_data)
}

create_qb_profile <- function(data, qbs, i) {
    print("here")
    adot_data <- create_qb_adot_data(data, qbs, i)
    print("here 2")
    cpoe_data <- create_qb_cpoe_data(data, qbs, i)
    print("here 3")
    epa_data <- create_qb_epa_data(data, qbs, i)

    print(ggplot(adot_data, aes(x = week, y = ADOT)) +
        geom_text_repel(aes(label = ADOT)) +
        geom_point() +
        geom_hline(yintercept = mean(
            adot_data$ADOT), color = "red", linetype = "dashed", alpha = 0.3) +
        ggtitle(str_interp("Season ADOT by Week for ${qbs[i]}")))

    print(ggplot(cpoe_data, aes(x = week, y = CPOE)) +
        geom_text_repel(aes(label = CPOE)) +
        geom_point() +
        geom_hline(yintercept = mean(
            cpoe_data$CPOE), color = "#583bda",
            linetype = "dashed", alpha = 0.3) +
        ggtitle(str_interp("Season CPOE by Week for ${qbs[i]}")))

    print(ggplot(epa_data, aes(x = week, y = EPA)) +
        geom_text_repel(aes(label = EPA)) +
        geom_point() +
        geom_hline(yintercept = mean(
            epa_data$EPA), color = "#00d9ff",
            linetype = "dashed", alpha = 0.3) +
        ggtitle(str_interp("Season EPA by Week for ${qbs[i]}")))
}

create_season_qb_profiles <- function(season, data, qbs, renamed_qbs,
        directory) {
    test <- unlist(renamed_qbs)
    for (i in seq_len(length(test))) {
        pdf(str_interp("${directory}/${test[i]}.pdf"))
        create_qb_profile(data, qbs, i)
        dev.off()
    }
}

get_qbs_and_renamed <- function(season, data) {
    qbs <- get_qualifying_qbs(data)
    renamed_qbs <- vector(, length(qbs))
    for (i in 1:(length(qbs))) {
        name <- paste(substr(qbs[i], 1, 1),
            substr(qbs[i], 3, length(qbs)), sep = "_")
        renamed_qbs[i] <- name
    }

    return(list(qbs, renamed_qbs))
}

get_qualifying_qbs <- function(data) {
    filtered_data <- pass_filter(data)
    qb_list <- filtered_data  %>%
        filter(!is.na(passer))  %>%
        group_by(passer) %>%
        summarize(plays = n())  %>%
        filter(plays >= NUM_WEEKS * ATTEMPT_INTERVAL)

    return(qb_list$passer)
}

make_season_profiles <- function(season, data, directory) {
    qb_lists <- get_qbs_and_renamed(season, data)
    create_season_qb_profiles(season, filtered_data, qb_lists[1], qb_lists[2],
        directory)
}

# Career QB profiles

get_all_qualifiers <- function() {
    qualifiers <- NULL
    renamed_qualifiers <- vector(, 0)
    for (year in BEGINNING:SEASON) {
        results <- get_qbs_and_renamed(year, load_pbp(year))
        if (is.null(qualifiers)) {
            qualifiers <- results[1]
            renamed_qualifiers <- results[2]
        } else {
            qualifiers <- Map(c, qualifiers, results[1])
            renamed_qualifiers <- Map(c, renamed_qualifiers, results[2])
        }
    }

    return(qualifiers)
}

sort_qualifiers <- function() {
    qualifiers <- get_all_qualifiers()
    test <- qualifiers[1]
    print(unique(test[[1]]))

    # return(qb_dict)
}

get_week_average_points <- function(data, season, nfl_week, team) {
    points_in_week <- 0
    num_teams <- 32
    for (team in teams) {
        total <- data %>%
            filter(data$week == nfl_week & (data$home_team == team)) %>%
            summarize(
                points = last(total_home_score)
            )

        if (is.na(total$points[[1]])) {
            total <- data %>%
            filter(week == nfl_week & (away_team == team)) %>%
            summarize(
                points = last(total_away_score)
            )
        }

        if (is.na(total$points[[1]])) {
            num_teams <- num_teams - 1
            next
        }

        points_in_week <- points_in_week + total$points[[1]]
    }

    return(points_in_week / num_teams)
}

get_average_fgs <- function(data, year, week) {
    games <- week * 32
    field_goals <- data %>%
        filter(week <= get_num_weeks(year)) %>%
        summarize(
            field_goals = round(sum(field_goal_attempt, na.rm = TRUE) / games, 2),
            makes = round(sum(field_goal_result == "made", na.rm = TRUE) / games, 2),
            pct = makes / field_goals
        ) 
}

get_average_tds <- function(data, year, week) {
    games <- week * 32
    touchdowns <- data %>%
        filter(week <= get_num_weeks(year)) %>%
        summarize(
            touchdowns = round(sum(touchdown, na.rm = TRUE) / games, 2),
        )
}

get_week_weight <- function(week) {
    val <- -0.086 + 0.193*week - 0.0071*week^2
    if (val > 1) {
        val <- 1
    }

    return(val)
}

meld_average_tds <- function(data, year, week) {
    last <- get_average_tds(data[[toString(year - 1)]], year - 1, get_num_weeks(year - 1))$touchdowns
    this <- get_average_tds(data[[toString(year)]], year, week)$touchdowns
    weight <- get_week_weight(week)

    return (this*weight + last*(1-weight))
}

meld_average_fgs <- function(data, year, week) {
    last <- get_average_fgs(data[[toString(year - 1)]], year - 1, get_num_weeks(year - 1))$makes
    this <- get_average_fgs(data[[toString(year)]], year, week)$makes

    weight <- get_week_weight(week)
    return (this*weight + last*(1-weight))
}

get_league_success_rate <- function(data, nfl_week) {
    success_rate_data <- data %>%
        filter(week <= nfl_week) %>%
        summarize(
            success_rate = round(mean(epa > 0, na.rm = TRUE), 3)
        )  %>%
        arrange(-success_rate)

    return(success_rate_data)
}

get_league_epa <- function(data, nfl_week) {
    epa_data <- data %>%
        filter(week <= nfl_week) %>%
        summarize(
            epa_per_play = round(mean(epa, na.rm = TRUE), 3)
        )  %>%
        arrange(-epa_per_play)

    return(epa_data)
}

get_league_exp <- function(data, nfl_week) {
    exp_data <- data %>%
        filter(week <= nfl_week) %>%
        summarize(
            plays = n(),
            explosive_plays = sum(yards_gained >= 20, na.rm = TRUE),
            rate = explosive_plays / plays
        )  %>%
        arrange(-rate)

    return(exp_data)
}

resize_cpoe <- function(num) {
  OLDMAX <- 7
  OLDMIN <- -7
  NEWMAX <- 1.75
  NEWMIN <- -1.75
  OLDDIFF <- OLDMAX - OLDMIN
  NEWDIFF <- NEWMAX - NEWMIN

  old_pct <- (num - OLDMIN) / OLDDIFF
  return(round(NEWDIFF * old_pct + NEWMIN, 1))
}

resize_grade <- function(num) {
  OLDMAX <- 100
  OLDMIN <- 40
  NEWMAX <- 1.75
  NEWMIN <- -1.75
  OLDDIFF <- OLDMAX - OLDMIN
  NEWDIFF <- NEWMAX - NEWMIN

  old_pct <- (num - OLDMIN) / OLDDIFF
  return(round(NEWDIFF * old_pct + NEWMIN, 1))
}

resize_off <- function(num) {
  OLDMAX <- 0.35
  OLDMIN <- -0.35
  NEWMAX <- 7
  NEWMIN <- -7
  OLDDIFF <- OLDMAX - OLDMIN
  NEWDIFF <- NEWMAX - NEWMIN

  old_pct <- (num - OLDMIN) / OLDDIFF
  return(round(NEWDIFF * old_pct + NEWMIN, 1))
}

resize_def <- function(num) {
  OLDMAX <- -0.25
  OLDMIN <- 0.25
  NEWMAX <- 7
  NEWMIN <- -7
  OLDDIFF <- OLDMAX - OLDMIN
  NEWDIFF <- NEWMAX - NEWMIN

  old_pct <- (num - OLDMIN) / OLDDIFF
  return(round(NEWDIFF * old_pct + NEWMIN, 1))
}

resize_qb_comp <- function(num) {
  OLDMAX <- 1.75
  OLDMIN <- -1.75
  NEWMAX <- 7
  NEWMIN <- -7
  OLDDIFF <- OLDMAX - OLDMIN
  NEWDIFF <- NEWMAX - NEWMIN

  old_pct <- (num - OLDMIN) / OLDDIFF
  return(round(NEWDIFF * old_pct + NEWMIN, 1))
}

resize_pyth <- function(num) {
  OLDMAX <- 0.7
  OLDMIN <- 0.3
  NEWMAX <- 5
  NEWMIN <- -5
  OLDDIFF <- OLDMAX - OLDMIN
  NEWDIFF <- NEWMAX - NEWMIN

  old_pct <- (num - OLDMIN) / OLDDIFF
  return(round(NEWDIFF * old_pct + NEWMIN, 1))
}

resize_grade <- function(num) {
  OLDMAX <- 1.5
  OLDMIN <- -1.5
  NEWMAX <- 5
  NEWMIN <- -5
  OLDDIFF <- OLDMAX - OLDMIN
  NEWDIFF <- NEWMAX - NEWMIN

  old_pct <- (num - OLDMIN) / OLDDIFF
  return(round(NEWDIFF * old_pct + NEWMIN, 1))
}

resize_qb_val <- function(num) {
  OLDMAX <- 0.4
  OLDMIN <- -0.6
  NEWMAX <- 5
  NEWMIN <- -5
  OLDDIFF <- OLDMAX - OLDMIN
  NEWDIFF <- NEWMAX - NEWMIN

  old_pct <- (num - OLDMIN) / OLDDIFF
  return(round(NEWDIFF * old_pct + NEWMIN, 1))
}


NUM_WEEKS <- 18
WEEK <- 8
TARGET_INTERVAL <- 3
ATTEMPT_INTERVAL <- 10
RUSH_INTERVAL <- 4
SEASON <- 2021
BEGINNING <- 2006

# print(top_epa_plays_season(season_data))
# print(top_wpa_plays_season(season_data))
# print(worst_epa_plays_season(season_data))
# print(worst_wpa_plays_season(season_data))

season <- 2023
# print_weekly_pdf(str_interp("Weekly Reports/${season}"), season, 76,
#     nflreadr::load_pbp(season))
# create_one_season_profile(2021)

dirname <- str_interp("Historical Reports")
# dir.create(dirname)

# print_history_pdf("ADOT vs EPA", calculate_season_adot_vs_epa, 2006)
# print_history_pdf("ADOT vs YPA", calculate_season_adot_vs_ypa, 1999)
# print_history_pdf("CPOE vs ADOT", calculate_season_cpoe_vs_adot, 2006)
# print_history_pdf("CPOE vs EPA", calculate_season_epa_vs_cpoe, 2006)
# print_history_pdf("EPA for QBs", calculate_season_qb_epa, 2006)
# print_roster_history_pdf("EPA vs RB Targets", calculate_epa_vs_rb_targets, 2006)
# print_history_pdf("Defensive Explosive Play Rate", calculate_def_explosive_play_rate, 1999)
# print_history_pdf("Defensive Passing Success Rate", calculate_def_pass_success_rate, 2006)
# print_history_pdf("Defensive Rushing Success Rate", calculate_def_run_success_rate, 2006)
# print_history_pdf("Defensive Pass EPA", calculate_def_pass_epa, 2006)
# print_history_pdf("Defensive Rush EPA", calculate_def_run_epa, 2006)
# print_history_pdf("Defensive EPA", calculate_def_epa, 2006)
# print_history_pdf("Explosive Play Rate", calculate_explosive_play_rate, 1999)
# print_history_pdf("Offensive EPA", calculate_team_epa, 2006)
# print_history_pdf("Pass Rate vs 3rd and Long Rate", early_down_pass_rate_vs_3rd_and_long, 1999)
# print_history_pdf("Passing Success Rate", calculate_pass_success_rate, 2006)
# print_history_pdf("Passing Success Rate vs EPA", calculate_pass_success_vs_epa, 2006)
# print_history_pdf("Pythagorean Projection vs Winning Percentage", team_wp_vs_pp, 2002)
# print_history_pdf("Rushing Success Rate", calculate_run_success_rate, 2006)
# print_history_pdf("Rushing Success Rate vs EPA", calculate_run_success_vs_epa, 2006)
# print_history_pdf("Season Pass Rate", calculate_season_pass_rate, 1999)
# print_history_pdf("Season Pass Rate vs EPA", calculate_early_down_pass_rate_vs_epa, 2006)
# print_history_pdf("Team Strength of Schedule", team_sos, 1999)

# print_all_weeks()

# calculate_team_success_rate(nflreadr::load_pbp(2022), 1)

# pass_vs_run_conversion <- function(year) {
#     data <- nflreadr::load_pbp(year)
#     run_conversion_data <- data %>%
#         filter(wp > .10 & wp < 0.9 & down >= 3 & rush == 1 &
#             play_type != "no_play" & ydstogo <= 2) %>%
#         summarize (
#             conversion_rate = round(mean(first_down_rush == 1), 3)
#         )
         
#     print(run_conversion_data)
    
#     pass_conversion_data <- data %>%
#         filter(wp > .10 & wp < 0.9 & down >= 3 & pass == 1 &
#             play_type != "no_play" & ydstogo <= 2) %>%
#         summarize (
#             conversion_rate = round(mean(first_down_pass == 1), 3)
#         )
         
#     print(pass_conversion_data)
# }

# pass_vs_run_goal_line_conversion <- function(year) {
#     data <- nflreadr::load_pbp(year)
#     run_conversion_data <- data %>%
#         filter(rush == 1 & yardline_100 <= 2 &
#             play_type != "no_play") %>%
#         group_by(posteam) %>%
#         summarize (
#             plays = n(),
#             conversion_rate = round(mean(rush_touchdown == 1), 3)
#         )
         
#     print(run_conversion_data)
    
#     pass_conversion_data <- data %>%
#         filter(pass == 1 & yardline_100 <= 2 &
#             play_type != "no_play") %>%
#         group_by(posteam) %>%
#         summarize (
#             plays = n(),
#             conversion_rate = round(mean(pass_touchdown == 1), 3)
#         )
         
#     print(pass_conversion_data)
# }

fourth_conversions <- function(year) {
    data <- nflreadr::load_pbp(year)
    run_conversion_data <- data %>%
        filter((pass == 1 | rush == 1) & down == 4 & play_type != "no_play" & ydstogo == 1) %>%
        summarize (
            plays = n(),
            conversion_rate = round(mean(first_down == 1), 3)
        )
         
    print(run_conversion_data)
}

# fourth_conversions(2017:2022)
# pass_vs_run_goal_line_conversion(2021)



garbage_time <- function(data) {
    gt_df <- data  %>%
        filter(down <= 4 & qtr <= 4) %>%
        filter(pass == 1 & play_type != "no_play")  %>%
        filter(week <= 6) %>%
        group_by(posteam) %>%
        summarize(
            epa_gt = mean(epa, na.rm = TRUE)
        )
        
    no_gt_df <- fetch_pass_epa(data, 6) %>%
    left_join(gt_df, by = "posteam") %>%
    left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

    print(ggplot(no_gt_df, aes(x = epa_per_play, y = epa_gt)) +
        geom_image(aes(image = team_logo_espn), image_fun = transparent,
            size = 0.03, asp = 7 / 7) +
        geom_smooth(method = "lm") +
        stat_cor(method = "pearson") +
        ggtitle("The Garbage Time Difference") +
        xlab("No Garbage Time") + ylab("Garbage Time"))
}

# garbage_time(nflreadr::load_pbp(2021))

# dev.off()