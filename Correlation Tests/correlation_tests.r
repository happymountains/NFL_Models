library(data.table)
library(ggimage)
library(ggpubr)
library(ggrepel)
library(gridExtra)
library(hash)
library(nflfastR)

source("stats.r")

options(scipen = 9999)

# spread_vs_pyth <- function(season, num_weeks) {
#     data <- load_pbp(season)
    
#     pyth <- team_pythagorean_projection(data, num_weeks)
#     spread <- team_spread_win_percentage(data, num_weeks)

#     teams <- keys(pyth)
#     pyth_values <- vector(, 32)
#     spread_values <- vector(, 32)

#     for (i in 1:length(teams)) {
#         team <- teams[i]
#         pyth_values[i] <- pyth[[team]]
#         spread_values[i] <- spread[[team]]
#     }

#     graph_df <- data.frame(team, pyth_values, spread_values)
#     ggplot(graph_df, aes(x = pyth_values,
#             y = spread_values)) +
#         geom_text_repel(aes(label = teams)) +
#         geom_point() +
#         geom_smooth(method = "lm") +
#         stat_cor(method = "pearson") +
#         ggtitle(str_interp("Test: ${season}"))
# }

# spread_vs_sos <- function(season, num_weeks) {
#     data <- load_pbp(season)
    
#     sos <- team_sos(data, num_weeks)
#     spread <- team_spread_win_percentage(data, num_weeks)
#     teams <- keys(sos)
#     sos_values <- vector(, 32)
#     spread_values <- vector(, 32)
#     for (i in 1:length(teams)) {
#         team <- teams[i]
#         sos_values[i] <- sos[[team]]
#         spread_values[i] <- spread[[team]]
#     }

#     graph_df <- data.frame(team, sos_values, spread_values)
#     ggplot(graph_df, aes(x = sos_values,
#             y = spread_values)) +
#         geom_text_repel(aes(label = teams)) +
#         geom_point() +
#         geom_smooth(method = "lm") +
#         stat_cor(method = "pearson") +
#         ggtitle(str_interp("Test: ${season}"))
# }

# spread_vs_def_epa <- function(season, num_weeks) {
#     data <- load_pbp(season)
    
#     depa <- calculate_def_epa(data, num_weeks)
#     spread <- team_spread_win_percentage(data, num_weeks)
#     teams <- keys(depa)
#     depa_values <- vector(, 32)
#     spread_values <- vector(, 32)
#     for (i in 1:length(teams)) {
#         team <- teams[i]
#         depa_values[i] <- depa[[team]]
#         spread_values[i] <- spread[[team]]
#     }

#     graph_df <- data.frame(team, depa_values, spread_values)
#     ggplot(graph_df, aes(x = depa_values,
#             y = spread_values)) +
#         geom_text_repel(aes(label = teams)) +
#         geom_point() +
#         geom_smooth(method = "lm") +
#         stat_cor(method = "pearson") +
#         ggtitle(str_interp("Test: ${season}"))
# }

resize_list <- function(list) {
    NEWMAX <- 100
    NEWMIN <- 0
    old_max <- max(list)
    old_min <- min(list)
    old_diff <- old_max - old_min

    converted <- c()
    for (index in 1:length(list)) {
        val <- list[[index]]
        old_pct <- (val - old_min) / old_diff
        tes <- round(NEWMAX * old_pct + NEWMIN, 1)
        converted <- append(converted, tes)
    }

    return(converted)
}

tds_vs_off_succ <- function(season, num_weeks) {
    data <- nflreadr::load_pbp(season)
    tds <- get_average_tds(data, season)
    lsr <- get_league_success_rate(data, num_weeks)
    return(c(tds$touchdowns, lsr$success_rate))
}

tds_vs_succ_epa <- function(season, num_weeks) {
    data <- nflreadr::load_pbp(season)
    tds <- get_average_tds(data, season, num_weeks)
    lsr <- get_league_success_rate(data, num_weeks)
    epa <- get_league_epa(data, num_weeks)
    return(c(tds$touchdowns, lsr$success_rate, epa$epa_per_play))
}

fgs_vs_succ_epa <- function(season, num_weeks) {
    data <- nflreadr::load_pbp(season)
    fgs <- get_average_fgs(data, season, num_weeks)
    lsr <- get_league_success_rate(data, num_weeks)
    epa <- get_league_epa(data, num_weeks)
    return(c(fgs$makes, lsr$success_rate, epa$epa_per_play))
}

fgs_vs_off_succ <- function(season, num_weeks) {
    data <- nflreadr::load_pbp(season)
    fgs <- get_average_fgs(data, season)
    lsr <- get_league_success_rate(data, num_weeks)
    return(c(fgs$makes, lsr$success_rate))
}

tds_vs_epa <- function(season, num_weeks) {
    data <- nflreadr::load_pbp(season)
    tds <- get_average_tds(data, season)
    epa <- get_league_epa(data, num_weeks)
    return(c(tds$touchdowns, epa$epa_per_play))
}

fgs_vs_epa <- function(season, num_weeks) {
    data <- nflreadr::load_pbp(season)
    fgs <- get_average_fgs(data, season)
    epa <- get_league_epa(data, num_weeks)
    return(c(fgs$makes, epa$epa_per_play))
}

tds_vs_exp <- function(season, num_weeks) {
    data <- nflreadr::load_pbp(season)
    tds <- get_average_tds(data, season)
    exp <- get_league_exp(data, num_weeks)
    return(c(tds$touchdowns, exp$rate))
}

fgs_vs_exp <- function(season, num_weeks) {
    data <- nflreadr::load_pbp(season)
    fgs <- get_average_fgs(data, season)
    exp <- get_league_exp(data, num_weeks)
    return(c(fgs$makes, exp$rate))
}



pdf("FGvSR-EPA_Correlation.pdf")
# num_weeks <- 17
# years <- c()
# x_axis <- c()
# y_axis <- c()
# for (year in 2006:2023) {
#     print(year)
#     if (year == 2021) {
#         num_weeks <- 18
#     }

#     result <- fgs_vs_exp(year, num_weeks - 1)
#     years <- append(years, year)
#     x_axis <- append(x_axis, result[[1]])
#     y_axis <- append(y_axis, result[[2]])
# }

num_weeks <- 17
years <- c()
x_axis <- c()
y_axis <- c()
comp1 <- c()
comp2 <- c()

for (year in 2006:2023) {
    print(year)
    if (year == 2021) {
        num_weeks <- 18
    }

    result <- fgs_vs_succ_epa(year, num_weeks - 1)
    years <- append(years, year)
    x_axis <- append(x_axis, result[[1]])
    comp1 <- append(comp1, result[[2]])
    comp2 <- append(comp2, result[[3]])
}

C1_WEIGHT <- 1.3
C2_WEIGHT <- -0.3
comp1 <- resize_list(comp1)
comp2 <- resize_list(comp2)
for (index in 1:length(comp1)) {
    c1 <- comp1[[index]]
    c2 <- comp2[[index]]
    y_axis <- append(y_axis, C1_WEIGHT * c1 + C2_WEIGHT * c2)
}

graph_df <- data.frame(years, x_axis, y_axis)
print(graph_df)
print(ggplot(graph_df, aes(x = x_axis,
        y = y_axis)) +
    geom_text_repel(aes(label = years)) +
    geom_point() +
    geom_smooth(method = "lm") +
    stat_cor(method = "pearson") +
    ggtitle(str_interp("KP Corr Test")))

dev.off()