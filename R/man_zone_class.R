source('R/load_utils.R')
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(zoo) 
library(magrittr)
library(mclust)

weeks <- data.frame()
# for each week, do the general cleanup and bind
for(i in 1:17) {
  week <- read_csv(paste0('data/week', i, '.csv' ))
  week$event[week$event == 'None'] <- NA
  week <- week %>%
    group_by(playId, gameId, nflId) %>%
    slice(-n())
  week <- week %>% fill(event, .direction = 'down')
  week$event[is.na(week$event)] <- 'pre_snap'
  week <- week %>% filter(event == 'ball_snap') 
  weeks <- rbind.data.frame(weeks, week)
}
weeks <- ungroup(weeks)

non_week <- read_non_week_files()
# set play direction to binary with left being 1
weeks$playDirection <- as.integer(weeks$playDirection == 'left')

# set explicit orientation difference from play direction
weeks$o_diff_from_play <- ifelse(weeks$playDirection == 1, abs(270 - weeks$o), abs(weeks$o - 90))

# check to see if player is turned away from LOS
weeks$turned_to_line <- ifelse(weeks$o_diff_from_play <= 90, 1, 0)
weeks <- weeks %>% 
  ungroup() %>%
  filter(!is.na(nflId))

# before going forward, want to measure distance from line of scrimmage
# need to merge plays dataset
weeks <- left_join(weeks, select(non_week$plays, gameId, playId, absoluteYardlineNumber))

# set up function to calculate distances for each frame
frame_func <- function(df) {
  distances <- as.matrix(dist(df[,c('x', 'y')]))
  
  # apply() on distances matrix
  def_dist <- unlist(apply(distances, 2, function(x) {
    min(x[which(x != 0 & df$position %in% DEFENSE_POSITIONS)])
  }))
  off_dist <- unlist(apply(distances, 2, function(x) {
    min(x[which(x != 0 & df$position %in% OFFENSE_POSITIONS)])
  }))
  
  teammate_x <- sapply(1:length(def_dist), function(y) {
    df$x[which(def_dist[y] == distances[,y])][1]
  })
  teammate_y <- sapply(1:length(def_dist), function(y) {
    df$y[which(def_dist[y] == distances[,y])][1]
  })
  
  opponent_x <- sapply(1:length(off_dist), function(y) {
    df$x[which(off_dist[y] == distances[,y])][1]
  })
  opponent_y <- sapply(1:length(off_dist), function(y) {
    df$y[which(off_dist[y] == distances[,y])][1]
  })
  
  opponent_dir <- sapply(1:length(off_dist), function(y) {
    df$dir[which(off_dist[y] == distances[,y])][1]
  })
  
  opponent_o <- sapply(1:length(off_dist), function(y) {
    df$o[which(off_dist[y] == distances[,y])][1]
  })
  
  
  df$teammate_x <- teammate_x
  df$teammate_y <- teammate_y
  df$opponent_x <- opponent_x
  df$opponent_y <- opponent_y
  df$def_dist <- def_dist
  df$off_dist <- off_dist
  df$opponent_dir <- opponent_dir
  df$opponent_o <- opponent_o
  return(df)
}

frame_func(filter(weeks, gameId == 2018100710, playId == 3659, frameId == 27))
# apply our frame function to groupings, filter out plays that undercounted players
df <- weeks %>%
  group_by(gameId, playId, frameId) %>%
  dplyr::distinct() %>% 
  dplyr::filter(n() >= 11) %>%
  dplyr::group_modify(~ frame_func(.)) %>%
  dplyr::ungroup()

rm(weeks)

# establish direction ratios
df$dir_ratio <- df$off_dist / sqrt((df$opponent_x - df$teammate_x)**2 + (df$opponent_y - df$teammate_y)**2)
df$dist_from_los <- abs(df$x - df$absoluteYardlineNumber)

# measure whether or not faced in same direction from nearest receiver
df$in_opp_direction <- ifelse(abs(df$o - df$opponent_o) < 45, 1, 0)

grouped <- df %>% group_by(nflId, gameId, playId, event ) %>%
  summarise(var_x = var(x, na.rm = T),
            var_y = var(y, na.rm = T),
            var_s = var(s, na.rm = T), 
            sd_o = sd(o, na.rm = T),
            var_dist = var(dis, na.rm = T),
            var_opp_dist = var(off_dist, na.rm = T),
            dir_ratio_var = var(dir_ratio, na.rm = T),
            dir_ratio_mean = mean(dir_ratio, na.rm = T),
            pct_within_o = mean(in_opp_direction, na.rm = T),
            pct_towards_los = mean(turned_to_line, na.rm = T)
            ) %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))

# left merge df <------ grouped
df <- left_join(df, grouped, by = c('gameId', 'event', 'playId', 'nflId'))


# set up gmm
# we don't need a ton to train on, but we want to train on DBs 
# can take model trained on DBs and apply it to LBs although it won't give a good result if player is   # blitzing 
# first only leave defensive players
# exclude direct player tracking
exclude_vars <- c('opponent_x', 'opponent_y', 'teammate_x', 'teammate_y', 'opponent_dir', 'opponent_o', 'in_opp_direction')

set.seed(123)
# grab train set
defense_train <- df %>% 
  filter(position %in% DEFENSIVE_BACK_POSITIONS) %>% 
  select(-exclude_vars) %>% 
  group_by(nflId, gameId, playId) %>%
  arrange(frameId) %>%
  slice(n()) %>% 
  ungroup() %>%
  sample_frac(0.3)
# scale values
scaled_train <- na.fill(scale(defense_train[,27:ncol(defense_train)]), 0)
# fit on 1/3 of DBs dataset
fit <- Mclust(scaled_train, G = 2)


# grab dataframe of all defense we care about
defense <- df %>% 
  filter(position %in% DEFENSIVE_BACK_POSITIONS | position %in% LINEBACKER_POSITIONS) %>%
  select(-exclude_vars) %>%
  group_by(nflId, gameId, playId) %>%
  arrange(frameId) %>%
  slice(n()) %>%
  ungroup() 

scaled_defense <- na.fill(scale(defense[,27:ncol(defense)]), 0)

# generate predictions
probs <- as.data.frame(predict(fit, newdata = scaled_defense)$z)

defense$man_prob <- probs$`1`
defense$zone_prob <- probs$`2`


# narrow and save predictions 
preds <- defense %>% select(gameId, playId, nflId, man_prob, zone_prob)
# save to csv 
write.csv('data/man_zone_preds.csv', x = preds)
