#' full feature engineering and classification for man v zone coverages
#' WARNING: this takes a lot of time to run
#'
#' @param df DataFrame of position data to be transformed
#' @return data.frame: man vs zone predictions for non-lineman on a given play
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom mclust Mclust
#' @importFrom tidyr fill
#' @import zoo
#' @import dplyr
#' @export
#'
generate_man_zone_class <- function() {

weeks <- data.frame()
# for each week, do the general cleanup and bind

# load in pbp data
weeks <- aggregate_week_files() %>% mutate(event = replace(event, event == 'None', NA)) %>%
  group_by(playId, gameId, nflId) %>%
  slice(-n()) %>%
  fill(event, .direction = 'down') %>%
  filter(event == 'ball_snap')

weeks <- ungroup(weeks)

non_week <- read_non_week_files()

# add binary play direction and player orientation from LOS, merge non_plays
weeks <- weeks %>%
  mutate(playDirection = as.integer(playDirection == 'left'),
         o_diff_from_play = ifelse(playDirection == 1, abs(270 - o), abs(o - 90)),
         turned_to_line = ifelse(o_diff_from_play <= 90, 1, 0)) %>%
  filter(!is.na(nflId))

# add yardline from plays dataset
weeks <- left_join(weeks, select(non_week$plays, gameId, playId, absoluteYardlineNumber))


# set up function to calculate distances for each frame
calculate_play_pairwise_dists <- function(df) {
  distances <- as.matrix(dist(df[,c('x', 'y')]))

  # apply() on distances matrix
  df$def_dist <- unlist(apply(distances, 2, function(x) {
    min(x[which(x != 0 & df$position %in% DEFENSE_POSITIONS)])
  }))
  df$off_dist <- unlist(apply(distances, 2, function(x) {
    min(x[which(x != 0 & df$position %in% OFFENSE_POSITIONS)])
  }))

  df$teammate_x <- unlist(purrr::map(1:length(df$def_dist), function(y) {
    df$x[which(df$def_dist[y] == distances[,y])][1]
  }))
  df$teammate_y <- unlist(purrr::map(1:length(df$def_dist), function(y) {
    df$y[which(df$def_dist[y] == distances[,y])][1]
  }))

  df$opponent_x <- unlist(purrr::map(1:length(df$off_dist), function(y) {
    df$x[which(df$off_dist[y] == distances[,y])][1]
  }))
  df$opponent_y <- unlist(purrr::map(1:length(df$off_dist), function(y) {
    df$y[which(df$off_dist[y] == distances[,y])][1]
  }))

  df$opponent_dir <- unlist(purrr::map(1:length(df$off_dist), function(y) {
    df$dir[which(df$off_dist[y] == distances[,y])][1]
  }))

  df$opponent_o <- unlist(purrr::map(1:length(df$off_dist), function(y) {
    df$o[which(df$off_dist[y] == distances[,y])][1]
  }))


  return(df)
}


# apply our frame function to groupings, filter out plays that undercounted players
# at the end we're adding additional directional and los variables
weeks <- weeks %>%
  group_by(gameId, playId, frameId) %>%
  distinct() %>%
  filter(n() >= 11) %>%
  group_modify(~ calculate_play_pairwise_dists(.)) %>%
  ungroup() %>%
  mutate(dir_ratio = off_dist / sqrt((opponent_x - teammate_x)**2 + (opponent_y - teammate_y)**2),
         dist_from_los = abs(x - absoluteYardlineNumber),
         in_opp_direction = ifelse(abs(o - opponent_o) < 45, 1, 0))




grouped_weeks <- weeks %>% group_by(nflId, gameId, playId, event ) %>%
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
  ) %>% mutate((across(where(is.numeric), function(x) ifelse(is.na(x), 0, x))))

# left merge weeks <------ grouped_weeks
weeks <- left_join(weeks, grouped_weeks, by = c('gameId', 'event', 'playId', 'nflId'))


# set up gmm
# we don't need a ton to train on, but we want to train on DBs
# can take model trained on DBs and apply it to LBs although it won't give a good result if player is   # blitzing
# first only leave defensive players
# exclude direct player tracking
exclude_vars <- c('opponent_x', 'opponent_y', 'teammate_x', 'teammate_y', 'opponent_dir', 'opponent_o', 'in_opp_direction')



# grab dataframe of all defense we care about
defense <- weeks %>%
  filter(position %in% DEFENSIVE_BACK_POSITIONS | position %in% LINEBACKER_POSITIONS) %>%
  select(-exclude_vars) %>%
  group_by(nflId, gameId, playId) %>%
  arrange(frameId) %>%
  slice(n()) %>%
  ungroup()


set.seed(123)
# grab train set
defense_train <- defense %>%
  filter(position %in% DEFENSIVE_BACK_POSITIONS) %>%
  sample_frac(0.7)
# scale values
scaled_train <- na.fill(scale(defense_train[,27:ncol(defense_train)]), 0)
# fit on 70% of DBs dataset
# G here is our number of clusters
fit <- Mclust(scaled_train, G = 2)



# scale whole defensive set
scaled_defense <- na.fill(scale(defense[,27:ncol(defense)]), 0)

# generate predictions
probs <- as.data.frame(predict(fit, newdata = scaled_defense)$z)

defense$man_prob <- probs$`1`
defense$zone_prob <- probs$`2`


# narrow and return predictions frame
preds <- defense %>% select(gameId, playId, nflId, man_prob, zone_prob)

return(preds)
}
