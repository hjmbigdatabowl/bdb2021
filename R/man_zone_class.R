#' full feature engineering and classification for man v zone coverages
#' WARNING: this takes a lot of time to run
#'
#' @return a dataframe of man vs zone predictions for non-lineman on a given play
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom mclust Mclust
#' @importFrom tidyr fill
#' @importFrom rlang .data
#' @importFrom stats sd var dist
#' @import zoo
#' @import dplyr
#' @import tidyselect
#' @export
#'
generate_man_zone_class <- function() {
  where <- NULL
  OFFENSE_POSITIONS <- get_constants("OFFENSE_POSITIONS")
  DEFENSE_POSITIONS <- get_constants("DEFENSE_POSITIONS")
  DEFENSIVE_BACK_POSITIONS <- NULL
  LINEBACKER_POSITIONS <- NULL

  # set up function to calculate distances for each frame
  calculate_play_pairwise_dists <- function(df) {
    distances <- as.matrix(dist(df[, c("x", "y")]))

    # apply() on distances matrix
    df$def_dist <- unlist(apply(distances, 2, function(x) {
      min(x[which(x != 0 & df$position %in% DEFENSE_POSITIONS)])
    }))
    df$off_dist <- unlist(apply(distances, 2, function(x) {
      min(x[which(x != 0 & df$position %in% OFFENSE_POSITIONS)])
    }))

    df$teammate_x <- unlist(purrr::map(1:length(df$def_dist), function(y) {
      df$x[which(df$def_dist[y] == distances[, y])][1]
    }))
    df$teammate_y <- unlist(purrr::map(1:length(df$def_dist), function(y) {
      df$y[which(df$def_dist[y] == distances[, y])][1]
    }))

    df$opponent_x <- unlist(purrr::map(1:length(df$off_dist), function(y) {
      df$x[which(df$off_dist[y] == distances[, y])][1]
    }))
    df$opponent_y <- unlist(purrr::map(1:length(df$off_dist), function(y) {
      df$y[which(df$off_dist[y] == distances[, y])][1]
    }))

    df$opponent_dir <- unlist(purrr::map(1:length(df$off_dist), function(y) {
      df$dir[which(df$off_dist[y] == distances[, y])][1]
    }))

    df$opponent_o <- unlist(purrr::map(1:length(df$off_dist), function(y) {
      df$o[which(df$off_dist[y] == distances[, y])][1]
    }))


    return(df)
  }

  non_week <- read_non_week_files()

  # load in pbp data
  weeks <- aggregate_week_files() %>%
    mutate(event = replace(.data$event, .data$event == "None", NA)) %>%
    group_by(.data$playId, .data$gameId, .data$nflId) %>%
    slice(-n()) %>%
    fill(.data$event, .direction = "down") %>%
    filter(.data$event == "ball_snap") %>%
    ungroup() %>%
    mutate(
      playDirection = as.integer(.data$playDirection == "left"),
      o_diff_from_play = ifelse(.data$playDirection == 1, abs(270 - .data$o), abs(.data$o - 90)),
      turned_to_line = ifelse(.data$o_diff_from_play <= 90, 1, 0)
    ) %>%
    filter(!is.na(.data$nflId)) %>%
    left_join(non_week$plays %>% select(.data$gameId, .data$playId, .data$absoluteYardlineNumber)) %>%
    group_by(.data$gameId, .data$playId, .data$frameId) %>%
    distinct() %>%
    filter(n() >= 11) %>%
    group_modify(~ calculate_play_pairwise_dists(.)) %>%
    ungroup() %>%
    mutate(
      dir_ratio = .data$off_dist / sqrt((.data$opponent_x - .data$teammate_x)**2 + (.data$opponent_y - .data$teammate_y)**2),
      dist_from_los = abs(.data$x - .data$absoluteYardlineNumber),
      in_opp_direction = ifelse(abs(.data$o - .data$opponent_o) < 45, 1, 0)
    )




  grouped_weeks <- weeks %>%
    group_by(.data$nflId, .data$gameId, .data$playId, .data$event) %>%
    summarise(
      var_x = var(.data$x, na.rm = T),
      var_y = var(.data$y, na.rm = T),
      var_s = var(.data$s, na.rm = T),
      sd_o = sd(.data$o, na.rm = T),
      var_dist = var(.data$dis, na.rm = T),
      var_opp_dist = var(.data$off_dist, na.rm = T),
      dir_ratio_var = var(.data$dir_ratio, na.rm = T),
      dir_ratio_mean = mean(.data$dir_ratio, na.rm = T),
      pct_within_o = mean(.data$in_opp_direction, na.rm = T),
      pct_towards_los = mean(.data$turned_to_line, na.rm = T)
    ) %>%
    mutate((across(where(is.numeric), function(x) ifelse(is.na(x), 0, x))))

  weeks <- weeks %>%
    left_join(grouped_weeks, by = c("gameId", "event", "playId", "nflId"))


  # set up gmm
  # we don't need a ton to train on, but we want to train on DBs
  # can take model trained on DBs and apply it to LBs although it won't give a good result if player is blitzing
  # first only leave defensive players
  # exclude direct player tracking
  exclude_vars <- c("opponent_x", "opponent_y", "teammate_x", "teammate_y", "opponent_dir", "opponent_o", "in_opp_direction")



  # grab dataframe of all defense we care about
  defense <- weeks %>%
    filter(.data$position %in% DEFENSIVE_BACK_POSITIONS | .data$position %in% LINEBACKER_POSITIONS) %>%
    select(-all_of(exclude_vars)) %>%
    group_by(.data$nflId, .data$gameId, .data$playId) %>%
    arrange(.data$frameId) %>%
    slice(n()) %>%
    ungroup()


  set.seed(123)
  # grab train set
  defense_train <- defense %>%
    filter(.data$position %in% DEFENSIVE_BACK_POSITIONS) %>%
    sample_frac(0.7)
  # scale values
  scaled_train <- na.fill(scale(defense_train[, 27:ncol(defense_train)]), 0)
  # fit on 70% of DBs dataset
  # G here is our number of clusters
  fit <- Mclust(scaled_train, G = 2)



  # scale whole defensive set
  scaled_defense <- na.fill(scale(defense[, 27:ncol(defense)]), 0)

  # generate predictions
  probs <- as.data.frame(predict(fit, newdata = scaled_defense)$z)

  defense$man_prob <- probs$`1`
  defense$zone_prob <- probs$`2`


  # narrow and return predictions frame
  preds <- defense %>% select(.data$gameId, .data$playId, .data$nflId, .data$man_prob, .data$zone_prob)

  return(preds)
}
