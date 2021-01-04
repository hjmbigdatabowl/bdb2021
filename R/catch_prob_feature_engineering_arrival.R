#' get_target_location_at_arrival returns a data frame of the locations of the targeted receiver at throw time on each play
#' @return a data frame with the positions
#' @param pbp a dataframe of play-by-play data
#' @param football_data the football data
#' @param targeted_receiver the targets data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate inner_join left_join
#' @importFrom rlang .data
#'
get_target_location_at_arrival <- function(pbp, football_data, targeted_receiver) {
  OFFENSE_POSITIONS <- get_constants("OFFENSE_POSITIONS")
  DEFENSE_POSITIONS <- get_constants("DEFENSE_POSITIONS")
  THROW_END_EVENTS <- get_constants("THROW_END_EVENTS")

  pbp %>%
    filter(
      .data$position %in% c(OFFENSE_POSITIONS, DEFENSE_POSITIONS),
      .data$team %in% c("home", "away")
    ) %>%
    inner_join(football_data, by = c("gameId", "playId", "frameId")) %>%
    mutate(
      offdef = ifelse(.data$position %in% OFFENSE_POSITIONS, "off", "def"),
      dist_to_ball = sqrt((.data$x - .data$footballX)**2 + (.data$y - .data$footballY)**2)
    ) %>%
    left_join(targeted_receiver, by = c("gameId", "playId", c("nflId" = "targetNflId"))) %>%
    mutate(istarget = ifelse(is.na(.data$istarget), FALSE, TRUE)) %>%
    filter(
      (.data$position %in% DEFENSE_POSITIONS) | .data$istarget,
      .data$event %in% THROW_END_EVENTS
    )
}

#' get_football_location_at_arrival returns a data frame of the location of the football on each play
#' @return a data frame with the positions of the football at arrival
#' @param pbp a dataframe of play-by-play data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select rename
#' @importFrom rlang .data
#'
get_football_location_at_arrival <- function(pbp) {
  THROW_END_EVENTS <- get_constants("THROW_END_EVENTS")

  pbp %>%
    filter(.data$displayName == "Football") %>%
    filter(.data$event %in% THROW_END_EVENTS) %>%
    select(.data$gameId, .data$playId, .data$x, .data$y) %>%
    rename(
      footballXArr = .data$x,
      footballYArr = .data$y
    )
}

#' get_defense_locs_at_arrival returns a data from of the locations of the defense at throw time
#' @return a data frame with the locations of the defenders at throw time
#' @param player_locs_at_arrival a data frame of player locations at throw time
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate select rename_with group_by
#' @importFrom rlang .data
#'
get_defense_locs_at_arrival <- function(player_locs_at_arrival) {
  . <- NULL
  DEFENSE_POSITIONS <- get_constants("defense_positions")

  player_locs_at_arrival %>%
    filter(.data$position %in% DEFENSE_POSITIONS) %>%
    select(.data$gameId, .data$playId, .data$frameId, .data$nflId, .data$x, .data$y) %>%
    select(
      .data$gameId, .data$playId, .data$frameId,
      .data$x, .data$y, .data$nflId
    ) %>%
    rename_with(function(x) paste0((x), "_def"), -c(.data$gameId, .data$playId, .data$frameId)) %>%
    group_by(.data$gameId, .data$playId, .data$frameId) %>%
    filter(n() <= 11) ## throw out plays with > 11 guys on the field
}

#' do_catch_prob_arrival_feat_eng build a data frame of catch prob features
#'
#' @param weeks_to_use Numeric: a numeric vector of weeks to use (default 1:17)
#' @return a data frame of features
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom tidyr pivot_longer pivot_wider drop_na separate
#' @importFrom stringr str_remove_all
#' @importFrom stats sd
#' @importFrom rlang .data
#' @importFrom cyphr encrypt key_sodium
#' @import nflfastR
#' @import dplyr
#' @export
#'
do_catch_prob_arrival_feat_eng <- function(weeks_to_use = 1:17) {
  . <- NULL
  nonweek <- read_non_week_files()
  targeted_receiver <- get_targeted_receiver()

  THROW_START_EVENTS <- get_constants("throw_start_events")
  THROW_END_EVENTS <- get_constants("throw_end_events")
  OFFENSE_POSITIONS <- get_constants("offense_positions")
  DEFENSE_POSITIONS <- get_constants("defense_positions")

  pbp_data <- weeks_to_use %>%
    map(read_individual_week) %>%
    bind_rows()

  football_locations <- get_football_locations(pbp_data)
  throw_distances <- get_throw_dists(pbp_data, THROW_START_EVENTS, THROW_END_EVENTS)
  max_throw_velo <- get_max_throw_velo(pbp_data)
  target_location_at_arrival <- get_target_location_at_arrival(pbp_data, football_locations, targeted_receiver)
  football_locations_at_arrival <- get_football_location_at_arrival(pbp_data)
  sacks_and_pis <- get_pi_and_sack(nonweek$plays)
  play_outcomes <- get_play_outcomes(pbp_data, sacks_and_pis)
  heights <- get_heights()

  ## i'll factor this out later
  receiver_skill <- pbp_data %>%
    select(.data$gameId, .data$playId, .data$nflId) %>%
    distinct() %>%
    inner_join(targeted_receiver %>% rename(nflId = .data$targetNflId), by = c("gameId", "playId", "nflId")) %>%
    inner_join(play_outcomes, by = c("gameId", "playId")) %>%
    group_by(.data$nflId) %>%
    summarize(skill = sum(.data$outcome == "Complete") / sqrt(n()), .groups = "drop")

  player_at_arrival <- target_location_at_arrival %>%
    filter(.data$event %in% THROW_END_EVENTS) %>%
    mutate(offdef = case_when(
      .data$position %in% OFFENSE_POSITIONS ~ "off",
      .data$position %in% DEFENSE_POSITIONS ~ "def"
    )) %>%
    select(
      .data$gameId, .data$playId, .data$event,
      .data$nflId, .data$team, .data$position,
      .data$frameId, .data$x:.data$dir, .data$playDirection,
      .data$route, .data$istarget
    ) %>%
    inner_join(play_outcomes, by = c("gameId", "playId"))

  # get locations of defenders at throw
  defense_locs <- get_defense_locs_at_arrival(player_at_arrival)

  defender_primary_positions <- pbp_data %>%
    select(.data$nflId, .data$position) %>%
    rename(
      nflId_def = .data$nflId,
      def_pos = .data$position
    ) %>%
    distinct() %>%
    mutate(
      def_pos = case_when(
        .data$def_pos %in% c("SS", "FS", "S") ~ "Safety",
        .data$def_pos %in% c("DB", "CB") ~ "Corner",
        .data$def_pos %in% c("ILB", "MLB", "LB", "OLB") ~ "Linebacker",
        .data$def_pos %in% c("DL", "DE") ~ "Line",
        .data$def_pos %in% OFFENSE_POSITIONS ~ "Offense",
        TRUE ~ .data$def_pos
      ),
      grouped_def_pos = case_when(
        .data$def_pos %in% c("Safety", "Corner") ~ "DB",
        .data$def_pos == "Offense" ~ "Offense",
        TRUE ~ "Other"
      )
    ) %>%
    drop_na()

  ## absolute yardline is the distance you are from the target endzone
  play_data <- nonweek$plays %>%
    select(
      .data$gameId, .data$playId, .data$quarter,
      .data$down, .data$yardsToGo, .data$absoluteYardlineNumber,
      .data$personnelO, .data$defendersInTheBox, .data$numberOfPassRushers,
      .data$personnelD, .data$preSnapVisitorScore, .data$preSnapHomeScore, .data$epa
    )

  weather <- build_weather_df(weeks_to_use)

  target_position_at_arrival <- target_location_at_arrival %>%
    filter(.data$istarget) %>%
    select(.data$gameId, .data$playId, .data$x, .data$y, .data$s, .data$a) %>%
    rename(
      targetXArrival = .data$x,
      targetYArrival = .data$y,
      targetAArrival = .data$a,
      targetSArrival = .data$s
    )

  # filter down to just the pass catcher at time of throw
  # calculate the distance to the nearest defender
  df <- player_at_arrival %>%
    filter(
      .data$position %in% OFFENSE_POSITIONS,
      .data$istarget
    ) %>%
    inner_join(defense_locs, by = c("gameId", "playId", "frameId")) %>%
    left_join(defender_primary_positions, by = "nflId_def") %>%
    group_by(.data$gameId, .data$playId, .data$frameId) %>%
    mutate(
      dist_to_def = sqrt((.data$x - .data$x_def)**2 + (.data$y - .data$y_def)**2),
      min_dist_to_def = min(.data$dist_to_def),
      mean_dist_to_def = mean(.data$dist_to_def),
      sd_dist_to_def = sd(.data$dist_to_def)
    ) %>%
    distinct() %>%
    arrange(.data$dist_to_def) %>%
    mutate(defenderId = 1:n()) %>%
    ungroup() %>% # CHANGED HERE
    pivot_wider(
      id_cols = c(.data$gameId, .data$playId, .data$frameId, .data$outcome, .data$min_dist_to_def, .data$sd_dist_to_def),
      names_from = .data$defenderId,
      names_sep = "_",
      values_from = c(.data$dist_to_def, .data$grouped_def_pos, .data$nflId_def)
    ) %>%
    left_join(targeted_receiver %>% select(-c(.data$istarget)), by = c("gameId", "playId")) %>%
    left_join(max_throw_velo, by = c("gameId", "playId")) %>%
    left_join(throw_distances, by = c("gameId", "playId")) %>%
    mutate(
      across(c(starts_with("dist_to_def")), function(x) ifelse(is.na(x), 999, x)),
      across(starts_with("grouped_def_pos"), function(x) ifelse(is.na(x), "Line", x))
    ) %>%
    group_by(.data$gameId, .data$playId) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    left_join(play_data, by = c("gameId", "playId")) %>%
    rowwise() %>%
    mutate(
      numberOfPassRushers = as.numeric(.data$numberOfPassRushers),
      defendersInTheBox = as.numeric(.data$defendersInTheBox),
      numberOfPassRushers = as.numeric(case_when(
        is.na(.data$numberOfPassRushers) & is.na(.data$defendersInTheBox) ~ as.numeric(sum(c_across(.data$dist_to_def_1:.data$dist_to_def_11) == 999)),
        is.na(.data$numberOfPassRushers) ~ as.numeric((sum(c_across(.data$dist_to_def_1:.data$dist_to_def_11) == 999) + .data$defendersInTheBox) / 2),
        TRUE ~ as.numeric(.data$numberOfPassRushers)
      ))
    ) %>%
    ungroup() %>% # CHANGED HERE
    left_join(weather, by = "gameId") %>%
    left_join(football_locations_at_arrival, by = c("gameId", "playId")) %>%
    left_join(target_position_at_arrival, by = c("gameId", "playId")) %>%
    left_join(receiver_skill, by = c("targetNflId" = "nflId")) %>%
    left_join(heights, by = c("targetNflId" = "nflId"))

  save_encrypted(df, file = "inst/data/catch_prob_features_arrival.Rdata")
  return(df)
}

