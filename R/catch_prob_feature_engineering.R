#' get_targeted_receiver get a data frame of the targeted receiver on each play
#'
#' @return a data frame with the target data
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
get_targeted_receiver <- function() {
  read_targets() %>%
    mutate(istarget = TRUE)
}

#' get_football_locations get a data frame of the location of the football on each play
#'
#' @return a data frame with the target data
#' @param pbp a dataframe of play-by-play data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select rename
#' @importFrom rlang .data
#'
get_football_locations <- function(pbp) {
  pbp %>%
    filter(.data$team == 'football') %>%
    select('gameId', 'playId', 'frameId', 'x', 'y') %>%
    rename(footballX = .data$x,
           footballY = .data$y)
}

#' get_throw_dists returns a data frame of throw distances on each play
#' @return a data frame with the game id, the play id, and the throw distance
#' @param pbp a dataframe of play-by-play data
#' @param throw_start_events a character vector of throw start events
#' @param throw_end_events a character vector of throw end events
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select group_by summarize ungroup n
#' @importFrom rlang .data
#'
get_throw_dists <- function(pbp, throw_start_events, throw_end_events) {
  pbp %>%
    filter(.data$team == 'football',
           .data$event %in% c(throw_start_events, throw_end_events)) %>%
    select('gameId', 'playId', 'frameId', 'x', 'y') %>%
    group_by(.data$gameId, .data$playId) %>%
    summarize(throwdist = sqrt((diff(.data$x) ** 2) + (diff(.data$y) ** 2)), .groups = 'keep') %>%
    filter(n() == 1) %>%
    ungroup()
}

#' get_max_throw_velo returns a data frame of maximum throw velos for each play
#' @return a data frame with the game id, the play id, and the max throw velo
#' @param pbp a dataframe of play-by-play data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by summarize
#' @importFrom rlang .data
#'
get_max_throw_velo <- function(pbp) {
  pbp %>%
    filter(.data$team == 'football') %>%
    group_by(.data$gameId, .data$playId) %>%
    summarize(max_throw_velo = max(.data$s), .groups = 'drop')
}

#' get_target_location_at_throw returns a data frame of the locations of the targeted receiver at throw time on each play
#' @return a data frame with the positions
#' @param pbp a dataframe of play-by-play data
#' @param football_data the football data
#' @param targeted_receiver the targets data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate inner_join left_join
#' @importFrom rlang .data
#'
get_target_location_at_throw <- function(pbp, football_data, targeted_receiver) {
  OFFENSE_POSITIONS <- get_constants('OFFENSE_POSITIONS')
  DEFENSE_POSITIONS <- get_constants('DEFENSE_POSITIONS')
  THROW_START_EVENTS <- get_constants('THROW_START_EVENTS')

  pbp %>%
    filter(.data$position %in% c(OFFENSE_POSITIONS, DEFENSE_POSITIONS),
           .data$team %in% c('home', 'away')) %>%
    inner_join(football_data, by = c('gameId', 'playId', 'frameId')) %>%
    mutate(offdef = ifelse(.data$position %in% OFFENSE_POSITIONS, 'off', 'def'),
           dist_to_ball = sqrt((.data$x - .data$footballX)**2 + (.data$y - .data$footballY)**2)) %>%
    left_join(targeted_receiver, by = c('gameId', 'playId', c('nflId' = 'targetNflId'))) %>%
    mutate(istarget = ifelse(is.na(.data$istarget), FALSE, TRUE)) %>%
    filter((.data$position %in% DEFENSE_POSITIONS) | .data$istarget,
           .data$event %in% THROW_START_EVENTS)

}

#' get_football_location_at_arrival returns a data frame of the location of the football on each play
#' @return a data frame with the positions of the football at arrival
#' @param pbp a dataframe of play-by-play data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select rename
#' @importFrom rlang .data
#'
get_football_location_at_arrival <- function(pbp) {
  THROW_END_EVENTS <- get_constants('THROW_END_EVENTS')

  pbp %>%
    filter(.data$displayName == 'Football') %>%
    filter(.data$event %in% THROW_END_EVENTS) %>%
    select(.data$gameId, .data$playId, .data$x, .data$y) %>%
    rename(footballXArr = .data$x,
           footballYArr = .data$y)

}

#' get_pi_and_sack returns a data frame of pass interferences and sacks
#' @return a data frame with the positions of the football at arrival
#' @param plays a dataframe of plays
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
get_pi_and_sack <- function(plays) {
  plays %>%
    select(.data$gameId, .data$playId, .data$passResult, .data$isDefensivePI)
}

#' get_play_outcomes returns a data frame of play outcomes
#' @return a data frame with the play outcomes
#' @param pbp the play by play data
#' @param pi_or_sack the play result data
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate distinct inner_join case_when
#' @importFrom rlang .data
#'
get_play_outcomes <- function(pbp, pi_or_sack) {
  THROW_END_EVENTS <- get_constants('THROW_END_EVENTS')

  pbp %>%
    inner_join(pi_or_sack, by = c('gameId', 'playId')) %>%
    filter(.data$event %in% THROW_END_EVENTS,
           .data$passResult %in% c('C', 'I', 'IN')) %>%
    mutate(outcome = case_when(.data$passResult %in% c('I', 'IN') ~ 'Incomplete',
                               .data$passResult == 'C' | .data$isDefensivePI ~ 'Complete')) %>%
    select(.data$gameId, .data$playId, .data$outcome) %>%
    distinct()
}
#' create_throw_vectors returns a data frame with throw vectors
#' @return a data frame with information about the throw vector
#' @param football_data a dataframe of football position data
#' @param throw_midpoint_frame_id dataframe with the midpoint of the throw for each play
#' @importFrom magrittr %>%
#' @importFrom dplyr inner_join rename mutate filter
#' @importFrom tidyr pivot_wider
create_throw_vectors <- function(football_data, throw_midpoint_frame_id){
  SECONDS_PER_FRAME <- .1  ## TODO: reconcile w/ value in matt_feature_eng.R
  return(
    throw_midpoint_frame_id %>%
      inner_join(football_data) %>%
      rename(x = .data$footballX, y = .data$footballY) %>%  ## TODO: will change football_data style
      pivot_wider(
        id_cols = c(.data$gameId, .data$playId),
        names_from = .data$frame,
        names_glue = "{frame}_{.value}",
        values_from = c(.data$frameId, .data$x, .data$y)
      ) %>%
      mutate(
        throwStartDist = sqrt((.data$startFrameId_x - .data$midpointFrameId_x)^2 + (.data$startFrameId_y - .data$midpointFrameId_y)^2),
        throwStartTime = (.data$midpointFrameId_frameId - .data$startFrameId_frameId) * SECONDS_PER_FRAME,
        throwSpeed = .data$throwStartDist / .data$throwStartTime,
        throwAngle = (atan2((.data$startFrameId_x - .data$midpointFrameId_x), (.data$startFrameId_y - .data$midpointFrameId_y)) - pi/2) * 180 / pi,
        throwAngle = ifelse(.data$throwAngle < 0, 360 + .data$throwAngle, .data$throwAngle)
      ) %>%
      filter(!is.na(.data$throwSpeed))
  )
}

#' get_defense_locs_at_throw returns a data from of the locations of the defense at throw time
#' @return a data frame with the locations of the defenders at throw time
#' @param player_locs_at_throw a data frame of player locations at throw time
#' @param throw_vectors the throw vectors
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate select rename_with group_by
#' @importFrom rlang .data
#'
get_defense_locs_at_throw <- function(player_locs_at_throw, throw_vectors) {
  . <- NULL
  DEFENSE_POSITIONS <- get_constants('defense_positions')

  player_locs_at_throw %>%
    filter(.data$position %in% DEFENSE_POSITIONS) %>%
    select(.data$gameId, .data$playId, .data$frameId, .data$nflId, .data$x, .data$y) %>%
    add_throw_vector_to_positions(., throw_vectors) %>%
    select(.data$gameId, .data$playId, .data$frameId,
           .data$x, .data$y, .data$nflId, .data$distanceToThrow,
           .data$angleToThrow, .data$timeToIntercept, .data$veloToIntercept) %>%
    rename_with(function(x) paste0((x), "_def"), -c(.data$gameId, .data$playId, .data$frameId)) %>%
    group_by(.data$gameId, .data$playId, .data$frameId) %>%
    filter(n() <= 11) ## throw out plays with > 11 guys on the field
}

#' add_throw_vector_to_positions takes player positions at release along with throw information to calculate how close the player is to the throw
#' @return a dataframe with the distance & time the player has to reach the throw
#' @param release_positions player positions at release of ball
#' @param throw_vectors information about the pass
#' @importFrom  magrittr %>%
#' @importFrom dplyr left_join mutate
#'
add_throw_vector_to_positions <- function(release_positions, throw_vectors) {
  return (
    release_positions %>%
      left_join(throw_vectors, by=c('gameId', 'playId')) %>%
      mutate(
        # pt0 = player, pt1 = release, pt2 = mid, https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
        distanceToThrow = abs((.data$x * (.data$midpointFrameId_y - .data$startFrameId_y)) - (.data$y * (.data$midpointFrameId_x - .data$startFrameId_x)) + (.data$midpointFrameId_x * .data$startFrameId_y) - (.data$midpointFrameId_y * .data$startFrameId_x)) /  sqrt((.data$startFrameId_x - .data$midpointFrameId_x)^2 + (.data$startFrameId_y - .data$midpointFrameId_y)^2),
        # try moving +90 degrees, calculating how far you are from the throw vector there
        angleToThrowPositive = .data$throwAngle + 90,
        interceptPositiveX = .data$x + (.data$distanceToThrow * cos(.data$angleToThrowPositive * pi / 180)),
        interceptPositiveY = .data$y + (.data$distanceToThrow * sin(.data$angleToThrowPositive * pi / 180)),
        interceptToThrowPositive = abs(
          ((.data$interceptPositiveX * (.data$midpointFrameId_y - .data$startFrameId_y))
           - (.data$interceptPositiveY * (.data$midpointFrameId_x - .data$startFrameId_x))
           + (.data$midpointFrameId_x * .data$startFrameId_y)
           - (.data$midpointFrameId_y * .data$startFrameId_x))
        ) / .data$throwStartDist,
        # then -90 degrees, then whichever one is smaller we take
        angleToThrowNegative = .data$throwAngle - 90,
        interceptNegativeX = .data$x + (.data$distanceToThrow * cos(.data$angleToThrowNegative * pi / 180)),
        interceptNegativeY = .data$y + (.data$distanceToThrow * sin(.data$angleToThrowNegative * pi / 180)),
        interceptToThrowNegative = abs(
          ((.data$interceptNegativeX * (.data$midpointFrameId_y - .data$startFrameId_y))
           - (.data$interceptNegativeY * (.data$midpointFrameId_x - .data$startFrameId_x))
           + (.data$midpointFrameId_x * .data$startFrameId_y)
           - (.data$midpointFrameId_y * .data$startFrameId_x))
        ) / .data$throwStartDist,
        angleToThrow = ifelse(
          .data$interceptToThrowNegative >= .data$interceptToThrowPositive,
          .data$angleToThrowPositive,
          .data$angleToThrowNegative
        ),
        interceptX = .data$x + (.data$distanceToThrow * sin(.data$angleToThrow * pi / 180)),
        interceptY = .data$y + (.data$distanceToThrow * cos(.data$angleToThrow * pi / 180)),
        interceptToThrow = abs(((.data$interceptX * (.data$midpointFrameId_y - .data$startFrameId_y)) - (.data$interceptY * (.data$midpointFrameId_x - .data$startFrameId_x)) + (.data$midpointFrameId_x * .data$startFrameId_y) - (.data$midpointFrameId_y * .data$startFrameId_x))) / .data$throwStartDist,
        releaseToIntercept = sqrt((.data$interceptX - .data$startFrameId_x) ^ 2 + (.data$interceptY - .data$startFrameId_y) ^ 2),
        timeToIntercept = .data$releaseToIntercept / .data$throwSpeed,
        veloToIntercept = .data$distanceToThrow / .data$timeToIntercept,
        interceptDiff = abs(((.data$midpointFrameId_y - .data$startFrameId_y) * (.data$interceptX - .data$midpointFrameId_x)) - ((.data$interceptY - .data$midpointFrameId_y) * (.data$midpointFrameId_x - .data$startFrameId_x))),
      )
  )
}

#' do_catch_prob_feat_eng build a data frame of catch prob features
#'
#' @param weeks_to_use Numeric: a numeric vector of weeks to use (default 1:17)
#' @return a data frame of features
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom tidyr pivot_longer pivot_wider drop_na separate
#' @importFrom stringr str_remove_all
#' @importFrom stats sd
#' @importFrom rlang .data
#' @import nflfastR
#' @import dplyr
#' @export
#'
do_catch_prob_feat_eng <- function(weeks_to_use = 1:17) {
  . <- NULL
  nonweek <- read_non_week_files()
  targeted_receiver <- get_targeted_receiver()

  THROW_START_EVENTS <- get_constants('throw_start_events')
  THROW_END_EVENTS <- get_constants('throw_end_events')
  OFFENSE_POSITIONS <- get_constants('offense_positions')
  DEFENSE_POSITIONS <- get_constants('defense_positions')

  pbp_data <- weeks_to_use %>%
    map(read_individual_week) %>%
    bind_rows()

  football_locations <- get_football_locations(pbp_data)
  throw_distances <- get_throw_dists(pbp_data, THROW_START_EVENTS, THROW_END_EVENTS)
  max_throw_velo <- get_max_throw_velo(pbp_data)
  target_location_at_throw <- get_target_location_at_throw(pbp_data, football_locations, targeted_receiver)
  football_locations_at_arrival <- get_football_location_at_arrival(pbp_data)
  sacks_and_pis <- get_pi_and_sack(nonweek$plays)
  play_outcomes <- get_play_outcomes(pbp_data, sacks_and_pis)

  ## i'll factor this out later
  receiver_skill <- pbp_data %>%
    select(.data$gameId, .data$playId, .data$nflId) %>%
    distinct() %>%
    inner_join(targeted_receiver %>% rename(nflId = .data$targetNflId), by = c('gameId', "playId", 'nflId')) %>%
    inner_join(play_outcomes, by = c('gameId', 'playId')) %>%
    group_by(.data$nflId) %>%
    summarize(skill = sum(.data$outcome == 'Complete') / sqrt(n()), .groups = 'drop')

  throw_midpoint_frame_id <- pbp_data %>%
    filter(.data$event %in% c(THROW_END_EVENTS, THROW_START_EVENTS),
           .data$team == 'football') %>%
    group_by(.data$gameId, .data$playId) %>%
    summarize(startFrameId = min(.data$frameId),
              afterThrowFrameId = min(.data$frameId) + 3,
              midpointFrameId = round(mean(.data$frameId)), .groups = 'drop') %>%
    pivot_longer(cols = c(.data$startFrameId, .data$afterThrowFrameId, .data$midpointFrameId),
                 names_to = 'frame', values_to = 'frameId')

  ## TODO: fold this LA/EV into this fxn?
  throw_vectors <- create_throw_vectors(football_locations, throw_midpoint_frame_id)

  player_at_throw <- target_location_at_throw %>%
    filter(.data$event %in% THROW_START_EVENTS) %>%
    mutate(offdef = case_when(.data$position %in% OFFENSE_POSITIONS ~ 'off',
                              .data$position %in% DEFENSE_POSITIONS ~ 'def')) %>%
    select(.data$gameId, .data$playId, .data$event,
           .data$nflId, .data$team, .data$position,
           .data$frameId, .data$x:.data$dir, .data$playDirection,
           .data$route, .data$istarget) %>%
    inner_join(play_outcomes, by = c('gameId', 'playId'))

  # get locations of defenders at throw
  defense_locs <- get_defense_locs_at_throw(player_at_throw, throw_vectors)

  defender_primary_positions <- pbp_data %>%
    select(.data$nflId, .data$position) %>%
    rename(nflId_def = .data$nflId,
           def_pos = .data$position) %>%
    distinct() %>%
    mutate(def_pos = case_when(.data$def_pos %in% c('SS', 'FS', 'S') ~ 'Safety',
                               .data$def_pos %in% c('DB', 'CB') ~ 'Corner',
                               .data$def_pos %in% c('ILB', 'MLB', 'LB', 'OLB') ~ 'Linebacker',
                               .data$def_pos %in% c('DL', 'DE') ~ 'Line',
                               .data$def_pos %in% OFFENSE_POSITIONS ~ 'Offense',
                               TRUE ~ .data$def_pos),
           grouped_def_pos = case_when(.data$def_pos %in% c('Safety', 'Corner') ~ 'DB',
                                       .data$def_pos == 'Offense' ~ 'Offense',
                                       TRUE ~ 'Other')) %>%
    drop_na()

  ## absolute yardline is the distance you are from the target endzone
  play_data <- nonweek$plays %>%
    select(.data$gameId, .data$playId, .data$quarter,
           .data$down, .data$yardsToGo, .data$absoluteYardlineNumber,
           .data$personnelO, .data$defendersInTheBox, .data$numberOfPassRushers,
           .data$personnelD, .data$preSnapVisitorScore, .data$preSnapHomeScore, .data$epa)

  weather <- build_weather_df(weeks_to_use)

  target_position_at_throw <- target_location_at_throw %>%
    filter(.data$istarget) %>%
    select(.data$gameId, .data$playId, .data$x,.data$y,.data$s,.data$a) %>%
    rename(targetXThrow = .data$x,
           targetYThrow = .data$y,
           targetAThrow = .data$a,
           targetSThrow = .data$s)

  # filter down to just the pass catcher at time of throw
  # calculate the distance to the nearest defender
  df <- player_at_throw %>%
    filter(.data$position %in% OFFENSE_POSITIONS,
           .data$istarget) %>%
    add_throw_vector_to_positions(., throw_vectors) %>%
    inner_join(defense_locs, by = c('gameId', 'playId', 'frameId')) %>%
    left_join(defender_primary_positions, by = 'nflId_def') %>%
    group_by(.data$gameId, .data$playId, .data$frameId) %>%
    mutate(dist_to_def = sqrt((.data$x - .data$x_def)**2 + (.data$y - .data$y_def)**2),
           min_dist_to_def = min(.data$dist_to_def),
           mean_dist_to_def = mean(.data$dist_to_def),
           sd_dist_to_def = sd(.data$dist_to_def),
           min_velo_def = min(.data$veloToIntercept_def)) %>%
    distinct() %>%
    arrange(.data$dist_to_def) %>%
    mutate(defenderId = 1:n()) %>%
    ungroup() %>%  # CHANGED HERE
    pivot_wider(id_cols = c(.data$gameId, .data$playId, .data$frameId, .data$outcome, .data$min_dist_to_def, .data$sd_dist_to_def, .data$min_velo_def),
                names_from = .data$defenderId,
                names_sep = "_",
                values_from = c(.data$dist_to_def, .data$grouped_def_pos, .data$veloToIntercept_def, .data$nflId_def)) %>%
    left_join(targeted_receiver %>% select(-c(.data$istarget)), by = c('gameId', 'playId')) %>%
    left_join(max_throw_velo, by = c('gameId', 'playId')) %>%
    left_join(throw_distances, by = c('gameId', 'playId')) %>%
    mutate(across(c(starts_with("dist_to_def"), starts_with("veloToIntercept")), function(x) ifelse(is.na(x), 999, x)),
           across(starts_with("grouped_def_pos"), function(x) ifelse(is.na(x), 'Line', x))) %>%
    group_by(.data$gameId, .data$playId) %>%
    filter(n() == 1) %>%
    ungroup() %>%  # CHANGED HERE
    left_join(play_data, by = c('gameId', 'playId')) %>%
    rowwise() %>%
    mutate(numberOfPassRushers = as.numeric(.data$numberOfPassRushers),
           defendersInTheBox = as.numeric(.data$defendersInTheBox),
           numberOfPassRushers = as.numeric(case_when(is.na(.data$numberOfPassRushers) & is.na(.data$defendersInTheBox) ~ as.numeric(sum(c_across(.data$dist_to_def_1:.data$dist_to_def_11) == 999)),
                                                      is.na(.data$numberOfPassRushers) ~ as.numeric((sum(c_across(.data$dist_to_def_1:.data$dist_to_def_11) == 999) + .data$defendersInTheBox) / 2),
                                                      TRUE ~ as.numeric(.data$numberOfPassRushers)))) %>%
    ungroup() %>%  # CHANGED HERE
    left_join(weather, by = 'gameId') %>%
    left_join(football_locations_at_arrival, by = c('gameId', 'playId')) %>%
    left_join(target_position_at_throw, by = c('gameId', 'playId')) %>%
    left_join(receiver_skill, by = c('targetNflId' = 'nflId')) %>%
    left_join(nonweek$players %>% select(.data$nflId, .data$height), by = c('targetNflId' = 'nflId'))

  return(df)
}

#' get_weather return the weather forecast in a given week
#'
#' @param week numeric, the week to get
#' @param year numeric, the year to get
#' @return a data frame of forecasts
#' @importFrom magrittr %>%
#' @importFrom rvest html_table
#' @importFrom xml2 read_html
#' @importFrom glue glue
#' @importFrom dplyr select mutate
#' @importFrom rlang .data
#' @export
#'
get_weather <- function(week, year) {
  page <- (read_html(glue("http://www.nflweather.com/en/week/{year}/week-{week}/")) %>%
             html_table())[[1]] %>%
    select(.data$Home, .data$Away, .data$Forecast) %>%
    mutate(week = week)

  return(page)
}

#' build_weather_df returns a dataframe with parsed weather information for each gameId
#'
#' @param weeks_to_use weeks to get weather data for
#' @return a data frame of forecasts matched to the appropriate game
#' @importFrom magrittr %>%
#' @importFrom tidyr separate
#' @importFrom dplyr select mutate left_join rename case_when
#' @importFrom stringr str_replace
#'
build_weather_df <- function(weeks_to_use){
  teamabbrs <- nflfastR::teams_colors_logos %>%
    select(.data$team_abbr, .data$team_nick) %>%
    mutate(team_nick = ifelse(.data$team_nick == 'Football Team', 'Washington', .data$team_nick))

  weather_raw <- map(weeks_to_use, get_weather, 2018) %>%
    bind_rows()

  nonweek <- read_non_week_files()

  weather <- nonweek$games %>%
    left_join(teamabbrs %>% rename(Home = .data$team_nick), by = c('homeTeamAbbr' = 'team_abbr')) %>%
    left_join(teamabbrs %>% rename(Away = .data$team_nick), by = c('visitorTeamAbbr' = 'team_abbr')) %>%
    left_join(weather_raw, by = c('Home', 'Away', 'week')) %>%
    select(.data$gameId, .data$Forecast) %>%
    mutate(Forecast = str_replace(.data$Forecast, "\\s", "|"),
           Forecast = case_when(.data$Forecast == 'DOME' ~ '70f|Dome',
                                TRUE ~ .data$Forecast)) %>%
    separate(.data$Forecast, into = c('temperature', 'conditions'), sep = "\\|") %>%
    mutate(temperature = str_remove_all(.data$temperature, "[^0-9]") %>% as.numeric(.data),
           conditions = case_when(grepl('rain', .data$conditions, ignore.case = T) | .data$conditions == 'Drizzle' ~ 'Rain',
                                  grepl('snow', .data$conditions, ignore.case = T) ~ 'Snow',
                                  .data$conditions == 'Dome' ~ 'Dome',
                                  .data$conditions == 'Foggy'  ~ 'Foggy',
                                  TRUE ~ 'Clear'))
  return(weather)

}

