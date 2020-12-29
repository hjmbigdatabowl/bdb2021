#' do_catch_prob_feat_eng build a data frame of catch prob features
#'
#' @param weeks_to_use Numeric: a numeric vector of weeks to use (default 1:17)
#' @return a data frame of features
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom tidyr pivot_longer pivot_wider drop_na separate
#' @importFrom stringr str_replace str_remove_all
#' @importFrom stats sd
#' @importFrom rlang .data
#' @import nflfastR
#' @import dplyr
#' @export
#'
do_catch_prob_feat_eng <- function(weeks_to_use = 1:17) {
  # get meta
  nonweek <- read_non_week_files(.data)

  # get pbp data
  weeks <- weeks_to_use %>%
    map(read_individual_week) %>%
    bind_rows(.data)

  ## get targeted receiver
  targeted_receiver <- read_targets(.data) %>%
    mutate(istarget = TRUE)

  # track location of football
  football_data <- weeks %>%
    filter(.data$team == 'football') %>%
    select(.data$gameId, .data$playId, .data$frameId, .data$x, .data$y) %>%
    rename(footballX = .data$x,
           footballY = .data$y)

  ## get the distance of the throw in yards
  football_dist_traveled <- weeks %>%
    filter(.data$team == 'football',
           .data$event %in% c(THROW_START_EVENTS, THROW_END_EVENTS)) %>%
    select(.data$gameId, .data$playId, .data$frameId, .data$x, .data$y) %>%
    group_by(.data$gameId, .data$playId) %>%
    summarize(throwdist = sqrt((diff(.data$x) ** 2) + (diff(.data$y) ** 2)), .groups = 'keep') %>%
    filter(n() == 1) %>%
    ungroup(.data)


  throw_midpoint_frame_id <- weeks %>%
    filter(.data$event %in% c(THROW_END_EVENTS, THROW_START_EVENTS),
           .data$team == 'football') %>%
    group_by(.data$gameId, .data$playId) %>%
    summarize(startFrameId = min(.data$frameId),
              afterThrowFrameId = min(.data$frameId) + 3,
              midpointFrameId = round(mean(.data$frameId)), .groups = 'drop') %>%
    pivot_longer(cols = c(.data$startFrameId, .data$afterThrowFrameId, .data$midpointFrameId),
                 names_to = 'frame', values_to = 'frameId')

  max_throw_velo <- weeks %>%
    filter(.data$team == 'football') %>%
    group_by(.data$gameId, .data$playId) %>%
    summarize(max_throw_velo = max(.data$s), .groups = 'drop')

  ## TODO: fold this LA/EV into this fxn?
  throw_vectors <- create_throw_vectors(football_data, throw_midpoint_frame_id)

  ## this takes a really long time but not sure how to speed it up.
  ## just a lot of data to deal with on the back end
  ## it outputs 17m rows
  target_at_throw <- weeks %>%
    filter(.data$position %in% c(OFFENSE_POSITIONS, DEFENSE_POSITIONS),
           .data$team %in% c('home', 'away')) %>%
    inner_join(football_data, by = c('gameId', 'playId', 'frameId')) %>%
    mutate(offdef = ifelse(.data$position %in% OFFENSE_POSITIONS, 'off', 'def'),
           dist_to_ball = sqrt((.data$x - .data$footballX)**2 + (.data$y - .data$footballY)**2)) %>%
    left_join(targeted_receiver, by = c('gameId', 'playId', c('nflId' = 'targetNflId'))) %>%
    mutate(istarget = ifelse(is.na(.data$istarget), FALSE, TRUE)) %>%
    filter((.data$position %in% DEFENSE_POSITIONS) | .data$istarget,
           .data$event %in% THROW_START_EVENTS)

  ## this takes a really long time but not sure how to speed it up.
  ## just a lot of data to deal with on the back end
  ## it outputs 17m rows
  football_loc_at_arrival <- weeks %>%
    filter(.data$displayName == 'Football') %>%
    filter(.data$event %in% THROW_END_EVENTS) %>%
    select(.data$gameId, .data$playId, .data$x, .data$y) %>%
    rename(footballXArr = .data$x,
           footballYArr = .data$y)

  pi_or_sack <- nonweek$plays %>%
    select(.data$gameId, .data$playId, .data$passResult, .data$isDefensivePI)

  ## 'R' pass result stands for Rush
  ## 'S' stands for Sack
  ## we probably don't want to keep those in the catch prob model
  play_outcomes <- weeks %>%
    inner_join(pi_or_sack, by = c('gameId', 'playId')) %>%
    filter(.data$event %in% THROW_END_EVENTS,
           .data$passResult %in% c('C', 'I', 'IN')) %>%
    mutate(outcome = case_when(.data$passResult %in% c('I', 'IN') ~ 'Incomplete',
                               .data$passResult == 'C' | .data$isDefensivePI ~ 'Complete')) %>%
    select(.data$gameId, .data$playId, .data$outcome) %>%
    distinct(.data)

  # get player locations at the time of throw
  player_at_throw <- target_at_throw %>%
    filter(.data$event %in% THROW_START_EVENTS) %>%
    mutate(offdef = case_when(.data$position %in% OFFENSE_POSITIONS ~ 'off',
                              .data$position %in% DEFENSE_POSITIONS ~ 'def')) %>%
    select(.data$gameId, .data$playId, .data$event,
           .data$nflId, .data$team, .data$position,
           .data$frameId, .data$x:.data$dir, .data$.data$playDirection,
           .data$route, .data$istarget) %>%
    inner_join(play_outcomes, by = c('gameId', 'playId'))

  # get locations of defenders at throw
  defense_locs <- player_at_throw %>%
    filter(.data$position %in% DEFENSE_POSITIONS) %>%
    select(.data$gameId, .data$playId, .data$frameId, .data$nflId, .data$x, .data$y) %>%
    add_throw_vector_to_positions(.data, throw_vectors) %>%
    select(.data$gameId, .data$playId, .data$frameId,
           .data$x, .data$y, .data$nflId, .data$distanceToThrow,
           .data$angleToThrow, .data$timeToIntercept, .data$veloToIntercept) %>%
    # rename(x_def = x,
    #               y_def = y,
    #               nflId_def = nflId)
    # https://stackoverflow.com/questions/29948876/adding-prefix-or-suffix-to-most-data-frame-variable-names-in-piped-r-workflow
    rename_at(vars(-c(.data$gameId, .data$playId, .data$frameId)), function(x) paste0((x), "_def")) %>%
    group_by(.data$gameId, .data$playId, .data$frameId) %>%
    filter(n() <= 11) ## throw out plays with > 11 guys on the field

  defender_primary_positions <- weeks %>%
    select(.data$nflId, .data$position) %>%
    rename(nflId_def = .data$nflId,
           def_pos = .data$position) %>%
    distinct(.data) %>%
    mutate(def_pos = case_when(.data$def_pos %in% c('SS', 'FS', 'S') ~ 'Safety',
                               .data$def_pos %in% c('DB', 'CB') ~ 'Corner',
                               .data$def_pos %in% c('ILB', 'MLB', 'LB', 'OLB') ~ 'Linebacker',
                               .data$def_pos %in% c('DL', 'DE') ~ 'Line',
                               .data$def_pos %in% OFFENSE_POSITIONS ~ 'Offense',
                               TRUE ~ .data$def_pos),
           grouped_def_pos = case_when(.data$def_pos %in% c('Safety', 'Corner') ~ 'DB',
                                       .data$def_pos == 'Offense' ~ 'Offense',
                                       TRUE ~ 'Other')) %>%
    drop_na(.data)

  ## absolute yardline is the distance you are from the target endzone
  play_data <- nonweek$plays %>%
    select(.data$gameId, .data$playId, .data$quarter,
           .data$down, .data$yardsToGo, .data$absoluteYardlineNumber,
           .data$personnelO, .data$defendersInTheBox, .data$numberOfPassRushers,
           .data$personnelD, .data$preSnapVisitorScore, .data$preSnapHomeScore, .data$epa)

  teamabbrs <- teams_colors_logos %>%
    select(.data$team_abbr, .data$team_nick) %>%
    mutate(team_nick = ifelse(.data$team_nick == 'Football Team', 'Washington', .data$team_nick))

  weather_raw <- map(weeks_to_use, get_weather, 2018) %>%
    bind_rows(.data)

  weather <- nonweek$games %>%
    left_join(.data$teamabbrs %>% rename(Home = .data$team_nick), by = c('homeTeamAbbr' = 'team_abbr')) %>%
    left_join(.data$teamabbrs %>% rename(Away = .data$team_nick), by = c('visitorTeamAbbr' = 'team_abbr')) %>%
    left_join(weather_raw, by = c('Home', 'Away', 'week')) %>%
    select(.data$gameId, .data$Forecast) %>%
    mutate(Forecast = .data$str_replace(.data$Forecast, "\\s", "|"),
           Forecast = .data$case_when(.data$Forecast == 'DOME' ~ '70f|Dome',
                                TRUE ~ .data$Forecast)) %>%
    separate(.data$Forecast, into = c('temperature', 'conditions'), sep = "\\|") %>%
    mutate(temperature = str_remove_all(.data$temperature, "[^0-9]") %>% as.numeric(.data),
           conditions = case_when(grepl('rain', .data$conditions, ignore.case = T) | .data$conditions == 'Drizzle' ~ 'Rain',
                                  grepl('snow', .data$conditions, ignore.case = T) ~ 'Snow',
                                  .data$conditions == 'Dome' ~ 'Dome',
                                  .data$conditions == 'Foggy'  ~ 'Foggy',
                                  TRUE ~ 'Clear'))

  target_position_at_throw <- target_at_throw %>%
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
    distinct(.data) %>%
    arrange(.data$dist_to_def) %>%
    mutate(defenderId = 1:n()) %>%
    ungroup(.data) %>%
    pivot_wider(id_cols = c(.data$gameId, .data$playId, .data$frameId, .data$outcome, .data$min_dist_to_def, .data$sd_dist_to_def, .data$min_velo_def),
                names_from = .data$defenderId,
                names_sep = "_",
                values_from = c(.data$dist_to_def, .data$grouped_def_pos, .data$veloToIntercept_def, .data$nflId_def)) %>%
    left_join(targeted_receiver %>% select(-c(.data$istarget)), by = c('gameId', 'playId')) %>%
    left_join(max_throw_velo, by = c('gameId', 'playId')) %>%
    left_join(football_dist_traveled, by = c('gameId', 'playId')) %>%
    mutate(across(c(starts_with("dist_to_def"), starts_with("veloToIntercept")), function(x) ifelse(is.na(x), 999, x)),
           across(starts_with("grouped_def_pos"), function(x) ifelse(is.na(x), 'Line', x))) %>%
    group_by(.data$gameId, .data$playId) %>%
    filter(n() == 1) %>%
    ungroup(.data) %>%
    left_join(play_data, by = c('gameId', 'playId')) %>%
    rowwise(.data) %>%
    mutate(numberOfPassRushers = as.numeric(.data$numberOfPassRushers),
           defendersInTheBox = as.numeric(.data$defendersInTheBox),
           numberOfPassRushers = as.numeric(case_when(is.na(.data$numberOfPassRushers) & is.na(.data$defendersInTheBox) ~ as.numeric(sum(c_across(.data$dist_to_def_1:.data$dist_to_def_11) == 999)),
                                                      is.na(.data$numberOfPassRushers) ~ as.numeric((sum(c_across(.data$dist_to_def_1:.data$dist_to_def_11) == 999) + .data$defendersInTheBox) / 2),
                                                      TRUE ~ as.numeric(.data$numberOfPassRushers)))) %>%
    ungroup(.data) %>%
    left_join(weather, by = 'gameId') %>%
    left_join(football_loc_at_arrival, by = c('gameId', 'playId')) %>%
    left_join(target_position_at_throw, by = c('gameId', 'playId'))

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
             html_table(.data))[[1]] %>%
    select(.data$Home, .data$Away, .data$Forecast) %>%
    mutate(week = .data$week)

  return(page)
}


