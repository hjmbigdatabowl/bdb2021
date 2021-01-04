#' standardize_position_coordinates Takes raw position data and normalizes it so that drives only go one way and X coordinates are relative to goal line rather than back of end zone
#'
#' @param df DataFrame of position data to be transformed
#' @return data.frame: normalized positional data
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#' @export
#'
standardize_position_coordinates <- function(df) {
  OFFENSE_POSITIONS <- get_constants("offense_positions")

  return(
    df %>%
      mutate(
        gameId = as.character(.data$gameId),
        playId = as.character(.data$playId),
        ToLeft = .data$playDirection == "left",
        IsOnOffense = .data$position %in% OFFENSE_POSITIONS,
        xStd = ifelse(.data$ToLeft, 120 - .data$x, .data$x) - 10,
        yStd = ifelse(.data$ToLeft, 160 / 3 - .data$y, .data$y),
        dirStd1 = ifelse(.data$ToLeft & .data$dir < 90, .data$dir + 360, .data$dir),
        dirStd1 = ifelse(!.data$ToLeft & .data$dir > 270, .data$dir - 360, .data$dir),
        dirStd2 = ifelse(.data$ToLeft, .data$dirStd1 - 180, .data$dirStd1),
        oStd1 = ifelse(.data$ToLeft & .data$o < 90, .data$o + 360, .data$o),
        oStd1 = ifelse(!.data$ToLeft & .data$o > 270, .data$o - 360, .data$o),
        oStd2 = ifelse(.data$ToLeft, .data$oStd1 - 180, .data$oStd1),
        x = .data$xStd,
        y = .data$yStd,
        dir = .data$dirStd2,
        o = .data$oStd2
      ) %>%
      select(-c(.data$xStd, .data$yStd, .data$dirStd1, .data$dirStd2, .data$oStd1, .data$oStd2))
  )
}

#' add_receiver_target_rates calculate how frequently receivers are targeted in entire dataset
#'
#' @param df DataFrame of target data
#' @return DataFrame with the target rates added on
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarize ungroup left_join
#' @export
#'
add_receiver_target_rates <- function(df) {
  receiver_target_rates <- df %>%
    group_by(.data$receiverId, .data$receiverName, .data$possessionTeam, .data$receiverPosition) %>%
    summarize(
      plays = n(),
      targetFrequency = mean(.data$targetFlag),
      regressedTargets = sum(.data$targetFlag) / sqrt(.data$plays), # balance between target rate, raw # of targets
      .groups = "keep"
    ) %>%
    ungroup()

  return(
    df %>%
      left_join(receiver_target_rates, by = c("receiverPosition", "receiverId", "receiverName", "possessionTeam"))
  )
}

#' do_target_prob_feat_eng build a data frame of target prob features
#'
#' @param weeks_to_use Numeric: a numeric vector of weeks to use (default 1:17)
#' @return a data frame of features
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom dplyr left_join inner_join mutate bind_rows filter select group_by summarize rename slice ungroup
#' @importFrom stats complete.cases
#' @export
#'
do_target_prob_feature_eng <- function(weeks_to_use = 1:17) {
  . <- NULL

  # get game & play meta, combine into one df
  nonweek <- read_non_week_files()
  play_metadata <- nonweek$plays
  game_metadata <- nonweek$games

  play_metadata <- play_metadata %>%
    left_join(game_metadata, by = c("gameId")) %>%
    mutate(defendingTeam = ifelse(.data$possessionTeam == .data$homeTeamAbbr,
      .data$visitorTeamAbbr,
      .data$homeTeamAbbr
    ))

  OFFENSE_POSITIONS <- get_constants("offense_positions")
  DEFENSE_POSITIONS <- get_constants("defense_positions")
  DEFENSIVE_LINE_POSITIONS <- get_constants("defensive_line_positions")
  LINEBACKER_POSITIONS <- get_constants("linebacker_positions")
  DEFENSIVE_BACK_POSITIONS <- get_constants("defensive_back_positions")
  THROW_START_EVENTS <- get_constants("throw_start_events")

  weather <- build_weather_df(weeks_to_use) %>%
    mutate(gameId = as.character(.data$gameId))

  # get position data, make changes to coordinate system to have all plays go in one direction
  plays <- weeks_to_use %>%
    map(read_individual_week) %>%
    bind_rows() %>%
    standardize_position_coordinates()

  # nflfastR play-by-play data
  pbp <- readRDS(url(paste0("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_2018.rds?raw=true")))

  pbp <- pbp %>%
    filter(.data$pass_attempt == 1) %>%
    mutate(
      gameId = as.character(.data$old_game_id),
      playId = as.character(.data$play_id)
    ) %>%
    select(.data$gameId,
      .data$playId,
      pbpReceiverName = .data$receiver,
      halfSecondsRemaining = .data$half_seconds_remaining,
      scoreDifferential = .data$score_differential
    ) %>%
    filter(!is.na(.data$pbpReceiverName))

  football_start_pos <- plays %>%
    filter(.data$displayName == "Football") %>%
    group_by(.data$gameId, .data$playId) %>%
    summarize(
      xInit = first(.data$x),
      yInit = first(.data$y)
    )

  # position data of QB when throw is made
  qb_throw_position <- plays %>%
    filter(.data$position == "QB"
    & .data$event %in% THROW_START_EVENTS) %>%
    rename(
      qbName = .data$displayName,
      qbId = .data$nflId,
      qbX = .data$x,
      qbY = .data$y,
      qbSpeed = .data$s,
      qbDir = .data$dir,
      qbO = .data$o
    ) %>%
    select(.data$gameId, .data$playId, contains("qb"))

  # receiver position at throw
  Y_MAX <- 160 / 3

  receiver_throw_position <- plays %>%
    filter(.data$position %in% OFFENSE_POSITIONS
    & .data$event %in% THROW_START_EVENTS
    & .data$position != "QB") %>%
    rename(
      receiverName = .data$displayName,
      receiverId = .data$nflId,
      receiverX = .data$x,
      receiverY = .data$y,
      receiverSpeed = .data$s,
      receiverDir = .data$dir,
      receiverO = .data$o,
      receiverRoute = .data$route,
      receiverPosition = .data$position
    ) %>%
    mutate(distSideLine = pmin(.data$receiverY, Y_MAX - .data$receiverY)) %>%
    select(.data$gameId, .data$playId, .data$ToLeft, .data$distSideLine, contains("receiver"))

  # targeted receiver on each play
  play_targeted_receiver <- read_targets() %>%
    mutate(
      gameId = as.character(.data$gameId),
      playId = as.character(.data$playId),
      targetNflId = as.character(.data$targetNflId)
    )

  # defender position at time of throw
  defenders <- plays %>%
    filter(.data$position %in% DEFENSE_POSITIONS
    & .data$event %in% THROW_START_EVENTS) %>%
    rename(defX = .data$x, defY = .data$y, defId = .data$nflId) %>%
    mutate(
      defPosition = case_when(
        .data$position %in% DEFENSIVE_BACK_POSITIONS ~ "DB",
        .data$position %in% LINEBACKER_POSITIONS ~ "LB",
        .data$position %in% DEFENSIVE_LINE_POSITIONS ~ "DL",
      )
    ) %>%
    select("defX", "defY", "defId", "defPosition", "gameId", "playId")

  throw_target_data <- receiver_throw_position %>%
    inner_join(football_start_pos, by = c("gameId", "playId")) %>%
    mutate(
      xAdj = .data$receiverX - .data$xInit,
      yAdj = .data$receiverY - .data$yInit
    ) %>%
    left_join(qb_throw_position, by = c("gameId", "playId")) %>%
    inner_join(play_targeted_receiver, by = c("gameId", "playId")) %>%
    inner_join(pbp, by = c("gameId", "playId")) %>%
    left_join(defenders, by = c("gameId", "playId")) %>%
    mutate(
      receiverName = gsub("(?<=^.).*?\\s", ".", .data$receiverName, perl = T),
      targetFlag = as.numeric(.data$receiverName == .data$pbpReceiverName),
      defDistance = sqrt((.data$receiverX - .data$defX)**2 + (.data$receiverY - .data$defY)**2)
    ) %>%
    group_by(.data$gameId, .data$playId, .data$receiverId) %>%
    arrange(.data$defDistance) %>%
    mutate(defPlayId = 1:n()) %>%
    filter(.data$defPlayId <= 3) %>%
    ungroup() %>%
    pivot_wider(
      names_from = .data$defPlayId,
      values_from = c(.data$defX, .data$defY, .data$defId, .data$defPosition, .data$defDistance),
      names_sep = ""
    ) %>%
    group_by(.data$gameId, .data$playId) %>%
    arrange(-.data$defDistance1) %>%
    mutate(
      minDefDistancePlay = min(.data$defDistance1),
      maxDefDistancePlay = max(.data$defDistance1),
      playOpenRank = 1:n()
    ) %>%
    ungroup() %>%
    group_by(.data$gameId, .data$playId) %>%
    mutate(targets = sum(.data$targetFlag)) %>%
    filter(.data$targets == 1) %>%
    ungroup() %>%
    mutate(
      receiverId = as.factor(.data$receiverId),
      qbId = as.factor(.data$qbId)
    ) %>%
    left_join(
      play_metadata %>%
        select(
          "gameId", "playId", "possessionTeam", "defendingTeam", "yardsToGo", "typeDropback", "defendersInTheBox",
          "numberOfPassRushers", "absoluteYardlineNumber", "epa"
        ) %>%
        mutate(
          gameId = as.factor(.data$gameId),
          playId = as.factor(.data$playId)
        ),
      by = c("gameId", "playId")
    ) %>%
    mutate(
      firstDownYardline = ifelse(.data$ToLeft,
        120 - .data$absoluteYardlineNumber - .data$yardsToGo - 10,
        .data$absoluteYardlineNumber + .data$yardsToGo - 10
      ),
      distToFirst = .data$receiverX - .data$firstDownYardline,
      distToEndzone = .data$receiverX - 100,
      receiverPosition = ifelse(.data$receiverPosition == "HB", "RB", .data$receiverPosition),
      oAdj = atan2(.data$xAdj, .data$yAdj) * 180 / pi
    ) %>%
    filter(complete.cases(.)) %>%
    left_join(weather, by = 'gameId')

  throw_target_data <- add_receiver_target_rates(throw_target_data)

  rm(nonweek, play_metadata, game_metadata, plays, pbp, play_targeted_receiver, receiver_throw_position, football_start_pos, defenders, qb_throw_position)

  return(throw_target_data)
}
