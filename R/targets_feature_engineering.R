#' do_target_prob_feat_eng build a data frame of target prob features
#'
#' @param weeks_to_use Numeric: a numeric vector of weeks to use (default 1:17)
#' @return a data frame of features
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom rlang .data
#' @import nflfastR
#' @import dplyr
#' @export
#'
do_target_prob_feature_eng <- function(weeks_to_use = 1:17){
  # get game & play meta, combine into one df
  nonweek <- read_non_week_files()
  play_metadata <- nonweek$plays
  game_metadata <- nonweek$games

  play_metadata <- play_metadata %>%
    left_join(game_metadata, by=c("gameId")) %>%
    mutate(defendingTeam = ifelse(.data$possessionTeam == .data$homeTeamAbbr,
                                  .data$visitorTeamAbbr,
                                  .data$homeTeamAbbr))

  OFFENSE_POSITIONS <- get_constants("offense_positions")
  DEFENSE_POSITIONS <- get_constants("defense_positions")
  DEFENSIVE_LINE_POSITIONS <- get_constants("defensive_line_positions")
  LINEBACKER_POSITIONS <- get_constants("linebacker_positions")
  DEFENSIVE_BACK_POSITIONS <- get_constants("defensive_back_positions")
  THROW_START_EVENTS <- get_constants("throw_start_events")

  # get position data, make changes to coordinate system to have all plays go in one direction
  plays <- weeks_to_use %>%
    map(read_individual_week) %>%
    bind_rows() %>%
    standardize_position_coordinates()

  # nflfastR play-by-play data
  pbp <- readRDS(url(paste0("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_2018.rds?raw=true")))

  pbp <- pbp %>%
    filter(.data$pass_attempt == 1) %>%
    mutate(gameId = as.character(.data$old_game_id),
           playId = as.character(.data$play_id)) %>%
    select(gameId,
           playId,
           pbpReceiverName = .data$receiver,
           halfSecondsRemaining = .data$half_seconds_remaining,
           scoreDifferential = .data$score_differential) %>%
    filter(!is.na(.data$pbpReceiverName))

  football_start_pos <- plays %>%
    filter(.data$displayName == "Football") %>%
    group_by(.data$gameId, .data$playId) %>%
    summarize(
      xInit = first(.data$x),
      yInit = first(.data$y))

  # position data of QB when throw is made
  qb_throw_position <- plays %>%
    filter(.data$position == "QB"
           & .data$event %in% THROW_START_EVENTS) %>%
    rename(qbName = .data$displayName,
           qbId = .data$nflId,
           qbX = .data$x,
           qbY = .data$y,
           qbSpeed = .data$s,
           qbDir = .data$dir,
           qbO = .data$o) %>%
    select(.data$gameId, .data$playId, contains("qb"))

  # receiver position at throw
  receiver_throw_position <- plays %>%
    filter(.data$position %in% OFFENSE_POSITIONS
           & .data$event %in% THROW_START_EVENTS
           & .data$position != "QB") %>%
    rename(receiverName = .data$displayName,
           receiverId = .data$nflId,
           receiverX = .data$x,
           receiverY = .data$y,
           receiverSpeed = .data$s,
           receiverDir = .data$dir,
           receiverO = .data$o,
           receiverRoute = .data$route,
           receiverPosition = .data$position) %>%
    select(.data$gameId, .data$playId, .data$ToLeft, contains("receiver"))

  # targeted receiver on each play
  play_targeted_receiver <- read_targets() %>%
    mutate(gameId = as.character(.data$gameId),
           playId = as.character(.data$playId),
           targetNflId = as.character(.data$targetNflId))

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

  Y_MAX <- 160 / 3

  throw_target_data <- receiver_throw_position %>%
    inner_join(football_start_pos, by=c("gameId", "playId")) %>%
    mutate(xAdj = .data$receiverX - .data$xInit,
           yAdj = .data$receiverY - .data$yInit,
           distSideLine = pmin(.data$receiverY, Y_MAX - .data$receiverY)) %>%
    left_join(qb_throw_position, by=c("gameId", "playId")) %>%
    inner_join(play_targeted_receiver, by=c("gameId", "playId")) %>%
    inner_join(pbp, by=c("gameId", "playId")) %>%
    left_join(defenders, by=c("gameId", "playId")) %>%
    mutate(receiverName = gsub("(?<=^.).*?\\s", ".", .data$receiverName, perl = T),
           targetFlag = as.numeric(.data$receiverName == .data$pbpReceiverName),
           defDistance = sqrt((.data$receiverX - .data$defX)**2 + (.data$receiverY - .data$defY)**2)) %>%
    group_by(.data$gameId, .data$playId, .data$receiverId) %>%
    slice(which.min(defDistance)) %>%
    ungroup() %>%
    group_by(.data$gameId, .data$playId) %>%
    mutate(targets = sum(.data$targetFlag)) %>%
    filter(.data$targets == 1) %>%
    ungroup() %>%
    select(c("receiverX", "receiverY", "receiverSpeed", "receiverDir", "receiverO", "receiverDir", "receiverRoute", "receiverPosition", "xAdj", "yAdj", "defX", "defY", "defDistance",
             "defPosition", "distSideLine", "halfSecondsRemaining", "scoreDifferential", "receiverId", "receiverName",
             "qbName", "qbId", "qbX", "qbY", "qbSpeed", "qbDir", "qbO", "targetFlag", "playId", "gameId", "ToLeft")) %>%
    mutate(receiverId = as.factor(.data$receiverId),
           qbId = as.factor(.data$qbId))%>%
    left_join(
      play_metadata %>%
        select("gameId", "playId", "possessionTeam", "defendingTeam", "yardsToGo", "typeDropback", "defendersInTheBox",
               "numberOfPassRushers", "absoluteYardlineNumber", "epa") %>%
        mutate(
          gameId = as.factor(.data$gameId),
          playId = as.factor(.data$playId)
        ),
      by=c("gameId", "playId")
    ) %>%
    mutate(
      firstDownYardline = ifelse(.data$ToLeft,
                                 120 - .data$absoluteYardlineNumber - .data$yardsToGo - 10,
                                 .data$absoluteYardlineNumber + .data$yardsToGo - 10),
      distToFirst = .data$receiverX - .data$firstDownYardline,
      receiverPosition = ifelse(.data$receiverPosition == "HB", "RB", .data$receiverPosition),
      oAdj = atan2(.data$xAdj, .data$yAdj) * 180 / pi,
      oAdjDiff = abs(.data$oAdj - .data$qbO),
      oAdjCos = cos((.data$oAdj - .data$qbO) * pi / 180)
    ) %>%
    filter(!is.na(.data$oAdjCos))

  receiver_target_rates <- throw_target_data %>%
    group_by(.data$receiverId, .data$receiverName, .data$possessionTeam, .data$receiverPosition) %>%
    summarize(
      plays = n(),
      targetFrequency = mean(targetFlag),
      regressedTargets = sum(targetFlag) / sqrt(plays),
      .groups="keep"
    ) %>%
    ungroup()

  throw_target_data <- throw_target_data %>%
    left_join(receiver_target_rates, by = c("receiverPosition", "receiverId", "receiverName", "possessionTeam"))

  rm(nonweek, play_metadata, game_metadata, plays, pbp, play_targeted_receiver, receiver_throw_position, football_start_pos, receiver_target_rates, defenders, qb_throw_position)

  return(throw_target_data)
}
