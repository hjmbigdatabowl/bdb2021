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
    mutate(gameId = as.character(.data$gameId),
           playId = as.character(.data$playId),
           ToLeft = .data$playDirection == "left",
           IsOnOffense = .data$position %in% OFFENSE_POSITIONS,
           xStd = ifelse(.data$ToLeft, 120 - .data$x, .data$x) - 10,
           yStd = ifelse(.data$ToLeft, 160/3 - .data$y, .data$y),
           dirStd1 = ifelse(.data$ToLeft & .data$dir < 90, .data$dir + 360, .data$dir),
           dirStd1 = ifelse(!.data$ToLeft & .data$dir > 270, .data$dir - 360, .data$dir),
           dirStd2 = ifelse(.data$ToLeft, .data$dirStd1 - 180, .data$dirStd1),
           oStd1 = ifelse(.data$ToLeft & .data$o < 90, .data$o + 360, .data$o),
           oStd1 = ifelse(!.data$ToLeft & .data$o > 270, .data$o - 360, .data$o),
           oStd2 = ifelse(.data$ToLeft, .data$oStd1 - 180, .data$oStd1),
           x = .data$xStd,
           y = .data$yStd,
           dir = .data$dirStd2,
           o = .data$oStd2) %>%
    select(-c(.data$xStd, .data$yStd, .data$dirStd1, .data$dirStd2, .data$oStd1, .data$oStd2))

  # nflfastR play-by-play data
  pbp <- readRDS(url(paste0("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_2018.rds?raw=true")))

  pbp <- pbp %>%
    filter(.data$pass_attempt == 1) %>%
    mutate(game_id = as.character(.data$old_game_id),
           play_id = as.character(.data$play_id)) %>%
    select(gameId = .data$game_id,
           playId = .data$play_id,
           receiverId = .data$receiver_id,
           receiverName = .data$receiver,
           halfSecondsRemaining = .data$half_seconds_remaining,
           scoreDifferential = .data$score_differential) %>%
    filter(!is.na(.data$receiverName))

  football_start_pos <- plays %>%
    filter(.data$displayName == "Football") %>%
    group_by(.data$gameId, .data$playId) %>%
    summarize(
      xInit = first(.data$x),
      yInit = first(.data$y))

  # position data of QB when throw is made
  qbs <- plays %>%
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

  # targeted receiver on each play
  receivers <- read_targets() %>%
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

  throw_target_data <- plays %>%
    filter(.data$position %in% OFFENSE_POSITIONS
           & .data$event %in% THROW_START_EVENTS
           & .data$position != "QB") %>%
    rename(receiverX = .data$x,
           receiverY = .data$y) %>%
    inner_join(football_start_pos, by=c("gameId", "playId")) %>%
    mutate(
      xAdj = .data$receiverX - .data$xInit,
      yAdj = .data$receiverY - .data$yInit,
      distSideLine = pmin(.data$receiverY, Y_MAX - .data$receiverY),
    ) %>%
    left_join(qbs, by=c("gameId", "playId")) %>%
    inner_join(receivers, by=c("gameId", "playId")) %>%
    inner_join(pbp, by=c("gameId", "playId")) %>%
    left_join(defenders, by=c("gameId", "playId")) %>%
    group_by(.data$gameId, .data$playId, .data$nflId) %>%
    slice(which.min(sqrt((.data$receiverX - .data$defX)**2 + (.data$receiverY - .data$defY)**2))) %>%
    ungroup() %>%
    mutate(displayName = gsub("(?<=^.).*?\\s", ".", .data$displayName, perl = T),
           targetFlag = as.numeric(.data$displayName == .data$receiverName),
           defDistance = sqrt((.data$receiverX - .data$defX)**2 + (.data$receiverY - .data$defY)**2)) %>%
    group_by(.data$gameId, .data$playId) %>%
    mutate(targets = sum(.data$targetFlag)) %>%
    filter(.data$targets == 1) %>%
    ungroup() %>%
    select(c("receiverX", "receiverY", "s", "a", "dis", "o", "dir", "route", "position", "xAdj", "yAdj", "defX", "defY", "defDistance",
             "defPosition", "distSideLine", "halfSecondsRemaining", "scoreDifferential", "nflId", "displayName",
             "qbName", "qbId", "qbX", "qbY", "qbSpeed", "qbDir", "qbO", "targetFlag", "playId", "gameId", "ToLeft")) %>%
    mutate(nfId = as.factor(.data$nflId),
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
      position = ifelse(.data$position == "HB", "RB", .data$position),
      oAdj = atan2(.data$xAdj, .data$yAdj) * 180 / pi,
      oAdjDiff = abs(.data$oAdj - .data$qbO),
      oAdjCos = cos((.data$oAdj - .data$qbO) * pi / 180)
    ) %>%
    filter(!is.na(.data$oAdjCos))

  receiver_target_rates <- throw_target_data %>%
    group_by(.data$nflId, .data$displayName, .data$possessionTeam, .data$position) %>%
    summarize(
      plays = n(),
      target_frequency = mean(targetFlag),
      regressed_targets = sum(targetFlag) / sqrt(plays),
      .groups="keep"
    ) %>%
    ungroup()

  throw_target_data <- throw_target_data %>%
    left_join(receiver_target_rates, by = c("position", "nflId", "displayName", "possessionTeam"))

  rm(nonweek, play_metadata, game_metadata, plays, pbp, receivers, football_start_pos, receiver_target_rates, defenders, qbs)

  return(throw_target_data)
}
