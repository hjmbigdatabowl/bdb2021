#' get_constants get project-wide constants
#'
#' @param const the constant to get (one of defense_positions, offense_positions, throw_start_events, throw_end_events, defensive_back_positions, linebacker_positions, and defensive_line_positions)
#' @return string: the constant
#' @export
#'
get_constants <- function(const) {
  return(
    switch(
      toupper(const),
      "DEFENSE_POSITIONS" = c('CB', 'DB', 'DE', 'DL', 'FS', 'ILB', 'LB', 'MLB', 'NT', 'OLB', 'S', 'SS'),
      "OFFENSE_POSITIONS" = c('QB', 'WR', 'TE', 'FB', 'HB', 'RB'),
      "THROW_START_EVENTS" = c('pass_forward', 'pass_shovel', 'pass_lateral'),
      "THROW_END_EVENTS" = c('pass_outcome_caught', 'pass_outcome_touchdown', 'pass_outcome_incomplete'),
      "DEFENSIVE_BACK_POSITIONS" = c('CB', 'DB', 'FS', 'S', 'SS'),
      "LINEBACKER_POSITIONS" = c('ILB', 'LB', 'MLB', 'OLB'),
      "DEFENSIVE_LINE_POSITIONS" = c('DE', 'DL', 'NT')
    )
  )
}


#' standardize_position_coordinates Takes raw position data and normalizes it so that drives only go one way and X coordinates are relative to goal line rather than back of end zone
#'
#' @param df DataFrame of position data to be transformed
#' @return data.frame: normalized positional data
#' @import dplyr
#' @export
#'
standardize_position_coordinates <- function(df) {
  OFFENSE_POSITIONS <- get_constants("offense_positions")

  return(
    df %>%
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
  )
}
