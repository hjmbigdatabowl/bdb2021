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
