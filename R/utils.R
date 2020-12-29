#' get_constants get project-wide constants
#'
#' @param const the constant to get (one of defense_positions, offense_positions, throw_start_events, throw_end_events, defensive_back_positions, linebacker_positions, and defensive_line_positions)
#' @return string: the constant
#' @export
#'
get_constants <- function(const) {
  return(
    switch(
      const,
      "defense_positions" = c('CB', 'DB', 'DE', 'DL', 'FS', 'ILB', 'LB', 'MLB', 'NT', 'OLB', 'S', 'SS'),
      "offense_positions" = c('QB', 'WR', 'TE', 'FB', 'HB', 'RB'),
      "throw_start_events" = c('pass_forward', 'pass_shovel', 'pass_lateral'),
      "throw_end_events" = c('pass_outcome_caught', 'pass_outcome_touchdown', 'pass_outcome_incomplete'),
      "defensive_back_positions" = c('CB', 'DB', 'FS', 'S', 'SS'),
      "linebacker_positions" = c('ILB', 'LB', 'MLB', 'OLB'),
      "defensive_line_positions" = c('DE', 'DL', 'NT')
    )
  )
}
