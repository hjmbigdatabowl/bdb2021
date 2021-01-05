#' build_raw_stat_table Get data on speed, acceleration to use in shiny app
#'
#' @importFrom dplyr filter group_by ungroup summarise arrange
#' @importFrom DBI dbWriteTable
#' @importFrom magrittr %>%
#' @export
#'

build_raw_stat_table <- function() {
  position_data <- aggregate_week_files()

  DEFENSE_POSITIONS <- get_constants('defense_positions')

  df <- position_data %>%
    filter(.data$position %in% DEFENSE_POSITIONS) %>%
    group_by(.data$nflId, .data$playId, .data$gameId) %>%
    summarise(maxSpeed = max(.data$s),
              maxAccel = max(.data$a)) %>%
    ungroup() %>%
    group_by(.data$nflId) %>%
    summarise(plays = n(),
              topSpeed = max(.data$maxSpeed),
              medianSpeed = stats::median(.data$maxSpeed),
              topAccel = max(.data$maxAccel),
              medianAccel = stats::median(.data$maxAccel)) %>%
    ungroup() %>%
    arrange(medianSpeed)

  engine <- connect_to_heroku_postgres()
  dbWriteTable(engine, 'speed_summary', df)
}
