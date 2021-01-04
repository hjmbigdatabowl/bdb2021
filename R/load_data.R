#' read_from_data load data from csv file
#'
#' @importFrom readr read_csv cols
#' @param file the file to load
#' @return data.frame with the loaded file
#' @export
read_from_data <- function(file) {
  read_csv(paste0("inst/data/", file), col_types = cols()) # suppresses col specs and uses default guess
}

#' read_non_week_files read all of the non-PBP data
#'
#' @importFrom utils unzip
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#' @importFrom stringr str_remove
#' @return a list of data.frames with the loaded files
#' @export
read_non_week_files <- function() {
  files <- unzip("inst/data/nfl-big-data-bowl-2021.zip", list = TRUE) %>%
    filter(!grepl("week", .data$Name)) %>%
    pull(.data$Name)

  fnames <- str_remove(files, ".csv")

  out <- c()
  for (i in 1:length(files)) {
    out[[fnames[i]]] <- read_from_data(files[i])
  }

  return(out)
}

#' read_individual_week load a single week of PBP data
#'
#' @importFrom purrr keep map
#' @importFrom dplyr bind_rows
#' @param week the week to read
#' @return a data.frame with the PBP data for the specified week
#' @export
read_individual_week <- function(week) {
  to_pluck <- paste0("week", week, ".csv")

  list.files("inst/data/") %>%
    keep(function(x) x == to_pluck) %>%
    map(read_from_data) %>%
    bind_rows()
}

#' aggregate_week_files aggregate all of the PBP data
#'
#' @importFrom purrr keep discard map
#' @importFrom dplyr bind_rows
#' @return a data.frame with the PBP data for the specified week
#' @export
aggregate_week_files <- function() {
  list.files("inst/data/") %>%
    keep(function(x) grepl("week", x)) %>%
    discard(function(x) grepl("coverages_week1.csv", x)) %>%
    map(read_from_data) %>%
    bind_rows()
}

#' read_targets load targeted receiver data
#'
#' @importFrom readr read_csv
#' @return a data.frame with the targets data
#' @export
read_targets <- function() {
  read_csv("inst/data/targetedReceiver.csv", col_types = cols())
}

#' read_pff load PFF grades
#'
#' @importFrom readr read_csv
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate case_when
read_pff <- function() {
  read_csv('inst/data/defense_summary.csv', col_types = cols()) %>%
    mutate(team_name = case_when(
      team_name == 'HST' ~ 'HOU',
      team_name == 'BLT' ~ 'BAL',
      team_name == 'CLV' ~  'CLE',
      team_name == 'ARZ' ~ 'ARI',
      T ~ team_name))
}
