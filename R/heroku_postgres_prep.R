#' heroku_postgres_inserts
#'
#' connect to our Heroku Postgres instance
#' @return the connection
#' @import dplyr
#' @importFrom DBI dbWriteTable
#' @importFrom rlang .data
#' @export
#'
heroku_postgres_inserts <- function() {
  engine <- connect_to_heroku_postgres()

  model_preds <- NULL
  arrival_results <- NULL
  throw_results <- NULL

  load("~/documents/github/bdb2021/inst/data/drops_added_arrival.Rdata")
  df <- arrival_results %>%
    arrange(desc(.data$drops_added_arrival))

  dbWriteTable(engine, name = "drops_added_arrival", df, overwrite = T)

  load("~/documents/github/bdb2021/inst/data/drops_added_throw.Rdata")
  df <- throw_results %>%
    arrange(desc(.data$drops_added_throw))

  dbWriteTable(engine, name = "drops_added_throw", df, overwrite = T)

  load("~/documents/github/bdb2021shiny/model_data/catch_prob_preds.Rdata")
  df <- load_encrypted("~/documents/github/bdb2021shiny/model_data/catch_prob_features.Rdata") %>%
    mutate(across(everything(), function(x) ifelse(x == 999, NA_real_, x))) %>%
    rowwise() %>%
    transmute(
      gameId = .data$gameId,
      playId = .data$playId,
      avg_dist_to_defs = mean(c_across(.data$dist_to_def_1:.data$dist_to_def_11), na.rm = T),
      min_dist_to_nearest_def = min(c_across(.data$dist_to_def_1:.data$dist_to_def_11), na.rm = T),
      defs_within_five_yards = sum(c_across(.data$dist_to_def_1:.data$dist_to_def_11) <= 5, na.rm = T),
      throw_distance = .data$throwdist,
      throw_velocity = .data$max_throw_velo,
      receiver_height = .data$height,
      receiver_skill = .data$skill,
      conditions = .data$conditions,
      temperature = .data$temperature,
      football_y_position_at_pass_arrival = .data$footballYArr,
      football_x_position_at_pass_arrival = .data$footballXArr,
      target_x_position_at_throw = .data$targetXThrow,
      target_y_position_at_throw = .data$targetYThrow,
      target_speed_at_throw = .data$targetSThrow,
      target_acceleration_at_throw = .data$targetAThrow,
      outcome = .data$outcome
    ) %>%
    ungroup() %>%
    left_join(model_preds %>% rename(catch_probability = .data$predprob), by = c("gameId", "playId"))

  dbWriteTable(engine, "aggregated_catch_prob_features", df, overwrite = T)
}
