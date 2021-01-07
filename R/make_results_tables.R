#' gt_theme_538 the Fivethirtyeight GT theme
#' Credit to Thomas Mock: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/#fivethirtyeight
#' @return list: the table in data frame form and gt form
#' @param data the data to use to build the table
#' @param ... other parameters
#' @importFrom magrittr %>%
#' @import gt
#' @export
#'
gt_theme_538 <- function(data, ...) {
  data %>%
    opt_all_caps() %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    ) %>%
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    )
}

#' make_catch_prob_table makes the drops added table
#' @return list: the table in data frame form and gt form
#' @param num the number of players to include in the table (default: 1000)
#' @param playcutoff the min number of plays (default: 300)
#' @param show_top True sorts by the top players, False sorts by the worst
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom stringr str_sub
#' @importFrom scales col_numeric
#' @importFrom utils head data
#' @import nflfastR
#' @import dplyr
#' @import gt
#' @export
#'
make_catch_prob_table <- function(num = 1000, playcutoff = 300, show_top = TRUE) {
  . <- NULL
  teams_colors_logos <- NULL
  final_xgb <- NULL

  load("inst/models/catch_prob_a_xgb.Rdata")
  arrival_xgb <- final_xgb

  load("inst/models/catch_prob_t_xgb.Rdata")
  throw_xgb <- final_xgb
  rm(final_xgb)

  arrival_data <- do_catch_prob_arrival_feat_eng() %>%
    mutate(arrival_preds = stepwise_catch_prob_predict(., arrival_xgb))
  throw_data <- do_catch_prob_throw_feat_eng() %>%
    mutate(throw_preds = stepwise_catch_prob_predict(., throw_xgb))

  data("teams_colors_logos", envir = environment())

  nonweek <- read_non_week_files()

  arrival_credit <- divvy_credit(arrival_data, arrival_xgb)
  throw_credit <- divvy_credit(throw_data, throw_xgb)

  throw_ids <- throw_data %>%
    left_join(throw_credit, by = c("gameId", "playId")) %>%
    select(.data$gameId, .data$playId, .data$nflId_def_1:.data$nflId_def_11) %>%
    pivot_longer(-c(.data$gameId, .data$playId),
      names_to = "defender",
      values_to = "nflId"
    ) %>%
    mutate(defender = str_sub(.data$defender, 11L, -1L))

  arrival_ids <- arrival_data %>%
    left_join(arrival_credit, by = c("gameId", "playId")) %>%
    select(.data$gameId, .data$playId, .data$nflId_def_1:.data$nflId_def_11) %>%
    pivot_longer(-c(.data$gameId, .data$playId),
      names_to = "defender",
      values_to = "nflId"
    ) %>%
    mutate(defender = str_sub(.data$defender, 11L, -1L))

  throw_creds <- throw_data %>%
    left_join(throw_credit, by = c("gameId", "playId")) %>%
    select(.data$gameId, .data$playId, .data$def_1:.data$def_11) %>%
    pivot_longer(-c(.data$gameId, .data$playId),
      names_to = "defender",
      values_to = "credit"
    ) %>%
    mutate(defender = str_sub(.data$defender, 5L, -1L))

  arrival_creds <- arrival_data %>%
    left_join(arrival_credit, by = c("gameId", "playId")) %>%
    select(.data$gameId, .data$playId, .data$def_1:.data$def_11) %>%
    pivot_longer(-c(.data$gameId, .data$playId),
      names_to = "defender",
      values_to = "credit"
    ) %>%
    mutate(defender = str_sub(.data$defender, 5L, -1L))

  all_throw_credit <- left_join(throw_ids, throw_creds, by = c("gameId", "playId", "defender"))
  all_arrival_credit <- left_join(arrival_ids, arrival_creds, by = c("gameId", "playId", "defender"))

  play_metadata <- nonweek$plays %>%
    left_join(nonweek$games, by = "gameId") %>%
    mutate(defendingTeam = ifelse(.data$possessionTeam == .data$homeTeamAbbr, .data$visitorTeamAbbr, .data$homeTeamAbbr)) %>%
    select(.data$gameId, .data$playId, .data$defendingTeam) %>%
    distinct()

  defender_teams <- throw_data %>%
    select(.data$gameId, .data$playId, .data$nflId_def_1:.data$nflId_def_11) %>%
    pivot_longer(cols = c(.data$nflId_def_1:.data$nflId_def_11), names_to = "pos", values_to = "nflId") %>%
    select(.data$gameId, .data$playId, .data$nflId) %>%
    distinct() %>%
    left_join(play_metadata, by = c("playId", "gameId")) %>%
    select(.data$nflId, .data$defendingTeam) %>%
    distinct() %>%
    rename(team = .data$defendingTeam)

  arrange_by <- function(data, show_top = show_top) {
    if (show_top) {
      arrange(data, rank)
    } else {
      arrange(data, desc(rank))
    }
  }

  arrival_results <- arrival_data %>%
    mutate(
      numeric_outcome = ifelse(.data$outcome == "Complete", 1, 0),
      marginal = .data$arrival_preds - .data$numeric_outcome
    ) %>%
    select(.data$gameId, .data$playId, .data$marginal, .data$nflId_def_1:.data$nflId_def_11) %>%
    pivot_longer(
      cols = -c(.data$gameId, .data$playId, .data$marginal),
      names_to = "defender",
      values_to = "nflId"
    ) %>%
    select(-.data$nflId) %>%
    mutate(defender = str_sub(.data$defender, 11L, -1L)) %>%
    left_join(all_arrival_credit, by = c("gameId", "playId", "defender")) %>%
    distinct() %>%
    drop_na(.data$nflId) %>%
    mutate(drops_added_arrival = .data$credit * .data$marginal) %>%
    group_by(.data$nflId) %>%
    summarize(
      drops_added_arrival = sum(.data$drops_added_arrival, na.rm = T),
      plays = n(), .groups = "drop"
    ) %>%
    mutate(
      drops_perplay_arrival = .data$drops_added_arrival / .data$plays) %>%
    left_join(nonweek$players, by = c("nflId" = "nflId")) %>%
    select(.data$nflId, .data$displayName, .data$position, .data$plays, .data$drops_added_arrival, .data$drops_perplay_arrival) %>%
    left_join(defender_teams, by = "nflId") %>%
    left_join(
      teams_colors_logos %>% select(.data$team_abbr, .data$team_logo_espn),
      by = c("team" = "team_abbr")
    ) %>%
    group_by(.data$nflId) %>%
    filter(
      row_number() == 1L,
      .data$position %in% c("CB", "S", "FS", "SS")
    ) %>%
    ungroup() %>%
    arrange(desc(.data$drops_added_arrival)) %>%
    mutate(rank = row_number()) %>%
    select(.data$rank, .data$position, .data$nflId, .data$displayName, .data$team_logo_espn, .data$plays, .data$drops_added_arrival, .data$drops_perplay_arrival) %>%
    arrange_by(show_top)

  arrival_tab <- arrival_results %>%
    filter(.data$plays > playcutoff) %>%
    head(num) %>%
    gt() %>%
    text_transform(
      locations = cells_body(vars("team_logo_espn")),
      fn = function(x) {
        web_image(
          url = x,
          height = 40
        )
      }
    ) %>%
    fmt_number(
      columns = vars("drops_added_arrival", "drops_perplay_arrival"),
      decimals = 2
    ) %>%
    data_color(
      columns = vars("drops_added_arrival"),
      colors = col_numeric(
        palette = c("red", "white", "blue"),
        domain = c(-29, 29)
      )
    ) %>%
    cols_label(
      position = "POS",
      displayName = "Name",
      team_logo_espn = "logo",
      plays = "Chances",
      drops_added_arrival = "Drops Added (Arrival)",
      drops_perplay_arrival = "Drops Added (per play)"
    ) %>%
    gt_theme_538()

  throw_results <- throw_data %>%
    left_join(arrival_data %>% select(.data$gameId, .data$playId, .data$arrival_preds), by = c('gameId', 'playId')) %>%
    mutate(
      marginal = .data$throw_preds - .data$arrival_preds
    ) %>%
    select(.data$gameId, .data$playId, .data$marginal, .data$nflId_def_1:.data$nflId_def_11) %>%
    pivot_longer(
      cols = -c(.data$gameId, .data$playId, .data$marginal),
      names_to = "defender",
      values_to = "nflId"
    ) %>%
    select(-.data$nflId) %>%
    mutate(defender = str_sub(.data$defender, 11L, -1L)) %>%
    left_join(all_throw_credit, by = c("gameId", "playId", "defender")) %>%
    distinct() %>%
    drop_na(.data$nflId) %>%
    mutate(drops_added_throw = .data$credit * .data$marginal) %>%
    group_by(.data$nflId) %>%
    summarize(
      drops_added_throw = sum(.data$drops_added_throw, na.rm = T),
      plays = n(), .groups = "drop"
    ) %>%
    mutate(
      drops_perplay_throw = .data$drops_added_throw / .data$plays) %>%
    left_join(nonweek$players, by = c("nflId" = "nflId")) %>%
    select(.data$nflId, .data$displayName, .data$position, .data$plays, .data$drops_added_throw, .data$drops_perplay_throw) %>%
    drop_na(.data$drops_added_throw) %>%
    left_join(defender_teams, by = "nflId") %>%
    left_join(
      teams_colors_logos %>% select(.data$team_abbr, .data$team_logo_espn),
      by = c("team" = "team_abbr")
    ) %>%
    group_by(.data$nflId) %>%
    filter(
      row_number() == 1L,
      .data$position %in% c("CB", "S", "FS", "SS")
    ) %>%
    ungroup() %>%
    arrange(desc(.data$drops_added_throw)) %>%
    mutate(rank = row_number()) %>%
    select(.data$rank, .data$position, .data$nflId, .data$displayName, .data$team_logo_espn, .data$plays, .data$drops_added_throw, .data$drops_perplay_throw) %>%
    arrange_by(show_top)

  throw_tab <- throw_results  %>%
    filter(.data$plays > playcutoff) %>%
    head(num) %>%
    gt() %>%
    text_transform(
      locations = cells_body(vars("team_logo_espn")),
      fn = function(x) {
        web_image(
          url = x,
          height = 40
        )
      }
    ) %>%
    fmt_number(
      columns = vars("drops_added_throw", "drops_perplay_throw"),
      decimals = 2
    ) %>%
    data_color(
      columns = vars("drops_added_throw"),
      colors = col_numeric(
        palette = c("red", "white", "blue"),
        domain = c(-29, 29)
      )
    ) %>%
    cols_label(
      position = "POS",
      displayName = "Name",
      team_logo_espn = "logo",
      plays = "Chances",
      drops_added_throw = "Drops Added (throw)",
      drops_perplay_throw = "Drops Added (per play)"
    ) %>%
    gt_theme_538()

  gtsave(arrival_tab, "catch_prob_rankings_arrival.png", path = "inst/tables")
  gtsave(throw_tab, "catch_prob_rankings_throw.png", path = "inst/tables")
  save(arrival_results, file = 'inst/data/drops_added_arrival.Rdata')
  save(throw_results, file = 'inst/data/drops_added_throw.Rdata')

  return(list(
    arrival_results = arrival_results,
    throw_results = throw_results,
    arrival_table = arrival_tab,
    throw_table = throw_tab
  ))
}

#' make_tendency_table makes the deterrence table
#' @return list: the table in data frame form and gt form
#' @param data the data to use to build the table
#' @param model the model
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom stringr str_sub
#' @importFrom utils View data
#' @import nflfastR
#' @import dplyr
#' @import gt
#' @export
#'
make_tendency_table <- function(data, model) {
  teams_colors_logos <- NULL

  nonweek_data <- read_non_week_files()
  players <- nonweek_data$players
  data("teams_colors_logos", envir = environment())

  # bin_pred <- predict(target_mod_bin, newdata = tendency_plays[test_idx,], type='response')
  pos_pred <- predict(model, data, type = "prob")$.pred_1

  results_df <- cbind(
    data[, c("gameId", "playId", "x_adj", "y_adj", "def_distance", "dist_side_line", "regressed_targets", "position", "target_flg")],
    pos_pred
  )

  results_df %>%
    group_by(round(.data$pos_pred, 2)) %>%
    summarize(
      predTargetsPosition = mean(.data$pos_pred),
      actualTargets = mean(.data$targetFlag),
      n = n()
    ) %>%
    ggplot() +
    # geom_point(aes(x=pred_targets_bin, y=actual_targets, size=n), alpha=0.7, color="coral") +
    geom_point(aes(x = .data$predTargetsPosition, y = .data$actualTargets, size = .data$n), alpha = 0.7) + # color="dodgerblue") +
    geom_abline(slope = 1, intercept = 0) +
    # facet_wrap(~position) +
    theme(axis.title = element_text()) +
    labs(title = "RF Target Model", x = "Predicted Target Rate", y = "Actual Target Rate")

  with_defender <- results_df %>%
    left_join(players, by = c("defId1" = "nflId"))


  def_agg <- with_defender %>%
    group_by(.data$defId1, .data$defPosition1, .data$displayName, .data$birthDate, .data$height, .data$weight, .data$defendingTeam) %>%
    summarise(
      plays = n(),
      expected_targets = sum(.data$target_pred),
      actual_targets = sum(.data$targetFlag),
      targets_added = sum(.data$target_pred - .data$targetFlag),
      regressed_targets_added = .data$targets_added / sqrt(.data$plays),
      strength_of_wr = mean(.data$regressedTargets)
    )


  def_agg %>%
    ungroup() %>%
    left_join(
      teams_colors_logos %>% select(.data$team_abbr, .data$team_logo_espn),
      by = c("defendingTeam" = "team_abbr")
    ) %>%
    filter(.data$plays >= 1) %>%
    arrange(desc(.data$targets_added)) %>%
    mutate(rank = row_number()) %>%
    select(
      .data$rank, .data$defPosition1, .data$displayName, .data$birthDate, .data$height, .data$weight,
      .data$team_logo_espn, .data$plays, .data$expected_targets, .data$actual_targets, .data$targets_added
    ) %>%
    arrange(-.data$rank) %>%
    head(20) %>%
    gt() %>%
    text_transform(
      locations = cells_body(vars("team_logo_espn")),
      fn = function(x) {
        web_image(
          url = x,
          height = 40
        )
      }
    ) %>%
    fmt_number(
      columns = vars("expected_targets", "targets_added"),
      decimals = 0
    ) %>%
    data_color(
      columns = vars("targets_added"),
      colors = col_numeric(
        palette = c("red", "white", "blue"),
        domain = c(-29, 29)
      )
    ) %>%
    cols_label(
      defPosition1 = "POS",
      # jerseyNumber = "",
      displayName = "Name",
      team_logo_espn = "logo",
      expected_targets = "xTargets",
      actual_targets = "targets",
      targets_added = "targets saved"
    ) %>%
    gt_theme_538()
}


#' load_player_summary_table Function that builds the overall results
#' @importFrom magrittr %>%
#'
#'
load_player_summary_table <- function(){
  `%>%` <- magrittr::`%>%`
  engine <- bdb2021::connect_to_heroku_postgres()

  catch_throw_agg <- engine %>%
    dplyr::tbl('drops_added_throw') %>%
    dplyr::rename(plays_throw = plays) %>%
    dplyr::collect()

  catch_arrival_agg <- engine %>%
    dplyr::tbl('drops_added_arrival') %>%
    dplyr::rename(plays_arrival = plays) %>%
    dplyr::collect()

  target_agg <- engine %>%
    dplyr::tbl('target_data_aggregated') %>%
    dplyr::collect()


  speed_dat <- engine %>%
    dplyr::tbl('speed_summary') %>%
    dplyr::collect() %>%
    dplyr::select(-.data$plays)

  df <- target_agg %>%
    dplyr::filter(plays > 50) %>%
    dplyr::left_join(catch_throw_agg %>% dplyr::select(.data$nflId, .data$plays_throw, .data$drops_added_throw),
                     by = "nflId") %>%
    dplyr::left_join(catch_arrival_agg %>% dplyr::select(.data$nflId, .data$plays_arrival, .data$drops_added_arrival),
                     by = "nflId") %>%
    dplyr::left_join(speed_dat, by="nflId") %>%
    dplyr::mutate(regressedDropsThrow = .data$drops_added_throw / .data$plays_throw,
                  regressedDropsArrival = .data$drops_added_arrival / .data$plays_arrival,
                  dropdownName = paste(.data$position, " ", .data$displayName, " (", .data$defendingTeam, ")", sep=""))

  summary_stats <- df %>%
    dplyr::group_by(position) %>%
    dplyr::summarise(meanCoverage = mean(regressedCoverage),
                     sdCoverage = sd(regressedCoverage),
                     meanDeterrence = mean(regressedDeterrence),
                     sdDeterrence = sd(regressedDeterrence),
                     meanDropsThrow = mean(regressedDropsThrow, na.rm = T),
                     sdDropsThrow = sd(regressedDropsThrow, na.rm = T),
                     meanDropsArrival = mean(regressedDropsArrival, na.rm = T),
                     sdDropsArrival = sd(regressedDropsArrival, na.rm = T),
                     .groups = 'drop')

  df <- df %>%
    dplyr::inner_join(summary_stats, by="position") %>%
    dplyr::mutate(
      coverageZ = (regressedCoverage - meanCoverage) / sdCoverage,
      coverageGrade = 100 * pnorm(.data$coverageZ),
      deterrenceZ = (regressedDeterrence - meanDeterrence) / sdDeterrence,
      deterrenceGrade = 100 * pnorm(.data$deterrenceZ),
      dropsThrowZ = (regressedDropsThrow - meanDropsThrow) / sdDropsThrow,
      dropsThrowGrade = 100 * pnorm(.data$dropsThrowZ),
      dropsArrivalZ = (regressedDropsArrival - meanDropsArrival) / sdDropsArrival,
      dropsArrivalGrade = 100 * pnorm(.data$dropsArrivalZ),
      totalGrade = (.data$coverageGrade + .data$deterrenceGrade + .data$dropsThrowGrade + .data$dropsArrivalGrade) / 4,
      totalGrade = 100 * pnorm((.data$totalGrade - mean(.data$totalGrade, na.rm = T)) / sd(.data$totalGrade, na.rm = T))
    ) %>%
    dplyr::select(-(meanCoverage:sdDropsArrival)) %>%
    dplyr::filter(plays > 200, position == "DB")

  rm(summary_stats)
  return(df)
}


#' build_final_leaderboard The top-15 leaderboard in the final report
#' @importFrom dplyr left_join select
#' @importFrom gt gt
#' @import webshot
#'
build_final_leaderboard <- function() {
  summary_dat <- load_player_summary_table() %>%
    left_join(nflfastR::teams_colors_logos %>% select(team_abbr, team_logo_espn),
              by = c("defendingTeam" = "team_abbr"))

  total_table <- summary_dat %>%
    arrange(-totalGrade) %>%
    head(15) %>%
    select(displayName, team_logo_espn, coverageGrade, deterrenceGrade, dropsThrowGrade, dropsArrivalGrade, totalGrade) %>%
    gt()  %>%
    text_transform(
      locations = cells_body(vars("team_logo_espn")),
      fn = function(x) {
        web_image(
          url = x,
          height = 40
        )
      }
    ) %>%
    fmt_number(
      columns = vars("coverageGrade", "deterrenceGrade", "dropsThrowGrade", "dropsArrivalGrade", "totalGrade"),
      decimals = 0
    ) %>%
    cols_label(
      displayName = "Name",
      team_logo_espn = "",
      coverageGrade = "Coverage",
      deterrenceGrade = "Deterrence",
      dropsThrowGrade = "Closing",
      dropsArrivalGrade = "Breakups",
      totalGrade = "Total"
    ) %>% data_color(
      columns = vars("coverageGrade", "deterrenceGrade", "dropsThrowGrade", "dropsArrivalGrade", "totalGrade"),
      colors = col_numeric(
        palette = c("red", "white", "blue"),
        domain = c(0, 100)
      )
    ) %>%
    tab_footnote(
      footnote = "538 table format courtesy Tom Mock from themockup.blog",
      locations=cells_title()
    ) %>%
    gt_theme_538()

  # webshot::install_phantomjs() needed to run this if not installed
  total_table %>% gtsave("inst/plots/final_leaderboard.png")
}
