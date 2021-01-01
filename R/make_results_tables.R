#' gt_theme_538 the Fivethirtyeight GT theme
#' Credit to Thomas Mock: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/#fivethirtyeight
#' @return list: the table in data frame form and gt form
#' @param data the data to use to build the table
#' @param ... other parameters
#' @importFrom magrittr %>%
#' @import gt
#' @export
#'
gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
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
    )  %>%
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
#' @param data the data to use to build the table
#' @param xgb_model the xgboost model
#' @param logit_model the logit model for platt scaling
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
make_catch_prob_table <- function(data, xgb_model, logit_model, num = 1000, playcutoff = 300, show_top = TRUE) {
  teams_colors_logos <- NULL

  preds <- stepwise_catch_prob_predict(data, xgb_model, logit_model)
  data('teams_colors_logos', envir = environment())
  nonweek <- read_non_week_files()

  credit <- divvy_credit(data, xgb_model, logit_model)

  ids <- data %>%
    left_join(credit, by = c("gameId", "playId")) %>%
    select(.data$gameId, .data$playId, .data$nflId_def_1:.data$nflId_def_11) %>%
    pivot_longer(-c(.data$gameId, .data$playId),
                 names_to = "defender",
                 values_to = "nflId") %>%
    mutate(defender = str_sub(.data$defender, 11L, -1L))

  creds <- data %>%
    left_join(credit, by = c("gameId", "playId")) %>%
    select(.data$gameId, .data$playId, .data$def_1:.data$def_11) %>%
    pivot_longer(-c(.data$gameId, .data$playId),
                 names_to = "defender",
                 values_to = "credit") %>%
    mutate(defender = str_sub(.data$defender, 5L, -1L))

  all_credit <- left_join(ids, creds, by = c("gameId", "playId", "defender"))

  epas <- nonweek$plays %>%
    select(.data$gameId, .data$playId, .data$epa)

  play_metadata <- nonweek$plays %>%
    left_join(nonweek$games, by = "gameId") %>%
    mutate(defendingTeam = ifelse(.data$possessionTeam == .data$homeTeamAbbr, .data$visitorTeamAbbr, .data$homeTeamAbbr)) %>%
    select(.data$gameId, .data$playId, .data$defendingTeam) %>%
    distinct()

  defender_teams <- data %>%
    select(.data$gameId, .data$playId, .data$nflId_def_1:.data$nflId_def_11) %>%
    pivot_longer(cols = c(.data$nflId_def_1:.data$nflId_def_11), names_to = "pos", values_to = "nflId") %>%
    select(.data$gameId, .data$playId, .data$nflId) %>%
    distinct() %>%
    left_join(play_metadata, by = c("playId", "gameId")) %>%
    select(.data$nflId, .data$defendingTeam) %>%
    distinct() %>%
    rename(team = .data$defendingTeam)

  results <- data %>%
    mutate(preds = preds,
           numeric_outcome = ifelse(.data$outcome == "Complete", 1, 0),
           marginal = .data$numeric_outcome - .data$preds) %>%
    select(.data$gameId, .data$playId, .data$marginal, .data$nflId_def_1:.data$nflId_def_11) %>%
    pivot_longer(cols = -c(.data$gameId, .data$playId, .data$marginal),
                 names_to = "defender",
                 values_to = "nflId") %>%
    select(-.data$nflId) %>%
    mutate(defender = str_sub(.data$defender, 11L, -1L)) %>%
    left_join(all_credit, by = c("gameId", "playId", "defender")) %>%
    left_join(epas, by = c("gameId", "playId")) %>%
    distinct() %>%
    drop_na(.data$nflId) %>%
    mutate(drops_added = .data$credit * .data$marginal) %>%
    group_by(.data$nflId) %>%
    summarize(drops_added = sum(.data$drops_added),
                     epa_added = sum(.data$epa),
                     plays = n(), .groups = "drop") %>%
    mutate(drops_perplay = .data$drops_added / .data$plays,
                  epa_perplay = .data$epa_added / .data$plays) %>%
    left_join(nonweek$players, by = c("nflId" = "nflId")) %>%
    arrange(desc(.data$epa_perplay)) %>%
    select(.data$nflId, .data$displayName, .data$position, .data$plays, .data$drops_added, .data$drops_perplay)

  arrange_by <- function(data, show_top = show_top) {
    if (show_top) {
      arrange(data, rank)
    } else {
      arrange(data, desc(rank))
    }
  }

  tab <- results %>%
    filter(.data$plays > playcutoff) %>%
    left_join(defender_teams, by = "nflId") %>%
    left_join(
      teams_colors_logos %>% select(.data$team_abbr, .data$team_logo_espn),
      by = c("team" = "team_abbr")
    ) %>%
    group_by(.data$nflId) %>%
    filter(row_number() == 1L,
           .data$position %in% c("CB", "S", "FS", "SS")) %>%
    ungroup() %>%
    arrange(desc(.data$drops_added)) %>%
    mutate(rank=row_number()) %>%
    select(.data$rank, .data$position, .data$displayName, .data$team_logo_espn, .data$plays, .data$drops_added, .data$drops_perplay) %>%
    arrange_by(show_top) %>%
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
      columns = vars("drops_added", "drops_perplay"),
      decimals = 2
    ) %>%
    data_color(
      columns = vars("drops_added"),
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
      drops_added = "Drops Added",
      drops_perplay = "Drops Added (per play)"
    ) %>%
    gt_theme_538()

  gtsave(tab, "catch_prob_rankings.png", path = "inst/tables")

  return(list(results = results,
              table = tab))
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
  data('teams_colors_logos', envir = environment())

  # bin_pred <- predict(target_mod_bin, newdata = tendency_plays[test_idx,], type='response')
  pos_pred <- predict(model, data, type="prob")$.pred_1

  results_df <- cbind(
    data[,c("gameId", "playId", "x_adj", "y_adj", "def_distance", "dist_side_line", "regressed_targets", "position", "target_flg")],
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
    geom_point(aes(x=.data$predTargetsPosition, y=.data$actualTargets, size=.data$n), alpha=0.7) + #color="dodgerblue") +
    geom_abline(slope=1, intercept = 0) +
    # facet_wrap(~position) +
    theme(axis.title= element_text()) +
    labs(title="RF Target Model", x="Predicted Target Rate", y="Actual Target Rate")

  with_defender <- results_df %>%
    left_join(players, by=c("defId1" = "nflId"))


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
    mutate(rank=row_number()) %>%
    select(.data$rank, .data$defPosition1, .data$displayName, .data$birthDate, .data$height, .data$weight,
           .data$team_logo_espn, .data$plays, .data$expected_targets, .data$actual_targets, .data$targets_added) %>%
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
