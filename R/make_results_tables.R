gt_theme_538 <- function(data,...) {
  data %>%
    gt::opt_all_caps()  %>%
    gt::opt_table_font(
      font = list(
        gt::google_font("Chivo"),
        gt::default_fonts()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom", color = "transparent", weight = gt::px(2)
      ),
      locations = gt::cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>%
    gt::tab_options(
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

make_catch_prob_table <- function(data, model, num = 1000, playcutoff = 300, show_top = TRUE) {
  `%>%` <- magrittr::`%>%`
  source("R/divvy_catch_credit.R")

  preds <- stepwise_predict(data, model)

  teams_colors_logos <- nflfastR::teams_colors_logos
  nonweek <- read_non_week_files()

  credit <- divvy_credit(data, model)

  ids <- data %>%
    left_join(credit, by = c('gameId', 'playId')) %>%
    select(gameId, playId, nflId_def_1:nflId_def_11) %>%
    pivot_longer(-c(gameId, playId),
                 names_to = 'defender',
                 values_to = 'nflId') %>%
    mutate(defender = stringr::str_sub(defender, 11L, -1L))

  creds <- data %>%
    left_join(credit, by = c('gameId', 'playId')) %>%
    select(gameId, playId, def_1:def_11) %>%
    pivot_longer(-c(gameId, playId),
                 names_to = 'defender',
                 values_to = 'credit') %>%
    mutate(defender = stringr::str_sub(defender, 5L, -1L))

  all_credit <- left_join(ids, creds, by = c('gameId', 'playId', 'defender'))

  epas <- nonweek$plays %>%
    select(gameId, playId, epa)

  play_metadata <- nonweek$plays %>%
    dplyr::left_join(nonweek$games, by = 'gameId') %>%
    dplyr::mutate(defendingTeam = ifelse(possessionTeam == homeTeamAbbr, visitorTeamAbbr, homeTeamAbbr)) %>%
    dplyr::select(gameId, playId, defendingTeam) %>%
    dplyr::distinct()

  defender_teams <- data %>%
    dplyr::select(gameId, playId, nflId_def_1:nflId_def_11) %>%
    tidyr::pivot_longer(cols = c(nflId_def_1:nflId_def_11), names_to = "pos", values_to = 'nflId') %>%
    dplyr::select(gameId, playId, nflId) %>%
    dplyr::distinct() %>%
    dplyr::left_join(play_metadata, by = c('playId', 'gameId')) %>%
    dplyr::select(nflId, defendingTeam) %>%
    dplyr:: distinct() %>%
    dplyr::rename(team = defendingTeam)

  results <- data %>%
    dplyr::mutate(preds = preds,
                  numeric_outcome = ifelse(outcome == 'Complete', 1, 0),
                  marginal = numeric_outcome - preds) %>%
    select(gameId, playId, marginal, nflId_def_1:nflId_def_11) %>%
    pivot_longer(cols = -c(gameId, playId, marginal),
                 names_to = 'defender',
                 values_to = 'nflId') %>%
    select(-nflId) %>%
    mutate(defender = stringr::str_sub(defender, 11L, -1L)) %>%
    left_join(all_credit, by = c('gameId', 'playId', 'defender')) %>%
    left_join(epas, by = c('gameId', 'playId')) %>%
    distinct() %>%
    drop_na(nflId) %>%
    mutate(drops_added = credit * marginal) %>%
    dplyr::group_by(nflId) %>%
    dplyr::summarize(drops_added = sum(drops_added),
                     epa_added = sum(epa),
                     plays = dplyr::n(), .groups = 'drop') %>%
    dplyr::mutate(drops_perplay = drops_added / plays,
                  epa_perplay = epa_added / plays) %>%
    dplyr::left_join(nonweek$players, by = c('nflId' = 'nflId')) %>%
    dplyr::arrange(desc(epa_perplay)) %>%
    dplyr::select(nflId, displayName, position, plays, drops_added, drops_perplay)

  arrange_by <- function(data, show_top = show_top) {
    if (show_top) {
      dplyr::arrange(data, rank)
    } else {
      dplyr::arrange(data, dplyr::desc(rank))
    }
  }

  tab <- results %>%
    dplyr::filter(plays > playcutoff) %>%
    dplyr::left_join(defender_teams, by = 'nflId') %>%
    dplyr::left_join(
      teams_colors_logos %>% dplyr::select(team_abbr, team_logo_espn),
      by = c("team" = "team_abbr")
    ) %>%
    dplyr::group_by(nflId) %>%
    dplyr::filter(row_number() == 1L,
                  position %in% c('CB', 'S', 'FS', 'SS')) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(drops_added)) %>%
    dplyr::mutate(rank=row_number()) %>%
    dplyr::select(rank, position, displayName, team_logo_espn, plays, drops_added, drops_perplay) %>%
    arrange_by(show_top) %>%
    head(num) %>%
    gt::gt() %>%
    gt::text_transform(
      locations = gt::cells_body(vars(team_logo_espn)),
      fn = function(x) {
        gt::web_image(
          url = x,
          height = 40
        )
      }
    ) %>%
    gt::fmt_number(
      columns = vars(drops_added, drops_perplay),
      decimals = 2
    ) %>%
    gt::data_color(
      columns = vars(drops_added),
      colors = scales::col_numeric(
        palette = c("red", "white", "blue"),
        domain = c(-29, 29)
      )
    ) %>%
    gt::cols_label(
      position = 'POS',
      displayName = 'Name',
      team_logo_espn = 'logo',
      plays = 'Chances',
      drops_added = 'Drops Added',
      drops_perplay = 'Drops Added (per play)'
    ) %>%
    gt_theme_538()

  gt::gtsave(tab, 'catch_prob_rankings.png', path = 'tables')

  return(list(results = results,
              table = tab))
}

make_tendency_table <- function(data, model) {

  # bin_pred <- predict(target_mod_bin, newdata = tendency_plays[test_idx,], type='response')
  pos_pred <- predict(model, data, type='prob')$.pred_1

  results_df <- cbind(
    data[,c('gameId', 'playId', 'x_adj', 'y_adj', 'def_distance', 'dist_side_line', 'regressed_targets', 'position', 'target_flg')],
    pos_pred
  )

  results_df %>%
    group_by(round(pos_pred, 2)) %>%
    summarize(
      # pred_targets_bin = mean(bin_pred),
      pred_targets_position = mean(pos_pred),
      actual_targets = mean(target_flg),
      n = n()
    ) %>%
    ggplot() +
    # geom_point(aes(x=pred_targets_bin, y=actual_targets, size=n), alpha=0.7, color='coral') +
    geom_point(aes(x=pred_targets_position, y=actual_targets, size=n), alpha=0.7) + #color='dodgerblue') +
    geom_abline(slope=1, intercept = 0) +
    # facet_wrap(~position) +
    theme_fivethirtyeight() +
    theme(axis.title= element_text()) +
    labs(title='RF Target Model', x='Predicted Target Rate', y='Actual Target Rate')

  with_defender <- tendency_plays %>%
    left_join(
      plays %>%
        filter(position %in% DEFENSE_POSITIONS & event %in% THROW_START_EVENTS) %>%
        rename(x_def=x,y_def=y,def_id=nflId) %>%
        select(x_def, y_def, def_id, gameId, playId, displayName, jerseyNumber, team)
      , by=c('gameId', 'playId', 'x_def', 'y_def') , suffix = c('', '_def'))

  def_agg <- with_defender %>%
    group_by(jerseyNumber, def_position, displayName_def, defendingTeam, def_id) %>%
    summarise(
      plays = n(),
      expected_targets = sum(target_pred),
      actual_targets = sum(target_flg),
      targets_added = sum(target_pred - target_flg),
      regressed_targets_added = targets_added / sqrt(plays),
      strength_of_wr = mean(regressed_targets)
    )

  def_agg %>% View()

  # QB tendencies
  qb_agg <- with_defender %>%
    group_by(qb_name, possessionTeam) %>%
    summarise(
      plays = n(),
      expected_targets = sum(target_pred),
      actual_targets = sum(target_flg),
      targets_added = sum(target_pred - target_flg),
      regressed_targets_added = targets_added / sqrt(plays),
      strength_of_wr = mean(regressed_targets)
    )

  with_defender %>%
    filter(qb_name %in% (qb_agg %>% filter(plays > 2000) %>% pull(qb_name))) %>%
    ggplot() +
    stat_summary_hex(aes(x=y_adj, y=x_adj, z=(target_pred-target_flg)), bins=8) +
    scale_fill_gradient2() +
    geom_vline(xintercept = 0, color='coral') +
    geom_hline(yintercept = 0, color='coral') +
    facet_wrap(~qb_name) +
    theme_fivethirtyeight()

  # sanity check
  with_defender %>%
    ggplot() +
    stat_summary_hex(aes(x=y_adj, y=x_adj, z=(target_pred-target_flg)), bins=20) +
    scale_fill_gradient2() +
    geom_vline(xintercept = 0, color='coral') +
    geom_hline(yintercept = 0, color='coral') +
    theme_fivethirtyeight()

  # any pairs that lean on each other
  qb_wr_agg <- with_defender %>%
    group_by(displayName, position, qb_name, possessionTeam) %>%
    summarise(
      plays = n(),
      expected_targets = sum(target_pred),
      actual_targets = sum(target_flg),
      targets_added = sum(target_flg - target_pred),
      regressed_targets_added = targets_added / sqrt(plays),
      strength_of_wr = mean(regressed_targets)
    )

  qb_wr_agg %>% View()

  # more targets -> underperform xTargets?
  wr_agg <- with_defender %>%
    group_by(displayName, position, possessionTeam) %>%
    summarise(
      plays = n(),
      expected_targets = sum(target_pred),
      actual_targets = sum(target_flg),
      targets_added = sum(target_flg - target_pred),
      regressed_targets_added = targets_added / sqrt(plays),
      strength_of_wr = mean(regressed_targets)
    )

  wr_agg %>%
    filter(plays > 100) %>%
    ggplot() +
    geom_point(aes(x=strength_of_wr, y=regressed_targets_added)) +
    theme_fivethirtyeight() +
    theme(axis.title= element_text()) +
    labs(title='', x='WR Strength (targets / sqrt(n))', y='Targets Added')

  def_agg %>%
    ungroup() %>%
    left_join(
      teams_colors_logos %>% select(team_abbr, team_logo_espn),
      by = c("defendingTeam" = "team_abbr")
    ) %>%
    filter(plays >= 300) %>%
    arrange(desc(targets_added)) %>%
    mutate(rank=row_number()) %>%
    select(rank, def_position, jerseyNumber, displayName_def, team_logo_espn, plays, expected_targets, actual_targets, targets_added) %>%
    arrange(-rank) %>%
    head(20) %>%
    gt() %>%
    text_transform(
      locations = cells_body(vars(team_logo_espn)),
      fn = function(x) {
        web_image(
          url = x,
          height = 40
        )
      }
    ) %>%
    fmt_number(
      columns = vars(expected_targets, targets_added),
      decimals = 0
    ) %>%
    data_color(
      columns = vars(targets_added),
      colors = scales::col_numeric(
        palette = c("red", "white", "blue"),
        domain = c(-29, 29)
      )
    ) %>%
    cols_label(
      def_position = 'POS',
      jerseyNumber = '',
      displayName_def = 'Name',
      team_logo_espn = 'logo',
      expected_targets = 'xTargets',
      actual_targets = 'targets',
      targets_added = 'targets saved'
    ) %>%
    gt_theme_538()

}
