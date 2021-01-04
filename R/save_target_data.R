#' build_target_results output all of the results of the target models to be used
#' @return a success string
#' @param final_model final result of the target model
#' @param scale_model logit model used to calibrate final results
#' @param prior_target_model model for target probability before play begins
#' @param data dataframe with all observations from season
#' @export
build_target_results <- function(final_model, scale_model, prior_target_model, data){
  . <- NULL
  pff <- read_pff()
  player_bio <- read_non_week_files()$players

  with_pred <- data %>%
    group_by(.data$gameId, .data$playId) %>%
    mutate(playTargets = sum(.data$regressedTargets)) %>%
    ungroup() %>%
    mutate(expectedTargetShare = .data$regressedTargets / .data$playTargets) %>%
    mutate(targetFlag = as.factor(.data$targetFlag),
           targetPred = predict(final_model, ., type='prob')$.pred_1,
           calibratedProb = stepwise_target_prob_predict_xgb(., final_model, scale_model),
           targetPredPrior = predict(prior_target_model, ., type='prob')$.pred_1)


  aggregated_target_data <- with_pred %>%
    group_by(.data$defId1, .data$defPosition1, .data$defendingTeam) %>%
    summarise(plays = n(),
              games = n_distinct(.data$gameId),
              averageSeperation = mean(.data$defDistance1),
              priorExpectedTargets = sum(.data$targetPredPrior),
              preThrowExpectedTargets = sum(.data$targetPred),
              actualTargets = sum(.data$targetFlag == 1),
              coverageTargetsAdded = sum(.data$targetPredPrior) - sum(.data$targetPred),
              deterrenceTargetsAdded = sum(.data$targetPred) - sum(.data$targetFlag == 1),
              regressedCoverage = .data$coverageTargetsAdded / sqrt(.data$plays),
              regressedDeterrence = .data$deterrenceTargetsAdded / sqrt(.data$plays),
              combinedGrade = .data$regressedCoverage + .data$regressedDeterrence,
              rawTargetsAdded = (.data$coverageTargetsAdded + .data$deterrenceTargetsAdded) / .data$plays) %>%
    select(.data$defId1, .data$defendingTeam, .data$plays:.data$rawTargetsAdded) %>%
    ungroup() %>%
    left_join(player_bio %>% select(-.data$position), by=c("defId1" = "nflId")) %>%
    left_join(pff %>%
                select(.data$player, .data$team_name, .data$snap_counts_coverage, .data$grades_defense, .data$grades_coverage_defense),
              by=c("displayName" = "player", "defendingTeam" = "team_name")) %>%
    rename(nflId = .data$defId1,
           position = .data$defPosition1)

  soduim_key <- load('inst/keys/sodium_key.Rdata')

  df <- with_pred
  save_encrypted(df, "inst/data/target_data.Rdata")

  df <- aggregated_target_data
  save_encrypted(df, "inst/data/target_aggregated.Rdata")
}
