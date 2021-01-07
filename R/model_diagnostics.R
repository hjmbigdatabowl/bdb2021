#' catch_prob_diagnostic_plots make catch prob model diagnostic plots
#' @param train the training set
#' @param test the test set
#' @param xgb_model the xgboost model
#' @param mod 'a' for arrival, 't' for throw
#' @return A success string
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom glue glue
#' @importFrom tidyr pivot_wider
#' @importFrom probably threshold_perf
#' @importFrom vip vip
#' @importFrom yardstick roc_auc
#' @importFrom graphics cdplot
#' @importFrom grDevices dev.off jpeg
#' @importFrom workflows pull_workflow_fit
#' @importFrom utils write.csv
#' @importFrom ggthemes theme_fivethirtyeight
#' @import ggplot2
#' @import dplyr
#' @export
catch_prob_diagnostic_plots <- function(train, test, xgb_model, mod = '') {
  . <- NULL
  preds <- train %>%
    mutate(
      target = as.factor(.data$outcome),
      calibratedprob = stepwise_catch_prob_predict(., xgb_model)
    )

  preds %>%
    threshold_perf(.data$target, .data$calibratedprob, thresholds = seq(.05, .95, by = .01)) %>%
    pivot_wider(id_cols = .data$.threshold, names_from = .data$.metric, values_from = .data$.estimate) %>%
    ggplot(aes(.data$.threshold, .data$distance)) +
    geom_point()

  THRESHOLD_TO_USE <- preds %>%
    threshold_perf(.data$target, .data$calibratedprob, thresholds = seq(0, 1, by = .01)) %>%
    pivot_wider(id_cols = .data$.threshold, names_from = .data$.metric, values_from = .data$.estimate) %>%
    filter(.data$distance == min(.data$distance)) %>%
    pull(.data$.threshold)

  results <- test %>%
    mutate(
      target = as.factor(.data$outcome),
      predprob = stepwise_catch_prob_predict(., xgb_model),
      pred_outcome = ifelse(.data$predprob > THRESHOLD_TO_USE, "Complete", "Incomplete")
    )

  (
    roc_plot <- results %>%
      threshold_perf(.data$target, .data$predprob, thresholds = seq(0, 1, by = .01)) %>%
      pivot_wider(id_cols = .data$.threshold, names_from = .data$.metric, values_from = .data$.estimate) %>%
      ggplot(aes(x = (1 - .data$spec), y = .data$sens)) +
      geom_point(aes(color = .data$.threshold)) +
      geom_line() +
      geom_abline(slope = 1, intercept = 0) +
      labs(x = "FPR", y = "TPR") +
      lims(x = c(0, 1), y = c(0, 1))
  )


  jpeg(glue("inst/plots/cdplot_{mod}.jpg"), width = 350, height = 300)
  cdplot(results$predprob, as.factor(results$target))
  dev.off()


  (
    predhist <- ggplot(results, aes(.data$predprob)) +
      geom_histogram()
  )

  confusion_matrix <- results %>%
    mutate(cmat = case_when(
      .data$pred_outcome == "Complete" & .data$outcome == "Complete" ~ "TP",
      .data$pred_outcome == "Complete" & .data$outcome == "Incomplete" ~ "FP",
      .data$pred_outcome == "Incomplete" & .data$outcome == "Complete" ~ "FN",
      .data$pred_outcome == "Incomplete" & .data$outcome == "Incomplete" ~ "TN"
    )) %>%
    count(.data$cmat)

  (
    metrics <- confusion_matrix %>%
      pivot_wider(names_from = .data$cmat, values_from = .data$n) %>%
      mutate(
        precision = .data$TP / (.data$TP + .data$FP),
        recall = .data$TP / (.data$TP + .data$FN),
        specificity = .data$TN / (.data$TN + .data$FP),
        acc = (.data$TP + .data$TN) / (.data$TP + .data$TN + .data$FP + .data$FN),
        fnr = (.data$FN) / (.data$FN + .data$TP),
        fpr = 1 - .data$specificity,
        auc = roc_auc(results, .data$target, .data$predprob)$.estimate
      ) %>%
      select(-c(.data$FN:.data$TP))
  )

  title <- if (mod == 't') 'Throw Time Calibration' else 'Arrival Time Calibration'

  (
    calplot <- results %>%
      mutate(predprob = round(.data$predprob * 20, digits = 0) / 20) %>%
      group_by(.data$predprob) %>%
      summarize(
        catches = sum(ifelse(.data$outcome == "Complete", 1, 0)) / n(),
        N = n(), .groups = "drop"
      ) %>%
      ggplot(aes(.data$predprob, .data$catches, size = .data$N)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0) +
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) +
      labs(x = 'Predicted Catch Probability',
           y = 'Actual Catch Proportion',
           title = title)
  )

  (
    varimp <-   pull_workflow_fit(xgb_model) %>%
      vip(40)
  )

  ggsave(glue("roc_plot_{mod}.png"), roc_plot, device = "png", path = "inst/plots/", height = 12, width = 10)
  ggsave(glue("calplot_{mod}.png"), calplot, device = "png", path = "inst/plots/", height = 12, width = 10)
  ggsave(glue("varimp_{mod}.png"), varimp, device = "png", path = "inst/plots/", height = 12, width = 10)
  write.csv(metrics, glue("inst/plots/metrics_{mod}.csv"), row.names = F)
  return("plots updated and saved in plots/ dir")
}

#' target_prob_diagnostic_plots same diagnostic plots for target probs
#' @param train dataframe of training set
#' @param test dataframe of testing set
#' @param xgb_model xgboost model produced by tune_target_prob_xgb
#' @param pre_snap_model logit model for the catch probability pre-snap
#' @param scale_model the platt scaler
#' @return success string
#' @importFrom magrittr %>%
#' @importFrom vip vip
#' @importFrom probably threshold_perf
#' @importFrom grDevices dev.off jpeg
#' @importFrom workflows pull_workflow_fit
#' @importFrom utils write.csv
#' @importFrom ggthemes theme_fivethirtyeight
#' @importFrom yardstick metrics precision recall
#' @export
#'
target_prob_diagnostic_plots <- function(train, test, xgb_model, pre_snap_model, scale_model){
  . <- NULL
  prior_target_model <- NULL
  preds <- train %>%
    mutate(targetPred = predict(xgb_model, ., type='prob')$.pred_1,
           calibratedPred = stepwise_target_prob_predict_xgb(., xgb_model, scale_model),
           preSnapProb = predict(prior_target_model, ., type='prob')$.pred_1)

  THRESHOLD_TO_USE <- preds %>%
    mutate(targetFlag = as.factor(.data$targetFlag)) %>%
    threshold_perf(.data$targetFlag, .data$targetPred, thresholds = seq(0, 1, by = .01)) %>%
    pivot_wider(id_cols = .data$.threshold, names_from = .data$.metric, values_from = .data$.estimate) %>%
    filter(.data$distance == max(.data$distance)) %>%
    pull(.data$.threshold)

  preds %>%
    mutate(targetFlag = as.factor(.data$targetFlag)) %>%
    threshold_perf(.data$targetFlag, .data$targetPred, thresholds = seq(.05, .95, by = .01)) %>%
    pivot_wider(id_cols = .data$.threshold, names_from = .data$.metric, values_from = .data$.estimate) %>%
    ggplot(aes(.data$.threshold, .data$distance)) +
    geom_point() +
    geom_vline(xintercept = THRESHOLD_TO_USE)

  results <- test %>%
    mutate(targetFlag = as.factor(.data$targetFlag),
           targetPred = predict(xgb_model, ., type='prob')$.pred_1,
           calibratedPred = stepwise_target_prob_predict_xgb(., xgb_model, scale_model),
           preSnapProb = predict(prior_target_model, ., type='prob')$.pred_1,
           predOutcomePreSnap = as.factor(ifelse(.data$preSnapProb > THRESHOLD_TO_USE, 1, 0)),
           predOutcomePreThrow = as.factor(ifelse(.data$calibratedPred > THRESHOLD_TO_USE, 1, 0)))

  (
    roc_plot <- results %>%
      threshold_perf(.data$targetFlag, .data$targetPred, thresholds = seq(0, 1, by = .01)) %>%
      pivot_wider(id_cols = .data$.threshold, names_from = .data$.metric, values_from = .data$.estimate) %>%
      ggplot(aes(x = (1-.data$spec), y = .data$sens)) +
      geom_point(aes(color = .data$.threshold), size=2) +
      geom_line() +
      geom_abline(slope = 1, intercept = 0) +
      labs(x = 'FPR', y = 'TPR') +
      lims(x = c(0,1), y = c(0,1)) +
      scale_color_gradient2(midpoint = 0.5)
  )

  (
    calplot_uncalibrated <- results %>%
      mutate(targetPred = round(.data$targetPred * 33, digits = 0) / 33) %>%
      group_by(.data$targetPred) %>%
      summarize(targets = sum(ifelse(.data$targetFlag == 1, 1, 0)) / n(),
                N = n(), .groups = 'drop') %>%
      ggplot(aes(.data$targetPred, .data$targets, size = .data$N)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0) +
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) +
      labs(x = 'Predicted Target Probability',
           y = 'Actual Target Proportion',
           title = 'Pre-Throw Model Calibration')
  )

  (
    calplot_calibrated <- results %>%
      mutate(targetPred = round(.data$calibratedPred * 33, digits = 0) / 33) %>%
      group_by(.data$targetPred) %>%
      summarize(targets = sum(ifelse(.data$targetFlag == 1, 1, 0)) / n(),
                N = n(), .groups = 'drop') %>%
      ggplot(aes(.data$targetPred, .data$targets, size = .data$N)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0) +
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) +
      labs(x = 'Predicted Target Probability',
           y = 'Actual Target Proportion',
           title = 'Pre-Throw Model Calibration')
  )

  (
    presnap_calplot <- results  %>%
      mutate(targetPred = round(.data$preSnapProb * 33, digits = 0) / 33) %>%
      group_by(.data$targetPred) %>%
      summarize(targets = sum(ifelse(.data$targetFlag == 1, 1, 0)) / n(),
                N = n(), .groups = 'drop') %>%
      ggplot(aes(.data$targetPred, .data$targets, size = .data$N)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0) +
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) +
      labs(x = 'Predicted Target Probability',
           y = 'Actual Target Proportion',
           title = 'Pre-Snap Model Calibration')
  )

  (
    varimp <- pull_workflow_fit(xgb_model) %>%
      vip(20)
  )

  pre_snap_metrics <- results %>%
    mutate(preSnapProb = 1 - .data$preSnapProb) %>%  # w/o this gives the opposite metric for AUC
    metrics(.data$preSnapProb, truth=.data$targetFlag, estimate=.data$predOutcomePreSnap) %>%
    rbind(results %>% recall(truth=.data$targetFlag, estimate=.data$predOutcomePreSnap)) %>%
    rbind(results %>% precision(truth=.data$targetFlag, estimate=.data$predOutcomePreSnap))

  pre_throw_metrics <- results %>%
    mutate(calibratedPred = 1 - .data$calibratedPred) %>%
    metrics(.data$calibratedPred, truth=.data$targetFlag, estimate=.data$predOutcomePreThrow) %>%
    rbind(results %>% recall(truth=.data$targetFlag, estimate=.data$predOutcomePreThrow)) %>%
    rbind(results %>% precision(truth=.data$targetFlag, estimate=.data$predOutcomePreThrow))

  write.csv(pre_snap_metrics, "inst/plots/metrics_target_snap.csv", row.names = F)
  write.csv(pre_throw_metrics, "inst/plots/metrics_target_throw.csv", row.names = F)

  ggsave('target_roc_plot.png', roc_plot, device = 'png', path = 'inst/plots/', height = 12, width = 10)
  ggsave('target_calplot_calibrated.png', calplot_calibrated, device = 'png', path = 'inst/plots/', height = 12, width = 10)
  ggsave('target_calplot_uncalibrated.png', calplot_uncalibrated, device = 'png', path = 'inst/plots/', height = 12, width = 10)
  ggsave('target_calplot_pre_snap.png', presnap_calplot, device = 'png', path = 'inst/plots/', height = 12, width = 10)
  ggsave('target_varimp.png', varimp, device = 'png', path = 'inst/plots/', height = 12, width = 10)
  return('plots updated and saved in plots/ dir')
}
