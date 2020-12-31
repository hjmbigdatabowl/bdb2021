#' recalc_prob recalculate the catch probabilities after removing a defender
#' @param train the training set
#' @param test the test set
#' @param xgb_model the xgboost model
#' @param logit_model the logit model used for Platt scaling
#' @return A success string
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom glue glue
#' @importFrom tidyr pivot_wider
#' @importFrom probably threshold_perf
#' @importFrom vip vip
#' @importFrom yardstick roc_auc
#' @importFrom graphics cdplot
#' @import ggplot2
#' @import dplyr
catch_prob_diagnostic_plots <- function(train, test, xgb_model, logit_model) {
  preds <- train %>%
    mutate(target = as.factor(outcome),
                  calibratedprob = stepwise_catch_prob_predict(.data, xgb_model, logit_model))

  (
    roc_plot <- preds %>%
      threshold_perf(target, calibratedprob, thresholds = seq(0, 1, by = .01)) %>%
      pivot_wider(id_cols = .threshold, names_from = .metric, values_from = .estimate) %>%
      ggplot(aes(x = (1-spec), y = sens)) +
      geom_point(aes(color = .threshold)) +
      geom_line() +
      geom_abline(slope = 1, intercept = 0) +
      labs(x = 'FPR', y = 'TPR') +
      lims(x = c(0,1), y = c(0,1))
  )


  preds %>%
    threshold_perf(target, calibratedprob, thresholds = seq(.05, .95, by = .01)) %>%
    pivot_wider(id_cols = .threshold, names_from = .metric, values_from = .estimate) %>%
    ggplot(aes(.threshold, distance)) +
    geom_point()

  THRESHOLD_TO_USE <- preds %>%
    threshold_perf(target, calibratedprob, thresholds = seq(0, 1, by = .01)) %>%
    pivot_wider(id_cols = .threshold, names_from = .metric, values_from = .estimate) %>%
    filter(distance == min(distance)) %>%
    pull(.threshold)

  results <- test %>%
    mutate(target = as.factor(outcome),
                  predprob = stepwise_catch_prob_predict(., xgb_model, logit_model),
                  pred_outcome = ifelse(predprob > THRESHOLD_TO_USE, 'Complete', 'Incomplete'))

  jpeg("inst/plots/cdplot.jpg", width = 350, height = 300)
  cdplot(results$predprob, as.factor(results$target))
  dev.off()


  (
    predhist <- ggplot(results, aes(predprob)) + geom_histogram()
  )

  confusion_matrix <- results %>%
    mutate(cmat = case_when(pred_outcome == 'Complete' & outcome == 'Complete' ~ 'TP',
                                          pred_outcome == 'Complete' & outcome == 'Incomplete' ~ 'FP',
                                          pred_outcome == 'Incomplete' & outcome == 'Complete' ~ 'FN',
                                          pred_outcome == 'Incomplete' & outcome == 'Incomplete' ~ 'TN')) %>%
    count(cmat)

  (
    metrics <- confusion_matrix %>%
      pivot_wider(names_from = cmat, values_from = n) %>%
      mutate(precision = TP / (TP + FP),
                    recall = TP / (TP + FN),
                    specificity = TN / (TN + FP),
                    acc = (TP + TN) / (TP + TN + FP + FN),
                    fnr = (FN) / (FN + TP),
                    fpr = 1 - specificity,
                    auc = roc_auc(results, target, predprob)$.estimate) %>%
      select(-c(FN:TP))
  )

  (
    calplot <- results %>%
      mutate(predprob = round(predprob, digits = 2)) %>%
      group_by(predprob) %>%
      summarize(catches = sum(ifelse(outcome == 'Complete', 1, 0)) / n(),
                       N = n(), .groups = 'drop') %>%
      ggplot(aes(predprob, catches, size = N)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0)
  )

  (
    varimp <-   pull_workflow_fit(xgb_model) %>%
      vip(20)
  )

  ggsave('roc_plot.png', roc_plot, device = 'png', path = 'inst/plots/')
  ggsave('calplot.png', calplot, device = 'png', path = 'inst/plots/')
  ggsave('varimp.png', varimp, device = 'png', path = 'inst/plots/')
  write.csv(metrics, 'inst/plots/metrics.csv', row.names = F)
  return('plots updated and saved in plots/ dir')
}
