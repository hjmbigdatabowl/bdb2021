#' run_catch_prob_tuning_pipeline runs the catch prob tuning pipeline
#' @return data.frame: normalized positional data
#' @importFrom magrittr %>%
#' @importFrom dplyr sample_frac setdiff
#' @export
#'
run_catch_prob_tuning_pipeline <- function() {
  set.seed(14159)
  df <- do_catch_prob_feat_eng()
  train <- df %>%
    sample_frac(.8)

  test <- df %>%
    setdiff(train)

  catch_prob_tuning_results <- tune_catch_prob_xgb(train)
  catch_prob_model <- fit_catch_prob_xgb(workflow = catch_prob_tuning_results$workflow,
                                         pars = catch_prob_tuning_results$parameters,
                                         data_split = catch_prob_tuning_results$data_split,
                                         data = catch_prob_tuning_results$data)
  logit_model <- fit_logit_platt_scaler(catch_prob_model$final_xgb, train)
  catch_prob_diagnostic_plots(train, test, catch_prob_model$final_xgb, logit_model)
  make_catch_prob_table(df, catch_prob_model$final_xgb, logit_model, 1000, 50, TRUE)
}
