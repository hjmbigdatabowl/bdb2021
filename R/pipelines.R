#' run_catch_prob_tuning_pipeline runs the catch prob tuning pipeline
#' @return NULL (invisible)
#' @param throw_or_arr the model (throwtime "t" or arrival time "a" to tune)
#' @importFrom magrittr %>%
#' @importFrom dplyr sample_frac setdiff
#' @export
#'
run_catch_prob_tuning_pipeline <- function(throw_or_arr = "") {
  set.seed(14159)
  df <- switch (throw_or_arr,
                't' = do_catch_prob_throw_feat_eng(),
                'a' = do_catch_prob_arrival_feat_eng()
              )

  train <- df %>%
    sample_frac(.8)

  test <- df %>%
    setdiff(train)

  catch_prob_tuning_results <- tune_catch_prob_xgb(train)
  catch_prob_model <- fit_catch_prob_xgb(
    workflow = catch_prob_tuning_results$workflow,
    pars = catch_prob_tuning_results$parameters,
    data_split = catch_prob_tuning_results$data_split,
    data = catch_prob_tuning_results$data
  )
  logit_model <- fit_logit_platt_scaler(catch_prob_model$final_xgb, train)
  catch_prob_diagnostic_plots(train, test, catch_prob_model$final_xgb, logit_model)
  make_catch_prob_table(df, catch_prob_model$final_xgb, logit_model, 1000, 50, TRUE)
}
