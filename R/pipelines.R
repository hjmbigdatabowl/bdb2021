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

#' run_target_prob_tuning_pipeline pipeline for target prob model
#' @param tune_file_name if the pipeline has been run before, the location of the tuning file
#' @importFrom magrittr %>%
#' @importFrom dplyr sample_frac setdiff
#' @export
run_target_prob_tuning_pipeline <- function(tune_file_name){
  set.seed(62-36)
  target_df <- do_target_prob_feature_eng()

  train_df <- target_df %>%
    group_by(.data$gameId, .data$playId) %>%
    slice_sample(prop=0.8) %>%
    ungroup()

  test_df <- target_df %>%
    setdiff(train_df) %>%
    ungroup()

  if(tune_file_name != ""){
    target_prob_tuning_results <- build_target_prob_tune_results(tune_file_name)
  } else{
    target_prob_tuning_results <- tune_target_prob_xgb(train_df)
  }

  target_prob_models <- fit_target_prob_xgb(workflow = target_prob_tuning_results$workflow,
                                            pars = target_prob_tuning_results$parameters,
                                            data_split = target_prob_tuning_results$data_split,
                                            data = target_prob_tuning_results$data)
  logit_scaling_model <- fit_logit_target_platt_scaler(target_prob_models$final_xgb, train_df)
  target_prob_diagnositc_plots(train_df, test_df, target_prob_models$final_xgb, logit_scaling_model)

  prior_target_model <- fit_prior_target_prob(target_df)
  build_target_results(target_prob_models$final_xgb, logit_scaling_model, prior_target_model, target_df)
}

#' build_target_prob_tune_results function to create target prob tuning results from stored file
#' @param file_name string with file location
#' @importFrom magrittr %>%
#' @export
#'
build_target_prob_tune_results <- function(file_name){
  xgb_spec <- xgb_res <- xgb_wf <- best_auc <- data_folds <- data_split <- NULL
  load(file_name)
  return(list(model_specification = xgb_spec,
              tune_parameters = xgb_res,
              workflow = xgb_wf,
              parameters = best_auc,
              data_folds = data_folds,
              data = data,
              data_split = data_split))
}
