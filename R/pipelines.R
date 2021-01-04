#' run_catch_prob_tuning_pipeline runs the catch prob tuning pipeline
#' @return null
#' @param throw_or_arr the model (throwtime "t" or arrival time "a" to tune)
#' @importFrom magrittr %>%
#' @importFrom dplyr sample_frac setdiff
#' @export
#'
run_catch_prob_tuning_pipeline <- function(throw_or_arr = "") {
  set.seed(14159)
  df <- switch (throw_or_arr,
                't' = do_catch_prob_throw_feat_eng() %>%
                  select(
                    .data$gameId, .data$playId, .data$dist_to_def_1:.data$veloToIntercept_def_11, .data$max_throw_velo, .data$throwdist,
                    .data$numberOfPassRushers, .data$targetXThrow, .data$targetYThrow, .data$footballXArr, .data$footballYArr,
                    .data$conditions, .data$temperature, .data$targetSThrow, .data$targetAThrow, .data$skill, .data$height, .data$outcome
                  ),
                'a' = do_catch_prob_arrival_feat_eng() %>%
                  select(
                    .data$gameId, .data$playId, .data$dist_to_def_1:.data$grouped_def_pos_11, .data$max_throw_velo, .data$throwdist,
                    .data$numberOfPassRushers, .data$targetXArrival, .data$targetYArrival, .data$footballXArr, .data$footballYArr,
                    .data$conditions, .data$temperature, .data$targetSArrival, .data$targetAArrival, .data$skill, .data$height, .data$outcome
                  )
              )

  train <- df %>%
    sample_frac(.8)

  test <- df %>%
    setdiff(train)

  catch_prob_tuning_results <- tune_catch_prob_xgb(train, mod = throw_or_arr, overnightmode = T)
  catch_prob_model <- fit_catch_prob_xgb(
    workflow = catch_prob_tuning_results$workflow,
    pars = catch_prob_tuning_results$parameters,
    data_split = catch_prob_tuning_results$data_split,
    data = catch_prob_tuning_results$data,
    mod = throw_or_arr
  )
  catch_prob_diagnostic_plots(train, test, catch_prob_model$final_xgb, throw_or_arr)

  return(invisible(NULL))
}

#' run_catch_prob_tuning_pipeline runs the catch prob tuning pipeline
#' @return NULL (invisible)
#' @export
#'
run_catch_prob_tuning_pipelines <- function() {
  run_catch_prob_tuning_pipeline('a')
  run_catch_prob_tuning_pipeline('t')
  make_catch_prob_table(1000, 50, TRUE)

  return(invisible(NULL))
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
