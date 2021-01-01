#' tune_catch_prob_xgb tune the xgboost catch prob model
#'
#' @param data a full data frame to tune the model on
#' @return a list with the data, the data_split, the workflow, the best set of tuning parameters, and the tuning results
#' @importFrom magrittr %>%
#' @importFrom tune finalize_workflow last_fit
#' @importFrom parsnip fit set_mode set_engine boost_tree
#' @importFrom dplyr select mutate across
#' @importFrom rlang .data
#' @importFrom rsample initial_split training testing vfold_cv
#' @importFrom tune tune tune_bayes control_bayes select_best
#' @importFrom recipes recipe step_other step_dummy all_outcomes all_nominal
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom yardstick roc_auc  f_meas kap accuracy bal_accuracy metric_set
#' @importFrom doParallel registerDoParallel
#' @importFrom utils askYesNo
#' @import dials
#' @export
#'
tune_catch_prob_xgb <- function(data) {
  where <- NULL
  ncores <- showPrompt(
    title = "Cores", message = "How many cores do you want to use? Use parallel::detectCores() to see how many cores are available.", default = 4
  )

  ncores <- as.integer(ncores)

  if (is.na(ncores)){
    stop("Error: Improper number of cores provided. Please provide an integer greater than or equal to 1.")
  } else if (ncores > detectCores()){
    stop("Error: Number of cores specified exceeds number of cores available on this machine. Please specify an integer between 1 and the value output by parallel::detectCores().")
  } else if (ncores == detectCores()) {
    askYesNo("Warning: Number of cores provided is equal to the number of cores detected on this machine. This may impact performance for other programs on your computer. Do you wish to proceed?")
  }

  data <- data %>%
    select(.data$dist_to_def_1:.data$veloToIntercept_def_11, .data$max_throw_velo, .data$throwdist,
           .data$numberOfPassRushers, .data$targetXThrow, .data$targetYThrow, .data$footballXArr, .data$footballYArr,
           .data$conditions, .data$temperature, .data$targetSThrow, .data$targetAThrow, .data$skill, .data$outcome) %>%
    mutate(across(where(is.character), as.factor))

  data_split <- initial_split(data, strata = .data$outcome)
  data_train <- training(data_split)
  data_test <- testing(data_split)

  xgb_spec <- boost_tree(
    trees = tune(),
    learn_rate = tune(),
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    mtry = tune(),
  ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")

  xgb_params <- parameters(
    trees(range = c(10, 2000)),
    learn_rate(range = c(.001, .4), trans = NULL),
    tree_depth(range = c(2, 10)),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), data_train)
  )

  prep_rec <- recipe(outcome ~ ., data = data_train) %>%
    step_other(all_nominal(), -all_outcomes(), threshold = 0.01) %>%
    step_dummy(all_nominal(),-all_outcomes())

  xgb_wf <- workflow() %>%
    add_recipe(prep_rec) %>%
    add_model(xgb_spec)

  data_folds <- vfold_cv(data_train, strata = .data$outcome)

  registerDoParallel(cores = ncores)
  xgb_res <- tune_bayes(
    xgb_wf,
    resamples = data_folds,
    param_info = xgb_params,
    iter = 500,
    metrics = metric_set(roc_auc,
                         bal_accuracy,
                         f_meas,
                         accuracy,
                         kap),
    initial = 20,
    control = control_bayes(no_improve = 200,
                                  uncertain = 50,
                                  save_pred = F,
                                  time_limit = 600,
                                  verbose = T)
  )

  best_auc <- select_best(xgb_res, "roc_auc")
  save(xgb_spec, xgb_res, xgb_wf, best_auc, data_folds, file = 'inst/models/catch_prob_xgb_xval.Rdata')
  return(list(data = data,
              data_split = data_split,
              workflow = xgb_wf,
              parameters = best_auc,
              tune_results = xgb_res))
}

#' tune_target_prob_rf tune the rf target prob model
#'
#' @param data a full data frame to tune the model on
#' @return a list with the data, the data_split, the workflow, the best set of tuning parameters, and the tuning results
#' @importFrom magrittr %>%
#' @importFrom tune finalize_workflow last_fit
#' @importFrom parsnip fit set_mode set_engine boost_tree rand_forest
#' @importFrom dplyr select mutate across
#' @importFrom rlang .data
#' @importFrom rsample initial_split training testing vfold_cv
#' @importFrom tune tune tune_bayes control_bayes select_best
#' @importFrom recipes recipe step_other step_dummy step_knnimpute all_outcomes all_nominal
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom yardstick roc_auc  f_meas kap accuracy bal_accuracy metric_set
#' @importFrom doParallel registerDoParallel
#' @importFrom rstudioapi showPrompt
#' @importFrom parallel detectCores
#' @import dials
#' @export
#'
tune_target_prob_rf <- function(data) {
  where <- NULL

  ncores <- showPrompt(
    title = "Cores", message = "How many cores do you want to use? Use parallel::detectCores() to see how many cores are available.", default = 4
  )

  ncores <- as.integer(ncores)

  if(is.na(ncores)){
    stop("Error: Improper number of cores provided. Please provide an integer greater than or equal to 1.")
  } else if(ncores > detectCores()){
    stop("Error: Number of cores specified exceeds number of cores available on this machine. Please specify an integer between 1 and the value output by parallel::detectCores().")
  }
  else if(ncores == detectCores()){
    askYesNo("Warning: Number of cores provided is equal to the number of cores detected on this machine. This may impact performance for other programs on your computer. Do you wish to proceed?")
  }
  data <- data %>%
    select(.data$xAdj, .data$yAdj, .data$defPosition,
           .data$receiverPosition, .data$defDistance, .data$distSideLine, .data$oAdjCos, .data$regressedTargets,
           .data$targetFlag) %>%
    mutate(across(where(is.character), as.factor))

  data_split <- initial_split(data, strata = targetFlag)
  data_train <- training(data_split)
  data_test <- testing(data_split)

  rf_spec <- rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = tune()
  ) %>%
    set_mode("classification") %>%
    set_engine("ranger")

  rf_params <- parameters(
    trees(),
    min_n(),
    finalize(mtry(), data_train)
  )

  prep_rec <-
    recipe(formula = targetFlag ~., data = data_train) %>%
    step_other(all_nominal(), -all_outcomes(), threshold = 0.01) %>%
    step_dummy(all_nominal(),-all_outcomes()) %>%
    step_knnimpute(all_numeric(),-all_outcomes()) %>%
    step_mutate(targetFlag = as.factor(targetFlag))

  rf_wf <- workflow() %>%
    add_recipe(prep_rec) %>%
    add_model(rf_spec)

  data_folds <- vfold_cv(data_train, strata = targetFlag)
  registerDoParallel(cores = ncores)

  rf_res <- tune_bayes(
    rf_wf,
    resamples = data_folds,
    param_info = rf_params,
    iter = 500,
    metrics = metric_set(roc_auc,
                         bal_accuracy,
                         f_meas,
                         accuracy,
                         kap),
    initial = 20,
    control = control_bayes(no_improve = 200,
                            uncertain = 50,
                            save_pred = F,
                            time_limit = 600,
                            verbose = T)
  )

  best_auc <- select_best(rf_res, "roc_auc")
  save(rf_spec, rf_res, rf_wf, best_auc, data_folds, file = 'inst/models/target_prob_rf_xval.Rdata')
  return(list(data = data,
              data_split = data_split,
              workflow = rf_wf,
              parameters = best_auc,
              tune_results = rf_res))
}
