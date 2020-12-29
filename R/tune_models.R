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
#' @import dials
#' @export
#'
tune_catch_prob_xgb <- function(data) {
  data <- data %>%
    select(.data$dist_to_def_1:.data$veloToIntercept_def_11, .data$max_throw_velo, .data$throwdist,
           .data$numberOfPassRushers, .data$targetXThrow, .data$targetYThrow, .data$footballXArr, .data$footballYArr,
           .data$conditions, .data$temperature, .data$targetSThrow, .data$targetAThrow, .data$outcome) %>%
    mutate(across(where(is.character), as.factor))

  data_split <- initial_split(data, strata = outcome)
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

  data_folds <- vfold_cv(data_train, strata = outcome)

  registerDoParallel(cores = 4)
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
  save(xgb_spec, xgb_res, xgb_wf, best_auc, data_folds, file = 'models/catch_prob_xgb_xval.Rdata')
  return(list(data = data,
              data_split = data_split,
              workflow = xgb_wf,
              parameters = best_auc,
              tune_results = xgb_res))
}
