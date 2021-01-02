#' fit_catch_prob_xgb fit the xgboost catch prob model
#'
#' @param workflow a workflow object
#' @param pars a tibble of the parameters (generally output from tune::select_best())
#' @param data_split a data_split object
#' @param data a full data frame to fit the model on
#' @return a list with the final xgboost model and the modeling results (auc, acc, etc.)
#' @importFrom magrittr %>%
#' @importFrom tune finalize_workflow last_fit
#' @importFrom parsnip fit
#' @export
#'
fit_catch_prob_xgb <- function(workflow, pars, data_split, data) {
  final_xgb <- tune::finalize_workflow(
    workflow,
    pars
  ) %>%
    parsnip::fit(data)

  final_res <- tune::last_fit(final_xgb, data_split)

  save(final_xgb, final_res, file = "inst/models/catch_prob_xgb.Rdata")

  return(list(
    final_xgb = final_xgb,
    final_res = final_res
  ))
}

#' fit_target_prob_rf fit the rf target prob model
#'
#' @param workflow a workflow object
#' @param pars a tibble of the parameters (generally output from tune::select_best())
#' @param data_split a data_split object
#' @param data a full data frame to fit the model on
#' @return a list with the final random forest model and the modeling results (auc, acc, etc.)
#' @importFrom magrittr %>%
#' @importFrom tune finalize_workflow last_fit
#' @importFrom parsnip fit
#' @importFrom stats predict
#' @export
#'
fit_target_prob_rf <- function(workflow, pars, data_split, data) {
  final_rf <- tune::finalize_workflow(
    workflow,
    pars
  ) %>%
    parsnip::fit(data)

  final_res <- tune::last_fit(final_rf, data_split)

  save(final_rf, final_res, file = "inst/models/target_prob_rf.Rdata")

  return(list(
    final_rf = final_rf,
    final_res = final_res
  ))
}

#' fit_logit_platt_scaler fit the Platt scaler to calibrate the xgboost predictions
#'
#' @param model an xgboost model fit with parsnip
#' @param data a data frame
#' @return a logistic regression (parsnip) model
#' @importFrom magrittr %>%
#' @importFrom tune finalize_workflow last_fit
#' @importFrom parsnip fit logistic_reg set_engine predict.model_fit
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom stats predict
#' @export
#'
fit_logit_platt_scaler <- function(model, data) {
  . <- NULL
  preds <- data %>%
    mutate(
      predprob = predict(model, ., type = "prob")$.pred_Complete,
      target = as.factor(.data$outcome)
    )

  logit_model <- logistic_reg() %>%
    set_engine("glm") %>%
    fit(target ~ predprob, data = preds)

  return(logit_model)
}

#' stepwise_catch_prob_predict Make calibrated predictions from xgboost + Platt scaling
#'
#' @param data a data frame
#' @param xgb_model the xgboost (parsnip) model
#' @param logit_model the logistic regression (parsnip) model
#' @return a vector of predicted probabilities
#' @importFrom magrittr %>%
#' @importFrom parsnip predict.model_fit
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom stats predict
#' @export
#'
stepwise_catch_prob_predict <- function(data, xgb_model, logit_model) {
  . <- NULL
  preds <- data %>%
    mutate(predprob = predict(xgb_model, ., type = "prob")$.pred_Complete) %>%
    mutate(calibratedprob = predict(logit_model, ., type = "prob")$.pred_Complete)

  return(preds$predprob)
}

#' stepwise_target_prob_predict Make calibrated predictions from rf + Platt scaling
#'
#' @param data a data frame
#' @param rf_model the rf (parsnip) model
#' @param logit_model the logistic regression (parsnip) model
#' @return a vector of predicted probabilities
#' @importFrom magrittr %>%
#' @importFrom parsnip predict.model_fit
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom stats predict
#' @export
#'
stepwise_target_prob_predict <- function(data, rf_model, logit_model) {
  . <- NULL
  preds <- data %>%
    mutate(predprob = predict(rf_model, ., type = "prob")$.pred_Complete) %>%
    mutate(calibratedprob = predict(logit_model, ., type = "prob")$.pred_Complete)

  return(preds$calibratedprob)
}
