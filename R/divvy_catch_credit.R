#' recalc_prob recalculate the catch probabilities after removing a defender
#'
#' @param defender_to_remove Numeric: the defender to remove (i.e. 1, 2, 3, .. 11)
#' @param data the data frame from which the probabilities are being predicted
#' @param xgb_model the xgboost model
#' @param logit_model the logit model used for Platt scaling
#' @return a numeric vector of predicted probabilities
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom glue glue
#'
recalc_prob <- function(defender_to_remove, data, xgb_model, logit_model) {
  . <- NULL

  data <- data %>%
    mutate(calibratedpreds = stepwise_catch_prob_predict(., xgb_model, logit_model))


  idcol_to_remove <- data[[glue('nflId_def_{defender_to_remove}')]]

  for (i in 1:11) {
    if (i > defender_to_remove & i <= 11) {
      idcol_to_replace <- glue("nflId_def_{i-1}")
      idcol_replacement <- glue("nflId_def_{i}")
      distcol_to_replace <- glue("dist_to_def_{i-1}")
      distcol_replacement <- glue("dist_to_def_{i}")
      groupedposcol_to_replace <- glue("grouped_def_pos_{i-1}")
      groupedposcol_replacement <- glue("grouped_def_pos_{i}")
      vtointcol_to_replace <- glue("veloToIntercept_def_{i-1}")
      vtointcol_replacement <- glue("veloToIntercept_def_{i}")

      data[[idcol_to_replace]] <- data[[idcol_replacement]]
      data[[distcol_to_replace]] <- data[[distcol_replacement]]
      data[[groupedposcol_to_replace]] <- data[[groupedposcol_replacement]]
      data[[vtointcol_to_replace]] <- data[[vtointcol_replacement]]
    }
  }

  data$nflId_def_11 <- idcol_to_remove
  data$dist_to_def_11 <- 999
  data$grouped_def_pos_11 <- 'Other'
  data$veloToIntercept_def_11 <- 999

  preds <- stepwise_catch_prob_predict(data, xgb_model, logit_model)

  return(preds)
}

#' divvy_credit divvy credit between defenders
#'
#' @param data the data frame from which the probabilities are being predicted
#' @param xgb_model the xgboost model
#' @param logit_model the logit model used for Platt scaling
#' @return a data frame of the weights to assign credit based on
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate rowwise ungroup select rename_with
#' @importFrom stringr str_sub
#' @importFrom rlang .data
#' @importFrom glue glue
#' @export
#'
divvy_credit <- function(data, xgb_model, logit_model) {

  original_preds <- stepwise_catch_prob_predict(data, xgb_model, logit_model)

  new_preds <- data.frame(gameId = data$gameId,
                          playId = data$playId,
                          original_preds = original_preds)
  for (i in 1:11) {
    new_preds[[i + 3]] <- (recalc_prob(i, data, xgb_model, logit_model))
  }

  diffs <- new_preds %>%
    rowwise() %>%
    mutate(across(-c(.data$original_preds, .data$gameId, .data$playId), function(x) abs(min(0, .data$original_preds - x)))) %>%
    ungroup()

  rowsums <- diffs %>%
    select(-c(.data$original_preds, .data$gameId, .data$playId)) %>%
    rowSums()

  diffs <- diffs %>%
    mutate(across(-c(.data$original_preds, .data$gameId, .data$playId), function(x) x / rowsums),
           across(-c(.data$original_preds, .data$gameId, .data$playId), function(x) ifelse(is.na(x), 1/11, x))) %>%
    rename_with(function(x) glue("def_{(str_sub(x, 2L, -1L) %>% as.numeric())-3}"),
                -c(.data$gameId, .data$playId, .data$original_preds))

  return(diffs)
}
