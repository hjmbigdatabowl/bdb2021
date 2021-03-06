% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_models.R
\name{stepwise_target_prob_predict}
\alias{stepwise_target_prob_predict}
\title{stepwise_target_prob_predict Make calibrated predictions from rf + Platt scaling}
\usage{
stepwise_target_prob_predict(data, rf_model, logit_model)
}
\arguments{
\item{data}{a data frame}

\item{rf_model}{the rf (parsnip) model}

\item{logit_model}{the logistic regression (parsnip) model}
}
\value{
a vector of predicted probabilities
}
\description{
stepwise_target_prob_predict Make calibrated predictions from rf + Platt scaling
}
