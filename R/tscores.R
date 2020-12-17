#' tscores
#'
#' @description Returns tscores for all coefficients of an Arima model object
#'
#' @param model Fitted model from an Arima() call
#'
#' @export

tscores <- function(model) {
  for (i in seq(1, length(model$coef))) {
    se <- sqrt(model$var.coef[i, i])
    t_score <-  model$coef[i]/se
    name <- names(model$coef)[i]
  }
  return (cbind(name, t_score))
}
