#' tscores
#'
#' @param model Fitted model

tscores <- function(model) {
  for (i in seq(1, length(model$coef))) {
    se <- sqrt(model$var.coef[i, i])
    t_score <-  model$coef[i]/se
    name <- names(model$coef)[i]
  }
  return (cbind(name, t_score))
}
