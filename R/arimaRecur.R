#' arimaRecur
#'
#' @param data Data to build the model from
#' @param order Optional 3-tuple specifiying order for the arima model
#' @param mean Optional boolean indicating whether to include a mean for the arima model
#' @param steps Numeric, how many steps ahead to forecast
#' @param start Numeric, where to start forecasting from in the data
#'
#'
#' @examples
#'  arimaRecur(data, c(2,0,0), steps = 12, start = 100)
#'
#' @importFrom forecast 'Arima'
#'
#' @export

arimaRecur <- function(data, order = c(1, 0, 1), mean = FALSE, steps, start) {
  results <- data
  for (i in seq(1, steps)) {
    #update the next row in the dataframe with the forecast
    #reestimate the model with the extra row, repeat iteratively
    model <- Arima(results[1:(start+i-1),], order, include.mean = mean)
    results[start + i] <- forecast(model, h=1)$mean[1]
  }
  return (results)
}
