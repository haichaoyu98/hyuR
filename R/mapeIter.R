#' mapeIter
#'
#' @description Returns the mean absolute percentage error for each iteration of a forecast, works with forecasts that are multiple steps ahead
#'
#' @param forecasts Vector or dataframe of forecasts, rows should be iterations while columns should be steps forecasted at each iteration
#' @param data Dataframe of data to compare forecasts to
#' @param last Optional numeric, index of last point in data to build model, denotes where to start comparisons
#'
#' @examples
#'
#' @importFrom MLmetrics 'MAPE'
#'
#' @export

mapeIter <- function(forecasts, data, last = 1) {
  mape <- forecasts[,1]
  for (i in seq(length(mape))) {
    pred <- forecasts[i,]
    pred <- pred[pred!=0]
    actual <- data[(last+i):(last+i+length(pred)-1)]
    mape[i] <- MAPE(pred, actual)
  }
  return (mape)
}
