#' mapeIter
#'
#' @param forecasts Vector or dataframe of forecasts
#' @param data Dataframe of data to compare forecasts to
#' @param last Optional numeric, index of last point in data to build model, denotes where to start comparisons
#'
#'
#'
#' @importFrom MLmetrics 'MAPE'

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
