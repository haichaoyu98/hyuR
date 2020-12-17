#' cointTest
#'
#' @param t1 String, ticker for stock 1
#' @param t2 String, ticker for stock 2
#' @param T Optional numeric, identifies how many days backward to include in the rolling window
#' @param prices Optional boolean for outputting the prices to a graph
#'
#'
#' @importFrom quantmod 'getSymbols'
#'
#' @export

cointTest <- function (t1, t2, T = 100,
                                 prices = FALSE) {

  #Tickers for Stocks
  t1data <- getSymbols(t1)
  t2data <- getSymbols(t2)
  #Get adjusted prices
  t1colname <- paste0(t1, '.Adjusted')
  t2colname <- paste0(t2, '.Adjusted')
  t1adj <- unclass(t1data[,t1colname])
  t2adj <- unclass(t2data[,t2colname])

  #Plot of Prices
  if (prices) {
    plot(t1adj, type="l", ylim=c(5.0, max(t2adj, t1adj) + 50),
         ylab="Backward-Adjusted Stock Price in USD", col="red")
    par(new=T)
    lines(t2adj, type="l",  xlab="", ylab="", col="green")
    par(new=F)
    legend(1, 1500,legend=c(t1, t2),
           col=c("red", "green"), lty=c(1,1), cex=0.8)
  }

  #ADF Test for Cointegration
  comb = lm(t1adj ~ t2adj)
  out <- summary(comb)
  #Rename coefficients in summary output
  rownames(out$coef) <- c('Mean', 'Hedge ratio')
  print(out$coef)
  print('ADF Test for Cointegration')
  adf.test(comb$residuals)

  obs = nrow(t1adj)
  Z_score = 1.96
  spread = data.frame(t1adj - out$coef[2] * t2adj)
  colnames(spread) = "Spread"
  avg = mean(spread$Spread)
  sd = sd(spread$Spread)
  spread$mean = rep(avg, obs)
  spread$lower_bound = rep(avg -( Z_score * sd ), length(spread$Spread))
  spread$higher_bound = rep(avg + ( Z_score * sd ), length(spread$Spread))

  t1adj = as.data.frame(t1adj)
  t2adj = as.data.frame(t2adj)
  rolling = na.omit(cbind(t1adj, t2adj))

  # Parameters
  Z_score = 1.96              # Amount of standard deviations to create bounds
  percent_cointegration = .05 # Percent we reject on the t-test
  EMA = .1/T                  # Smaller value creates a less reactive mean
  obs = nrow(rolling)         # Number of observations

  # Initialize empty columns of a dataframe
  rolling$hedge_ratio = NA
  rolling$Spread = NA
  rolling$mean = 1
  rolling$sd = NA
  #rolling$Cointegrated = NA  #initialize this line and line 94 to see the rolling ADF test

  #t is todays observation
  for (t in T:obs){

    reg = lm(rolling[t1colname][(t-T):t,1]~rolling[t2colname][(t-T):t,1])
    rolling$hedge_ratio[t] = reg$coefficients[2]
    rolling$Spread[t] = rolling[t1colname][t,1] -
      rolling$hedge_ratio[t] * rolling[t2colname][t,1]

    rolling$mean[t] = EMA * (rolling$Spread[t]) +
      ((1-EMA) *rolling$mean[(t-1)])
    rolling$sd[t] = sd(rolling$Spread[(t-T):t])
    #rolling$Cointegrated[t] = pass_DF_test(reg$residuals, percent_cointegration)

  }

  rolling = na.omit(rolling)
  rolling$lower_bound = rolling$mean - ( Z_score *  rolling$sd )
  rolling$higher_bound = rolling$mean + ( Z_score *  rolling$sd )

  plot(rolling$Spread,
       main=paste(t1, t2, 'Mean Reversion'),
       ylab="Spread",
       type="l",
       col="black")
  lines(rolling$mean, lty = "dashed", col = "red" )
  lines(rolling$lower_bound, lty = "dotted", col = "green")
  lines(rolling$higher_bound, lty = "dotted", col = "green")
  legend("topleft",
         c("Spread","Mean","Mean +- x SD"),
         fill=c("black","red", "green"))

  return (rolling)
}
