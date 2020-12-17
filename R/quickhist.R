#' quickhist
#'
#' @importFrom magrittr '%>%'
#'
#' @param data Structured dataframe
#' @param factor Optional argument, factor variable for grouped histograms
#'
#' @return No return value, prints histograms for each variable in the dataframe
#'
#' @export

quickhist <- function(data, factor = NA) {
  if(factor) {
    data %>% gather(key, value, -factor) %>%
      ggplot(aes(value, fill = factor)) +
      facet_wrap(~key, scales = 'free') +
      geom_histogram()
  }
  else {
    data %>% gather(key, value) %>%
      ggplot(aes(value)) +
      facet_wrap(~key, scales = 'free') +
      geom_histogram()
  }
}
