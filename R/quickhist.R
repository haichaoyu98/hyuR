#' quickhist
#'
#' @importFrom magrittr '%>%'
#'
#' @param data Structured dataframe
#'
#' @return No return value, prints histograms for each variable in the dataframe
#'
#' @export

quickhist <- function(data) {
  data %>% keep(is.numeric()) %>% gather(key, value) %>%
      ggplot(aes(value)) +
      facet_wrap(~key, scales = 'free') +
      geom_histogram()
}
