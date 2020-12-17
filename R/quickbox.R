#' quickbox
#'
#' @param data Structured dataframe
#'
#' @importFrom magrittr '%>%'
#' @importFrom purrr 'keep'
#'

quickbox <- function (data) {
    data %>% keep(is.numeric) %>%
      gather(key, value) %>%
      ggplot(aes(value)) +
      facet_wrap(~key, scales = 'free') +
      geom_boxplot()
  }
