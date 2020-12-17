#' quickbox
#'
#' @description Outputs boxplots for all numeric variables in a dataset, good for preliminary descriptive analysi
#'
#' @param data Structured dataframe
#'
#' @importFrom magrittr '%>%'
#' @importFrom purrr 'keep'
#'
#' @export

quickbox <- function (data) {
    data %>% keep(is.numeric) %>%
      gather(key, value) %>%
      ggplot(aes(value)) +
      facet_wrap(~key, scales = 'free') +
      geom_boxplot()
  }
