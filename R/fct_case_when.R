#' Factor case_when
#'
#' @param ... A sequence of two-sided formulas consistent with dplyr::case_when
#'
#' @return A vector factor
#' @export

fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}
