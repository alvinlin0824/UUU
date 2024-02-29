#' Fix Round Half to Even
#'
#' @param x A Numeric Vector
#' @param digits Integer Indicating the Number of Decimal Places
#'
#' @return A Numeric Vector
#' @export
#'
#' @examples
#' x <- c(0.125,0.215)
#' round_normal(x,2)

round_normal <- function(x, digits){
  ifelse(x >= 0,round(x + 10^-(digits + 3),digits),round(x - 10^-(digits + 3),digits))
}
