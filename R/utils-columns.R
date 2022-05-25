#' @title convert letter to column number
#' @name letnum
#' @description Converts an upper / lowercase letter into the resulting column
#' number. Starts at 1.
#' `r lifecycle::badge('stable')`
#' @return Column number as integer
#'
#' @param x A letter to convert to a column number starting with 1.
#'
#' @examples
#' letnum("A")
#'
letnum <- function(x) {
  if (any(nchar(x) > 1, length(x) > 1, x == "")) {
    return(cli::cli_alert_danger("Error in letnum(): values must be single letters"))
  }
  which(LETTERS %in% toupper(x))
}
