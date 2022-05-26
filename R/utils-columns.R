#' @title convert letter to column number
#' @name letnum
#' @description Converts an upper / lowercase letter into the resulting column
#' number. Starts at 1. Limited to current Excel maximum of XFD (16,384 columns)
#' `r lifecycle::badge('stable')`
#' @return Column number as integer
#'
#' @param x A letter to convert to a column number starting with 1.
#'
#' @examples
#' letnum("A")
#'
#' History
#' 2022-05 Eben Pendleton written.

letnum <- function(x) {

  # Current max column reference is XFD
  maxcol <- "XFD"
  # Generate 2 letter combinations
  l2 <-  sort(c(outer(LETTERS,LETTERS,FUN=paste0)))

  # Generate 3 combinations up to the current Excel max
  l3 <- sort(c(outer(c(outer(LETTERS,LETTERS,FUN=paste0)), LETTERS,FUN=paste0)))
  l3 <- l3[1:which(l3 %in% maxcol)]
  # combine all possible letter combination
  lets <- c(LETTERS, l2, l3)

  each_let <-function(x) {
    # see if we match a letter
    result <- which(lets %in% toupper(x))
    # if there is no letter match integer(0) is returned. Check and provide an
    # error
    if (identical(result, integer(0))) {
      cli::cli_alert_danger(paste0("Error in letnum(): value not known: ",x
                                   , "\nNA used in replacement"))
      return(NA)
    }
    # explicitly return our result
    return(result)
  }
  # try and return a number match for each letter
  return(sapply(x, each_let, USE.NAMES = F))

}
