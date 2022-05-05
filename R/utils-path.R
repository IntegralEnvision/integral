# History
# 2022-05-04. Created Eben Pendleton.

#' @title Utility Windows function
#' @name ask
#' @description A function to ask a yes/no question and return TRUE/FALSE
#' `r lifecycle::badge('stable')`
#' @return boolean response from Yes / No
#'
#' @param question input Yes / No question
#' @param default Optionally set the default response value

#' @examples
#' \dontrun{
#' convert_winpath(<path>)
#' )
#' @export
# from renv. MIT License
convert_winpath <- function(x) {
  x <- gsub('[\"]', '', x)
  x <- gsub("\\\\", "/", x)
  return(x)
}
