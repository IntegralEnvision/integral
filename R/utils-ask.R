# History
# 2022-05-04. Created Eben Pendleton.

#' Returns TRUE or FALSE from Y/n or y/N prompt.
#' @param question input Yes / No question
#' @param default Optionally set the default response value
#' @return boolean response from Yes / No
#' @examples
#' ask("Do you like coding?")
#' @export

# from renv. MIT License
ask <- function(question, default = TRUE) {
  selection <- if (default) "[Y/n]" else "[y/N]"
  prompt <- sprintf("%s %s: ", question, selection)
  response <- tolower(trimws(readline(prompt)))
# if no response is entered return the default boolean value
if (!nzchar(response)) return(default)
  substring(response, 1L, 1L) == "y"
}
