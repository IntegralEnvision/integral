#' Remove a named list element
#' @description
#' `r lifecycle::badge('experimental')`
#' Opposite of `purrr::pluck()`.  Returns the original list without the element.
#' @param ... Unquoted variable names to search for duplicates. This takes a tidyselect specification (starts_with, contains, ends_with, etc).
#' @examples
#' x <- lst(a = mtcars, b = diamonds, aaa = mtcars)
#' x %>% huck(a)
#' x %>% huck(starts_with("a"))
#' @export

huck <- function(.data, ...) {
  vars <- rlang::names2(.data)
  vars <- tidyselect::vars_select(vars, ...)
  within(.data, rm(list = vars))

}
