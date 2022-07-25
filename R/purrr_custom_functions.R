#' Remove a named list element
#' @description
#' `r lifecycle::badge('experimental')`
#' Opposite of `purrr::pluck()`.  Returns the original list without the element.
#' @param  .data a named list
#' @param ... Unquoted variable names to search for duplicates. This takes a tidyselect specification (starts_with, contains, ends_with, etc).
#' @examples
#' obj1 <- list("a", list(1, elt = "foo"))
#' obj2 <- list("b", list(2, elt = "bar"))
#' x <- list(a = obj1, b = obj2)
#' x %>% huck(a)
#' x %>% huck(starts_with("a"))
#' @export

huck <- function(.data, ...) {
  vars <- rlang::names2(.data)
  vars <- tidyselect::vars_select(vars, ...)
  within(.data, rm(list = vars))

}
