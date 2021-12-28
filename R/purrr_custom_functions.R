#' Remove a named list element
#' @description
#' `r lifecycle::badge('experimental')`
#' Opposite of `purrr::pluck()`.  Returns the original list without the element.
#' @param element_name Name of the element to remove
#' @export

huck <- function(.x, element) {
  discard(.x, names(.x) == element)
}
