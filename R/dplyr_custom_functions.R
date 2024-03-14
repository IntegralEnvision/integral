
#' Tibble Preview
#' @description
#' `r lifecycle::badge('experimental')`
#' Show a sample of all tibble data without hiding columns.
#' @param .data a tibble or data frame (default: .Last.value)
#' @param rows number of rows to print (default: 10)
#' @importFrom magrittr "%>%"
#' @return A preview of a tibble.
#' @export
tp <- function(.data = .Last.value, rows = 10) {
  .data <- dplyr::sample_n(.data, size = rows)
  print(.data, n = Inf, width = Inf)
}

#' Ordered Factor case_when()
#' @description
#' `r lifecycle::badge('experimental')`
#' Can replace `case_when()` syntax to output an ordered factor in the same order as the cases, useful for meaningful ordering in plots and tables.  This is because for `case_when()` the arguments are evaluated in order, so you must proceed from the most specific to the most general. Tables and plots will therefor be ordered by the evaluation order.
#' @param ... A sequence of two-sided formulas. See ?dplyr::case_when for details
#' @return An ordered factor vector of length 1 or n, matching the length of the logical input or output vectors, with the type (and attributes) of the first RHS. Inconsistent lengths or types will generate an error.
#' @importFrom magrittr "%>%"
#' @export
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]]) # extract RHS of formula
  levels <- levels[!is.na(levels)]
  ordered(dplyr::case_when(...), levels = levels)
}


#' Remove variables from tibble
#' @description
#' `r lifecycle::badge('stable')`
#' This is a simple negation of `dplyr::select`.
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr). See Methods, below, for more details.
#' @param ... \<tidy-select\> One or more unquoted expressions separated by commas. Variable names can be used as if they were positions in the data frame, so expressions like x:y can be used to select a range of variables.
#' @return An object of the same type as .data, with the specified columns removed.
#' @importFrom magrittr "%>%"
#' @export
deselect <- function(.data, ...) {
  dplyr::select(.data, -c(...))
}

#' Slice head and tail
#' @description
#' `r lifecycle::badge('experimental')`
#' This is a modified version of dplyr::slice_* that allows slicing n rows from both head and tail.
#' @param .data A data frame, or data frame extension (e.g. a tibble).
#' @param n The number of rows to slice from both head and tail (e.g. n = 5 means 5 from head and 5 from tail). Defaults to 5.
#' @param prop asdf
#' @importFrom magrittr "%>%"
#' @export
slice_ht <- function(.data, n, prop) {
  data_head <- dplyr::slice_head(.data, n = n, prop = prop)
  data_tail <- dplyr::slice_tail(.data, n = n, prop = prop)
  data_ht <- bind_rows(data_head, data_tail)
  return(data_ht)
}

