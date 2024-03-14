
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

#' Ordered Factor case_match()
#' @description
#' `r lifecycle::badge('experimental')`
#' Can replace `case_match()` syntax to output an ordered factor in the same order as the cases, useful for meaningful ordering in plots and tables.  This is because for `case_match()` the arguments are evaluated in order, so you must proceed from the most specific to the most general. Tables and plots will therefor be ordered by the evaluation order.
#' @param .x 	A vector to match against.
#' @param ... <dynamic-dots> A sequence of two-sided formulas: old_values ~ new_value. The right hand side (RHS) determines the output value for all values of .x that match the left hand side (LHS). See ?case_when for details.
#' The LHS must evaluate to the same type of vector as .x. It can be any length, allowing you to map multiple .x values to the same RHS value. If a value is repeated in the LHS, i.e. a value in .x matches to multiple cases, the first match is used.
#' The RHS inputs will be coerced to their common type. Each RHS input will be recycled to the size of .x.
#' @param .default The value used when values in .x aren't matched by any of the LHS inputs. If NULL, the default, a missing value will be used.
#' .default is recycled to the size of .x.
#' @param .ptype An optional prototype declaring the desired output type. If not supplied, the output type will be taken from the common type of all RHS inputs and .default.
#' @return An ordered factor vector with the same size as .x
#' @importFrom magrittr "%>%"
#' @export


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
