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
