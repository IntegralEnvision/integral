#' Convert raw comma separated values into a vector
#' @description
#' `r lifecycle::badge('experimental')`
#' Converts a series of comma-separated values into vector input format for R.
#' (Note, this is identical to Hmisc::Cs).
#' @param ... any number of names separated by commas
#' @return character string vector
#' @examples
#' \dontrun{
#' quote_values(a, b, c, d, e, f)
#' }
#' @export
quote_values <- function(...) {
  as.character(sys.call())[-1]
}


#' Write Clip shortcut
#' @description
#' `r lifecycle::badge('stable')`
#' Copies an object to the clipboard. Defaults to .Last.value
#' @export
wc <- function(x = .Last.value) {
  clipr::write_clip(x)
  cli::cli_alert_success("Value copied to clipboard")
}

#' Interactive filter helper
#' @description
#' `r lifecycle::badge('stable')`
#' This is a shortcut for interactive data exploration that makes it easy to
#' filter rows that match a value in the key column of a tibble.
#'
#'
#' It produces the same result as filter(your_key_col == key_value). However it
#' has the following advantages:
#'
#' \itemize{
#' \item Less typing
#' \item Saves the key value to a hidden environment
#' variable, so that calling it subsequently (perhaps on a different table) it
#' is not necessary to specify the key value. }
#'
#' @section Motivation: When working with a set of tables that have an in-common
#'   id column (for example "unitid" in IPEDS or offender number in UDC data) it
#'   is often the case that when I find an individual with some issue in one
#'   table, I then want to see what the data for that individual is in another
#'   table. Rather than constantly having to use filter() with a
#'   matching argument, this function lets you just specify the id value on its
#'   own. And because the goal is often to look up an id value in one table and
#'   then immediately look it up in the other table, this function also saves
#'   whatever the last id value was so that it isn't necessary to specify it on
#'   subsequent function calls.
#'
#' @section Warning: This function should not be used in non-interactive (ie in
#'   console) or non-diagnostic script code (i.e. anywhere that an object is
#'   written vs just printed).
#'
#' @usage options(idf_data_key = "your_data_key_column")
#'
#' df \%>\% idf(id = .last_id)
#'
#' @param id A vector containing the key value(s) you want to filter. After the first time the function
#'   is called with a specified key value, the value is stored in a hidden
#'   object and does not need to be specified again.
#' @param glimpse Print output using dplyr::glimpse, defaults to FALSE.
#' @return The data filtered by the key value on the key column stored in options("idf_data_key")
#' @export
idf <- function(.data, id = .last_id, glimpse = F) {

  .last_id <<- id

  key <- getOption("idf_data_key")

  if(is.null(key)) {
    cli::cli_alert_danger("{.val idf_data_key} is not set.")
    key <- readline(prompt = "Please enter the column name: ") %>%
      stringr::str_remove_all(., pattern = "\"|\'")

    options("idf_data_key" = key)
    cli::cli_alert_success("{.val idf_data_key} set to {.val {key}}.")
    cli::cli_alert_info("You can avoid this prompt by setting {.code options(idf_data_key = \"{key}\")} at the start of a script.")
  }

  key <- rlang::sym(key)

  result <- .data %>%
    dplyr::filter(!!key %in% !!id)

  if(glimpse) result %>% dplyr::glimpse()
  else result

}



#' common_vars
#' @description
#' `r lifecycle::badge('experimental')`
#' Shows the variable names that are in common between two or more tibbles.
#' Considering deprecating this as janitor::compare_df_cols() does something similar.
#' @param ... Bare, unquoted tibble object names.
#' @export
common_vars <- function(...) {

  objects <- lst(...)

  if(length(objects) < 2) stop("At least two objects required.")

  varnames <- map(objects, function(var) {
    enframe(names(var), name = NULL, value = "name")
  }) %>% bind_rows(.id = "object")

  varnames %>%
    add_column(present = T) %>%
    complete(name, object) %>%
    pivot_wider(names_from = object, values_from = present)
}


#' View selected data frame
#'
#' The RStudio Environment pane variable name column is too narrow for long
#' names, so it can be difficult to find the right data frame in long list of similar
#' names. Select the variable and use shortcut to use data viewer on it.
#'
#' Stolen from: https://github.com/dracodoc/mischelper/blob/master/R/misc.R
#' @export

view_df <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  selection_start <- context$selection[[1]]$range$start
  selection_end <- context$selection[[1]]$range$end
  if (any(selection_start != selection_end)) { # text selected
    selected <- context$selection[[1]]$text
    # this will show "View(get(selected))" in viewer, not optimal
    # View(get(selected))
    formated <- stringr::str_c("View(", selected, ')')
    rstudioapi::sendToConsole(formated, execute = TRUE)
  }
}

