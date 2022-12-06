#' Trim strings with repeated line breaks
#' @description
#' `r lifecycle::badge('experimental')`
#' Removes line breaks  (\\n and \\r) from the start and end of a string and
#' repeated line breaks inside a string.
#'
#' This is an analogue of stringr::str_squish(), which trims white space from
#' the start and end of a string and removes repeated whitespace inside a
#' string. However stringr::str_squish() will replace line breaks inside a
#' string with a space, which is not always desired.
#'
#' Note that if there is a repeat of two different line break characters, i.e. a newline (\\n)
#' and line return (\\r), the first occurrence will be kept.
#'
#' See: https://github.com/tidyverse/stringr/issues/477
#'
#' @param string String to squish
#'
#' @return String with repeeated line returns and whitespaces removed
#' @export
#'
#' @examples
#' stringr::str_squish("hello\nworld")
#' stringr::str_squish("hello\n\nworld")
#' str_squish_line_breaks("hello\nworld")
#' str_squish_line_breaks("hello\n\nworld")

str_squish_line_breaks <- function(string) {
  stringr::str_replace_all(string, "(\\s)(\\s)(\\1\\2)+", "\\1\\2") %>%
    stringr::str_replace_all("(\\s)\\1+", "\\1") %>%
    stringr::str_trim()
}
