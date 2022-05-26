
index_identical_rows <- function(.data, column, startRow = 1) {
  column <- rlang::enquo(column)

  .data %>%
    dplyr::mutate(start = (!!column != dplyr::lag(!!column) | is.na(dplyr::lag(!!column))) & !is.na(!!column),
           end = (!!column != dplyr::lead(!!column) | is.na(dplyr::lead(!!column))) & !is.na(!!column)) %>%
    dplyr::summarize(start = which(start),
              end = which(end)) %>%
    dplyr::filter(start != end) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~ . + startRow - 1)) %>%
    array_tree(margin = 1)
}

