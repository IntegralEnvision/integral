
index_identical_rows <- function(.data, column, startRow = 1) {
  column <- rlang::enquo(column)

  .data %>%
    dplyr::mutate(
      start = (!!column != dplyr::lag(!!column) | is.na(dplyr::lag(!!column))) & !is.na(!!column),
      end = (!!column != dplyr::lead(!!column) | is.na(dplyr::lead(!!column))) & !is.na(!!column)
    ) %>%
    dplyr::summarize(
      start = which(start),
      end = which(end)
    ) %>%
    dplyr::filter(start != end) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~ . + startRow - 1)) %>%
    purrr::array_tree(margin = 1)
}

oxl_named_region_to_rowcol <- function(wb, sheet, named_region) {
  x <- openxlsx::getNamedRegions(wb)

  # TODO: Add checking that sheet exists.

  # TODO: Is there a way to make this directly plug in the rows and cols for openxlsx functions, so that rather than doing the extra work of extracting the sequences and having to call it twice, it can just set the rows and cols values in the function call?

  if (named_region %ni% x) {
    return({
      cli::cli_alert_danger("A region named {named_region} does not exist in sheet \"{sheet}\".")
      cli::cli_alert_info("The named regions in {sheet} are {crayon::bold(x)}")
    })
  }

  z <- tibble::enframe(x, name = NULL, value = "region") %>%
    dplyr::bind_cols(attributes(x) %>% tibble::as_tibble()) %>%
    dplyr::filter(sheet == !!sheet)

  z %>%
    tidyr::separate(position, into = c("from", "to"), sep = ":") %>%
    dplyr::mutate(
      fromcol = stringr::str_extract(from, "[A-Z]+"),
      tocol = stringr::str_extract(to, "[A-Z]+")
    ) %>%
    dplyr::mutate(
      fromrow = stringr::str_extract(from, "[0-9]+"),
      torow = stringr::str_extract(to, "[0-9]+")
    ) %>%
    dplyr::mutate(
      fromcol = letnum(fromcol),
      tocol = letnum(tocol)
    )
}
