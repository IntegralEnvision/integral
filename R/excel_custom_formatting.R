#' Read Excel workbook(s) and return nested list with all workbooks and worksheets
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @usage read_wb(fpath, fname = NULL, sheet = NULL)
#'
#' @param fpath File path containing Excel workbooks
#' @param fname File name(s) of workbooks
#' @param sheet Sheet index to extract
#' @export
#'

# library(dplyr)
# library(purrr)
# library(readxl)
# library(tidyr)
# library(tibble)
# library(stringr)

# reads all workbooks and worksheets in directory
read_wb <- function(fpath, fname = NULL, sheet = NULL) {

  # get filenames
  fnames <- dir(fpath, pattern = ".*(\\.xls|\\.xlsx)$", full.names = TRUE) %>%
    {if(!is.null(fname)) .[basename(.) %in% fname] else .}

  # read workbook
  purrr::map(
    fnames,
    ~ purrr::map(
      1:length(readxl::excel_sheets(.x)) %>%
        purrr::set_names(readxl::excel_sheets(.x)), # n worksheets
      ~ readxl::read_excel(.y, sheet = .x, col_names = FALSE),
      .y=.x
    ) %>%
      {if(!is.null(sheet)) .[[sheet]] else .}
  ) %>% purrr::set_names(basename(tools::file_path_sans_ext(fnames))) %>%
    {if(!is.null(fname) & length(fname) == 1) purrr::pluck(., 1) else .}

}

#' Get list of breaks to be used as drows argument in `to_fff`
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @usage get_breaks(dat, col_num = 1, rm_grps = 1)
#'
#' @param dat Unformatted dataframe containing crosstabbed data.
#' @param col_num Numeric value specifying column number to use for identifying breaks
#' @param rem_grps Numeric vector specifying group indices to remove (defaults to 1 for headers)
#' @export

# gets list of breaks for groups
get_breaks <- function(dat, col_num = 1, rm_grps = 1) {

  # set variables
  drows <- list()
  idx <- 1

  # indices of non-NA cells within col
  temp <- dat[,col_num] %>%
    purrr::set_names("x") %>%
    tibble::rowid_to_column() %>%
    dplyr::filter(!is.na(x)) %>%
    dplyr::select(-x) %>%
    dplyr::pull()

  # append list of vectors specifying rows w/ data
  for (i in 2:length(temp)) {
    if(temp[i] != temp[i-1]+1) {
      drows <- drows %>%
        append(list(idx:temp[i-1]))
      idx <- temp[i]
    } else if(i == length(temp)) {
      drows <- drows %>%
        append(list(idx:temp[i]))
    }
  }

  # return without specified groups (if rm_grps supplied)
  if (!is.null(rm_grps)) {
    return(drows[-rm_grps])
  } else {
    return(drows)
  }

}

#' Generate flatfile from crosstabbed (pivoted) data stored in an Excel spreadsheet.
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @source Some of the syntax and inspiration for this function comes from the
#'  python library `un-xtab` \url{https://un-xtab.readthedocs.io/}
#' @usage to_fff(dat, my_vals, my_cols, my_hrow, hrows, drows = NULL, dcols = NULL, to_fill = NULL, gname = NULL, hdate = NULL)
#'
#' @param dat Unformatted dataframe containing crosstabbed data.
#' @param my_vals Character or numeric vector specifying the data columns to be preserved (see `row_headers`). Supplying a named vector will add column names.
#' @param my_cols Either: a) Character vector with names of repeated columns (e.g. `c("result", "qualifier")`); or b) Numeric value specifying (see `column_group_count`)
#' @param my_hrow Numeric value specifying header row to get names for vals/cols (should not be included in `hrows`; see `row_headers_row`)
#' @param hrows Numeric vector specifying the rows containing column headers (see `column_header_rows`). Supplying a named vector will add column names.
#' @param drows Numeric vector or list of numeric vectors specifying the rows containing data (see `data_rows`). Supplying a named list will add column for group name (arg `gname`).
#' @param dcols Numeric vector specifying the columns containing data (see `data_columns`)
#' @param to_fill Character or numeric vector specifying header rows to fill down (e.g. merged cells)
#' @param gname Character value specifying the name of grouped data column
#' @param hdate Character value specifying the name of a header column to transform to date
#' @export
#'
#' @examples
#' to_fff(dat, my_vals = 1:2, my_cols = 2, hrows = 2:5)
#' to_fff(dat, my_vals = c(analyte = 1, units = 2), my_cols = c('result', 'qualifiers'), hrows = c(location_id = 2, sample_depth = 3, sample_date = 4, lab_id = 5), drows = drows, dcols = 3:198, gname = 'analyte_group', hdate = 'sample_date')

# to flatfile format
to_fff <- function(dat, my_vals, my_cols, my_hrow = NULL, hrows, drows = NULL, dcols = NULL, to_fill = NULL, gname = NULL, hdate = NULL) {

  # set all columns as char
  my_dat <- dat %>%
    dplyr::mutate(dplyr::across(tidyr::everything(), ~as.character(.x)))

  # set drows if not supplied
  if (is.null(drows)) {
    temp <- get_breaks(dat, col = 1, rm_grps = 1)
    drows <- seq(dplyr::last(hrows)+2, dplyr::last(temp[[length(temp)]]), 1)
  }

  # set dcols to all columns
  if (is.null(dcols)) {
    dcols <- seq(length(my_vals)+1, ncol(my_dat), 1)
  }

  # set names for my_cols
  if (is.numeric(my_cols)) {
    my_cols <- my_dat[{if(is.null(my_hrow)) dplyr::last(hrows)+1 else my_hrow},dcols[1:my_cols]] %>%
      unlist() %>%
      janitor::make_clean_names()
  }

  # number of samples
  n <- length(dcols)/length(my_cols)

  # get header names
  if (is.null(names(hrows))) {
    hrows <- my_dat[hrows,my_vals[1]] %>% # defaults to first value column
      dplyr::pull() %>%
      janitor::make_clean_names() %>%
      {purrr::set_names(x = hrows, nm = .)}
  }

  # get value names
  if (is.null(names(my_vals))) {
    my_vals <- my_dat[{if(is.null(my_hrow)) dplyr::last(hrows)+1 else my_hrow},my_vals] %>%
      unlist() %>%
      janitor::make_clean_names() %>%
      {purrr::set_names(x = my_vals, nm = .)}
  }

  # set group names
  if (is.null(gname) & purrr::is_list(drows)) {
    gname = "group"
  }

  # extract headers
  headers <- my_dat %>%
    tibble::as_tibble() %>%
    dplyr::slice(hrows) %>%
    dplyr::select(seq(dcols[1], dplyr::last(dcols), length(my_cols))) %>%
    dplyr::bind_cols(id = names(hrows)) %>%
    tidyr::pivot_longer(cols = -id) %>%
    tidyr::pivot_wider(names_from = id, values_from = value) %>%
    { if(!is.null(to_fill)) tidyr::fill(., to_fill) else . } %>%
    dplyr::mutate(name = stringr::str_remove(name, "...")) %>%
    { if(!is.null(hdate)) dplyr::mutate(., !!as.name(hdate) := janitor::excel_numeric_to_date(as.numeric(!!as.name(hdate)))) else . }

  # extract body
  body <- purrr::map_df(
    drows,
    ~my_dat %>%
      dplyr::slice(.x) %>%
      dplyr::select(c(my_vals, dcols)) %>%
      purrr::set_names(c(names(my_vals), paste0(rep(my_cols, n), "_", rep(1:n, each = length(my_cols))))) %>%
      tidyr::pivot_longer(cols = -my_vals, names_sep = "_", names_to = c(".value", "name")) %>%
      {if(!is.null(gname)) dplyr::mutate(., !!as.name(gname) := my_dat[.x[1]-1,1] %>% dplyr::pull()) else .}
  )

  # combine body and headers
  body %>%
    dplyr::left_join(headers) %>%
    dplyr::select(-name)
}

