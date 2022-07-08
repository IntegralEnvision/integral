#' Open cheatsheet
#' @description
#' `r lifecycle::badge('experimental')`
#' Shortcut to open a cheatsheet
#' @param sheet Name of the cheatsheet, if known. Partial matches work as well. If NULL (default) a list is presented.
#' @export
cheat <- function(sheet = NULL) {
  files <- dir(system.file("extdata/cheatsheets", package = "integral")) %>%
    tools::file_path_sans_ext()

  if (!methods::hasArg(sheet)) { # No sheet specified; full listing
    cli::cli_alert_info("Please select cheatsheet:")
    sheet <- utils::select.list(choices = files)
  } else if (any(stringr::str_detect(files, sheet))) { # Matches
    matches <- files[stringr::str_detect(files, sheet)]
    if (length(matches) == 1) {
      sheet <- matches
    } else {
      cli::cli_alert_info("Multiple matching cheatsheets, select one:")
      sheet <- utils::select.list(choices = matches)
    }
  } else {
    cli::cli_alert_warning("No matching cheatsheets. Select from the list below:")
    sheet <- utils::select.list(choices = files)
  }


  sheet <- paste0(sheet, ".pdf")

  fpath <- system.file("extdata/cheatsheets", sheet, package = "integral")
  system2("open", fpath)
}
