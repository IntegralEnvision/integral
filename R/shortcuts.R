#' Open cheatsheet
#' @description
#' `r lifecycle::badge('experimental')`
#' Shortcut to open a cheatsheet
#' @param sheet Name of the cheatsheet, if known. If NULL (default) a list is presented.
#' @export
cheat <- function(sheet = NULL) {

  if (!methods::hasArg(fpath)) {
    cli::cli_alert_info("Please select cheetsheet:")
    sheet <- utils::select.list(choices = dir(system.file("extdata/cheatsheets", package = "integral")))
  }

  fpath <- system.file("extdata/cheatsheets", sheet, package = "integral")
  system2("open", fpath)

}
