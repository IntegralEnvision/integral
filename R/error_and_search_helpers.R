#' Search Stack Overflow for your most recent error message.
#' @description
#' `r lifecycle::badge('experimental')`
#' @export
so_last_error <- function() {
  last_error <- geterrmessage()
  if (is.null(last_error)) {
    stop("No error message available")
  }

  query <- utils::URLencode(paste("[r]", last_error))
  browseURL(paste0("http://stackoverflow.com/search?q=", query))
}


#' Search Google for your most recent error message.
#' @description
#' `r lifecycle::badge('experimental')`
#' @export
google_last_error <- function() {
  last_error <- geterrmessage()
  if (is.null(last_error)) {
    stop("No error message available")
  }
  query <- utils::URLencode(paste('tbs=qdr:y&q="R"+', last_error, "+-site%3Ardocumentation.org -site%3Ardrr.io"))
  utils::browseURL(paste0("https://www.google.com/search?", query))
}



#' Search Stackoverflow
#' @description Improve your workflow by searching Stackoverflow directly from R console.
#' `r lifecycle::badge('experimental')`
#' @param search_terms Search terms encapsulated in " ".
#' @keywords web workflow stackoverflow
#' @examples
#' stackoverflow("r date conversion")
#' so("r ggplot2 geom_smooth()")

#' @export
stackoverflow <- function(search_terms) {
  if (missing(search_terms)) {
    cli::cli_alert_warning("No search terms passed. Opening Stackoverflow in browser.")
    query <- utils::URLencode("[r]")
    utils::browseURL(paste0("https://stackoverflow.com?q=", query))
  }
  else {
    cli::cli_alert_info("Opening Stackoverflow search for \"", search_terms, "\" in browser")
    query <- utils::URLencode(paste("[r]", search_terms))
    utils::browseURL(paste0("https://stackoverflow.com/search?q=", query))
  }
}

#' @export
#' @rdname stackoverflow
so <- stackoverflow

#' Search Google for R related results
#' @description Improve your workflow by searching Stackoverflow directly from R console.
#' `r lifecycle::badge('experimental')`
#' The required term "R" is automatically included.
#' Note: Prevents results from sites that show the same info as the help documentation within R.
#' @param search_terms Search terms encapsulated in " ".
#' @keywords web workflow stackoverflow
#' @examples
#' google("date conversion")

#' @export
google <- function(search_terms) {
  if (missing(search_terms)) {
    return(cli::cli_alert_error("No search terms passed."))
  }
  else {
    cli::cli_alert_info(paste0("Opening Google search for \"", search_terms, "\" in browser"))
    query <- utils::URLencode(paste('tbs=qdr:y&q="R"+', search_terms, "+-site%3Ardocumentation.org -site%3Ardrr.io "))
    utils::browseURL(paste0("https://www.google.com/search?", query))
  }
}

