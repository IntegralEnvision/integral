#' Update the integral package
#' @description
#' `r lifecycle::badge('experimental')`
#' Automatically unloads, updates, and reloads the integral package.
#' @export
ic_update <- function() {
  old_version <- packageVersion("integral")

  if(remotes:::local_sha("integral") == remotes:::remote_sha(structure(remotes:::package2remote("integral"), class = "github_remote"))) return(cli::cli_alert_success("Package is already up to date."))

  cli::cli_alert_info("Unloading and updating \`integral\`...")
  devtools::unload("integral")
  devtools::update_packages("integral", upgrade = "always", dependencies = FALSE)
  library(integral)
  cli::cli_alert_success("Successfully updated \`integral\`")

  new_version <- packageVersion("integral")

  if(old_version != new_version) {
    #integral::ic_news() #Removed for now.
    #See: https://github.com/jimhester/devtools/commit/f2f077b6c8c8180ae71c53d6fb6744368c5225b7
    #and: https://github.com/r-lib/devtools/issues/942
    cli::cat_line()

    print(cli::rule(
      center = paste0("Major Update"),
      line_col = "red"
    ))
    #cli::cli_alert_info("New features added. Run \`ic_news()\` to view notes.")
    ic_news()
    cli::cli_alert_info("Run \`ic_news(all = T)\` to view previous update news.")

  } else {
    cli::cat_line()

    print(cli::rule(
      left = paste0("integral update"),
      line_col = "blue"
    ))

    cli::cat_line()
    cli::cli_alert_info("Minor update, no new features added.  Run \'ic_news(all = T)\` to view changelog for previous major updates.")
    cli::cat_line()
  }
}


#' Show news for new version
#' @description
#' `r lifecycle::badge('experimental')`
#' Shows the news for the package
#' @param all Logical. Show all previous news in viewer. Defaults to FALSE.
#' @export
ic_news <- function(all = FALSE) {

  if(all) {
    news(package = "integral")
  } else {

    newsdb <- news(Version == as.character(packageVersion("integral")), package = "integral")

    cli::cat_line()

    print(cli::rule(
      center = paste0("Update news for integral ", packageVersion("integral"), ""),
      line_col = "yellow"
    ))

    newstext <- newsdb$Text %>%
      stringr::str_split("  - ") %>%
      unlist() %>%
      stringr::str_remove("\\\n") %>%
      stringr::str_squish() %>%
      .[-1] #end up with an empty first line

    cli::cat_line()
    cli::cli_li(newstext)
    cli::cat_line()

  }

}




# Currently removed because I don't know what it's used for - JZ 5/25/22
# #Internal only. Get OS (from https://conjugateprior.org/2015/06/identifying-the-os-from-r/)
# get_os <- function(){
#   sysinf <- Sys.info()
#   if (!is.null(sysinf)){
#     os <- sysinf['sysname']
#     if (os == 'Darwin')
#       os <- "osx"
#   } else { ## mystery machine
#     os <- .Platform$OS.type
#     if (grepl("^darwin", R.version$os))
#       os <- "osx"
#     if (grepl("linux-gnu", R.version$os))
#       os <- "linux"
#   }
#   tolower(os)
# }


#' Check whether R is running on Citrix
#' @description
#' `r lifecycle::badge('experimental')`
#' Returns TRUE if the current system is Citrix.
#' @export
is_citrix <- function() {
  x <- Sys.info()["nodename"]

  cli::cli_alert_info("R appears to be running on: {current_r_location()}")

  stringr::str_detect(x, "APP\\d+")
}

#' Determine which system R is running on
#' @description
#' `r lifecycle::badge('experimental')`
#' Returns one of: "citrix", "linux", "local" depending on which integral environment R is being run on.
#' @export
get_system <- function() {
  x <- Sys.info()["nodename"]

  if(stringr::str_detect(x, "APP\\d+")) return("citrix") else
    if(stringr::str_detect(x, "rstudio")) return("linux") else
      return("local")

}

