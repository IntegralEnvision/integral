#' Start a new R script with header
#' `r lifecycle::badge('experimental')`
#' @description Creates a new R script with header
#' @param filepath File to be created. If path is not included, it will be created in the current working directory.
#' @examples
#' \dontrun{
#' ic_new_r_file("myfile.R")
#' }
#' @export
ic_new_r_file <- function(filepath, open = open) {
  usethis::use_template("rfile_w_header.R", save_as = filepath, open = open, package = "integral")
}



#' Start a new RMarkdown document with Integral header
#' @description Drafts a new RMarkdown document with Integral header
#' `r lifecycle::badge('experimental')`
#' @param filepath File to be created. If path isn't provided, working directory is used.
#' @examples
#' \dontrun{
#' ig_new_rmd_file("myrmd.Rmd")
#' }
#' @export
ig_new_rmd_file <- function(file) {
  if(dirname(file) == ".") {
    message("Directory not provided. Using working directory.")
    file <- file.path(getwd(), file)
  }

  if(stringr::str_to_lower(tools::file_ext(file)) != "rmd") {
    cli::cli_alert_danger("File extension should be .Rmd")
    return(NULL)
  }

  rmarkdown::draft(file = file,
                   template = "r_markdown_with_header",
                   package = "integral",
                   edit = F)

  file.edit(file.path(tools::file_path_sans_ext(file), basename(file)))
}
