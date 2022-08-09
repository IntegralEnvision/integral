# History
# 2022-05-04. Created Eben Pendleton.

#' @title Utility Windows function
#' @name convert_winpath
#' @description A function to ask a yes/no question and return TRUE/FALSE
#' `r lifecycle::badge('stable')`
#' @return boolean response from Yes / No
#'
#' @param question input Yes / No question
#' @param default Optionally set the default response value

#' @examples
#' \dontrun{
#' convert_winpath(<path>)
#' }
#' @export
# from renv. MIT License
convert_winpath <- function(x) {
  x <- gsub('[\"]', '', x)
  x <- gsub("\\\\", "/", x)
  return(x)
}

#' @title determine_path
#' @name determine_path
#' @description Function to find file path
#' `r lifecycle::badge('stable')`
#' @return path to file
#'
#' @examples
#' \dontrun{
#' determine_path()
#' }
#' @export

determine_path <- function() {
  if (!is.null(whereami::thisfile_source())) {
    path <- paste(whereami::thisfile_source(), sep = " ")
  } else if (!is.null(whereami::thisfile_rscript())) {
    path <- paste(whereami::thisfile_rscript(), sep = " ")
  } else {
    path <- paste(paste0(getwd(), .Platform$file.sep, whereami::thisfile_r()),
                  sep = " ")
  }

  ### if a linux path convert to a windows path
  if (grepl( Sys.getenv("HOME"), path, fixed = TRUE)) {
    npath <- gsub(paste0(Sys.getenv("HOME"), "/mnt/"), '', path)

    path <- dplyr::case_when(

      # change mapped drives
      grepl("Home", npath, fixed = TRUE) ~gsub("Home", "H:", npath),
      grepl("Departments", npath, fixed = TRUE) ~gsub("Departments", "M:", npath),
      grepl("Resources", npath, fixed = TRUE) ~gsub("Resources", "N:", npath),
      grepl("Transfer", npath, fixed = TRUE) ~gsub("Transfer", "Q:", npath),
      # otherwise add //pfs1w
      TRUE ~ paste0("//pfs1w/", npath)
    )
  }

  # switch slashes
  path <- paste0(gsub('/', '\\\\', path))

  return(path)

}
