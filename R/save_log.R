#' Write csv with log
#' @title Add to save log when csv is written
#' @name write_csv_wlog
#' @description A wrapper for write_csv that adds a project-level log with pertinent info into file_log.csv
#' `r lifecycle::badge('stable')`
#' @return starting or adding a file_log.csv at project-level, as well as saving csv as expected
#' @param x A data frame or tibble to write to disk.
#' @param file File or connection to write to.
#' @param ... Additional arguments from readr::write_csv()
#' @examples
#' \dontrun{
#' df <- data.frame(fishes <- c("red", "white", "blue"))
#' write_csv_wlog(df, tempfile())
#' }
#' @export
write_csv_wlog <- function(x, file, ...){
  readr::write_csv(x, file, ...)

  # Check if working in a project
  if (length(list.files(getwd(), pattern = ".Rproj")) > 0){

    # Prep logging info
    date <- Sys.time()
    source_loc <- ifelse(rstudioapi::getActiveDocumentContext()$path == "",
                         NA, rstudioapi::getActiveDocumentContext()$path)
    user_info <- Sys.info()["user"]
    new_line <- c(file, as.character(date), source_loc, user_info)
    names(new_line) <- c("file_name", "date_written", "source_script", "user_id")
    new_line_df <- as.data.frame(t(new_line))

    # Check for existing log
    if (length(list.files(getwd(), pattern = "file_log.csv")) > 0){
      message("Log file will be added to.")
      file_log <- readr::read_csv("file_log.csv", show_col_types = FALSE)
      file_log <- dplyr::mutate(file_log, date_written = as.character(file_log$date_written))
      new_file_log <- dplyr::bind_rows(new_line_df, file_log)
      readr::write_csv(new_file_log, "file_log.csv")

    } else {
      message(paste0("New log file created here: ", getwd(), "/file_log.csv"))
      readr::write_csv(new_line_df, "file_log.csv")
      }

    } else {message("No .Rproj file found in working directory, no save log written")}
} # fxn close

#' Copy file with log
#' @title Copy file and log where it was copied from
#' @name copy_file_wlog
#' @description A wrapper for fs::file_copy() that adds a project-level log with pertinent info into file_log.csv
#' `r lifecycle::badge('stable')`
#' @return starting or adding a file_log.csv at project-level, as well as copying file as expected
#' @param  path A character vector of one or more paths.
#' @param new_path A character vector of paths to the new locations.
#' @param overwrite Overwrite files if they exist. Default is FALSE. If this is FALSE and the file exists an error will be thrown.
#' @examples
#' \dontrun{
#' .old_wd <- setwd(tempdir())
#' fs::file_create("foo")
#' copy_file_wlog("foo", "bar")
#' }
#' @export
copy_file_wlog <- function(path, new_path, overwrite = FALSE){
  fs::file_copy(path, new_path, overwrite)

  # Check if working in a project
  if (length(list.files(getwd(), pattern = ".Rproj")) > 0){

    # Prep logging info
    date <- Sys.time()
    source_loc <- ifelse(rstudioapi::getActiveDocumentContext()$path == "",
                         NA, rstudioapi::getActiveDocumentContext()$path)
    user_info <- Sys.info()["user"]
    new_line <- c(new_path, as.character(date), source_loc, user_info, path)
    names(new_line) <- c("file_name", "date_written", "source_script", "user_id", "copied_from")
    new_line_df <- as.data.frame(t(new_line))

    # Check for existing log
    if (length(list.files(getwd(), pattern = "file_log.csv")) > 0){
      message("Log file will be added to.")
      file_log <- readr::read_csv("file_log.csv", show_col_types = FALSE)
      file_log <- dplyr::mutate(file_log, date_written = as.character(file_log$date_written))
      new_file_log <- dplyr::bind_rows(new_line_df, file_log)
      readr::write_csv(new_file_log, "file_log.csv")

    } else {
      message(paste0("New log file created here: ", getwd(), "/file_log.csv"))
      readr::write_csv(new_line_df, "file_log.csv")
    }

  } else {message("No .Rproj file found in working directory, no save log written")}
} # fxn close
