#' @title Add to save log when csv is written
#' @name write_csv_wlog
#' @description A wrapper for write_csv that adds a project-level log with pertinent info into file_log.csv
#' `r lifecycle::badge('stable')`
#' @return starting or adding a file_log.csv at project-level, as well as saving csv as expected
#'
#' @param x A data frame or tibble to write to disk.
#' @param file File or connection to write to.
#' @param ... Additional arguments from readr::write_csv()
#'
#' @examples
#' df <- data.frame(fishes <- c("red", "white", "blue"))
#' write_csv_wlog(df, tempfile())

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
      new_file_log <- rbind(new_line_df, file_log)
      readr::write_csv(new_file_log, "file_log.csv")

    } else {
      message(paste0("New log file created here: ", getwd(), "/file_log.csv"))
      readr::write_csv(new_line_df, "file_log.csv")
      }

    } else {message("No .Rproj file found in working directory, no save log written")}
} # fxn close

