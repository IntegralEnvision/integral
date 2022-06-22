#' Table Template
#'
#' Apply Integral table style to table for report-ready output
# Note this applies the same style to every sheet in a multi sheet
#'
#' @param sheet The sheet(s) added to the workbook
#' @param d The data frame the table is created from
#' @param title The title of the table
#' @param author Author name
#' @param fpath Optional file path to write the table to
#' @param wb Optional workbook option
#' @param style Orientation and page size of the table
#' @param leftjust Number of columns to be left justified
#' @param startcol Optional start column for table
#' @param startrow Optional start row for table
#'
#' @return A formatted data table
#'
#' @examples
#' int_vec <- c('1', '2', '3')
#' char_vec <- c("a", "b", "c")
#' bool_vec <- c(TRUE, TRUE, FALSE)
#' d <- data.frame(
#'   int_vec, char_vec,
#'   bool_vec
#' )
#' # Example using a file path
#' # Note: temp file used to pass build check. Use a non temp file path in reality.
#' fpath <- tempfile(fileext = ".xlsx")
#' sheet <- "Sheet1"
#' title <- "Example Table"
#' author <- "Eben Pendleton"
#' outpath <- xlsxtable(sheet, d, title, author, fpath = fpath)
#'
#' # Example using a workbook object
#' fpath <- tempfile(fileext = ".xlsx")
#' sheet <- "Sheet2"
#' title <- "Example Table"
#' author <- "Eben Pendleton"
#' # create workbook
#' wb <- openxlsx::createWorkbook()
#' # This returns the Excel workbook object if successful.
#' wb <- xlsxtable(sheet, d, title, author, wb = wb)
#' # Save the workbook
#' openxlsx::saveWorkbook(wb, file = fpath, overwrite = TRUE)
#'
#' @export
#'
# HISTORY:
# Date		    Author         Remarks
# ----------- ------------   --------------------------------------------------
# 2020-11-05  Ana Lindborg   Created by Ana Lindborg
# 2021-02-15  Eben Pendleton Renamed xlsxtable and added to package.
# 2021-02-16  Eben Pendleton Added source sheet and multi sheet ability.
# 2021-03-12  Eben Pendleton Rewrote to multi sheet ability to workbook
# 2022-03-18  Eben Pendleton Add max(column) in for column width
# ----------- -------------- --------------------------------------------------

xlsxtable <- function(sheet, d, title, author, fpath = NULL,  wb = NULL,
                      style = "L",leftjust = 1, startcol = 1,
                      startrow = 2) {
  # if missing both a file path and workbook then stop
  if ((methods::hasArg(fpath) == FALSE &  methods::hasArg(wb) == FALSE)) {
    stop("Missing file path and workbook for table(s). Provide either.")
  }

  if ((methods::hasArg(fpath) &  methods::hasArg(wb))) {
    stop("Has both a file path and workbook for table(s). Choose one.")
  }

  if (missing(sheet)) {
    stop("Missing sheet for table")
  }

  if (sheet == "source") {
    stop("source is a reserved sheet name")
  }

  if (missing(title)) {
    stop("Missing title for table")
  }

  hs <- openxlsx::createStyle(
    halign = "center", valign = "bottom",
    border = "bottom", borderColour = "black", borderStyle = "double", wrapText = TRUE
  )
  tb <- openxlsx::createStyle(border = "top")
  bb <- openxlsx::createStyle(border = "bottom")
  fc <- openxlsx::createStyle(halign = "left")
  cc <- openxlsx::createStyle(halign = "center")

  # Check to see if file doesn't exist
  if (methods::hasArg(fpath)) {
    if (!file.exists(fpath)) {
    # create workbook
    wb <- openxlsx::createWorkbook()
    } else {
        # create workbook
        wb <- openxlsx::createWorkbook()
    }
  } else {
    if (methods::hasArg(wb)) {
      sheets <- names(wb)
      # remove any source sheet
      if ("source" %in% sheets) {
        openxlsx::removeWorksheet(wb, "source")
      }

      # error on same sheet
      if (sheet %in% sheets) {
        stop("Sheet already exists in workbook. Remove or chose another name. Sheet: ", sheet)
      }
    }
  }

  # Insert created sheets into created workbook
  worksheet <- openxlsx::addWorksheet(wb = wb, sheetName = sheet)
  openxlsx::modifyBaseFont(wb, fontSize = 10, fontColour = "black", fontName = "Arial")

  # Add data to workbook
  openxlsx::writeData(wb, sheet, d, startCol = startcol, startRow = startrow)

  row.num <- nrow(d) + 2
  openxlsx::addStyle(wb, sheet, style = hs, rows = 2, cols = 1:ncol(d), stack = TRUE)
  openxlsx::addStyle(wb, sheet, style = tb, rows = 2, cols = 1:ncol(d), stack = TRUE)
  openxlsx::addStyle(wb, sheet, style = fc, rows = 2:(2 + nrow(d)), cols = leftjust:leftjust, stack = TRUE)
  openxlsx::addStyle(wb, sheet, style = bb, rows = 2 + nrow(d), cols = 1:ncol(d), stack = TRUE)
  openxlsx::addStyle(wb, sheet, style = cc, rows = 2:(2 + nrow(d)), cols = (leftjust + 1):ncol(d), stack = TRUE,        gridExpand = TRUE)
  # This still needs to be updated to properly wrap column headers or size to max variable
  # Test code for this is in the example script
  #openxlsx::setColWidths(wb, sheet, cols = 1:ncol(d), widths = "auto", ignoreMergedCells = TRUE)

  # updated 03/18/2022 Eben Pendleton
  for (c in 1:ncol(d)) {
    # openxlsx::setColWidths(wb, sheet, cols=c, widths="auto")
    openxlsx::setColWidths(wb, sheet, cols=c,widths=max(nchar(c(names(d[c]),d[,c]))))
    # print(max(nchar(c(names(d[c]),d[,c]))))
  }

  openxlsx::writeData(wb, sheet, title, startCol = 1, startRow = 1)
  # styles
  if (style == "L") {
    openxlsx::pageSetup(wb, sheet, orientation = "landscape", fitToWidth = TRUE, paperSize = 1, printTitleRows = 2)
  }
  else if (style == "P") {
    openxlsx::pageSetup(wb, sheet, orientation = "portrait", fitToWidth = TRUE, paperSize = 1, printTitleRows = 2)
  }
  else if (style == "LD") {
    openxlsx::pageSetup(wb, sheet, orientation = "landscape", fitToWidth = TRUE, paperSize = 3, printTitleRows = 2)
  }
  else {
    stop("Style should be L for letter-landscape, P for letter-portrait, or LD for ledger")
  }

  # add source sheet with script path and creator / system date
  openxlsx::addWorksheet(wb, "source")
  openxlsx::writeData(wb, "source", paste("Generated By:", determine_path()))
  openxlsx::writeData(wb, "source", paste("Created By:", author, "on", Sys.Date()),
    startRow = 2
  )


  # Output saved file
  # overwrite will overwrite existing "table_output" document
  if (methods::hasArg(fpath)) {
    openxlsx::saveWorkbook(wb, file = fpath, overwrite = TRUE)
  return(fpath)
  }

  # if workbook return the workbook
  if (methods::hasArg(wb)) {
    return(wb)
  }

} # end function
