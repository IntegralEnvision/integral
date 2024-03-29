% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xlsxtable.R
\name{xlsxtable}
\alias{xlsxtable}
\title{Table Template}
\usage{
xlsxtable(
  sheet,
  d,
  title,
  author,
  fpath = NULL,
  wb = NULL,
  style = "L",
  leftjust = 1,
  startcol = 1,
  startrow = 2
)
}
\arguments{
\item{sheet}{The sheet(s) added to the workbook}

\item{d}{The data frame the table is created from}

\item{title}{The title of the table}

\item{author}{Author name}

\item{fpath}{Optional file path to write the table to}

\item{wb}{Optional workbook option}

\item{style}{Orientation and page size of the table}

\item{leftjust}{Number of columns to be left justified}

\item{startcol}{Optional start column for table}

\item{startrow}{Optional start row for table}
}
\value{
A formatted data table
}
\description{
Apply Integral table style to table for report-ready output
}
\examples{
int_vec <- c('1', '2', '3')
char_vec <- c("a", "b", "c")
bool_vec <- c(TRUE, TRUE, FALSE)
d <- data.frame(
  int_vec, char_vec,
  bool_vec
)
# Example using a file path
# Note: temp file used to pass build check. Use a non temp file path in reality.
fpath <- tempfile(fileext = ".xlsx")
sheet <- "Sheet1"
title <- "Example Table"
author <- "Eben Pendleton"
outpath <- xlsxtable(sheet, d, title, author, fpath = fpath)

# Example using a workbook object
fpath <- tempfile(fileext = ".xlsx")
sheet <- "Sheet2"
title <- "Example Table"
author <- "Eben Pendleton"
# create workbook
wb <- openxlsx::createWorkbook()
# This returns the Excel workbook object if successful.
wb <- xlsxtable(sheet, d, title, author, wb = wb)
# Save the workbook
openxlsx::saveWorkbook(wb, file = fpath, overwrite = TRUE)

}
