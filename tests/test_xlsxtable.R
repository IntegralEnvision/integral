#===============================================================================
# PURPOSE:
#   Example code utilizing integraltables package for report-ready table output
#
# NOTES:
#
# PROJECT INFORMATION:
#   Name: All projects
#   Number: OH01
#
# HISTORY:
# Date		    Author         Remarks
# ----------- ------------   --------------------------------------------------
#	2021-02-15  Eben Pendleton Created.
#	2021-03-12  Eben Pendleton Updated to handle new workbook case


#===============================================================================
# Packages
library(integral)
#===============================================================================

## Example code
table.num <- 1
#Create title
title <- paste("Table A-",table.num, "Example Table",sep="")

#Select style (LL for letter-landscape, LP for letter-portrait, and LD for Ledger (11x17))
style <- "LD"

#Select number of identifier columns (non-numeric columns such as "chemical name") that should be left justified (instead of centered)
leftjust <- 2

int_vec <- c('1', '2', '3')
char_vec <- c("a", "b", "c")
bool_vec <- c(TRUE, TRUE, FALSE)
d <- data.frame(int_vec, char_vec, bool_vec)

fpath <- "tests/outtable.xlsx"
sheet <- "Sheet1"
title <- "Example Table"
author <- "Eben Pendleton"
# This returns the Excel out file path if successful.
outpath <- xlsxtable(sheet, d, title, author, fpath = fpath)

fpath <- "tests/outtable.xlsx"
sheet <- "Sheet2"
title <- "Example Table"
author <- "Eben Pendleton"

# create workbook
wb <- openxlsx::createWorkbook()

# This returns the Excel workbook object if successful.
wb <- xlsxtable(sheet, d, title, author, wb = wb)

# Save the workbook
openxlsx::saveWorkbook(wb, file = fpath, overwrite = TRUE)

