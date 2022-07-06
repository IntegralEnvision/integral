# PURPOSE:
#   Example code utilizing integraltemplates package for creating a PowerPoint
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
#	2022-04-12  Eben Pendleton Created. Added header and switched to local
#                                     data source
#===============================================================================
# Packages

library(tidyr)
library(dataRetrieval)
library(rvg)
library(officer)
library(ggplot2)

library(integral)

###Theme
theme <- integral::ppttheme()
###Data
#Q <- readNWISuv(site = "01114500", parameterCd = "00060",
# startDate = as.Date("2020-01-01"))

# read in data
# if the package is installed
fname <- "01114500.csv"
fpath <- system.file("inst/extdata", fname, package = "integral")

# if we are running a check on an uninstalled package use here::here() to
# get the Rcheck directory
if (fpath == "") {
  fpath <- here::here("integral", "inst/extdata", fname)
}

# remove any double naming
fpath <- gsub("integral/integral", "integral", fpath)

Q <- read.csv(fpath)
# set characters to dates
Q$dateTime <- as.POSIXct(Q$dateTime)


###Figure
p <- ggplot() +
  geom_path(data = Q, aes(x = dateTime, y = X_00060_00000)) +
  geom_hline(yintercept = 200, color = "red", size = 1.5) +
  annotate("text", x = as.POSIXct("2020-04-01"), y = 200, label = "Line", vjust = 1.8) +
  labs(title = "USGS Stream Gage 01114500, 2020", x = NULL, y = "Discharge (cfs)") +
  theme

###pptfig
### landscape 11 x 8.5
orientation <- "L"
size <- "letter"
fig <- dml(ggobj = p)
section <- "X"
fig.num <- 1
doc.title.1 <- "Discharge at USGS Stream Gage 01114500"
doc.title.2 <- "Project Description 1"
doc.title.3 <- "Project Description 2"
draft <- "DRAFT"
note <- "Note: Data pulled using readNWISuv."
author <- "Eben Pendleton"

my_pres <- read_pptx_template(size, orientation)
my_pres <- pptfig(my_pres, size, orientation, fig, section, fig.num, note, author, doc.title.1,
                  doc.title.2, doc.title.3, draft)

print(my_pres, "tests/OutputL8.5x11.pptx")

### portrait 8.5 x 11
size <- "letter"
orientation <- "P"
fig <- dml(ggobj = p)
section <- "X"
fig.num <- 1
doc.title.1 <- "Discharge at USGS Stream Gage 01114500"
doc.title.2 <- "Project Description 1"
doc.title.3 <- "Project Description 2"
draft <- "DRAFT"
note <- "Note: Data pulled using readNWISuv."

my_pres <- read_pptx_template(size, orientation)
my_pres <- pptfig(my_pres, size, orientation, fig, section, fig.num, note, author, doc.title.1,
                  doc.title.2, doc.title.3, draft)
print(my_pres, "tests/OutputP8.5x11.pptx")

# ledger
# landscape 17 x 11
size <- "ledger"
orientation <- "L"
fig <- dml(ggobj = p)
section <- "X"
fig.num <- 1
doc.title.1 <- "Discharge at USGS Stream Gage 01114500"
doc.title.2 <- "Project Description 1"
doc.title.3 <- "Project Description 2"
draft <- "DRAFT"
note <- "Note: Data pulled using readNWISuv."

my_pres <- read_pptx_template(size, orientation)
my_pres <- pptfig(my_pres, size, orientation, fig, section, fig.num, note, author, doc.title.1,
                  doc.title.2, doc.title.3, draft)

print(my_pres, "tests/OutputL17x11.pptx")
#
# ### portrait 11 x 17
size <- "ledger"
orientation <- "P"
fig <- dml(ggobj = p)
section <- "X"
fig.num <- 1
doc.title.1 <- "Discharge at USGS Stream Gage 01114500"
doc.title.2 <- "Project Description 1"
doc.title.3 <- "Project Description 2"
draft <- "DRAFT"
note <- "Note: Data pulled using readNWISuv."

my_pres <- read_pptx_template(size, orientation)
my_pres <- pptfig(my_pres, size, orientation, fig, section, fig.num, note, author, doc.title.1,
                  doc.title.2, doc.title.3, draft)
print(my_pres, "tests/OutputP11x17.pptx")
