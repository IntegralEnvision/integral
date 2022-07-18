#
# Copyright: Copyright 2022, Integral Consulting Inc. All rights reserved.
#
# Title: 01_data_fetch.R
# Purpose: Update data sources
#
# Project Information:
#   Name:
#   Number: CF1144
#
# History:
# Date	     Remarks
# 20220705    K Heal started script
#

# PACKAGES & SPECIAL FUNCTIONS ----
library(tidyverse)
library(here)
library(integral)

# SET UPDATE AND LOCATION INFO ----
refresh <- T
location <- "/home/kheal/mnt/cf1100-cf3999/CF1144_Evraz/Working_Files"

# COPY DATA FILES ----
if (refresh) {
  ## sediment data from IDB ----
  # QA: 1423 | most recent data? log written out?
  integral::copy_file_wlog(paste0(location, "/Task 0502 Allocation Support/Forensics/06_Data/Riverwide/InRiver/ChemSummary_swanson20220331.csv"), "input/chemSummary_swanson.csv")

  ## data from core logs -----
  # phase 1 logs
  # QA: 7305 | most recent data? log written out?
  core_logs <- list.files(paste0(location, "/Task 0803_EvaluationReport/LogPlot/input files/Subsurface_Sediment/Phase I"), pattern = "xlsx", full.names = T)
  new_core_logs <- core_logs %>%
    str_replace(paste0(location, "/Task 0803_EvaluationReport/LogPlot/input files/Subsurface_Sediment/Phase I/"), "input/p1_")
  for (i in 1:length(core_logs)) {
    integral::copy_file_wlog(core_logs[i], new_core_logs[i], overwrite = refresh)
  }

  # phase 2 logs
  # QA: 3020 | most recent data? log written out?
  core_logs <- list.files(paste0(location, "/Task 0803_EvaluationReport/LogPlot/input files/Subsurface_Sediment/Phase II"), pattern = "xlsx", full.names = T)
  new_core_logs <- core_logs %>%
    str_replace(paste0(location, "/Task 0803_EvaluationReport/LogPlot/input files/Subsurface_Sediment/Phase II/"), "input/p2_")
  for (i in 1:length(core_logs)) {
    integral::copy_file_wlog(core_logs[i], new_core_logs[i], overwrite = refresh)
  }
}

#QA test
