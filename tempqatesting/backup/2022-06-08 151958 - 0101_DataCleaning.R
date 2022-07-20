# COPYRIGHT  --------------------------------------------------------------
# Copyright 2022, Integral Consulting Inc. All rights reserved.

# PURPOSE -----------------------------------------------------------------
# Get data into good shape for analysis

# PROJECT INFORMATION -----------------------------------------------------
# Name: Woodbury 3M
# Number: C1522

# HISTORY -----------------------------------------------------------------
# Date       Author          Remarks
# 20220316  K Heal        Starting script

# NOTES -------------------------------------------------------------------
# non detects set to RDL

# SETUP -------------------------------------------------------------------
## Packages and special functions-----
library(here)
library(tidyverse)
library(janitor)
library(snakecase)
library(readxl)

# LOAD DATA --------------------------------------------------------------------
## PFAS data ----
# QA files are brought in with logical names
pfas_data_file <- "WB_PFAS_Data_20220309.xlsx"
chem_dat <- read_excel(path = here("raw_inputs", pfas_data_file), sheet = 1) %>%
  suppressWarnings()
well_dat <- read_excel(path = here("raw_inputs", pfas_data_file), sheet = 3)
smp_dat <- read_excel(path = here("raw_inputs", pfas_data_file), sheet = 2)

## Oakdale PFAS data -----
# QA files are brought in with logical names
oakdale_data_file <- "Oakdale_selectData.xlsx"
oak_chem_dat <- read_excel(path = here("raw_inputs", oakdale_data_file), sheet = 1)
oak_well_dat <- read_excel(path = here("raw_inputs", oakdale_data_file), sheet = 3)
oak_smp_dat <- read_excel(path = here("raw_inputs", oakdale_data_file), sheet = 2)

## Additional off site PFAS data ------
# QA: 8214 | files are brought in with logical names
add_off_data_file <- "Select_offsite_PFAS_Data.xlsx"
add_chem_dat <- read_excel(path = here("raw_inputs", add_off_data_file), sheet = 1)

## VAP data ----
# QA: 2227 | files are brought in with logical names
vap_dat_file <- "PFCs_GW-and-Soil_SBPZ-9-11-2020_Vlookup.csv"
vap_dat <- read_csv(here("raw_inputs", vap_dat_file),
                    show_col_types = FALSE) %>% suppressWarnings()

## Aquifier classification data ----
# QA: 2227 | files are brought in with logical names
aquifier_lookup_file <- "aquifer_lookup.csv"
aquifier_lookup_dat <- read_csv(here("raw_inputs", aquifier_lookup_file),
                       show_col_types = FALSE)

## Aquifier classification in VAPs ----
# QA: 7830 | files are brought in with logical names
aquifier_lookup_vap_file <- "aquifer_lookup_vap.csv"
aquifier_lookup_vap_dat <- read_csv(here("raw_inputs", aquifier_lookup_vap_file),
                                show_col_types = FALSE)


## Analyte lookup ----
# QA: 3559 | files are brought in with logical names
analyte_lookup_file <- "analyte_lookup.csv"
analyte_lookup_dat <- read_csv(here("raw_inputs", analyte_lookup_file),
                       show_col_types = FALSE)

# clean names on all (to snake case)
# check/deal with dates on all
# make analyte_short from Parameter
# detected to bool
# RDL is filled in if nondetected
# make a good long dat and save out

# CLEAN DATA --------------
## CLEAN CHEM DATA -------
### Clean chem bulk of data ----
chem_dat2 <- chem_dat %>%
  # QA: 6448 | add oakdale data
  bind_rows(oak_chem_dat) %>%
  # QA: 8594 | change to snakecase
  clean_names() %>%
  # QA: 8504 | rename to more recognizable names
  rename(sample_id = field_sample_id,
         analyte = parameter,
         concentration = numeric_result,
         detected = is_detect) %>%
  # QA: 4810 | drop the one sample that has a replicated field sample and analyte combo, keep the first analysis with higher concentration
  filter(!(sample_id == "WBMN-GW-S04SP-0-181205" & concentration == 0.0395)) %>%
  # QA: 6858 | drop the samples with NA in concentration - no RDL reported
  filter(!is.na(concentration))%>%
  select(sample_id, sample_date, sample_location,
         analyte, concentration, units, qualifier, detected) %>%
  mutate(detected = ifelse(detected == "Yes", T, F)) %>%
  left_join(analyte_lookup_dat, by = "analyte")


### Clean select additional PFAS data ------
add_chem_dat2 <- add_chem_dat %>%
  # QA: 1972 | rename colmns to match chem_dat2
  rename(sample_location = sys_loc_code,
         units = result_unit) %>%
  select(sample_location,  sample_date, units, contains(".ndDL")) %>%
  # QA: 1782 | pivot longer
  pivot_longer(cols = contains("ndDL"),
               names_to = "analyte_short",
               values_to = "concentration") %>%
  # QA: 4583 | change units (they are equal, but make them the same)
  mutate(units = ifelse(units == "ug/L",
                        "ng/mL", units)) %>%
  # QA: 1511 | change analyte names and join with longer names
  mutate(analyte_short  = analyte_short %>%
           str_replace(".ndDL", "")) %>%
  left_join(analyte_lookup_dat, by = "analyte_short") %>%
  # QA: 7113 | convert sample_date into datetime object
  mutate(sample_date = case_when(
    str_detect(sample_date, "/") ~ as.POSIXct(sample_date,
                                              format = "%m/%d/%Y %H:%M",
                                              tz = "America/New_York"),
    str_detect(sample_date, "-") ~ as.POSIXct(sample_date,
                                              format = "%Y-%m-%d %H:%M:%S",
                                              tz = "America/New_York"),
    TRUE ~ as.POSIXct(as.numeric(sample_date) * (60*60*24)
                      , origin="1899-12-30"
                      , tz="America/New_York")
    ) %>%
      suppressWarnings()) %>%
  # QA: 2733 | create a sample_ID column
  mutate(sample_id = paste0(sample_location,
                            "_", format(sample_date, format = "%Y%m%d"))) %>%
  mutate(detected = T)


### Clean VAP data additional PFAS data ------
vap_dat2 <- vap_dat %>%
  # QA: 6373 | dump rows with no data
  remove_empty(which = c("rows"))%>%
  # QA: 5285 | select columns with pertinant data
  select(SampleLocation, SampleDate, `Identifier - table 6`,
         contains('ug/L_ppb_result'),
         contains('Q'))%>%
  # QA: 9713 | pivot concentrations
  pivot_longer(cols = `PFBS_ug/L_ppb_result`:`PFOA_ug/L_ppb_result`,
               names_to = "analyte",
               values_to = "concentration") %>%
  # QA: 3467 | pivot qualifiers
  pivot_longer(cols = `Q_PFBS`:`Q_PFOA`,
             names_to = "analyte_2",
             values_to = "qualifier") %>%
  # QA: 4120 | fix analyte names, filter so only those that have matching names are left
  mutate(analyte = str_remove(analyte, "_ug/L_ppb_result"),
         analyte_2 = str_remove(analyte_2, "Q_"),
         units = "ng/mL") %>%
  filter(analyte == analyte_2) %>%
  select(-analyte_2) %>%
  # QA: 3672 | rename columns to match chemdat2
  rename(sample_id = `Identifier - table 6`,
         sample_date = SampleDate,
         sample_location = SampleLocation,
         analyte_short = analyte) %>%
  # QA: 3705 | add detected column
  mutate(detected = ifelse(!str_detect(qualifier, "U")|is.na(qualifier),
                            T,F)) %>%
  # QA: 9761 | join to analyte lookup
  left_join(analyte_lookup_dat, by = "analyte_short") %>%
  # QA: 4643 | fix date, join to analyte lookup
  mutate(sample_date = as.POSIXct(sample_date,
                                      format = "%m/%d/%Y",
                                      tz="America/New_York"))

## Combine chemdat2 with additional data
chem_dat2 <- chem_dat2 %>%
  bind_rows(add_chem_dat2) %>%
  bind_rows(vap_dat2)

chem_dat2 <- chem_dat2 %>%
  group_by(sample_id) %>%
  # QA: 3903 | drop samples where there are not all 5 analytes measured
  filter(n() == 5)

print(paste0(length(unique(chem_dat2$sample_id)), " samples included"))

## CLEAN WELL DATA -------------
### Clean aquifer dat -----
aquifier_lookup_dat2 <- aquifier_lookup_dat %>%
  select(WELLID, Aquifer_Refined, Aquifer) %>%
  # QA: 1838 | clean up column names
  clean_names() %>%
  rename(well_id = wellid,
         aquifer_code = aquifer_refined,
         aquifer_desc = aquifer) %>%
  mutate(sample_location = well_id %>%
           str_remove("-")) %>%
  select(sample_location, well_id, aquifer_code, aquifer_desc )

### Make an output of sample_id/aquifer for future use
well_dat_lookup <- chem_dat2 %>%
  # QA: 7363 | Get sample id and sample locs
  select(sample_id, sample_location) %>%
  distinct() %>%
  # QA: 8152 | join to aquifer lookup
  inner_join(aquifier_lookup_dat2 %>%
              select(sample_location,
                     aquifer_code,
                     aquifer_desc),
             by = "sample_location") %>%
  # QA: 5263 | dump the VAP samps
  filter(aquifer_code != "VAP") %>%
  bind_rows(chem_dat2 %>%
              # Get sample id and sample locs
              select(sample_id, sample_location) %>%
              distinct() %>%
              # join to aquifer lookup
              inner_join(aquifier_lookup_vap_dat %>%
                           rename(aquifer_code = aquifer_short,
                                  aquifer_desc = aquifer_long),
                         by = "sample_id"))
# QA checks ----
qa_check_df <- chem_dat2 %>%
  left_join(well_dat_lookup, by = c("sample_id", "sample_location"))
print(paste0(length(unique(qa_check_df$sample_id)), " samples with in this vis dat"))
# QA: 7531 | All columns are expected data type
# QA: 2666 | No NAs where we would not expect them
visdat::vis_dat(qa_check_df)


# SAVE DATA -----
# QA: 1646 | Correct R object is saved and logically named
write_csv(chem_dat2, here("01_DataWrangling",
                          "Outputs",
                          "chem_dat_long.csv"))
# QA: 3683 | Correct R object is saved and logically named
write_csv(well_dat_lookup, here("01_DataWrangling",
                          "Outputs",
                          "well_dat.csv"))

