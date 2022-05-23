# all_code <- z
# all_code <- x #rmd
# all_code <- y #r

qa <- function(filepath) {

  if(!fs::file_exists(filepath)) stop(cli::cli_alert_danger(paste0("File `", filepath, "` does not exist. If it is not in the root project directory, specify the path relative to the root project directory.")))

  if(!str_detect(str_to_lower(filepath), ".*\\.(r|rmd)$")) stop(cli::cli_alert_danger(paste0("File `", filepath, "` is not an .R or .Rmd file.")))

  filepath <- path_real(filepath)

  qa_file <- qa_file(filepath)

  qa_wb(filepath, qa_file)

  parsed_qa <- qa_parse(filepath)

  qawb <- qa_wb_load(filepath)

  qa_update_sheet(qawb, parsed_qa, filepath)

  }


qa_parse <- function(filepath) {

  filetype <- tools::file_ext(filepath) %>% str_to_lower()

  script_qa <- fs::path_expand(filepath)

  all_code <- readr::read_lines(script_qa) %>%
    tibble::enframe(name = "line", value = "code") %>%
    tibble::add_column(file_full = as.character(script_qa))

  #Check for QA tags and QA tags without IDs
  has_qa_tags <- any(all_code %>%
                       mutate(has_qa = stringr::str_detect(code, "QA:")) %>%
                       pull(has_qa))
  missing_qa_ids <- any(all_code %>%
                          mutate(has_id = stringr::str_detect(code, "(QA: )(\\d+)(.*)")) %>%
                          mutate(missing_id = stringr::str_detect(code, "#\\s?QA:\\s(?=\\D)")) %>%
                          mutate(doublecheck = !has_id & missing_id) %>%
                          pull(doublecheck))

  if(!has_qa_tags) stop("The file does not contain any QA tags.")

  if(has_qa_tags & missing_qa_ids) { #TODO: Danger zone, editing script source.  Backup method is temporary
    add_ids <- usethis::ui_yeah("The file is missing QA ID numbers for one or more QA tags.  Would you like them to be added automatically? This will modify your script file.", shuffle = F)
    if(add_ids) {

      #Temporary backup solution
      if(!fs::dir_exists(path(path_dir(filepath), "backup"))) fs::dir_create(path(path_dir(filepath), "backup"))
      fs::file_copy(filepath, path(path_dir(filepath), "backup", paste0(fs::path_sanitize(lubridate::now()), " - ", path_file(filepath))))
      #/Temporary backup solution

      all_code <- all_code %>%
        mutate(has_qa = stringr::str_detect(code, "QA:")) %>%
        mutate(has_id = stringr::str_detect(code, "(QA: )(\\d+)(.*)")) %>%
        mutate(missing_id = stringr::str_detect(code, "#\\s?QA:\\s(?=\\D)")) %>%
        rowwise() %>%
        mutate(code = if_else(has_qa & !has_id & missing_id, str_replace(code, "#\\s?QA:\\s(?=\\D)", paste0("# QA: ", round(runif(1, 1, 10000)), " | ")), code)) %>%
        ungroup() %>%
        select(-c(has_qa, has_id, missing_id))

      write_lines(all_code$code, filepath)
      cli::cli_alert_success("Your script has been modified to include QA ID tags. If it is currently open, you will be prompted to reload it. A backup has been created in the 'backups' subdirectory.")
    }
  }

  if(filetype == "rmd") {
    all_code <- all_code %>% #Add flag to indicate whether the lines are in a text chunk. We use this later when parsing section headers
      mutate(chunk_deliniator = str_detect(code, "^```")) %>%
      mutate(chunk_num = cumsum(chunk_deliniator)) %>%
      mutate(is_text_chunk = chunk_num %% 2 == 0 & !chunk_deliniator) %>%
      select(-c(chunk_deliniator, chunk_num))
  } else if(filetype == "r") all_code <- all_code %>% add_column(is_text_chunk = F)

  headers <- all_code %>%
    dplyr::mutate(is_code_header = str_detect(code, "(#+)[^\\t]([a-zA-Z0-9\\(\\)&\\s]*)(?=-+)")) %>%
    dplyr::mutate(is_text_header = is_text_chunk & str_detect(code, "(#+)\\s(.*?)")) %>%
    mutate(code = str_remove_all(code, "-{2,}")) %>%
    filter(is_code_header | is_text_header) %>%
    mutate(section = str_match(code, "(#+)\\s(.*)")) %>%
    mutate(section = as_tibble(section)) %>%
    unnest_wider(section, names_sep = "_") %>%
    rename(full_string = section_V1,
           hash = section_V2,
           title = section_V3) %>%
    mutate(full_string = str_squish(full_string),
           title = str_squish(title)) %>%
    mutate(section_level = str_count(hash, "#")) %>%
    select(-c(is_text_header, is_code_header, full_string, hash, is_text_chunk))

  section_depth <- headers %>% summarize(section_depth = max(section_level)) %>% pull(section_depth)

  wh <- headers %>% #wide headers
    select(-file_full) %>%
    pivot_wider(names_from = section_level, values_from = title, names_prefix = "level_")

  for(i in seq(section_depth)) {
    wh <- wh %>%
      fill(paste0("level_", i), .direction = "down") %>%
      group_by_at(paste0("level_", i), .add = T)
  }

  wh <- wh %>% ungroup()

  qa_lines <- all_code %>%
    select(-c(is_text_chunk, file_full)) %>%
    filter(stringr::str_detect(code, "QA:"))

  qa_lines <- qa_lines %>%
    mutate(qa_comment = str_match(code, "(QA: )(\\d+)(.*)")) %>%
    mutate(qa_comment = as_tibble(qa_comment)) %>%
    unnest_wider(qa_comment, names_sep = "_") %>%
    rename(qa_id = qa_comment_V3,
           comment = qa_comment_V4) %>%
    select(-c(qa_comment_V2, qa_comment_V1, code)) %>%
    mutate(comment = str_remove(comment, " \\| "))

  #Determine the location of QA lines within sections
  m_ind <- qa_lines %>%
    select(line) %>%
    add_column(table = "qa") %>%
    bind_rows(wh %>%
                select(line) %>%
                add_column(table = "section")) %>%
    arrange(line) %>%
    mutate(grouping = cumsum(table == "section"))

  wh <- wh %>%
    left_join(m_ind %>%
                filter(table == "section")) %>%
    select(-c(code, line, table))

  qa_lines <- qa_lines %>%
    left_join(m_ind %>%
                filter(table == "qa")) %>%
    select(-c(table))

  parsed_qa <- wh %>%
    left_join(qa_lines, by = "grouping") %>%
    select(-grouping)

  return(parsed_qa)

}


qa_file <- function(filepath) { #TODO add status messages as to what is happening

  code_file <- path_file(filepath)

  code_path <- path_dir(filepath)

  project_path <- rstudioapi::getActiveProject()

  project_name <- str_extract(project_path, "(?!(.*\\/)).*")

  if(code_path == project_path) { #If we're working with a script in the project root, it gets a QA sheet with the same name as the project

    qa_file <- path(code_path, "QA", paste0("QA_", project_name, ".xlsx"))

    fs::dir_create(path_dir(qa_file)) #function ignores command if dir already exists

  } else if(str_detect(code_path, project_path)) { #If the script is in a subdir of project root, it gets a QA sheet with the name of the subdir.

    code_subfolder <- str_remove(code_path, project_path) %>%
      str_remove("/")

    qa_file <- path(code_path, "QA", paste0("QA_", code_subfolder, ".xlsx"))

    fs::dir_create(path_dir(qa_file)) #function ignores command if dir already exists
  }

  return(qa_file)
}

qa_wb <- function(filepath, qa_file) {


  has_existing_qa_file <- fs::file_exists(qa_file)

  if(has_existing_qa_file) {

    if(code_file %in% getSheetNames(qa_file)) {

      cli::cli_alert_warning("A QA file and code review worksheet for this script already exists. If you continue, the worksheet code review section will be over-written (checklist and other sheets will not be affected).") #TODO: Check if there is any user-entered changes that will be deleted, and either make sure they aren't by lining up the QA tags, or prompt the user about this.  For now we are prompting every time regarding overwrite.

      user_overwrite <- usethis::ui_yeah("Do you want to proceed and overwrite the existing QA code review section?", shuffle = F)

      if(user_overwrite) {

        qawb <- loadWorkbook(qa_file)

        backup_sheet <- paste(code_file, lubridate::now()) %>% str_remove_all("-|:") #TODO: Need to limit chars to 31, so need better naming
        backup_sheet <- abbreviate(backup_sheet, 31) #FIXME temporary until above is fixed!

        cloneWorksheet(qawb, backup_sheet, clonedSheet = code_file)

        sheetVisibility(qawb)[sheetNamesIndex(qawb, backup_sheet)] <- "hidden"


        } else stop("User exited.") #TODO Do something better here

    } else { #Sheet does not exist but QA file does
      cli::cli_alert_info('A QA file for for the scripts in this directory already exists, but a worksheet for this script does not. It will be added as a new spreadsheet (named "{maybe_qa_sheet}") in the file: {qa_file}')

      qawb <- loadWorkbook(qa_file)

      cloneWorksheet(qawb, code_file, clonedSheet = "Code_Review_Template")
      sheetVisibility(qawb)[sheetNamesIndex(qawb, code_file)] <- "visible"
    }

  } else { #No QA sheet exists yet

    cli::cli_alert_info("A QA file for this script was not detected. A new one will be created: {qa_file}, and a worksheet for the script {code_file} will be added.")

    qawb <- loadWorkbook(fs::path_package("integral", "extdata/QA_Template_Coded_Analysis.xlsx"))

    cloneWorksheet(qawb, code_file, clonedSheet = "Code_Review_Template")
    sheetVisibility(qawb)[sheetNamesIndex(qawb, code_file)] <- "visible"

  }

  return(qawb)
}


#Internal function to translate sheet name to sheet index for openxlsx functions that don't accept names.
sheetNamesIndex <- function(qawb, lookup) {

  name_ind <- enframe(names(qawb), name = "index", value = "name")

  if(is.character(lookup)) {
    name_ind %>%
      filter(name == !!lookup) %>%
      pull(index)
  } else
  if(is.numeric(lookup)) {
    name_ind %>%
      filter(index == name_ind) %>%
      pull(name)
  } else stop("Sheet name or index number does not exist in workbook.")
}



qa_update_sheet <- function(qawb, parsed_qa, filepath) {

  code_file <- path_file(filepath)

  qa_file <- qa_file(filepath)

  writeData(qawb, code_file, parsed_qa, startRow = 14, colNames = F)

  saveWorkbook(qawb, )
}

#
# merge_cols <-  enframe(which(str_detect(names(four), "\\.Q")), name = NULL, value = "end") %>%
#   mutate(start = end - 1, .before = end) %>%
#   array_tree(margin = 1)
#
#
# parsed_qa %>%
#   mutate(lvl1 = dense_rank(level_1))
#
# parsed_qa %>% mutate(row = row_number())
#
#
#
#
#
#
#
#
#
















