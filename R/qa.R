# all_code <- z
# all_code <- x #rmd
# all_code <- y #r

qa <- function(filepath) {

  if(!fs::file_exists(filepath)) stop(cli::cli_alert_danger(paste0("File `", filepath, "` does not exist. If it is not in the root project directory, specify the path relative to the root project directory.")))

  if(!str_detect(str_to_lower(filepath), ".*\\.(r|rmd)$")) stop(cli::cli_alert_danger(paste0("File `", filepath, "` is not an .R or .Rmd file.")))

  filepath <- path_real(filepath)

  qafile <- qa_file(filepath)

  qawb <- qa_wb(filepath, qafile)

  parsed_qa <- qa_parse(filepath)

  qa_update_sheet(qawb, parsed_qa, filepath, qafile)

  }


qa_parse <- function(filepath) {

  filetype <- tools::file_ext(filepath) %>% stringr::str_to_lower()

  script_qa <- fs::path_expand(filepath)

  all_code <- readr::read_lines(script_qa) %>%
    tibble::enframe(name = "line", value = "code") %>%
    tibble::add_column(file_full = as.character(script_qa))

  #Check for QA tags and QA tags without IDs
  has_qa_tags <- any(all_code %>%
                       dplyr::mutate(has_qa = stringr::str_detect(code, "QA:")) %>%
                       dplyr::pull(has_qa))
  missing_qa_ids <- any(all_code %>%
                          dplyr::mutate(has_id = stringr::str_detect(code, "(QA: )(\\d+)(.*)")) %>%
                          dplyr::mutate(missing_id = stringr::str_detect(code, "#\\s?QA:\\s(?=\\D)")) %>%
                          dplyr::mutate(doublecheck = !has_id & missing_id) %>%
                          dplyr::pull(doublecheck))

  if(!has_qa_tags) stop("The file does not contain any QA tags.")

  if(has_qa_tags & missing_qa_ids) { #TODO: Danger zone, editing script source.  Backup method is temporary
    add_ids <- usethis::ui_yeah("The file is missing QA ID numbers for one or more QA tags.  Would you like them to be added automatically? This will modify your script file.", shuffle = F)
    if(add_ids) {

      #Temporary backup solution
      if(!fs::dir_exists(fs::path(fs::path_dir(filepath), "backup"))) fs::dir_create(fs::path(fs::path_dir(filepath), "backup"))
      fs::file_copy(filepath, fs::path(fs::path_dir(filepath), "backup", paste0(fs::path_sanitize(lubridate::now()), " - ", fs::path_file(filepath))))
      #/Temporary backup solution

      all_code <- all_code %>%
        dplyr::mutate(has_qa = stringr::str_detect(code, "QA:")) %>%
        mutate(has_id = stringr::str_detect(code, "(QA: )(\\d+)(.*)")) %>%
        mutate(missing_id = stringr::str_detect(code, "#\\s?QA:\\s(?=\\D)")) %>%
        rowwise() %>%
        dplyr::mutate(code = dplyr::if_else(has_qa & !has_id & missing_id, stringr::str_replace(code, "#\\s?QA:\\s(?=\\D)", paste0("# QA: ", round(runif(1, 1, 10000)), " | ")), code)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-c(has_qa, has_id, missing_id))

      readr::write_lines(all_code$code, filepath)
      cli::cli_alert_success("Your script has been modified to include QA ID tags. If it is currently open, you will be prompted to reload it. A backup has been created in the 'backups' subdirectory.")
    }
  }

  if(filetype == "rmd") {
    all_code <- all_code %>% #Add flag to indicate whether the lines are in a text chunk. We use this later when parsing section headers
      dplyr::mutate(chunk_deliniator = stringr::str_detect(code, "^```")) %>%
      dplyr::mutate(chunk_num = cumsum(chunk_deliniator)) %>%
      dplyr::mutate(is_text_chunk = chunk_num %% 2 == 0 & !chunk_deliniator) %>%
      dplyr::select(-c(chunk_deliniator, chunk_num))
  } else if(filetype == "r") all_code <- all_code %>% tibble::add_column(is_text_chunk = F)

  headers <- all_code %>%
    dplyr::mutate(is_code_header = stringr::str_detect(code, "(#+)[^\\t]([a-zA-Z0-9\\(\\)&\\s]*)(?=-+)")) %>%
    dplyr::mutate(is_text_header = is_text_chunk & stringr::str_detect(code, "(#+)\\s(.*?)")) %>%
    dplyr::mutate(code = stringr::str_remove_all(code, "-{2,}")) %>%
    dplyr::filter(is_code_header | is_text_header) %>%
    dplyr::mutate(section = stringr::str_match(code, "(#+)\\s(.*)")) %>%
    dplyr::mutate(section = dplyr::as_tibble(section)) %>%
    tidyr::unnest_wider(section, names_sep = "_") %>%
    dplyr::rename(full_string = section_V1,
           hash = section_V2,
           title = section_V3) %>%
    dplyr::mutate(full_string = stringr::str_squish(full_string),
           title = stringr::str_squish(title)) %>%
    dplyr::mutate(section_level = stringr::str_count(hash, "#")) %>%
    dplyr::select(-c(is_text_header, is_code_header, full_string, hash, is_text_chunk))

  section_depth <- headers %>% dplyr::summarize(section_depth = max(section_level)) %>% dplyr::pull(section_depth)

  wh <- headers %>% #wide headers
    dplyr::select(-file_full) %>%
    tidyr::pivot_wider(names_from = section_level, values_from = title, names_prefix = "level_")

  for(i in seq(section_depth)) {
    wh <- wh %>%
      fill(paste0("level_", i), .direction = "down") %>%
      dplyr::group_by_at(paste0("level_", i), .add = T)
  }

  wh <- wh %>% dplyr::ungroup()

  qa_lines <- all_code %>%
    dplyr::select(-c(is_text_chunk, file_full)) %>%
    dplyr::filter(stringr::str_detect(code, "QA:"))

  qa_lines <- qa_lines %>%
    dplyr::mutate(qa_comment = stringr::str_match(code, "(QA: )(\\d+)(.*)")) %>%
    dplyr::mutate(qa_comment = dplyr::as_tibble(qa_comment)) %>%
    tidyr::unnest_wider(qa_comment, names_sep = "_") %>%
    dplyr::rename(qa_id = qa_comment_V3,
           comment = qa_comment_V4) %>%
    dplyr::select(-c(qa_comment_V2, qa_comment_V1, code)) %>%
    dplyr::mutate(comment = stringr::str_remove(comment, " \\| ")) %>%
    dplyr::mutate(qa_id = as.numeric(qa_id))

  #Determine the location of QA lines within sections
  m_ind <- qa_lines %>%
    dplyr::select(line) %>%
    tibble::add_column(table = "qa") %>%
    dplyr::bind_rows(wh %>%
                dplyr::select(line) %>%
                tibble::add_column(table = "section")) %>%
    dplyr::arrange(line) %>%
    dplyr::mutate(grouping = cumsum(table == "section"))

  wh <- wh %>%
    dplyr::left_join(m_ind %>%
                dplyr::filter(table == "section"), by = "line") %>%
    dplyr::select(-c(code, line, table))

  qa_lines <- qa_lines %>%
    dplyr::left_join(m_ind %>%
                dplyr::filter(table == "qa"), by = "line") %>%
    dplyr::select(-c(table))

  parsed_qa <- wh %>%
    dplyr::left_join(qa_lines, by = "grouping") %>%
    dplyr::select(-grouping)

  return(parsed_qa)

}


qa_file <- function(filepath) { #TODO add status messages as to what is happening

  code_file <- fs::path_file(filepath)

  code_path <- fs::path_dir(filepath)

  project_path <- rstudioapi::getActiveProject()

  project_name <- stringr::str_extract(project_path, "(?!(.*\\/)).*")

  if(code_path == project_path) { #If we're working with a script in the project root, it gets a QA sheet with the same name as the project

    qa_file <- fs::path(code_path, "QA", paste0("QA_", project_name, ".xlsx"))

    fs::dir_create(fs::path_dir(qa_file)) #function ignores command if dir already exists

  } else if(str_detect(code_path, project_path)) { #If the script is in a subdir of project root, it gets a QA sheet with the name of the subdir.

    code_subfolder <- str_remove(code_path, project_path) %>%
      str_remove("/")

    qa_file <- fs::path(code_path, "QA", paste0("QA_", code_subfolder, ".xlsx"))

    fs::dir_create(fs::path_dir(qa_file)) #function ignores command if dir already exists
  }

  return(qa_file)
}

qa_wb <- function(filepath, qa_file) {

  code_file <- fs::path_file(filepath)

  has_existing_qa_file <- fs::file_exists(qa_file)

  if(has_existing_qa_file) {

    if(code_file %in% openxlsx::getSheetNames(qa_file)) {

      cli::cli_alert_warning("A QA file and code review worksheet for this script already exists. If you continue, the worksheet code review section will be over-written (checklist and other sheets will not be affected).") #TODO: Check if there is any user-entered changes that will be deleted, and either make sure they aren't by lining up the QA tags, or prompt the user about this.  For now we are prompting every time regarding overwrite.

      user_overwrite <- usethis::ui_yeah("Do you want to proceed and overwrite the existing QA code review section?", shuffle = F)

      if(user_overwrite) {

        qawb <- openxlsx::loadWorkbook(qa_file)

        backup_sheet <- paste(code_file, lubridate::now()) %>% stringr::str_remove_all("-|:") #TODO: Need to limit chars to 31, so need better naming
        backup_sheet <- abbreviate(backup_sheet, 31) #FIXME temporary until above is fixed!

        openxlsx::cloneWorksheet(qawb, backup_sheet, clonedSheet = code_file)

        openxlsx::sheetVisibility(qawb)[sheetNamesIndex(qawb, backup_sheet)] <- "hidden"


        } else stop("User exited.") #TODO Do something better here

    } else { #Sheet does not exist but QA file does
      cli::cli_alert_info('A QA file for for the scripts in this directory already exists, but a worksheet for this script does not. It will be added as a new spreadsheet (named "{maybe_qa_sheet}") in the file: {qa_file}')

      qawb <- openxlsx::loadWorkbook(qa_file)

      openxlsx::cloneWorksheet(qawb, code_file, clonedSheet = "Code_Review_Template")
      openxlsx::sheetVisibility(qawb)[sheetNamesIndex(qawb, code_file)] <- "visible"
    }

  } else { #No QA sheet exists yet

    cli::cli_alert_info("A QA file for this script was not detected. A new one will be created: {qa_file}, and a worksheet for the script {code_file} will be added.")

    qawb <- openxlsx::loadWorkbook(fs::path_package("integral", "extdata/QA_Template_Coded_Analysis.xlsx"))

    openxlsx::cloneWorksheet(qawb, code_file, clonedSheet = "Code_Review_Template")
    openxlsx::sheetVisibility(qawb)[sheetNamesIndex(qawb, code_file)] <- "visible"

  }

  return(qawb)
}


#Internal function to translate sheet name to sheet index for openxlsx functions that don't accept names.
sheetNamesIndex <- function(qawb, lookup) {

  name_ind <- tibble::enframe(names(qawb), name = "index", value = "name")

  if(is.character(lookup)) {
    name_ind %>%
      dplyr::filter(name == !!lookup) %>%
      dplyr::pull(index)
  } else
  if(is.numeric(lookup)) {
    name_ind %>%
      dplyr::filter(index == name_ind) %>%
      dplyr::pull(name)
  } else stop("Sheet name or index number does not exist in workbook.")
}



qa_update_sheet <- function(qawb, parsed_qa, filepath, qa_file) {

  code_file <- fs::path_file(filepath)

  qa_file <- qa_file(filepath)

  openxlsx::writeData(qawb, code_file, parsed_qa, startRow = 14, colNames = F)

  openxlsx::saveWorkbook(qawb, qa_file, overwrite = TRUE)
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




#prefixer::prefixer()












