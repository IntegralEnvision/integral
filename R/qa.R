#' Automated QA form population
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Scrape an .R or .Rmd file for QA tags, and populate the Integral QA sheet for scripted analyses.
#'
#' To use, tag QA review items by commenting:
#'
#'    QA: Review comment / request"
#'
#' @section Function Process:
#'
#' The function will perform the following actions:
#'
#' \itemize{
#' \item Scrape the file for section headers and '# QA:' comment tags
#' \item Assign unique ID's to QA tags in the code (the file will be modified) if they do not have one.
#' \item Create a QA subdirectory and a QA excel workbook if one does not exist
#' \item Add a code review spreadsheet for the file if one does not exist; update the spreadsheet for the file if one already exists
#' }
#'
#'
#' @usage qa(filepath = "youfile.R")
#'
#' @param filepath The file to QA. Can include either an absolute path or a relative path (including "~" home references).
#' @export

qa <- function(filepath) {

  filepath <- fs::path_real(filepath)

  if(!fs::file_exists(filepath)) stop(cli::cli_alert_danger(paste0("File `", filepath, "` does not exist. If it is not in the root project directory, specify the path relative to the root project directory.")))

  if(!stringr::str_detect(stringr::str_to_lower(filepath), ".*\\.(r|rmd)$")) stop(cli::cli_alert_danger(paste0("File `", filepath, "` is not an .R or .Rmd file.")))


  qafile <- qa_file(filepath)

  qawb <- qa_wb(filepath, qafile)

  parsed_qa <- qa_parse(filepath)

  qa_update_sheet(qawb, parsed_qa, filepath, qafile)

  openxlsx::openXL(qawb)

  }

qa_file <- function(filepath) { #TODO add status messages as to what is happening

  code_file <- fs::path_file(filepath)

  code_path <- fs::path_dir(filepath)

  project_path <- rstudioapi::getActiveProject()

  if(is.null(project_path)) project_path <- code_path #if outside of an Rproj, we just use the parent dir name.

  project_name <- stringr::str_extract(project_path, "(?!(.*\\/)).*")

  if(code_path == project_path) { #If we're working with a script in the project root, it gets a QA sheet with the same name as the project

    qa_file <- fs::path(code_path, "QA", paste0("QA_", project_name, ".xlsx"))

    fs::dir_create(fs::path_dir(qa_file)) #function ignores command if dir already exists

  } else if(stringr::str_detect(code_path, project_path)) { #If the script is in a subdir of project root, it gets a QA sheet with the name of the subdir.

    code_subfolder <- stringr::str_remove(code_path, project_path) %>%
      stringr::str_remove("/")

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
        dplyr::mutate(has_id = stringr::str_detect(code, "(QA: )(\\d+)(.*)")) %>%
        dplyr::mutate(missing_id = stringr::str_detect(code, "#\\s?QA:\\s(?=\\D)")) %>%
        dplyr::rowwise() %>%
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
      dplyr::select(-c(chunk_deliniator, chunk_num)) %>%
      dplyr::mutate(code = dplyr::if_else(stringr::str_detect(code, "^<!--"), "", code)) # NOTE: decision made here to ignore all HTML commented out lines. They are replaced with blanks so that the row numbers don't shift.
  } else if(filetype == "r") all_code <- all_code %>% tibble::add_column(is_text_chunk = F)

  headers <- all_code %>%
    dplyr::mutate(is_code_header = stringr::str_detect(code, "(#+)[^\\t]([a-zA-Z0-9\\(\\)&\\s]*)(?=-+)")) %>%
    dplyr::mutate(is_text_header = is_text_chunk & stringr::str_detect(code, "(#+)\\s(.*?)")) %>%
    dplyr::mutate(code = stringr::str_remove_all(code, "-{2,}")) %>%
    dplyr::filter(is_code_header | is_text_header) %>%
    dplyr::mutate(section = stringr::str_match(code, "(#+)\\s(.*)")) %>%
    dplyr::mutate(section = magrittr::set_colnames(section, c("full_string", "hash", "title"))) %>%
    dplyr::mutate(section = dplyr::as_tibble(section)) %>%
    tidyr::unnest_wider(section, names_sep = "_") %>%
    # dplyr::rename(full_string = section_V1,
    #        hash = section_V2,
    #        title = section_V3) %>%
    # dplyr::mutate(full_string = stringr::str_squish(full_string),
    #        title = stringr::str_squish(title)) %>%
    dplyr::mutate(section_level = stringr::str_count(section_hash, "#")) %>%
    dplyr::select(-c(is_text_header, is_code_header, section_full_string, section_hash, is_text_chunk))

  section_depth <- headers %>% dplyr::summarize(section_depth = max(section_level)) %>% dplyr::pull(section_depth)

  wh <- headers %>% #wide headers
    dplyr::select(-file_full) %>%
    tidyr::pivot_wider(names_from = section_level, values_from = section_title, names_prefix = "level_")

  for(i in seq(section_depth)) {
    wh <- wh %>%
      tidyr::fill(paste0("level_", i), .direction = "down") %>%
      dplyr::group_by_at(paste0("level_", i), .add = T)
  }

  wh <- wh %>% dplyr::ungroup()


  for(missingcol in setdiff(c("level_1", "level_2", "level_3", "level_4"), names(wh))) { #add empty columns to keep column alignment in worksheet
    wh <- wh %>% add_column(!!sym(missingcol) := NA_character_)
  }


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

  parsed_qa <- parsed_qa %>%
    dplyr::filter(!is.na(line)) #remove any sections that don't have QA tags

  return(parsed_qa)

}

qa_update_sheet <- function(qawb, parsed_qa, filepath, qa_file) {

  code_file <- fs::path_file(filepath)


  openxlsx::writeData(qawb, code_file, parsed_qa, startRow = 14, colNames = F)

  for(levcol in c("level_1", "level_2", "level_3", "level_4")) {

    merge_rows <- index_identical_rows(parsed_qa, !!sym(levcol), startRow = 14)
    colnum <- readr::parse_number(levcol)

    for(i in seq(merge_rows)) {
      openxlsx::mergeCells(qawb, code_file, rows = merge_rows[[i]]["start"]:merge_rows[[i]]["end"], cols = colnum)
      openxlsx::addStyle(qawb, code_file, style = openxlsx::createStyle(valign = "center"), rows = merge_rows[[i]]["start"]:merge_rows[[i]]["end"], cols = colnum, stack = T)
    }

    unborder_rows <- parsed_qa %>% summarize(!!sym(levcol) := which(is.na(!!sym(levcol)))) %>% pull(!!sym(levcol))
    unborder_rows <- unborder_rows + 13
    openxlsx::addStyle(qawb, code_file, style = createStyle(border = "TopBottomLeftRight", borderStyle = c("thin", "thin", "none", "none")), rows = unborder_rows, cols = colnum, gridExpand = T)

  }


  openxlsx::activeSheet(qawb) <- code_file

  openxlsx::saveWorkbook(qawb, qa_file, overwrite = TRUE)
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



