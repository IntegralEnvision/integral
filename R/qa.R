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
#' @param filepath The file to QA. Can include either an absolute path or a relative path (including "~" home references). If omitted, a file selection dialog box will appear.
#' @export

qa <- function(filepath) {

  if(rlang::is_missing(filepath)) {
    filepath <- rstudioapi::selectFile(
      caption = "Select File to QA",
      label = "Run QA",
      path = rstudioapi::getActiveProject(),
      filter = "R files (*.R | *.Rmd)",
      existing = TRUE
    )
  }

  filepath <- fs::path_real(filepath)

  if(!fs::file_exists(filepath)) stop(cli::cli_alert_danger(paste0("File `", filepath, "` does not exist. If it is not in the root project directory, specify the path relative to the root project directory."), wrap = T))

  if(!stringr::str_detect(stringr::str_to_lower(filepath), ".*\\.(r|rmd|py)$")) stop(cli::cli_alert_danger(paste0("File `", filepath, "` is not an .R, .Rmd, or .py file."), wrap = T))


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

    qafile <- fs::path(code_path, "QA", paste0("QA_", project_name, ".xlsx"))

    fs::dir_create(fs::path_dir(qafile)) #function ignores command if dir already exists

  } else if(stringr::str_detect(code_path, project_path)) { #If the script is in a subdir of project root, it gets a QA sheet with the name of the subdir.

    code_subfolder <- stringr::str_remove(code_path, project_path) %>%
      stringr::str_remove("/")

    qafile <- fs::path(code_path, "QA", paste0("QA_", code_subfolder, ".xlsx"))

    fs::dir_create(fs::path_dir(qafile)) #function ignores command if dir already exists
  }

  return(qafile)
}

qa_wb <- function(filepath, qafile) {

  sheet <- fs::path_file(filepath)

  has_existing_qafile <- fs::file_exists(qafile)

  if(has_existing_qafile) {

    if(sheet %in% openxlsx::getSheetNames(qafile)) {

      cli::cli_alert_warning("A QA file and code review worksheet for this script already exists. If you continue, the worksheet code review section will be over-written (checklist and other sheets will not be affected).", wrap = T) #TODO: Check if there is any user-entered changes that will be deleted, and either make sure they aren't by lining up the QA tags, or prompt the user about this.  For now we are prompting every time regarding overwrite.

      user_overwrite <- usethis::ui_yeah("Do you want to proceed and overwrite the existing QA code review section?", shuffle = F)

      if(user_overwrite) {

        qawb <- openxlsx::loadWorkbook(qafile)

        backup_sheet <- paste(sheet, lubridate::now()) %>% stringr::str_remove_all("-|:") #TODO: Need to limit chars to 31, so need better naming
        backup_sheet <- abbreviate(backup_sheet, 31) #FIXME temporary until above is fixed!

        openxlsx::cloneWorksheet(qawb, backup_sheet, clonedSheet = sheet)

        openxlsx::sheetVisibility(qawb)[sheetNamesIndex(qawb, backup_sheet)] <- "hidden"


      } else stop("User exited.") #TODO Do something better here

    } else { #Sheet does not exist but QA file does
      cli::cli_alert_info('A QA file for for the scripts in this directory already exists, but a worksheet for this script does not. It will be added as a new spreadsheet (named "{sheet}") in the file: {qafile}', wrap = T)

      qawb <- openxlsx::loadWorkbook(qafile)

      openxlsx::cloneWorksheet(qawb, sheet, clonedSheet = "Code_Review_Template")
      openxlsx::sheetVisibility(qawb)[sheetNamesIndex(qawb, sheet)] <- "visible"
    }

  } else { #No QA sheet exists yet

    cli::cli_alert_info("A QA file for this script was not detected. A new one will be created: {qafile}, and a worksheet for the script {sheet} will be added.", wrap = T)

    qawb <- openxlsx::loadWorkbook(fs::path_package("integral", "extdata/QA_Template_Coded_Analysis.xlsx"))

    openxlsx::cloneWorksheet(qawb, sheet, clonedSheet = "Code_Review_Template")
    openxlsx::sheetVisibility(qawb)[sheetNamesIndex(qawb, sheet)] <- "visible"

  }

  return(qawb)
}

qa_parse <- function(filepath) {

  filetype <- tools::file_ext(filepath) %>% stringr::str_to_lower()

  script_qa <- fs::path_expand(filepath)

  all_code <- readr::read_lines(script_qa) %>%
    tibble::enframe(name = "line", value = "code") %>%
    tibble::add_column(file_full = as.character(script_qa))

  #Detect QA tags
  all_code <- all_code %>%
    mutate(is_qa = str_detect(code, "\\s*#\\s?QA"))

  original_code <- all_code

  if(!any(all_code %>%
          dplyr::pull(is_qa))) stop("The file does not contain any recognized QA tags. Tags should start with '# QA' or `#QA`.")

  #Format QA tags.  See https://regex101.com/r/W68Elc/1
  all_code <- all_code %>%
    mutate(code = ifelse(is_qa,
                         str_replace(code, "(\\s*)(#\\s?QA)(:?\\s?)\\s?(\\d{0,5}\\s?)(\\|?\\s?)(\\s?.*)", "\\1# QA: \\4\\| \\6"), code)) %>%
    mutate(qa_id = stringr::str_extract(code, "(?<=# QA: )\\d+") %>% as.numeric()) %>%
    dplyr::mutate(is_missing_id = is.na(qa_id) & is_qa) %>%
    add_count(qa_id, name = "duplicates") %>%
    mutate(is_duplicate = duplicates > 1 & is_qa & !is_missing_id) %>%
    mutate(duplicates = duplicates * is_duplicate)  #set non-dupes value to 0 by multiplying by FALSE


  #Check for duplicate QA IDs and fix if user approves

  if(any(all_code$is_duplicate)) {

    cli::cli_alert_danger("Duplicate QA ID's detected.  Would you like to automatically replace them with unique IDs? Doing so may unlink responses to QA if they exist.\n\n\n", wrap = T) #TODO: Once I pull data from an existing sheet, should check on existence of responses to existing IDs that would be changed.

    print(all_code %>% mutate(code = str_squish(code)) %>% filter(is_duplicate) %>% select(line, code, qa_id, duplicates) %>% arrange(qa_id, line)) #Show duplicates and counts

    replace_dupes <- usethis::ui_yeah("\n\nReplace duplicate QA ID's?", shuffle = F, yes = "Yes, automatically replace IDs", no = "No, do not change IDs (I will change them manually")

    if(replace_dupes) {
      available_ids <- setdiff(seq(1000, 9999), all_code$qa_id) #don't suggest ids that exist
      new_ids <- enframe(sample(available_ids, nrow(all_code %>% filter(is_duplicate)), replace = F), name = "dupe_join_id", value = "qa_id")

      all_code <- all_code %>%
        group_by(is_duplicate) %>%
        mutate(dupe_join_id = cumsum(is_duplicate)) %>%
        ungroup() %>%
        dplyr::rows_update(new_ids, by = "dupe_join_id") %>%
        mutate(code = if_else(is_duplicate, str_replace(code, "\\d* \\|", paste0(qa_id, " \\|")), code))

      cli::cli_alert_success("Duplicate QA ID's have been replaced with unique IDs.", wrap = T)

      print(all_code %>% mutate(code = str_squish(code)) %>% filter(is_duplicate) %>% select(line, code, qa_id))


    } else stop("User exited") #TODO Exit without error
  }


  #Check for QA tags and QA tags without IDs
  if(any(all_code$is_missing_id)) {

    missing_qa_ids <- all_code %>%
      filter(is_qa & is_missing_id) %>%
      mutate(code = str_squish(code)) %>%
      select(line, code)

    cli::cli_alert_danger("The file is missing QA ID numbers for one or more QA tags.  Would you like them to be added automatically? This will modify your script.\n\n", wrap = T)

    print(all_code %>% filter(is_qa & is_missing_id) %>% mutate(code = str_squish(code)) %>% select(line, code, qa_id))

    add_ids <- usethis::ui_yeah("\n\nAdd QA ID numbers to {crayon::bold(fs::path_file(script_qa))}?", yes = "Yes, add QA ID numbers.", no = "No, do not add QA ID numbers.", shuffle = F)
    if(add_ids) {
      available_ids <- setdiff(seq(1000, 9999), all_code$qa_id)
      new_ids <- enframe(sample(available_ids, nrow(missing_qa_ids), replace = F), name = "missing_join_id", value = "qa_id")

      all_code <- all_code %>%
        group_by(is_missing_id) %>%
        mutate(missing_join_id = cumsum(is_missing_id)) %>%
        ungroup() %>%
        dplyr::rows_update(new_ids, by = "missing_join_id") %>%
        mutate(code = if_else(is_missing_id, str_replace(code, "\\|", paste0(qa_id, " \\|")), code))

      cli::cli_alert_success("QA ID numbers have been added.", wrap = T)

      print(all_code %>% filter(is_qa & is_missing_id) %>% mutate(code = str_squish(code)) %>% select(line, code, qa_id))
    } else stop("User exited") #TODO exit but not error
  }

  all_code <- all_code %>%
    select(-c(is_duplicate, duplicates, is_missing_id, dupe_join_id, missing_join_id))

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
    wh <- wh %>% tibble::add_column(!!rlang::sym(missingcol) := NA_character_)
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

qa_update_sheet <- function(qawb, parsed_qa, filepath, qafile) {

  sheet <- fs::path_file(filepath)

  openxlsx::removeCellMerge(qawb, sheet, )

  openxlsx::writeData(qawb, sheet, parsed_qa, startRow = 14, colNames = F)


  for(levcol in c("level_1", "level_2", "level_3", "level_4")) {

    merge_rows <- index_identical_rows(parsed_qa, !!rlang::sym(levcol), startRow = 14)
    colnum <- readr::parse_number(levcol)

    for(i in seq(merge_rows)) {
      openxlsx::mergeCells(qawb, sheet, rows = merge_rows[[i]]["start"]:merge_rows[[i]]["end"], cols = colnum)
      openxlsx::addStyle(qawb, sheet, style = openxlsx::createStyle(valign = "center"), rows = merge_rows[[i]]["start"]:merge_rows[[i]]["end"], cols = colnum, stack = T)
    }

    border_sections <- parsed_qa %>% dplyr::summarize(!!rlang::sym(levcol) := which(is.na(!!rlang::sym(levcol)))) %>% dplyr::pull(!!rlang::sym(levcol))
    border_sections <- border_sections + 13
    openxlsx::addStyle(qawb, sheet, style = openxlsx::createStyle(border = "TopBottomLeftRight", borderStyle = c("thin", "thin", "thin", "thin")), rows = border_sections, cols = colnum, gridExpand = T)

  }


  openxlsx::activeSheet(qawb) <- sheet

  openxlsx::saveWorkbook(qawb, qafile, overwrite = TRUE)
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



