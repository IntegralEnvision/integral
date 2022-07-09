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
  if (rlang::is_missing(filepath)) {
    filepath <- rstudioapi::selectFile(
      caption = "Select File to QA",
      label = "Run QA",
      path = rstudioapi::getActiveProject(),
      filter = "R files (*.R | *.Rmd)",
      existing = TRUE
    )
  }

  filepath <- fs::path_real(filepath)

  if (!fs::file_exists(filepath)) stop(cli::cli_alert_danger(paste0("File `", filepath, "` does not exist. If it is not in the root project directory, specify the path relative to the root project directory."), wrap = T))

  if (!stringr::str_detect(stringr::str_to_lower(filepath), ".*\\.(r|rmd|py)$")) stop(cli::cli_alert_danger(paste0("File `", filepath, "` is not an .R, .Rmd, or .py file."), wrap = T))


  # TODO: Check whether file has unsaved changes, and even perhaps if it's been committed. If not, prompt the user to do so.  If they have unsaved changes, they will be lost by functions that modify the script QA tags. Currently this does not appear to be possible: https://github.com/rstudio/rstudioapi/issues/230

  qafile <- qa_file(filepath)

  qawb <- qa_wb(filepath, qafile)

  parsed_qa <- qa_parse(filepath, include_empty_sections = T) # TODO: maybe add to args?

  qa_update_sheet(qawb, parsed_qa, filepath, qafile)

  # openxlsx::openXL(qawb)
  if (get_system() == "linux") {
    cli::cli_alert_info("Temporarily not opening the workbook for testing on linux")
    # set the sytem path a la https://askubuntu.com/a/1374700
    # Sys.setenv("LD_LIBRARY_PATH" = paste0("/usr/lib/libreoffice/program:/usr/lib/x86_64-linux-gnu/:$", "LD_LIBRARY_PATH"))
    # system2("xdg-open", paste0("'", qafile, "'"))
  } else {
    system2("open", paste0("'", qafile, "'"))
  }
}
qa_file <- function(filepath) { # TODO add status messages as to what is happening

  #code_file <- fs::path_file(filepath) #not used?

  code_path <- fs::path_dir(filepath)

  if (rstudioapi::isAvailable()) {
    project_path <- rstudioapi::getActiveProject()
    if (!stringr::str_detect(code_path, project_path)) { # If an Rstudio project is active, but the file is not in it or a subdir
      cli::cli_alert_warning("Warning: The script file being QA'd is not in the active RStudio project directory.")
      project_path <- code_path
    }
  } else if (is.null(project_path)) project_path <- code_path # if outside of an Rproj, we just use the parent dir name.

  project_name <- stringr::str_extract(project_path, "(?!(.*\\/)).*")

  qafile <- fs::path(project_path, "QA", paste0("QA_", project_name, ".xlsx"))
  fs::dir_create(fs::path_dir(qafile)) # function ignores command if dir already exists

  if (code_path == project_path) { # If we're working with a script in the project root, it gets a QA sheet with the same name as the project. Same if the script is outside of the project path or there is no active project.

    qafile <- fs::path(code_path, "QA", paste0("QA_", project_name, ".xlsx"))

    fs::dir_create(fs::path_dir(qafile)) # function ignores command if dir already exists
  } else if (stringr::str_detect(code_path, project_path)) { # If the script is in a subdir of project root, it gets a QA sheet with the name of the subdir.

    code_subfolder <- stringr::str_remove(code_path, project_path) %>%
      stringr::str_remove("/")

    qafile <- fs::path(code_path, "QA", paste0("QA_", code_subfolder, ".xlsx"))

    fs::dir_create(fs::path_dir(qafile)) # function ignores command if dir already exists
  }

  return(qafile)
}

qa_wb <- function(filepath, qafile) {
  sheet <- fs::path_file(filepath)


  # Update/Add QA sheet ------


  has_existing_qafile <- fs::file_exists(qafile)

  if (has_existing_qafile) {
    if (sheet %in% openxlsx::getSheetNames(qafile)) {
      cli::cli_alert_warning("A QA file and code review worksheet for this script already exists. If you continue, the worksheet code review section will be over-written (checklist and other sheets will not be affected).", wrap = T) # TODO: Check if there is any user-entered changes that will be deleted, and either make sure they aren't by lining up the QA tags, or prompt the user about this.  For now we are prompting every time regarding overwrite.

      user_overwrite <- usethis::ui_yeah("Do you want to proceed and overwrite the existing QA code review section?", shuffle = F)

      if (user_overwrite) {
        qawb <- openxlsx::loadWorkbook(qafile)

        backup_sheet <- paste(sheet, lubridate::now()) %>% stringr::str_remove_all("-|:") # TODO: Need to limit chars to 31, so need better naming
        backup_sheet <- abbreviate(backup_sheet, 31) # FIXME temporary until above is fixed!

        openxlsx::cloneWorksheet(qawb, backup_sheet, clonedSheet = sheet)

        openxlsx::sheetVisibility(qawb)[sheetNamesIndex(qawb, backup_sheet)] <- "hidden"
      } else {
        stop("User exited.")
      } # TODO Do something better here
    } else { # Sheet does not exist but QA file does
      cli::cli_alert_info("A QA file for for the scripts in this directory already exists, but a worksheet for this script does not. It will be added as a new spreadsheet (named {crayon::italic({sheet})}) in the file: {.file {qafile}}", wrap = T)

      qawb <- openxlsx::loadWorkbook(qafile)

      openxlsx::cloneWorksheet(qawb, sheet, clonedSheet = "Code_Review_Template")
      openxlsx::sheetVisibility(qawb)[sheetNamesIndex(qawb, sheet)] <- "visible"
    }
  } else { # No QA sheet exists yet

    cli::cli_alert_info("A QA file for this script was not detected. A new one will be created and a worksheet for the script {sheet} will be added: {qafile}.", wrap = T)

    qawb <- openxlsx::loadWorkbook(fs::path_package("integral", "extdata/QA_Template_Coded_Analysis.xlsx"))

    openxlsx::cloneWorksheet(qawb, sheet, clonedSheet = "Code_Review_Template")
    openxlsx::sheetVisibility(qawb)[sheetNamesIndex(qawb, sheet)] <- "visible"
  }

  return(qawb)
}

qa_parse <- function(filepath, include_empty_sections = TRUE) {
  filetype <- tools::file_ext(filepath) %>% stringr::str_to_lower()

  script_qa <- fs::path_expand(filepath)

  all_code <- readr::read_lines(script_qa) %>%
    tibble::enframe(name = "line", value = "code")

  # Detect QA tags
  all_code <- all_code %>%
    dplyr::mutate(is_qa = stringr::str_detect(code, "\\s*#\\s?QA") & !stringr::str_detect(code, "^#+.*-{4,}")) # matches any #QA (with varied spacing) unless is has 4 or more dashes

  if (!any(all_code %>%
    dplyr::pull(is_qa))) {
    stop("The file does not contain any recognized QA tags. Tags should start with '# QA' or `#QA`.")
  }

  # Format QA tags.  See https://regex101.com/r/W68Elc/1 ----
  all_code <- all_code %>%
    dplyr::mutate(code = dplyr::if_else(is_qa,
      stringr::str_replace(code, "(\\s*)(#\\s?QA)(:?\\s?)\\s?(\\d{0,5}\\s?)(\\|?\\s?)(\\s?.*)", "\\1# QA: \\4\\| \\6"), code
    )) %>%
    dplyr::mutate(qa_id = stringr::str_extract(code, "(?<=# QA: )\\d+") %>% as.numeric()) %>%
    dplyr::mutate(is_missing_id = is.na(qa_id) & is_qa) %>%
    dplyr::add_count(qa_id, name = "duplicates") %>%
    dplyr::mutate(is_duplicate = duplicates > 1 & is_qa & !is_missing_id) %>%
    dplyr::mutate(duplicates = duplicates * is_duplicate) # set non-dupes value to 0 by multiplying by FALSE


  # Check for QA tags and QA tags without IDs ----
  if (any(all_code$is_missing_id)) {
    cli::cli_alert_danger("The file is missing QA ID numbers for one or more QA tags.  Would you like them to be added automatically? This will modify your script.", wrap = T)

    print(all_code %>% dplyr::filter(is_qa & is_missing_id) %>% dplyr::mutate(code = stringr::str_squish(code)) %>% dplyr::select(line, code, qa_id))

    add_ids <- usethis::ui_yeah(cli::cli_text("\n\nAdd QA ID numbers to {.file (fs::path_file(script_qa))}?"), yes = "Yes, add QA ID numbers.", no = "No, do not add QA ID numbers.", shuffle = F)
    if (add_ids) {
      available_ids <- dplyr::setdiff(seq(1000, 9999), all_code$qa_id)
      new_ids <- tibble::enframe(sample(available_ids, nrow(all_code %>% dplyr::filter(is_missing_id)), replace = F), name = "missing_join_id", value = "qa_id")

      all_code <- all_code %>%
        dplyr::group_by(is_missing_id) %>%
        dplyr::mutate(missing_join_id = cumsum(is_missing_id)) %>%
        dplyr::ungroup() %>%
        dplyr::rows_update(new_ids, by = "missing_join_id") %>%
        dplyr::mutate(code = dplyr::if_else(is_missing_id, stringr::str_replace(code, "\\|", paste0(qa_id, " \\|")), code))

      cli::cli_alert_success("QA ID numbers have been added.", wrap = T)

      print(all_code %>% dplyr::filter(is_qa & is_missing_id) %>% dplyr::mutate(code = stringr::str_squish(code)) %>% dplyr::select(line, code, qa_id))

      rewrite_code <- T # Set flag for if the file needs to be written to.
    } else {
      cli::cli_alert_info("Please add IDs manually to the lines above and re-run {.code qa()}. Alternatively, re-run {.code qa()} and select the option to automatically add missing IDs when prompted.", wrap = T)
      stop_quietly()
    }
    # TODO exit but not error
  } else {
    cli::cli_alert_success("No missing QA ID's found.")
  }


  # Check for duplicate QA IDs and fix if user approves ----
  if (any(all_code$is_duplicate)) {
    cli::cli_alert_danger("Duplicate QA ID's detected.  Would you like to automatically replace them with unique IDs? Doing so may unlink responses to these QAs if they exist in the QA sheet.\n\n\n", wrap = T) # TODO: Once I pull data from an existing sheet, should check on existence of responses to existing IDs that would be changed.

    print(all_code %>% dplyr::mutate(code = stringr::str_squish(code)) %>% dplyr::filter(is_duplicate) %>% dplyr::select(line, code, qa_id, duplicates) %>% dplyr::arrange(qa_id, line)) # Show duplicates and counts

    replace_dupes <- usethis::ui_yeah("\n\nReplace duplicate QA ID's?", shuffle = F, yes = "Yes, automatically replace IDs", no = "No, do not change IDs (I will change them manually.")

    if (replace_dupes) {
      available_ids <- dplyr::setdiff(seq(1000, 9999), all_code$qa_id) # don't suggest ids that exist
      new_ids <- tibble::enframe(sample(available_ids, nrow(all_code %>% dplyr::filter(is_duplicate)), replace = F), name = "dupe_join_id", value = "qa_id")

      all_code <- all_code %>%
        dplyr::group_by(is_duplicate) %>%
        dplyr::mutate(dupe_join_id = cumsum(is_duplicate)) %>%
        dplyr::ungroup() %>%
        dplyr::rows_update(new_ids, by = "dupe_join_id") %>%
        dplyr::mutate(code = dplyr::if_else(is_duplicate, stringr::str_replace(code, "\\d* \\|", paste0(qa_id, " \\|")), code))

      cli::cli_alert_success("Duplicate QA ID's have been replaced with unique IDs.", wrap = T)

      print(all_code %>% dplyr::mutate(code = stringr::str_squish(code)) %>% dplyr::filter(is_duplicate) %>% dplyr::select(line, code, qa_id))

      rewrite_code <- T # Set flag for if the file needs to be written to.
    } else {
      stop("User exited")
    } # TODO Exit without error
  } else {
    cli::cli_alert_success("No duplicate QA ID's found.")
  }


  all_code <- all_code %>%
    dplyr::select(-dplyr::matches(c("is_duplicate", "duplicates", "is_missing_id", "dupe_join_id", "missing_join_id")))

  if (exists("rewrite_code") && rewrite_code) { # If any changes have been made to the code that need to be written back to the file
    if (!fs::dir_exists(fs::path(fs::path_dir(filepath), "backup"))) fs::dir_create(fs::path(fs::path_dir(filepath), "backup"))
    fs::file_copy(filepath, fs::path(fs::path_dir(filepath), "backup", paste0(fs::path_sanitize(lubridate::now()), " - ", fs::path_file(filepath))))

    readr::write_lines(all_code$code, filepath)
    cli::cli_alert_success("Your script QA IDs have been modified. A backup has been created in the 'backups' subdirectory.", wrap = T)
  }

  if (filetype == "rmd") {
    all_code <- all_code %>% # Add flag to indicate whether the lines are in a text chunk. We use this later when parsing section headers
      dplyr::mutate(chunk_deliniator = stringr::str_detect(code, "^```")) %>%
      dplyr::mutate(chunk_num = cumsum(chunk_deliniator)) %>%
      dplyr::mutate(is_text_chunk = chunk_num %% 2 == 0 & !chunk_deliniator) %>%
      dplyr::select(-c(chunk_deliniator, chunk_num)) %>%
      dplyr::mutate(code = dplyr::if_else(stringr::str_detect(code, "^<!--"), "", code)) # NOTE: decision made here to ignore all HTML commented out lines. They are replaced with blanks so that the row numbers don't shift.
  } else {
    all_code <- all_code %>% tibble::add_column(is_text_chunk = F)
  }

  headers <- all_code %>%
    # dplyr::mutate(is_code_header = stringr::str_detect(code, "(#+)[^\\t]([a-zA-Z0-9\\(\\)&\\s]*)(?=-+)")) %>% #https://regex101.com/r/L9A1VJ/1
    dplyr::mutate(is_code_header = stringr::str_detect(code, "^\\s*#+.*-{4,}")) %>%
    dplyr::mutate(is_text_header = is_text_chunk & stringr::str_detect(code, "(#+)\\s(.*?)")) %>%
    dplyr::filter(is_code_header | is_text_header) %>%
    dplyr::filter(!stringr::str_detect(code, "COPYRIGHT|PURPOSE|PROJECT INFORMATION|HISTORY|NOTES")) %>% # Remove codeless header sections
    dplyr::mutate(code = stringr::str_remove_all(code, "-{4,}") %>%
      stringr::str_squish()) %>%
    dplyr::mutate(section_title = stringr::str_extract(code, "(?<=#\\s).*")) %>%
    dplyr::mutate(section_level = stringr::str_count(code, "#")) %>%
    dplyr::select(line, section_title, section_level)

  wh <- headers %>% # wide headers
    tidyr::pivot_wider(names_from = section_level, values_from = section_title, names_prefix = "level_")

  has_headers <- nrow(wh) > 0

  if (has_headers) { # If there are any section headers, fill down to next one

    section_depth <- headers %>%
      dplyr::summarize(section_depth = max(section_level)) %>%
      dplyr::pull(section_depth)



    for (i in seq(section_depth)) {
      wh <- wh %>%
        tidyr::fill(paste0("level_", i), .direction = "down") %>%
        dplyr::group_by_at(paste0("level_", i), .add = T)
    }

    wh <- wh %>% dplyr::ungroup()
  }


  for (missingcol in dplyr::setdiff(c("level_1", "level_2", "level_3", "level_4"), names(wh))) { # add empty columns to keep column alignment in worksheet
    wh <- wh %>% tibble::add_column(!!rlang::sym(missingcol) := NA_character_)
  }


  qa_lines <- all_code %>%
    dplyr::select(-c(is_text_chunk)) %>% # TODO: check that the new code for dupes and missings works with RMD, we didn't filter text chunks
    dplyr::filter(is_qa) %>%
    dplyr::mutate(comment = stringr::str_extract(code, "(?<=\\|\\s).*"))

  if (has_headers) {

    # Determine the location of QA lines within sections
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
      dplyr::select(tidyselect::starts_with("level_"), grouping)

    qa_lines <- qa_lines %>%
      dplyr::left_join(m_ind %>%
        dplyr::filter(table == "qa"), by = "line") %>%
      dplyr::select(-c(table))

    parsed_qa <- wh %>%
      dplyr::left_join(qa_lines, by = "grouping")
  } else { # add empty columns for sections

    cols_wh <- wh %>%
      dplyr::select(-line) %>%
      names()

    parsed_qa <- dplyr::bind_cols(setNames(rep(list(NA), length(cols_wh)), cols_wh), qa_lines)
  }

  parsed_qa <- parsed_qa %>%
    dplyr::select(tidyselect::starts_with("level_"), line, qa_id, comment)

  if (include_empty_sections) {
    parsed_qa <- parsed_qa %>%
      dplyr::filter(!is.na(line)) # remove any sections that don't have QA tags
  }

  return(parsed_qa)
}

qa_update_sheet <- function(qawb, parsed_qa, filepath, qafile) {
  sheet <- fs::path_file(filepath)

  openxlsx::removeCellMerge(qawb, sheet, rows = 14:100, cols = 1:4) # TODO: finish this with the sheetNamesIndex?
  suppressWarnings(openxlsx::deleteNamedRegion(qawb, "populated_data"))

  openxlsx::writeData(qawb, sheet, parsed_qa, startRow = 14, colNames = F, borders = "all", name = "populated_data")


  for (levcol in c("level_1", "level_2", "level_3", "level_4")) {
    merge_rows <- index_identical_rows(parsed_qa, !!rlang::sym(levcol), startRow = 14)
    colnum <- readr::parse_number(levcol)

    for (i in seq(merge_rows)) {
      openxlsx::mergeCells(qawb, sheet, rows = merge_rows[[i]]["start"]:merge_rows[[i]]["end"], cols = colnum)
      openxlsx::addStyle(qawb, sheet, style = openxlsx::createStyle(valign = "center"), rows = merge_rows[[i]]["start"]:merge_rows[[i]]["end"], cols = colnum, stack = T)
    }

    border_sections <- parsed_qa %>%
      dplyr::summarize(!!rlang::sym(levcol) := which(is.na(!!rlang::sym(levcol)))) %>%
      dplyr::pull(!!rlang::sym(levcol))
    border_sections <- border_sections + 13
    openxlsx::addStyle(qawb, sheet, style = openxlsx::createStyle(border = "TopBottomLeftRight", borderStyle = c("thin", "thin", "thin", "thin")), rows = border_sections, cols = colnum, gridExpand = T)
  }

  openxlsx::addStyle(qawb, sheet, style = openxlsx::createStyle(wrapText = T), rows = 14:200, cols = 7, stack = T, gridExpand = T)


  openxlsx::activeSheet(qawb) <- sheet # TODO put sheet after Instructions. #openxlsx::worksheetOrder(), but do I have to overwrite?

  openxlsx::saveWorkbook(qawb, qafile, overwrite = TRUE)
}


# Internal function to translate sheet name to sheet index for openxlsx functions that don't accept names.
sheetNamesIndex <- function(qawb, lookup) {
  name_ind <- tibble::enframe(names(qawb), name = "index", value = "name")

  if (is.character(lookup)) {
    name_ind %>%
      dplyr::filter(name == !!lookup) %>%
      dplyr::pull(index)
  } else
  if (is.numeric(lookup)) {
    name_ind %>%
      dplyr::filter(index == name_ind) %>%
      dplyr::pull(name)
  } else {
    stop("Sheet name or index number does not exist in workbook.")
  }
}



stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
