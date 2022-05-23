# all_code <- z
# all_code <- x #rmd
# all_code <- y #r

qa <- function(filepath) {

  if(!fs::file_exists(filepath)) stop(cli::cli_alert_danger(paste0("File `", filepath, "` does not exist. If it is not in the root project directory, specify the path relative to the root project directory.")))

  if(!str_detect(str_to_lower(filepath), ".*\\.(r|rmd)$")) stop(cli::cli_alert_danger(paste0("File `", filepath, "` is not an .R or .Rmd file.")))

  filepath <- path_real(filepath)

  parsed_qa <- qa_parse(filepath)

  qawb <- qa_wb_load(filepath)

  }


qa_parse <- function(filepath) {



  filetype <- tools::file_ext(filepath) %>% str_to_lower()

  script_qa <- fs::path_expand(filepath)

  all_code <- readr::read_lines(script_qa) %>%
    tibble::enframe(name = "line", value = "code") %>%
    tibble::add_column(file_full = as.character(script_qa))

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

qa_wb_load <- function(filepath) {


  code_file_only <- path_file(filepath)

  code_path_only <- path_dir(filepath)

  project_path <- rstudioapi::getActiveProject()

  project_name <- str_extract(project_path, "(?!(.*\\/)).*")

  if(path_only == project_path) { #If we're working with a script in the project root, it gets a QA sheet with the same name as the project

    maybe_qa_file <- path(code_path_only, "QA", paste0("QA_", project_name, ".xlsx"))

  } else if(str_detect(code_path_only, project_path)) { #If the script is in a subdir of project root, it gets a QA sheet with the name of the subdir.

    code_subfolder <- str_remove(code_path_only, project_path) %>%
      str_remove("/")

    maybe_qa_file <- path(code_path_only, "QA", paste0("QA_", code_subfolder, ".xlsx"))
  }

  has_existing_qa_file <- fs::file_exists(maybe_qa_file)

  if(has_existing_qa_file) {

    if(maybe_qa_sheet %in% getSheetNames(maybe_qa_file)) {

      cli::cli_alert_info("A QA file and code review worksheet for this script already exists. It will be updated")

      qawb <- loadWorkbook(maybe_qa_file)

    } else {
      #TODO: The new worksheet will need to be a copy of the one from the template file, not blank.  Could either load it from template, or have the initial template with a hidden sheet that is used as the template.
      cli::cli_alert_info('A QA file for for the scripts in this directory already exists, but a worksheet for this script does not. It will be added as a new spreadsheet (named "{maybe_qa_sheet}") in the file: {maybe_qa_file}')

      qawb <- loadWorkbook(maybe_qa_file)

      addWorksheet(qawb, code_file_only) #TODO: as above, need to start with the template, not a blank worksheet.

    }

  } else {
    cli::cli_alert_info("A QA file for this script was not detected. A new one will be created: {maybe_qa_file}, and a worksheet for the script will be added")
    qawb <- loadWorkbook(fs::path_package("integral", "extdata/QA_Template_Coded_Analysis.xlsx"))
  }

  return(qawb)
}

qawb <- qa_wb_load(filepath)



