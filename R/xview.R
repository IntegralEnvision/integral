#' Show a table in Excel
#' @description
#' `r lifecycle::badge('experimental')`
#' Opens the table in a temporary file in excel.  Currently only works on Mac OS.
#' @param .data A table.
#' @param include_rownames Logical. Convert rownames to column? Default: TRUE
#' @examples
#' \dontrun{
#' diamonds %>% xview()
#' }
#' @export

xview <- function (.data, include_rownames = TRUE) {

  unique_filename_addition <- paste0(sample(c(letters, as.character(1:9)), 4), collapse = "") #Makes it so that excel will open a new window if this is run multiple times in a way that generates the same file name.

  tempFilePath <- paste(tempfile(), ".xlsx")
  tempPath <- dirname(tempFilePath)

  preferredFile <- paste(deparse(substitute(.data)), "_", unique_filename_addition, ".xlsx", sep = "")

  preferredFile <- stringr::str_remove(preferredFile, "^\\._") #in case this was an unnamed object (ie piped)

  preferredFilePath <- file.path(tempPath, preferredFile)


  if(length(dim(.data))>2){
    stop('Too many dimensions')
  }
  if(is.null(dim(.data))){
    .data = as.data.frame(.data)
  }
  #if (is.null(rownames(.data))) {
  #  tmp = 1:nrow(.data)
  # }else {
  #  tmp = rownames(.data)
  #}
  #rownames(.data) = NULL
  #.data = data.frame(RowLabels = tmp, .data)

  if(include_rownames & tibble::has_rownames(.data)) .data <- .data %>% tibble::rownames_to_column()

  WriteAttempt <- try(
    WriteXLS::WriteXLS(as.character(bquote(.data)), ExcelFileName=preferredFilePath, FreezeRow=1, FreezeCol=1, BoldHeaderRow=T, AdjWidth=F, AutoFilter=T, row.names=F),
    silent = TRUE)
  if ("try-error" %in% class(WriteAttempt)) {
    WriteXLS::WriteXLS(as.character(bquote(.data)), ExcelFileName=tempFilePath, FreezeRow=1, FreezeCol=1, BoldHeaderRow=T, AdjWidth=F, AutoFilter=T, row.names=F)
    system2("open", tempFilePath) #TODO: add windows functionality back in replacing this with shell.exec(tempFilePath)
  } else {
    system2("open", preferredFilePath)
  }
}

#' Show a table in Excel
#' @description
#' `r lifecycle::badge('experimental')`
#' Opens the table in a temporary file in excel.
#' @param .data A table.
#' @param include_rownames Logical. Convert rownames to column? Default: TRUE
#' @param auto_conditional Automatically add conditional formatting. Default: TRUE
#' @examples
#' \dontrun{
#' diamonds %>% xview2()
#' }
xview2 <- function (.data, include_rownames = TRUE, auto_conditional = TRUE) {


  if(length(dim(.data))>2){
    stop('Too many dimensions')
  }
  if(is.null(dim(.data))){
    .data = as.data.frame(.data)
  }

  if(include_rownames & tibble::has_rownames(.data)) .data <- .data %>% tibble::rownames_to_column() else
    .data <- .data %>% dplyr::mutate(rownum = dplyr::row_number(), .before = 1)



  if(include_rownames & tibble::has_rownames(.data)) .data <- .data %>% tibble::rownames_to_column()

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "xview")


  openxlsx::writeData(wb, "xview", .data)

  openxlsx::addFilter(wb, 1, rows = 1, cols = 1:ncol(.data))

  openxlsx::freezePane(wb, 1, firstRow = T, firstCol = T)

  #Styles
  numbers <-  c("cyan",  "darkorchid1", "white")
  ordered <- c("#edf8b1", "#7fcdbb", "#2c7fb8")
  logical_false <- createStyle(fontColour = "red")
  logical_true <- createStyle(fontColour = "darkgreen")

  if(auto_conditional) {
    colclasses <- lapply(.data, class) %>%
      tibble::as_tibble() %>%
      dplyr::slice(1) %>%
      tidyr::pivot_longer(tidyselect::everything()) %>%
      dplyr::mutate(colnum = dplyr::row_number()) %>%
      dplyr::filter(name != "rowname") %>%
      dplyr::filter(name != "rownum")

    character_limited <- .data %>% dplyr::summarize(tibble::as_tibble(where(is.character), ~dplyr::n_distinct(.))) %>%  tidyr::pivot_longer(dplyr::everything(), values_to = "distinct_char")

    colclasses <- colclasses %>%
      dplyr::left_join(character_limited) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(value = dplyr::if_else(isTRUE(distinct_char <= 10), "character_limited", value)) %>%
      dplyr::ungroup()

    colclasses %>%
      dplyr::rowwise() %>%
      dplyr::group_split() %>%
      purrr::walk(function(x) {
        switch(x$value,
               numeric = openxlsx::conditionalFormatting(wb, 1, cols = x$colnum, rows = 2:nrow(.data) + 1,
                                               type = "colourScale",
                                               style = numbers),

               integer = openxlsx::conditionalFormatting(wb, 1, cols = x$colnum, rows = 2:nrow(.data) + 1,
                                               type = "colourScale",
                                               style = numbers),

               ordered = openxlsx::conditionalFormatting(wb, 1, cols = x$colnum, rows = 2:nrow(.data) + 1,
                                               type = "colourScale",
                                               style = ordered),
               logical = {
                 openxlsx::conditionalFormatting(wb, 1, cols = x$colnum, rows = 2:nrow(.data) + 1,
                                                          rule = "=='TRUE'", style = logical_true)
                 openxlsx::conditionalFormatting(wb, 1, cols = x$colnum, rows = 2:nrow(.data) + 1,
                                                 rule = "=='FALSE'", style = logical_false)
               },

               character_limited = openxlsx::conditionalFormatting(wb, 1, cols = x$colnum, rows = 2:nrow(.data) + 1, style = ordered, type = "colourScale") #FIXME This is incomplete, will probably need to assign styling to specific cells rather than using conditional formatting.
        )
      })
  }

  openxl_temp <- openxlsx::temp_xlsx()
  openxlsx::saveWorkbook(wb, openxl_temp)

  openxlsx::openXL(openxl_temp)


}
