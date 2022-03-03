#' Show a table in Excel
#' @description
#' `r lifecycle::badge('experimental')`
#' Opens the table in a temporary file in excel.  Currently only works on Mac OS.
#' @param .data A table.
#' @param add_rownames Logical. Convert rownames to column? Default: TRUE
#' @examples
#' \dontrun{
#' diamonds %>% xview()
#' }
#' @export

xview <- function (.data, add_rownames = TRUE) {

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

  if(add_rownames & tibble::has_rownames(data)) .data <- .data %>% tibble::rownames_to_column()

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
#' @param add_rownames Logical. Convert rownames to column? Default: TRUE
#' @param auto_conditional Automatically add conditional formatting. Default: TRUE
#' @examples
#' \dontrun{
#' diamonds %>% xview2()
#' }
#' @export
xview2 <- function (.data, add_rownames = TRUE, auto_conditional = TRUE) {


  if(length(dim(.data))>2){
    stop('Too many dimensions')
  }
  if(is.null(dim(.data))){
    .data = as.data.frame(.data)
  }

  if(add_rownames & tibble::has_rownames(.data)) .data <- .data %>% tibble::rownames_to_column() else
    .data <- .data %>% mutate(rownum = row_number(), .before = 1)



  if(add_rownames & tibble::has_rownames(data)) .data <- .data %>% tibble::rownames_to_column()

  wb <- createWorkbook()

  addWorksheet(wb, "xview")


  writeData(wb, "xview", .data)

  addFilter(wb, 1, rows = 1, cols = 1:ncol(.data))

  freezePane(wb, 1, firstRow = T, firstCol = T)

  if(auto_conditional) {
    colclasses <- lapply(.data, class) %>%
      as_tibble %>%
      slice(1) %>%
      pivot_longer(everything()) %>%
      mutate(colnum = row_number()) %>%
      filter(name != "rowname") %>%
      filter(name != "rownum")

    colclasses %>%
      rowwise() %>%
      group_split() %>%
      walk(function(x) {
        switch(x$value,
               numeric = conditionalFormatting(wb, 1, cols = x$colnum, rows = 2:nrow(.data) + 1,
                                               type = "colourScale",
                                               style = c("cyan", "white", "darkorchid1")),

               integer = conditionalFormatting(wb, 1, cols = x$colnum, rows = 2:nrow(.data) + 1,
                                               type = "colourScale",
                                               style = c("cyan", "white", "darkorchid1")),

               ordered = conditionalFormatting(wb, 1, cols = x$colnum, rows = 2:nrow(.data) + 1,
                                               type = "colourScale",
                                               style = c("#edf8b1", "#7fcdbb", "#2c7fb8")),
        )
      })
  }

  openxl_temp <- temp_xlsx()
  saveWorkbook(wb, openxl_temp)

  openXL(openxl_temp)


}
