# History
# 2022-05-04. Created Eben Pendleton.

#' @title Utility ask function
#' @name ask
#' @description A function to ask a yes/no question and return TRUE/FALSE
#' `r lifecycle::badge('stable')`
#' @return boolean response from Yes / No
#'
#' @param question input Yes / No question
#' @param default Optionally set the default response value

#' @examples
#' \dontrun{
#' ask("Do you like coding?")
#' }
ask <- function(question, default = TRUE) {
  selection <- if (default) "[Y/n]" else "[y/N]"
  prompt <- sprintf("%s %s: ", question, selection)
  response <- tolower(trimws(readline(prompt)))
# if no response is entered return the default boolean value
if (!nzchar(response)) return(default)
  substring(response, 1L, 1L) == "y"
}
#' @title Utility ask function that returns the response
#' @name askyn_letter
#' @description A function to ask a yes/no question and return Y/N
#' `r lifecycle::badge('stable')`
#' @return Y/y or N/n
#'
#' @param question input Yes / No question
#' @param default Optionally set the default response value

#' @examples
#' \dontrun{
#' askyn_letter("Do you like coding?")
#' }
askyn_letter <- function(question, default = TRUE) {
  selection <- if (default) "[Y/n]" else "[y/N]"
  prompt <- sprintf("%s %s: ", question, selection)
  response <- trimws(readline(prompt))
  # if no response is entered return the default boolean value
  if (!nzchar(response)) {
    if (default) {
      response <- "Y"
    } else {
      response <- "N"
    }
  }
  return(response)
}

#' @title Utility ask function that confirms and returns TRUE / FALSE
#' @name askyn_confirm
#' @description A function to ask a yes/no question and return Y/N after confirming
#' `r lifecycle::badge('stable')`
#' @return Y/y or N/n
#'
#' @param question input Yes / No question
#' @param default Optionally set the default response value

#' @examples
#' \dontrun{
#' ask_yn_confirm("Do you like coding?")
#' }
askyn_confirm <- function(question, default = TRUE) {
  confirm <- F
  response <- askyn_letter(question, default = default)
  while(confirm == F) {
    confirm <- ask(paste0("Is [", response, "] the desired input?"))
    if (confirm == F) {
      response <- askyn_letter(question, default)
    }
  }

  # convert to boolean
  response <- tolower(response)
  substring(response, 1L, 1L) == "y"

}

#' @title Utility ask function that confirms and returns the response
#' @name ask_confirm
#' @description A function to ask a question and return the response after confirming
#' `r lifecycle::badge('stable')`
#' @return response
#'
#' @param question input question

#' @examples
#' \dontrun{
#' ask_confirm("Where do you live?")
#' }


ask_confirm <- function(question) {
  confirm <- F
  prompt <- sprintf("%s :", question)
  response <- trimws(readline(prompt))
  while(confirm == F) {
    confirm <- ask(paste0("Is ", response, " the desired input?"))
    if (confirm == F) {
      prompt <- sprintf("%s ", question)
      response <- trimws(readline(prompt))
    }
  }
  return(response)
}
