# History
# 2022-03-31. Created Eben Pendleton.
# 2022-04-01. Added git and file pattern

#' @title Create a new project and R files following Integral standards
#' @name new_proj
#' @description Function to create a new R project and files following integral
#' standards
#' `r lifecycle::badge('experimental')`
#' @return path to new project

#' @param create_dirs Y/n whether to create input, output and QA folders
#' @param create_rproj Y/n whether to create a .Rproj file
#' @param create_rscript Y/n whether to create a R script file
#' @param create_rmarkdown Y/n whether to create a R Markdown file
#' @param create_qa Y/n whether to create a QA file
#' @param create_git Y/n whether to create a git repository
#' @param create_renv Y/n whether to create a renv
#'
#' Creates a new R Project and populates with Integral standards
#' @examples
#' \dontrun{
#' new_proj()
#' }
#' @export
new_proj <- function(create_dirs = ask("Would you like to create input, output and QA folders?")
                        ,create_rproj = ask("Would you like to create an Rproj file?")
                        ,create_rscript = ask("Would you like to create an R script file?")
                        ,create_rmarkdown = ask("Would you like to create an Rmarkdown file?"
                                                , default = F)
                        ,create_qa = ask("Would you like to create a QA file?")
                        ,create_git = ask("Would you like to create a git repository?")
                        ,create_renv = ask("Would you like to create an renv?")
                        ,switch_proj = ask("Would you like to switch to the newly created project?")) {

  # create the directory at the path.
  path = convert_winpath(readline("What is the directory path to create the project in?: "))
  suppressWarnings(dir.create(path))

  # get the files to copy over
  # if the package is installed
  extpath <- system.file("templates", "example_project", package = "integral")

  # if we are running a check on an uninstalled package use here::here() to
  # get the Rcheck directory
  if (extpath == "") {
    extpath <- here::here("integral", "templates", "example_project")
  }

  if (create_dirs) {
  # get the directories
  d <- list.dirs(path = extpath)
  d <- basename(d[2:length(d)])
  # remove renv if present
  d <- d[!d %in% c("renv", "cellar")]

  func <- function(x) {suppressWarnings(dir.create(paste(path
                      ,x, sep='/')))}
  lapply(d, func)
  }

  if (create_rproj) {
    suppressWarnings(file.copy(paste(extpath,"example_proj.Rproj", sep = '/')
              ,paste(path, paste0(basename(path), ".Rproj")
              ,sep = "/"), overwrite=F))
    }

  if (create_rscript) {
    rename <- readline(paste("What would you like to name the file?"
                       ,"Don't include the file extension.: "))
    suppressWarnings(file.copy(paste(extpath,"rfile_w_header.R", sep = '/')
              ,paste(path, paste0(rename, ".R")
              ,sep = "/"), overwrite=F))
  }

  if (create_rmarkdown) {
    rename <- readline(paste("What would you like to name the file?"
                             ,"Don't include the file extension.: "))
    suppressWarnings(file.copy(paste(extpath,"example_Rmarkdown.Rmd", sep = '/')
              ,paste(path, paste0(rename, ".Rmd")
              ,sep = "/"), overwrite=F))
  }

  if (create_qa) {
    rename <- readline(paste("What would you like to name the file?"
                             ,"Don't include the file extension.: "))

    if (dir.exists(paste(path, "QA"))) {
    suppressWarnings(file.copy(paste(extpath,"QA/QA_Template_Coded_Analysis.xlsx", sep = '/')
              ,paste(path, "QA", paste0(rename, ".xlsx")
              ,sep = "/"), overwrite=F))
  } else {
    suppressWarnings(file.copy(paste(extpath,"QA/QA_Template_Coded_Analysis.xlsx", sep = '/')
                               ,paste(path, paste0(rename, ".xlsx")
                                      ,sep = "/"), overwrite=F))
  }
  }

  # switch the working directory to the path
  w <- getwd()
  setwd(path)
  # create git
  if (create_git) {
    system("git init")
    system("git checkout --orphan main")
    suppressWarnings(file.copy(paste(extpath,".gitignore", sep = '/')
              ,paste(path, ".gitignore", sep = "/"), overwrite=F))
  }

  if (switch_proj) {
    rstudioapi::openProject(path = path
  , newSession = ask("Would you like to open the project in a new session?"))
  }

  #reset the working directory to whatever we came in with
  setwd(w)

  # return the project path
  # Should return parameters
  return(path)
}
