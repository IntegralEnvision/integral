
# History
# 2022-03-31. Created Eben Pendleton.
# 2022-04-01. Added git and file pattern

# Notes: .gitignore is created when git is run. Only copies folders one level
# deep

#' Creates a new R Project and populates with Integral standards
#' @export
create_project <- function(create_dirs = ask("Would you like to create input, output and QA folders?")
                        ,create_rproj = ask("Would you like to create an Rproj file?")
                        ,create_rscript = ask("Would you like to creat an R script file?")
                        ,create_rmarkdown = ask("Would you like to create an Rmarkdown file?"
                                                , default = F)
                        ,create_qa = ask("Would you like to create a QA file?")
                        ,create_git = ask("Would you like to create a git repository?")
                        ,create_renv = ask("Would you like to create an renv?")
                        ,switch_proj = ask("Would you like to switch to the newly created project?")) {

  # create the directory at the path.
  path = readline("What is the directory path to create the project in?: ")
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
  d[!d %in% c("renv")]

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
    suppressWarnings(file.copy(paste(extpath,"example_markdown.Rmd", sep = '/')
              ,paste(path, paste0(rename, ".Rmd")
              ,sep = "/"), overwrite=F))
  }

  if (create_qa) {
    rename <- readline(paste("What would you like to name the file?"
                             ,"Don't include the file extension.: "))
    suppressWarnings(file.copy(paste(extpath,"QA/QA_Template_Coded_Analysis.xlsx", sep = '/')
              ,paste(path, "QA", paste0(rename, ".xlsx")
              ,sep = "/"), overwrite=F))
  }

  # switch the working drectory to the path
  w <- getwd()
  setwd(path)
  # create git
  if (create_git) {
    system("git init -b main")
    suppressWarnings(file.copy(paste(extpath,".gitignore", sep = '/')
              ,paste(path, ".gitignore", sep = "/"), overwrite=F))
  }

  if (create_renv) {

    # copy renv contents
    suppressWarnings(dir.create(paste(path, "renv", sep = "/")))
    suppressWarnings(file.copy(list.files(paste(extpath, "renv", sep = "/")
              ,full.names = T), paste(path, "renv", sep ="/")
              ,recursive=T ))
    # copy lock file
    suppressWarnings(file.copy(paste(extpath,"renv.lock", sep = '/')
              , paste(path, "renv.lock", sep = "/")
              , overwrite=F))
    # copy .RProfile
    suppressWarnings(file.copy(paste(extpath,".Rprofile", sep = '/')
                               , paste(path, "Rprofile", sep = "/")
                               , overwrite=F))

  }

  if (switch_proj) {
    rstudioapi::openProject(path = path
  , newSession = ask("Would you like to open the project in a new session?"))
  }
  # return the path
  return(path)
}
