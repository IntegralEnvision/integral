
# History
# 2022-03-31. Created Eben Pendleton.
# 2022-04-01. Added git and file pattern

# Notes: .gitignore is created when git is run. Only copies folders one level
# deep

#' Creates a new R Project and populates with Integral standards
#' @export

create_project <- function(path = readline("What is the directory path to create the project in?: ")
                        ,create_dirs = ask("Would you like to create input, output and QA folders?")
                        ,create_rproj = ask("Would you like to create an Rproj file?")
                        ,create_rscript = ask("Would you like to creat an R script file?")
                        ,create_rmarkdown = ask("Would you like to create an Rmarkdown file?")
                        ,create_qa = ask("Would you like to create a QA file?")
                        ,create_git = ask("Would you like to create a git repository?")
                        ,create_renv = ask("Would you like to create an renv?")
                        ,swtich_proj = ask("Would you like to switch to the newly created project?")) {

  	# create the directory at the path.
  # Warns if the directory already exists
  if (path) {dir.create(path)}

  # get the files to copy over
  # if the package is installed
  extpath <- system.file("extdata", "example_project", package = "integral")

  # if we are running a check on an uninstalled package use here::here() to
  # get the Rcheck directory
  if (extpath == "") {
    extpath <- here::here("integral", "extdata", "example_project")
  }

  if (create_dirs) {
  # get the directories
  d <- list.dirs(path = extpath)
  d <- basename(d[2:length(d)])

  func <- function(x) {dir.create(paste(path,x, sep='/'))}
  lapply(d, func)
  }

  if (create_rproj) {
    file.copy(paste(extpath,"example_proj.Rproj", sep = '/')
              , paste(path, paste0(basename(path), ".Rproj")), overwrite=F)

  }

  if (create_rscript) {
    rename <- readline(paste("What would you like to name the file?"
                       ,"Don't include the file extension.: "))
    file.copy(paste(extpath,"example_script.R", sep = '/')
              , paste(path, paste0(rename, ".R")), overwrite=F)
  }

  if (create_rmarkdown) {
    rename <- readline(paste("What would you like to name the file?"
                             ,"Don't include the file extension.: "))
    file.copy(paste(extpath,"example_markdown.Rmd", sep = '/')
              , paste(path, paste0(rename, ".Rmd")), overwrite=F)
  }

  if (create_qa) {
    rename <- readline(paste("What would you like to name the file?"
                             ,"Don't include the file extension.: "))
    file.copy(paste(extpath,"QA/QA_file.xlsx", sep = '/')
              , paste(path, paste0(rename, ".xlsx")), overwrite=F)
  }

  # Get the files at the extpath location
  #file.copy(list.files(path = extpath, pattern=pattern, recursive=T), path, overwrite  = F)

  w <- getwd()
  setwd(path)
  # create git
  if (create_git) {
    system("git init -b main")
    file.copy(paste(extpath,".gitignore", sep = '/')
              , paste(path, ".gitignore"), overwrite=F)
  }

  if (create_renv) {

    # copy renv contents
    dir.create(paste(path, "renv", sep = "/"))
    file.copy(list.files(paste(extpath, "renv", sep = "/")
              ,full.names = T), paste(path, "renv", sep ="/")
              ,recursive=T )
    # copy lock file
    file.copy(paste(extpath,"renv.lock", sep = '/')
              , paste(path, "renv.lock"), overwrite=F)

  }

  if (switch_proj) {
    rstudioapi::openProject(path = path
  , newSession = ask("Would you like to open the project in a new session?"))
  }
  return(path)
}
