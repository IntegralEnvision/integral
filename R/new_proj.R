# History
# 2022-03-31. Created Eben Pendleton.
# 2022-04-01. Added git and file pattern

#' @title Create a new project and R files following Integral standards
#' @name ic_new_proj
#' @description Function to create a new R project and files following integral
#' standards
#' `r lifecycle::badge('experimental')`
#' @return path to new project
#'
#' @param path Path to new project.  Defaults to user input during function.
#' @param create_dirs Y/n whether to create input, output and QA folders
#' @param create_rproj Y/n whether to create a .Rproj file
#' @param create_git Y/n whether to create a git repository
#' @param switch_projf Y/n whether to switch to newly created project
#' Creates a new R Project and populates with Integral standards
#' @examples
#' \dontrun{
#' new_proj()
#' }
#' @export
ic_new_proj <- function(
    path = "",
    create_dirs = T,
    create_rproj = T,
    create_git = ask("Would you like to create a git repository?"),
    switch_proj = ask("Would you like to switch to the newly created project?")) {
  proj_setup <- "ic_new_proj("

  # if no path is supplied than ask for it
  if (path == "") {
    path <- convert_winpath(readline(paste0("What is the directory path to create the project in?"
    ," If blank, a dialog will be shown: ")))
  }

  # if still blank prompt for a path
  if (path == "") {
    path <- rstudioapi::selectDirectory()
    path <- gsub("~", path.expand("~"), path)
    path <- stringr::str_replace_all(path,"C:","/")
    path <- ifelse(!grepl("^//",path),paste("/",path,sep=""),path)
    if (length(path) > 0) {
    cli::cli_alert_success(paste( "Project directory selected:", path, sep = " "))
    } else {
      return(cli::cli_alert_danger("Project path selection canceled by user."))
    }
  }

  # check if the directory is empty
  if (length(path) > 1) {
    response <- ask("Project directory is not empty! Would you like to continue?", default = F)
    if (response == F) {return(cli::cli_alert_danger("Project creation canceled by user."))}
  }

  # create the path
  suppressWarnings(dir.create(path))

  proj_setup <- paste0("ic_new_proj(path = ", "'", path, "'")

  if (create_dirs) {
    d <- c("inputs", "outputs")
    func <- function(x) {
      if (!dir.exists(paste(path,
                            x,
                            sep = "/"))) {
      dir.create(paste(path,
        x,
        sep = "/"
      ))
        cli::cli_alert_success(paste(x, "folder created", sep = " "))
      } else{
        cli::cli_alert_danger(paste("Folder already exists!"
                                    ,paste0(x, " folder creation canceled.")
                                    ,sep = " "))
      }
    }

    lapply(d, func)

    proj_setup <- paste(proj_setup, "create_dirs = T", sep = ", ")


  }

  if (create_rproj) {
    rprojfile_path <- fs::path_package(
      "integral",
      "templates/example_project/example_proj.Rproj"
    )

    ic_copy_script(paste(path, paste0(basename(path), ".Rproj"), sep = "/"),
      rprojfile_path,
      open = F
    )

    proj_setup <- paste(proj_setup, "create_rproj = T", sep = ", ")
  }

  # create git
  if (create_git) {
    ic_new_git(path)

    proj_setup <- paste(proj_setup, "create_git = T", sep = ", ")
  }

  if (switch_proj) {
    rstudioapi::openProject(
      path = path,
      newSession <- ask("Would you like to open the project in a new session?")
    )
    sess_response <- "F"
    if (newSession) {sess_response <- "T"}

    proj_setup <- paste(proj_setup, "switch_proj = T",
      paste0("newSession = ", sess_response),
      sep = ", "
    )
  } else {
    proj_setup <- paste(proj_setup, "switch_proj = F",
      sep = ", "
    )
  }

  # return parameters
  cli::cli_alert("This project can be recreated with the following function: ")
  proj_setup <- paste0(proj_setup, ")")
  return(cli::cli_alert(proj_setup))
}
