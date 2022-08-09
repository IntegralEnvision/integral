#' Find a new file interactively
#' `r lifecycle::badge('stable')`
#' @description Finds a new file from user inout
#' @param ext file extension to check for. Can be R, Rmd etc
#' @param startdir Optional start directory
#' @examples
#' \dontrun{
#' find_file("Rmd")
#' }
#'

find_file <- function(ext, startdir = rstudioapi::getActiveProject()) {
  if (rstudioapi::isAvailable()) {
    filepath <- rstudioapi::selectFile(caption = "Save File"
      ,label = paste0("Save ", ext)
      ,existing = FALSE
      ,path = startdir
    )

    filepath <- gsub("~", path.expand("~"), filepath)
  }

  while (!tools::file_ext(filepath) == ext) {
    cli::cli_alert(paste0("File extension is not .", ext, " Please save a . ", ext))
    Sys.sleep(0.5)

    filepath <- rstudioapi::selectFile(
      caption = "Save File",
      label = paste0("Save ", ext),
      existing = FALSE
    )

    filepath <- gsub("~", path.expand("~"), filepath)

    if (length(filepath) == 0) {
      filepath <- "zzz"
    }

  }


  if (filepath == "") {
    return(cli::cli_alert_danger("Filepath not provided"))
  }

  return(filepath)
}


#' Start a new file with header
#' `r lifecycle::badge('experimental')`
#' @description Creates a new file with header. Function is not exported.
#' @param filepath File to be created.
#' @param rfile_path R file to source
#' @param open Should the file be open (True/False)
#' @examples
#' \dontrun{
#' ic_new_file("myfile.R", fs::path_package(
#'   "integral",
#'   "templates/example_project/rfile_w_header.R"
#' ))
#' }
#'
ic_copy_script <- function(filepath, rfile_path, open = F) {
  tryCatch(
    {
      fs::file_copy(rfile_path, filepath, overwrite = F)
      cli::cli_alert_success(paste0(basename(filepath), " created"))
    },

    # add cli warning
    error = function(cond) {
      cli::cli_alert_danger(paste("File already exists!",
        paste0(basename(filepath), " creation canceled."),
        sep = " "
      ))
    }
  )
  if (open) {
    utils::file.edit(filepath)
  }
}

#' Start a new script file with Integral's header
#' `r lifecycle::badge('experimental')`
#' @description Creates a new script with header
#' @param filepath File to be created.
#' @param open Open the file in the default editor (TRUE/FALSE)
#' @examples
#' \dontrun{
#' ic_new_script("myfile.R")
#' ic_new_r_file("myfile.R")
#' ic_new_python_file("myfile.py")
#' }
#' @export
ic_new_script <- function(filepath, open = F) {
  rfile_path <- fs::path_package(
    "integral",
    "templates/example_project/rfile_w_header.R"
  )

  ic_copy_script(filepath, rfile_path, open = F)
}

#' @rdname ic_new_script
#' @export
ic_new_r_file <- function(filepath = "", open = F) {
  rfile_path <- fs::path_package(
    "integral",
    "templates/example_project/rfile_w_header.R"
  )

  if (filepath == "") {
    filepath <- find_file("R")
  }

  ic_copy_script(filepath, rfile_path, open = F)
}

#' @rdname ic_new_script
#' @export
ic_new_python_file <- function(filepath = "", open = F) {
  rfile_path <- fs::path_package(
    "integral",
    "templates/example_project/rfile_w_header.R"
  )

  if (filepath == "") {
    filepath <- find_file("py")
  }

  ic_copy_script(filepath, rfile_path, open = F)
}


#' Start a new RMarkdown document with Integral header
#' @description Drafts a new RMarkdown document with Integral header
#' `r lifecycle::badge('experimental')`
#' @param filepath File to be created.
#' @param open Open the file in the default editor (TRUE/FALSE)
#' @examples
#' \dontrun{
#' ig_new_rmd_file("myrmd.Rmd")
#' }
#' @export
ic_new_rmd_file <- function(filepath = "", open = F) {
  if (filepath == "") {
      filepath <- find_file("Rmd")
  }
  rmdfile_path <- fs::path_package(
    "integral",
    "templates/example_project/Example_Rmarkdown.Rmd"
  )

  ic_copy_script(filepath, rmdfile_path, open = F)
}

#' Start a new git repository with Integral gitignore
#' `r lifecycle::badge('experimental')`
#' @description Creates a new git repository at the given path
#' @param dirpath File to be created.
#' @examples
#' \dontrun{
#' ic_new_git("some directory path")
#' }
#'
ic_new_git <- function(dirpath) {

  # error if directory doesn't exist
  if (!dir.exists(dirpath)) {
    stop(cli::format_error("Git creation path not a directory!"))
  }

  response <- T
  if (dir.exists(paste(dirpath,
    ".git",
    sep = "/"
  ))) {
    response <- ask("A git repository already exists. Do you want to reinitialize it?", default = F)
  }

  if (response) {
    # switch the working directory to the path
    w <- getwd()
    setwd(dirpath)
    system("git init")
    system("git checkout --orphan main")

    gitfile_path <- fs::path_package(
      "integral",
      "templates/example_project/ic.gitignore"
    )

    ic_copy_script(paste(dirpath, ".gitignore", sep = "/"), gitfile_path, open = F)

    # reset the working directory to whatever we came in with
    setwd(w)
    # end response
  }
}
