
# History
# 2020-04-13. Created by Hannah Podzorski
# 2020-12-23. Added ledger size and a few default options. Eben Pendleton
# 2021-02-15. Renamed to pptfig.R. Eben Pendleton.
# 2021-03-05. Added path. Eben Pendleton.
# 2022-01-24. Added P & C label. Made blanks not print placeholder.
#             Eben Pendleton
# 2022-03-16. Switched to Windows paths. Eben Pendleton.

#' Function to place R generated figures into a PowerPoint Template

#' @param size Presentation size (letter or ledger)
#' @param orientation Presentation orientation landscape (L) or Portrait (P)
#' @return PowerPrint presentation template

#' @examples
#' size <- c("letter")
#' orientation <- c("L")
#' fig <- ggplot2::ggplot()
#' section <- c("X")
#' fignum <- c("1")
#' note <- "Figure Note"
#' author <- "Eben Pendleton"
#' doctitle1 <- "Document Title"
#' my_pres <- read_pptx_template(size, orientation)

#' @importFrom dplyr case_when
#' @export

read_pptx_template <- function(size, orientation) {

	if (!orientation %in% c("P", "L")) { # Check to see if orientation is specified
      stop("Orientation should be P for Portrait, or L for Landscape")
    }

    if (!size %in% c("letter", "ledger")) { # Check to see if orientation is specified
      stop("Paper size should be letter or ledger")
    }

    # Read in the PowerPoint presentation
    fname <- dplyr::case_when(
      (size == "letter" & orientation == "P") ~ "TempP8.5x11Note.pptx",
      (size == "letter" & orientation == "L") ~ "TempL8.5x11Note.pptx",
      (size == "ledger" & orientation == "P") ~ "TempP11x17Note.pptx",
      (size == "ledger" & orientation == "L") ~ "TempL11x17Note.pptx",
      TRUE ~ as.character(NA)
    )
    # if the package is installed
    fpath <- system.file("extdata", fname, package = "integral")

    # if we are running a check on an uninstalled package use here::here() to
    # get the Rcheck directory
    if (fpath == "") {
      fpath <- here::here("integral", "extdata", fname)
    }
    # read in the presentation template
    my_pres <- officer::read_pptx(fpath)

	return(my_pres)
}

#' Function to place R generated figures into a PowerPoint Template
#'
#' @param my_pres from read_pptx_template function
#' @param size Presentation size (letter or ledger)
#' @param orientation Presentation orientation landscape (L) or Portrait (P)
#' @param fig graphics object that is compactiable with officer
#' @param section section number X-
#' @param fignum figure number after the section-figure number
#' @param note Figure notes text to include under figure
#' @param author The script author
#' @param doctitle1 Document Title 1
#' @param doctitle2 Optional Document Title 2
#' @param doctitle3 Optional Document Title 3
#' @param draft Optional Bold red text to be included next to figure caption (ex. "DRAFT")
#' @param pc Optional Privileged and Confidential label
#' @param showpath Optional Show script path on page. Default is True
#' @return PowerPoint presentation with added figure

#' Note: function dml() creates an editable figure in PowerPoint
#' fig <- dml(ggobj = p) #for ggplot object
#' fig <- dml({grid.arrange(p1, p2, p3, p4, nrow = 4)})

#' @examples
#' size <- c("letter")
#' orientation <- c("L")
#' fig <- ggplot2::ggplot()
#' section <- c("X")
#' fignum <- c("1")
#' note <- "Figure Note"
#' author <- "Eben Pendleton"
#' doctitle1 <- "Document Title"
#' my_pres <- read_pptx_template(size, orientation)
#' my_pres <- pptfig(my_pres, size, orientation, fig, section, fignum, note, author,
#'   doctitle1,
#'   doctitle2 = "", doctitle3 = "",
#'   draft = "DRAFT"
#' )
#'
#' # Print presentation in calling script
#' # print(my_pres, "Output_File-Name.pptx") #must end in .pptx
#' @importFrom stats update
#' @importFrom dplyr case_when
#' @importFrom magrittr %>%
#' @export

pptfig <-
  function(my_pres, size, orientation, fig, section, fignum, note, author,
           doctitle1, doctitle2 = "", doctitle3 = "",
           draft = "DRAFT", pc = "Privileged and Confidential", showpath = T) {

    # Formats for text
    fp_normal <- officer::fp_text(font.size = 9)
    fp_note <- officer::fp_text(font.size = 8)
    fp_italic <- update(fp_normal, italic = TRUE)
    fp_bold <- update(fp_normal, bold = TRUE)
    fp_draft <- officer::fp_text(font.size = 14, color = "red", bold = TRUE)
    fp_pc <- officer::fp_text(font.size = 6, bold = TRUE)
	  fp_path <- officer::fp_text(font.size = 6)

    # Creating Figure Caption Text Block
    pars <- officer::block_list(
      officer::fpar(
        officer::ftext(paste0("Figure ", section, "-", fignum, ".\n"), fp_bold),
        officer::ftext(paste0(doctitle1, "\n"), fp_italic),
        officer::ftext(paste0(doctitle2, "\n"), fp_normal),
        officer::ftext(doctitle3, fp_normal)
      )
    )

    ### Creating Draft Text Block
    parsDraft <- officer::block_list(
      officer::fpar(officer::ftext(draft, fp_draft))
    )

    ### Creating Notes Text Block
    parsNotes <- officer::block_list(
      officer::fpar(officer::ftext(note, fp_note))
    )

    ### Creating the P & C Block
    if (pc != '') {parsPC <- officer::block_list(
      officer::fpar(officer::ftext(pc, fp_pc))
    )
    }

  if (showpath == TRUE) {
	path <- determine_path()
	## Add date and author
	path <- paste(path, Sys.Date(), author, sep = " ")

  } else {
    path <- ""
    }

    # if characters are longer than 190 then resize
    # 200 is too large. Not sure if around 200.

    if (nchar(path) > 190) {
      fp_path <- officer::fp_text(font.size = 5)
      }

	### Creating Path Text Block
    parsPath <- officer::block_list(
      officer::fpar(officer::ftext(path, fp_path))
    )

    ### Creating Slide - adding figure and text

	# Portrait
    if (orientation == "P") {
      # Portrait Letter
      if (size == "letter") {
        left <- 0.82
        top <- 0.55
        width <- 7
        height <- 8.75
      }
      # Portrait Ledger
      # These were determined by mocking this up in PowerPoint
      if (size == "ledger") {
        # this is the horizontal position
        left <- 1.25
        # this is the vertical position
        top <- 0.83
        # this is the frame width and height
        width <- 8.26
        height <- 13.67
      }
    } # P

    if (orientation == "L") {
      if (size == "letter") {
        # this is the horizontal position
        left <- 0.55
        # this is the vertical position
        top <- 0.8
        # this is the frame width and height
        width <- 9.75
        height <- 6.25
      }

      # These were determined by mocking this up in PowerPoint
      if (size == "ledger") {
        # this is the horizontal position
        left <- 0.92
        # this is the vertical position
        top <- 1.25
        # this is the frame width and height
        width <- 15.13
        height <- 7.75
      }
      # Landscape
      # end check orientation
    } #L

    # write the elements to the slide

    my_pres <- my_pres %>%
      officer::add_slide(layout = "Fig_Temp", master = "Office Theme") %>%
      officer::ph_with(fig, location = officer::ph_location(left = left, top = top, width = width, height = height)) %>%
      officer::ph_with(pars, location = officer::ph_location_type(type = "title"))

    # if blank don't add the placeholder
      if(draft != '') {
        my_pres <- my_pres %>%
      officer::ph_with(location = officer::ph_location_type(type = "body", value = parsDraft), value = parsDraft)
      }
      if(pc != '') {
        my_pres <- my_pres %>%
          officer::ph_with(location = officer::ph_location_label(ph_label = "PC Placeholder"), value = parsPC)
      }
    if (note != '') {
    my_pres <- my_pres %>%
      officer::ph_with(location = officer::ph_location_label(ph_label = "Note Placeholder"), value = parsNotes)
    }
    if (path != '') {
      my_pres <- my_pres %>%
      officer::ph_with(location = officer::ph_location_label(ph_label = "Path Placeholder"), value = parsPath)
    }
    # return presentation
    return(my_pres)
    # end PPT Function
  }
