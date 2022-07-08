
# History
# 2021-05-06. Created Eben Pendleton.

#' Integral theme

#' theme for PDF
#' @export
theme_integral <- function() {

  # From "M:\DataAnalysis\R\00_DataAnalysis_Resources\01_R_BestPractices\
  # GGPlot_UniversalThemeSettings\General-Theme.R"
  return(ggplot2::theme(text = ggplot2::element_text(family = "sans")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(color = "black", size = 8),
      legend.background = ggplot2::element_rect(size = .5, linetype = "solid", color = "black")
    ) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black", size = 1)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(color = "black", size = 8, vjust = 0.5, angle = 0)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(color = "black", size = 8, vjust = 0.5, angle = 0)) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(color = "black", size = 9, face = "bold")) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(color = "black", size = 9, face = "bold")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, face = "bold")) +
    ggplot2::theme(axis.ticks = ggplot2::element_line(size = 1)) +
    ggplot2::theme(axis.line = ggplot2::element_line(size = 1)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white")) +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, size = 0.25)) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 9)) +
    ggplot2::theme(plot.margin = grid::unit(c(0.5, 0.5, .5, 0.5), "cm")))
}

#' theme for PowerPoint slide
#' @export

# panel border is controlled in the master slide for now.
ppttheme <- function() {
  return(ggplot2::theme(text = ggplot2::element_text(family = "sans")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(color = "black", size = 8),
      legend.background = ggplot2::element_rect(size = .5, linetype = "solid", color = "black")
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black", size = 1)
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(color = "black", size = 8, vjust = 0.5, angle = 0)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(color = "black", size = 8, vjust = 0.5, angle = 0)) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(color = "black", size = 9, face = "bold")) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(color = "black", size = 9, face = "bold")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, face = "bold")) +
    ggplot2::theme(axis.ticks = ggplot2::element_line(size = 1)) +
    ggplot2::theme(axis.line = ggplot2::element_line(size = 1)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white")) +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, size = 0.25)) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 9)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 0.5, .5, 0.5), "cm")))
}
