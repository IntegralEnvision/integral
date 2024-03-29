% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/templates.R
\name{ic_new_rmd_file}
\alias{ic_new_rmd_file}
\title{Start a new RMarkdown document with Integral header}
\usage{
ic_new_rmd_file(filepath = "", open = F)
}
\arguments{
\item{filepath}{File to be created.}

\item{open}{Open the file in the default editor (TRUE/FALSE)}
}
\description{
Drafts a new RMarkdown document with Integral header
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\examples{
\dontrun{
ig_new_rmd_file("myrmd.Rmd")
}
}
