% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xview.R
\name{xview2}
\alias{xview2}
\title{Show a table in Excel}
\usage{
xview2(.data, include_rownames = TRUE, auto_conditional = TRUE)
}
\arguments{
\item{.data}{A table.}

\item{include_rownames}{Logical. Convert rownames to column? Default: TRUE}

\item{auto_conditional}{Automatically add conditional formatting. Default: TRUE}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
Opens the table in a temporary file in excel.
}
\examples{
\dontrun{
diamonds \%>\% xview2()
}
}
