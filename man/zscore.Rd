% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{zscore}
\alias{zscore}
\title{Convenience wrapper for scale that returns a vector instead of a matrix / array}
\usage{
zscore(.data, center = TRUE, scale = TRUE)
}
\arguments{
\item{.data}{numeric vector of values to be scaled}

\item{center}{either a logical value or numeric-alike vector of length equal to the number of columns of x, where ‘numeric-alike’ means that as.numeric(.) will be applied successfully if is.numeric(.) is not true.}

\item{scale}{either a logical value or a numeric-alike vector of length equal to the number of columns of x.}
}
\value{
A vector of scaled values.
}
\description{
Wrapper for scale() that returns a vector.
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
}
\examples{
# 1:20 \%>\% zscore()
#
# zscore(1:20)
}
