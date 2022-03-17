#' Scale format to International System of Units (k, M, )
#' @description
#' #' `r lifecycle::badge('stable')`
#' Format a vector of numeric values according to the International System of Units.
#'
#' (ie: 1,000 becomes "1 k", 2,500 becomes "2.5 k") See: http://en.wikipedia.org/wiki/SI_prefix
#' @param sep Seperator to use between number and unit (defaults to " ")
#' @param ... Passed by \code{ggplot2::scale_*_continuous}
#'
#' @return Used with scale_*_continuous, returns formatted axis labels
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(diamonds, aes(x = price)) + geom_density() + scale_x_continuous(labels = scale_si_unit())
#' }

ic_scale_si_unit <- function(sep = " ", ...) {
  # Based on code by Ben Tupper
  # https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html

  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "µ",   "m",   " ",   "k",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")

    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)

    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)

    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i], sep = sep)
  }
}


#' Scale large dollar amounts to shortened dollar amounts (k, M, B, T)
#' @description
#' `r lifecycle::badge('stable')`
#' Format a vector of numeric values according to monetary abbreviations.
#'
#' (ie: 1,000 becomes "1 k", 2,500,000 becomes "2.5 B") See: https://www.wallstreetoasis.com/forums/abbreviation-for-thousands-millions-billions-trillion
#' @param sep Seperator to use between number and unit (defaults to " ").
#' @param suffix_n Use "Bn" and "Tn" instead of "B" and "T".
#'
#' @return Character vector of formatted dollar values.
#' @examples
#' \dontrun{
#' ic_scale_big_dollar(1000)
#' ic_scale_big_dollar(1000000000)
#' ic_scale_big_dollar(1000000000, suffix_n = T)
#' }
#' @export
ic_scale_big_dollar <- function(x, sep = " ", suffix_n = F) {
  x <- as.numeric(x)
  limits <- c(1, 1000, 1e+06, 1e+09, 1e+12)
  suffix <- c(" ", "k", "M", "B", "T")
  if(suffix_n) suffix <- c(" ", "k", "M", "Bn", "Tn")
  i <- findInterval(abs(x), limits)
  i <- ifelse(i == 0, which(limits == 1), i)
  paste0("$", format(round(x/limits[i], 1), trim = TRUE, scientific = FALSE), sep, suffix[i])
}
