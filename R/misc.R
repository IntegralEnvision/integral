#' Not In: Inverse Value Matching
#' @description
#' `r lifecycle::badge('stable')`
#' Returns a logical vector indicating if there is NOT a match for the LHS vector anywhere in the RHS vector. Opposide of \code{\%in\%}.
#' @usage x %ni% y
#' @return A logical vector, indicating if there was no match for each element of x. Values are TRUE or FALSE and never NA
#' @rdname ni
#' @export
"%ni%" <- Negate("%in%")


#' @title Creating a new column with significance labels
#' @name signif_column
#' @author Indrajeet Patil
#' @description This function will add a new column with significance labels to
#'   a dataframe containing *p*-values.
#' `r lifecycle::badge('maturing')`
#' @return Returns the dataframe in tibble format with an additional column
#'   corresponding to APA-format statistical significance labels.
#'
#' @param data Data frame from which variables specified are preferentially to
#'   be taken.
#' @param p The column containing *p*-values.
#' @param ... Currently ignored.
#'
#'
#' @family helper_stats
#'
#' @examples
#' # preparing a new dataframe
#' df <- cbind.data.frame(
#'   x = 1:5,
#'   y = 1,
#'   p.value = c(0.1, 0.5, 0.00001, 0.05, 0.01)
#' )
#'
#' # dataframe with significance column
#' signif_column(data = df, p = p.value)
#' @export
signif_column <- function(data, p, ...) {
  # add new significance column based on standard APA guidelines

  p <- rlang::ensym(p)


  if(!(rlang::as_string(p)  %in% names(data))) {stop("Specified p value column not found in data.")}

  data %>%
    dplyr::mutate(
      .data = .,
      significance = dplyr::case_when(
        {{ p }} >= 0.050 ~ "ns",
        {{ p }} < 0.050 & {{ p }} >= 0.010 ~ "*",
        {{ p }} < 0.010 & {{ p }} >= 0.001 ~ "**",
        {{ p }} < 0.001 ~ "***"
      )
    ) %>% # convert to tibble dataframe
    tibble::as_tibble(.)
}



#' @title Convenience wrapper for scale that returns a vector instead of a matrix / array
#' @name zscore
#' @description Wrapper for scale() that returns a vector.
#' `r lifecycle::badge('stable')`
#' @return A vector of scaled values.
#'
#' @param .data numeric vector of values to be scaled
#' @param center either a logical value or numeric-alike vector of length equal to the number of columns of x, where ‘numeric-alike’ means that as.numeric(.) will be applied successfully if is.numeric(.) is not true.
#' @param scale either a logical value or a numeric-alike vector of length equal to the number of columns of x.
#'
#' @examples
#' # 1:20 %>% zscore()
#' #
#' # zscore(1:20)

#' @export
zscore <- function(.data, center = TRUE, scale = TRUE) {
  scale(.data, center = center, scale = scale)[,1]
}

#' @title Select string between two strings
#' @name str_between
#' @description A stringr-like function to pull out a string between two string anchors
#' `r lifecycle::badge('experimental')`
#' @return string or vector of strings
#'
#' @param string Input vector. Either a character vector, or something coercible to one.
#' @param pattern1 Pattern that will act as starting anchor in extraction, and be excluded. The default interpretation is a regular expression, as described in stringi::stringi-search-regex. Control options with regex().
#' @param pattern2 Pattern that will act as ending anchor in extraction, and be excluded.
#'
#' @examples
#' # str_between("apple", "ap", "e")
#' @export
str_between <- function(string, pattern1, pattern2){
  string1 <- stringr::str_extract(string,  paste0("(?<=", pattern1,").+(?<=", pattern2, ")"))
  return(string1)
}
