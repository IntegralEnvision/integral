#' Write Clip shortcut
#' @description
#' `r lifecycle::badge('stable')`
#' Copies an object to the clipboard. Defaults to .Last.value
#' @export
wc <- function(x = .Last.value) {
  clipr::write_clip(x)
  cli::cli_alert_success("Value copied to clipboard")
}

#' Not In: Inverse Value Matching
#' @description
#' `r lifecycle::badge('stable')`
#' Returns a logical vector indicating if there is NOT a match for the LHS vector anywhere in the RHS vector. Opposide of \code{\%in\%}.
#' @usage x %ni% y
#' @param x vector or NULL: the values to check for non-match
#' @param y vector or NULL: the values to be matched against
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
