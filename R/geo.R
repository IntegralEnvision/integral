#' Identify the location of states to facilitate direction of popups.
#' @description
#' \lifecycle{experimental}
#' Returns a table of state names, state abbreviations, and the location of each state.
#' @param albers specify whether the map being used is shifted to albers. Defaults to TRUE.
#' @return Table with relative location of each state
#' @export

state_locations <- function(albers = TRUE) {
  x <- tigris::fips_codes %>%
    dplyr::filter(state_code < 60) %>%
    dplyr::rename(state_abb = state) %>%
    dplyr::distinct(state_name, state_abb, state_code) %>%
    dplyr::mutate(location = dplyr::case_when(state_abb %in% c("WA", "OR", "MT", "WY", "ID", "ND", "SD", "NE", "MN", "IA", "WI", "IN", "IL", "MI", "OH", "KY", "WV", "MD", "PA", "DE", "NJ", "NY", "CT", "VT", "NH", "MA", "CT", "RI", "MA", "ME", "DC") ~ "top",
                                state_abb %in% c("NV", "UT", "CO", "KS", "MO", "KY", "VA") ~ "middle",
                                TRUE ~ "bottom"))

  #If not using albers, Alaska needs to be labelled "top"
  if(!albers) x <-  x %>%
      dplyr::mutate(location = dplyr::if_else(state_abb == "AK", "top", location))

  return(x)

}
