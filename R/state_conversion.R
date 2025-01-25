#' Convert State To Abbreviation
#'
#' @description
#' `state_to_abb()` and `abb_to_state()` convert state names into their
#' abbreviations, or abbreviations into names.
#' The list include District of Columbia, DC
#'
#' @param state a state name, capitalized
#'
#' @return a two character capitalized abbreviation of that state
#'
#' @examples
#' state <- "New York"
#' abb <- state_to_abb(state) # "NY"
#'
#' abb <- "AZ"
#' state <- abb_to_state(abb) # "Arizona"
#'
#' @export
#'
state_to_abb <- function(state) {
  vec_idx <- which(c(datasets::state.name, "District of Columbia") == state)
  abbr <- c(datasets::state.abb, "DC")[vec_idx]
  return(abbr)
}

#' Convert Abbreviation to State
#'
#' @description
#' `abb_to_state()` and `state_to_abb()` convert state names into their
#' abbreviations, or abbreviations into names.
#' The list include District of Columbia, DC
#'
#'
#' @param abb a two character capitalized abbreviation of a US state
#'
#' @return a capitalized name of the state associated with the given abbreviation
#'
#' @examples
#' abb <- "NY"
#' state <- abb_to_state(abb) # New York
#'
#' @export
#'
abb_to_state <- function(abb) {
  vec_idx <- which(c(datasets::state.abb, "DC") == abb)
  state <- c(datasets::state.name, "District of Columbia")[vec_idx]
  return(state)
}

#' Is this a State Name?
#'
#' @param state a state name, capitalized
#'
#' @return TRUE if the input is a valid state name,
#'         FALSE if it is not
#'
is_state_name <- function(state) {
  return(state %in% c(datasets::state.name, "District of Columbia"))
}

#' Is this a State Abbreviation?
#'
#' @param abb a state abbreviation, capitalized
#'
#' @return TRUE if the input is a valid state abbreviation,
#'         FALSE if it is not
#'
is_state_abb <- function(abb) {
  return(abb %in% c(datasets::state.abb, "DC"))
}
