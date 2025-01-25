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
state_to_abb <- function(state) {
  vec_idx <- which(c(state.name, "District of Columbia") == state)
  abbr <- c(state.abb, "DC")[vec_idx]
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
abb_to_state <- function(abb) {
  vec_idx <- which(c(state.abb, "DC") == abb)
  state <- c(state.name, "District of Columbia")[vec_idx]
  return(state)
}

#' Is this a State Name?
#'
#' @param state a state name, capitalized
#'
#' @return TRUE if the input is a valid state name,
#'         FALSE if it is not
#'
#' @examples
#' # Returns TRUE
#' is_state_name("Minnesota")
#'
#' # Returns FALSE
#' is_state_name("Minesota")
#'
#' # Returns FALSE, as it is not capitalized
#' is_state_name("minnesota")
#'
is_state_name <- function(state) {
  return(state %in% c(state.name, "District of Columbia"))
}

#' Is this a State Abbreviation?
#'
#' @param abb a state abbreviation, capitalized
#'
#' @return TRUE if the input is a valid state abbreviation,
#'         FALSE if it is not
#'
#' @examples
#' # Returns TRUE
#' is_state_abb("TX")
#'
#' # Returns FALSE
#' is_state_abb("TRX")
#'
#' # Returns FALSE, as it is not capitalized
#' is_state_abb("tx")
#'
is_state_abb <- function(abb) {
  return(abb %in% c(state.abb, "DC"))
}
