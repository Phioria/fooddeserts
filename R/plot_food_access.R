#' Plot a state's food access
#'
#' @description
#' This function will generate a food access plot with county level detail.
#' Currently the only plot feature options are "lila" and "la" which refer to
#' "Low Income and Low Access" and "Low Access" repectively.
#'
#'
#' @param data data frame - The foodatlas dataset
#' @param state character - The state to plot given in the format "NY" or "New York"
#' @param feature character - The feature to plot, either "lila" or "la"
#' @param title character - An optional title for the plot
#' @param subtitle character - An optional subtitle for the plot
#' @param caption character - An optional caption for the plot
#' @param pal character - The color pallete for the plot with options: "YlOrRd", "Red-Yellow" or "BluYl"
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#'
#' plot_state(data = foodatlas, state = "NY", feature = "lila")
#'
#' plot_state(foodatlas, "Georgia")
#'
plot_state <- function(data, state, feature = c("lila", "la"),
                       title = NULL, subtitle = NULL, caption = NULL,
                       pal = c("YlOrRd", "Red-Yellow", "BluYl")) {
  # Bind data frame column names that R is complaining about
  county <- pop2010 <- la_half_10 <- li_la_half_10 <- la_county_pop <- county_pop <- li_la_county_pop <- NULL

  if (missing(data)) stop("Argument `data` is required.")
  if (missing(state)) stop("Argument `state` is required.")

  feature <- match.arg(feature)
  feature_val <- switch(feature,
                        "lila" = "li_la_half_10",
                        "la" = "la_half_10")
  feature_plot <- switch(feature,
                         "lila" = "li_la_tract_percent",
                         "la" = "la_tract_percent")

  pal <- match.arg(pal)

  if (!is_plottable(data, state, feature_val)) {
    warning("Could not generate this plot. See warnings.")
    return(NULL)
  }

  # Ensure proper state format
  if (is_state_abb(state)) {
    state <- abb_to_state(state)
  }

  # Ensure the state is present in the dataset
  if (!is_state_present(data, state)) {
    warning("The state provided was not present in the dataset, please try another.")
    return(NULL)
  }

  #### Generate plot text ####
  # Set title to default if not provided
  if (is.null(title)) {
    title_pre <- paste("Share of", state, "tracts per county with")
    title_mid <- switch(feature,
                         "la" = "low access",
                         "lila" = "low income and low access")
    title_suf <- "to groceries"
    title <- paste(title_pre, title_mid, title_suf)
  } else {
    stopifnot("`title` must be of type character or left blank." = is.character(title))
  }

  # Set subtitle to default if not provided
  if (is.null(subtitle)) {
    subtitle <- paste(state, "2015 - 2019")
  } else {
    stopifnot("`subtitle` must be of type character or left blank." = is.character(subtitle))
  }

  # Set caption to default if not provided
  if (is.null(caption)) {
    caption <- "Data from: https://catalog.data.gov/dataset/food-access-research-atlas"
  } else {
    stopifnot("`caption` must be of type character or left blank." = is.character(caption))
  }

  fill_str <- "Share of Tracts"

  # filter doesn't play nice with column and variable names being the same
  f_state <- state

  #### Summarize data ####
  subset_df <- data %>%
    dplyr::filter(state == f_state) %>%
    dplyr::group_by(county) %>%
    dplyr::summarize(county_tracts = dplyr::n(),
                     county_la_tracts = sum(la_half_10),
                     county_li_la_tracts = sum(li_la_half_10)) %>%
    dplyr::mutate(la_tract_percent = round(county_la_tracts / county_tracts, digits = 4) * 100,
                  li_la_tract_percent = round(county_li_la_tracts / county_tracts, digits = 4) * 100) %>%
    dplyr::mutate(fips = usmap::fips(f_state, county))



  #### Generate plot ####
  p <- usmap::plot_usmap(regions = c("counties"),
                         include = c(state),
                         data = subset_df,
                         values = feature_plot) +
    colorspace::scale_fill_continuous_sequential(palette = pal) +
    ggplot2::labs(title = title,
                  subtitle = subtitle,
                  caption = caption,
                  fill = fill_str) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14),
                   plot.subtitle = ggplot2::element_text(size = 10),
                   legend.position = "right")

  return(p)
}

    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14),
                   plot.subtitle = ggplot2::element_text(size = 10),
                   legend.position = "right")

  return(p)
}

is_state_present <- function(df, s) {
  # Check if given state is in the data
  # Assumes s has already been converted to full name if needed
  return(s %in% df$state)
}

is_plottable <- function(data, state, feature) {
is_plottable <- function(data, state = NULL, feature, detail = c("state_by_county", "us_by_county", "us_by_state")) {
  plottable <- TRUE

  detail <- match.arg(detail)

  if (detail == "state_by_county") {
    if (!is_valid_state(state)) {
      # Warning happens in is_valid_state
      plottable <- FALSE
    }
  }

  if (!is_valid_df(data, feature)) {
    # Warning happens in is_valid_df
    plottable <- FALSE
  }

  return(plottable)
}

is_valid_state <- function(s) {
  valid_state_names <- c(datasets::state.name, "District of Columbia")
  valid_state_abb <- c(datasets::state.abb, "DC")
  if (s %in% valid_state_names == FALSE && s %in% valid_state_abb == FALSE) {
    warning(paste0("Supplied state: '", s, "' is not valid"))
    return(FALSE)
  }
  return(TRUE)
}

is_valid_df <- function(df, feature) {
  valid_df <- TRUE
  cols <- colnames(df)

  # Check for state column
  if (!("state" %in% cols)) {
    warning("Input data frame must have a column named 'state'")
    valid_df <- FALSE
  }

  # TODO This needs to be made optional if plotting by country at state level
  # Check for county column
  if (!("county" %in% cols)) {
    warning("Input data frame must have a column named 'county'")
    valid_df <- FALSE
  }

  # Check for feature column
  if (!(feature %in% cols)) {
    warning(paste0("Input data frame must have a column named '", feature, "'"))
    valid_df <- FALSE
  }

  # Check for population column
  if (!("pop2010" %in% cols)) {
    warning("Input data frame must have a column named 'pop2010'")
    valid_df <- FALSE
  }

  return(valid_df)
}
