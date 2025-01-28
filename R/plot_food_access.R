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
#' @param sensitivity character - How strict should the plot be about what constitutes a low income or low access area? "low", "medium" or "high"
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
#' plot_state(data = foodatlas, state = "NY", feature = "lila", sensitivity = "medium")
#'
#' plot_state(foodatlas, "Georgia")
#'
plot_state <- function(data, state, feature = c("lila", "la"), sensitivity = c("low", "medium", "high"),
                       title = NULL, subtitle = NULL, caption = NULL,
                       pal = c("YlOrRd", "Red-Yellow", "BluYl")) {
  # Bind data frame column names that R is complaining about
  county <- pop2010 <- county_pop <- .data <- feature_tracts <- county_tracts <- NULL


  if (missing(data)) stop("Argument `data` is required.")
  if (missing(state)) stop("Argument `state` is required.")

  # Least to most sensitive
  lila_vals <- c("li_la_1_20", "li_la_1_10", "li_la_half_10")
  la_vals <- c("la_1_20", "la_1_10", "la_half_10")

  feature <- match.arg(feature)
  sensitivity <- match.arg(sensitivity)

  sensitivity <- switch(sensitivity,
                        "low" = 1,
                        "medium" = 2,
                        "high" = 3)

  feature_val <- switch(feature,
                        "lila" = lila_vals[sensitivity],
                        "la" = la_vals[sensitivity])
  feature_plot <- "feature_percent"

  pal <- match.arg(pal)

  if (!is_plottable(data, state, feature_val, detail = "state_by_county")) {
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

  #### Summarize data ####
  subset_df <- data %>%
    dplyr::filter(state == {{ state }}) %>%
    dplyr::group_by(county) %>%
    dplyr::summarize(county_tracts = dplyr::n(),
                     feature_tracts = sum(.data[[feature_val]])) %>%
    dplyr::mutate(feature_percent = round(feature_tracts / county_tracts, digits = 4) * 100) %>%
    dplyr::mutate(fips = usmap::fips({{ state }}, county))

  #### Setup Palette ####
  get_bin_range <- function(vals) {
    low_bin <- ceiling(min(vals / 10))
    if (low_bin == 0) low_bin <- 1
    high_bin <- ceiling(max(vals / 10))

    return(c("min" = low_bin, "max" = high_bin))
  }

  get_palette <- function(low_bin, high_bin, pal) {
    full_palette <- colorspace::sequential_hcl(n = 10, palette = pal, rev = TRUE)

    return(full_palette[low_bin:high_bin])
  }

  BINS <- get_bin_range(subset_df$feature_percent)

  COLORS <- get_palette(BINS["min"], BINS["max"], pal)

  #### Generate plot ####
  p <- usmap::plot_usmap(regions = c("counties"),
                         include = c(state),
                         data = subset_df,
                         values = feature_plot) +
    ggplot2::scale_fill_gradientn(colors = COLORS) +
    ggplot2::labs(title = title,
                  subtitle = subtitle,
                  caption = caption,
                  fill = fill_str) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14),
                   plot.subtitle = ggplot2::element_text(size = 10),
                   legend.position = "right")

  return(p)
}







#' Plot food access across the US
#'
#' @param data data frame - The foodatlas dataset
#' @param include_counties bool - Plot just state data, or include counties as well
#' @param feature character - The feature to plot, either "lila" or "la"
#' @param sensitivity character - How strict should the plot be about what constitutes a low income or low access area? "low", "medium" or "high"
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
#' plot_us(data = foodatlas, feature = "la")
#'
#' plot_us(foodatlas, include_counties = "TRUE", feature = "lila", sensitivity = "Medium",
#'         title = "Share of tracts per state with low income and low access to groceries")
#'
plot_us <- function(data, include_counties = FALSE, feature = c("lila", "la"),
                    sensitivity = c("low", "medium", "high"),
                    title = NULL, subtitle = NULL, caption = NULL,
                    pal = c("YlOrRd", "Red-Yellow", "BluYl")) {

  if (missing(data)) stop("Argument `data` is required.")

  feature <- match.arg(feature)
  pal <- match.arg(pal)
  sensitivity <- match.arg(sensitivity)

  if (include_counties) {
    print("True")
    # plot_us_by_county()
  } else {
    plot_us_by_state(data, feature, sensitivity, title, subtitle, caption, pal)
  }
}





plot_us_by_state <- function(data, feature = c("lila", "la"),
                             sensitivity = c("low", "medium", "high"),
                             title = NULL, subtitle = NULL, caption = NULL,
                             pal = c("YlOrRd", "Red-Yellow", "BluYl")) {

  if (missing(data)) stop("Argument `data` is required.")

  feature <- match.arg(feature)

  sensitivity <- match.arg(sensitivity)

  sensitivity <- switch(sensitivity,
                        "low" = 1,
                        "medium" = 2,
                        "high" = 3)

  # Least to most sensitive
  lila_vals <- c("li_la_1_20", "li_la_1_10", "li_la_half_10")
  la_vals <- c("la_1_20", "la_1_10", "la_half_10")

  feature_val <- switch(feature,
                        "lila" = lila_vals[sensitivity],
                        "la" = la_vals[sensitivity])

  feature_plot <- "feature_percent"

  pal <- match.arg(pal)

  if (!is_plottable(data, state = NULL, feature_val, detail = "us_by_state")) {
    warning("Could not generate this plot. See warnings.")
    return(NULL)
  }

  #### Generate plot text ####
  # Set title to default if not provided
  if (is.null(title)) {
    title_pre <- paste("Share of tracts per state with")
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
    subtitle <- "2015 - 2019"
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

  #### Summarize data ####
  subset_df <- data %>%
    dplyr::group_by(state) %>%
    dplyr::summarize(state_tracts = dplyr::n(),
                     feature_tracts = sum(.data[[feature_val]])) %>%
    dplyr::mutate(feature_percent = round(feature_tracts / state_tracts, digits = 4) * 100) #%>%
    #dplyr::mutate(fips = usmap::fips({{ state }}, county))

  #### Setup Palette ####
  get_bin_range <- function(vals) {
    low_bin <- ceiling(min(vals / 10))
    if (low_bin == 0) low_bin <- 1
    high_bin <- ceiling(max(vals / 10))

    return(c("min" = low_bin, "max" = high_bin))
  }

  get_palette <- function(low_bin, high_bin, pal) {
    full_palette <- colorspace::sequential_hcl(n = 10, palette = pal, rev = TRUE)

    return(full_palette[low_bin:high_bin])
  }

  BINS <- get_bin_range(subset_df$feature_percent)

  COLORS <- get_palette(BINS["min"], BINS["max"], pal)

  #### Generate plot ####
  p <- usmap::plot_usmap(data = subset_df,
                         values = feature_plot) +
    ggplot2::scale_fill_gradientn(colors = COLORS) +
    ggplot2::labs(title = title,
                  subtitle = subtitle,
                  caption = caption,
                  fill = fill_str) +
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

get_la_lower_bound <- function(pop, flag_status) {
  # At least 500 or at least 0.33
  # This is so flexible as to be meaningless
  if (!flag_status) {
    return(0)
  }
  if (pop > 500) {
    return(500)
  } else {
    # Using ceiling for census tracts that are considered low access but only have a population of 1 and wouldn't be counted otherwise
    return(ceiling(pop * 0.33))
  }
}

get_la_lower <- Vectorize(get_la_lower_bound)


get_la_upper_bound <- function(pop, flag_status) {
  if (!flag_status) {
    return(0)
  } else {
    return(pop)
  }
}

get_la_upper <- Vectorize(get_la_upper_bound)







