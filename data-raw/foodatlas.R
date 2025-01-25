# Download and preprocess Food Access Atlas file
lcl <- "data-raw/atlas"
src <- "https://ers.usda.gov/sites/default/files/_laserfiche/DataFiles/80591/2019%20Food%20Access%20Research%20Atlas%20Data.zip?v=60094"
atl <- "Food Access Research Atlas.csv"

os <- .Platform$OS.type

if (os == "windows") {
  d_mode <- "wb"
} else {
  d_mode <- "w"
}

if (!file.exists(lcl)) {
  tmp <- tempfile(fileext = ".zip")
  tst <- download.file(src, tmp, quiet = TRUE, mode = d_mode)

  dir.create(lcl)
  unzip(tmp, exdir = lcl, junkpaths = TRUE)
}

raw_data <- readr::read_csv(file.path(lcl, atl), na = c("", "NA", "NULL"))

# Keep only the columns without a significant number of NAs
foodatlas <- raw_data[,1:25]

# Rename columns for clarity and uniform lowercase
new_colnames <- c("census_tract", "state", "county", "urban_flag", "pop2010",
                  "ohu2010", "group_quarters_flag", "num_in_group_quarters",
                  "pct_in_group_quarters", "li_la_1_10", "li_la_half_10",
                  "li_la_1_20", "li_la_vehicle", "la_lva_flag",
                  "low_income_tracts", "poverty_rate", "median_family_income",
                  "la_1_10", "la_half_10", "la_1_20", "la_tracts_half", "la_tracts_1",
                  "la_tracts_10", "la_tracts_20", "la_tracts_vehicle_20")

colnames(foodatlas) <- new_colnames

foodatlas <- foodatlas %>%
  dplyr::mutate_at(c("urban_flag", "group_quarters_flag", "li_la_1_10", "li_la_half_10",
                     "li_la_1_20", "li_la_vehicle", "la_lva_flag", "low_income_tracts",
                     "la_1_10", "la_half_10", "la_1_20", "la_tracts_half", "la_tracts_1",
                     "la_tracts_10", "la_tracts_20", "la_tracts_vehicle_20"), as.factor)

usethis::use_data(foodatlas, overwrite = TRUE)
