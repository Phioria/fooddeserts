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

# After reaching out to the USDA in regards to the meaning of the "NULL"s present in the file,
# I learned that they can be treated as zeros. They do not represent missing data.
# readr will pull in most columns as type character due to the "NULL" strings present.
# Substitute "0" for "NULL" and then convert appropriate columns to numeric.
# guess_max = Inf necessary to prevent issues with read_csv assuming a column is numeric
# when only reading the first 1000 lines. In some cases the first "NULL" is after 1000 lines.
# TODO We could speed this up by telling read_csv what each column should be.
raw_data <- readr::read_csv(file.path(lcl, atl), na = c("", "NA"), guess_max = Inf)

# Working with a limited number of features for now
foodatlas <- raw_data[,1:25]

foodatlas[foodatlas == "NULL"] <- "0"
foodatlas <- foodatlas %>%
  dplyr::mutate_at(c(4:ncol(foodatlas)), as.numeric)

# Rename columns for clarity and uniform lowercase
new_colnames <- c("census_tract", "state", "county", "urban_flag", "pop2010",
                  "ohu2010", "group_quarters_flag", "num_in_group_quarters",
                  "pct_in_group_quarters", "li_la_1_10", "li_la_half_10",
                  "li_la_1_20", "li_la_vehicle", "la_lva_flag",
                  "low_income_tracts", "poverty_rate", "median_family_income",
                  "la_1_10", "la_half_10", "la_1_20", "la_tracts_half", "la_tracts_1",
                  "la_tracts_10", "la_tracts_20", "la_tracts_vehicle_20")

colnames(foodatlas) <- new_colnames

# Remove Alaska observations until we can convert the old county data to their new counties.
foodatlas <- foodatlas %>%
  dplyr::filter(state != "Alaska")

# Convert Connecticut county names to work with usmapdata/usmap
foodatlas_only_ct <- foodatlas %>%
  dplyr::filter(state == "Connecticut")

ct_tracts <- foodatlas_only_ct$census_tract

# Couldn't do a simple name swap.
# New counties have different shapes encapsulating different census tracts.
# Iterate through each census tract and change them to the new associated county.
# The object ct_data was stored in the package via usethis::use_data
for (tract in ct_tracts) {
  tract_idx <- which(ct_data$census_tract == tract)
  new_county <- ct_data$new_county[tract_idx]
  tgt_idx <- which(foodatlas$census_tract == tract)
  foodatlas$county[tgt_idx] <- new_county
}

# Clean up, Clean up
rm(lcl, src, atl, os, d_mode, raw_data, new_colnames, foodatlas_only_ct,
   ct_tracts, tract, tract_idx, new_county, tgt_idx)

usethis::use_data(foodatlas, overwrite = TRUE)
