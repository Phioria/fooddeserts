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

foodatlas <- raw_data[,1:25]

usethis::use_data(foodatlas, overwrite = TRUE)
