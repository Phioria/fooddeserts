#' USDA Food Access Research Atlas
#'
#' A subset of data from the USDA's Food Access Research Atlas dataset
#'
#' @format ## `foodatlas`
#' A data frame with 72,531 rows and 25 columns
#' \describe{
#'   \item{census_tract}{Census tract number}
#'   \item{state}{State name}
#'   \item{county}{County name}
#'   \item{urban_flag}{Flag for urban tract}
#'   \item{pop2010}{Population count from 2010 census}
#'   \item{ohu2010}{Occupied housing unit count from 2010 census}
#'   \item{group_quarters_flag}{Flag for tract where >=67%}
#'   \item{num_in_group_quarters}{Count of tract population residing in group quarters}
#'   \item{pct_in_group_quarters}{Percent of tract population residing in group quarters}
#'   \item{li_la_1_10}{Flag for low-income and low access when considering low accessibilty at 1 and 10 miles}
#'   \item{li_la_half_10}{Flag for low-income and low access when considering low accessibilty at 1/2 and 10 miles}
#'   \item{li_la_1_20}{Flag for low-income and low access when considering low accessibilty at 1 and 20 miles}
#'   \item{li_la_vehicle}{Flag for low-income and low access when considering vehicle access or at 20 miles}
#'   \item{la_lva_flag}{Flag for tract where >= 100 of households do not have a vehicle, and beyond 1/2 mile from supermarket}
#'   \item{low_income_tracts}{Flag for low income tract}
#'   \item{poverty_rate}{Share of the tract population living with income at or below the Federal poverty thresholds for family size}
#'   \item{median_family_income}{Tract median family income}
#'   \item{la_1_10}{Flag for low access tract at 1 mile for urban areas or 10 miles for rural areas}
#'   \item{la_half_10}{Flag for low access tract at 1/2 mile for urban areas or 10 miles for rural areas}
#'   \item{la_1_20}{Flag for low access tract at 1 mile for urban areas or 20 miles for rural areas}
#'   \item{la_tracts_half}{Flag for low access tract when considering 1/2 mile distance}
#'   \item{la_tracts_1}{Flag for low access tract when considering 1 mile distance}
#'   \item{la_tracts_10}{Flag for low access tract when considering 10 mile distance}
#'   \item{la_tracts_20}{Flag for low access tract when considering 20 mile distance}
#'   \item{la_tracts_vehicle_20}{Flag for tract where >= 100 of households do not have a vehicle, and beyond 1/2 mile from supermarket; or >= 500 individuals are beyond 20 miles from supermarket ; or >= 33% of individuals are beyond 20 miles from supermarket}
#' }
#' @source <https://www.ers.usda.gov/data-products/food-access-research-atlas/download-the-data>
"foodatlas"
