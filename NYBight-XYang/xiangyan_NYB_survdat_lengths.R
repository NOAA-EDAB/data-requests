# get survey estimated for NYbight strata by length
# provided survdat information for the New York Bight to Xiangyan Yang at Stony
#Brook back in April. The info is here:
#  https://github.com/NOAA-EDAB/data-requests/tree/main/NYBight-XYang
# She was updating me on her progress last week and wanted to know if it
# would be possible to get the same survey data but with length compositions.
# She is using this to parameterize an OSMOSE model for the New York Bight.
# Obviously I no longer have access to run this, but the main difference would
# be running this code with getLengths = T at line 13
# https://github.com/NOAA-EDAB/data-requests/blob/main/NYBight-XYang/xiangyan_NYB_survdat.R
#The object coming out of that would have lengths.
# I think she ultimately wants catch at length from the survey.
# I don’t see a function for this in the survdat package but it would likely
# look a lot like the catch expansion to lengths that Andy does in mscatch
#https://noaa-edab.github.io/mscatch/articles/catchExpansion.html.
#But it should be simpler since its survey.

channel <- dbutils::connect_to_database(server = server, uid = user)

## pul survey data with lengths
data_length <- survdat::get_survdat_data(channel = channel, getLengths = T)

data <- data_length

# strata of interest
NYBstrata <- c(3010:3200, 1010:1080, 1730:1760)

data_length$survdat <- data_length$survdat |>
  dplyr::filter(STRATUM %in% NYBstrata)
data_length$notes <- "The full survdat pull was filterd by STRATA = c(3010:3200, 1010:1080, 1730:1760)"

saveRDS(data_length, here::here("NYBight-XYang/NYBstrata_lengths.rds"))


# swept area by season in strata of interest
NYBspring <- survdat::calc_swept_area(
  surveyData = data$survdat,
  filterByArea = NYBstrata,
  filterBySeason = "SPRING"
)

NYBfall <- survdat::calc_swept_area(
  surveyData = data$survdat,
  filterByArea = NYBstrata,
  filterBySeason = "FALL"
)

# stratified mean by season in strata of interest
NYBspring_mean <- survdat::calc_stratified_mean(
  surveyData = data$survdat,
  filterByArea = NYBstrata,
  filterBySeason = "SPRING"
)

NYBfall_mean <- survdat::calc_stratified_mean(
  surveyData = data$survdat,
  filterByArea = NYBstrata,
  filterBySeason = "FALL"
)

# save all output
saveRDS(NYBspring, here::here("NYBight-XYang/NYBspring_lengths.rds"))
saveRDS(NYBfall, here::here("NYBight-XYang/NYBfall_lengths.rds"))
saveRDS(
  NYBspring_mean,
  here::here("NYBight-XYang/NYBspring_mean_lengths.rds")
)
saveRDS(NYBfall_mean, here::here("NYBight-XYang/NYBfall_mean_lengths.rds"))

# strata areas
area <- sf::st_read(
  dsn = system.file("extdata", "strata.shp", package = "survdat")
)
areas <- survdat::get_area(areaPolygon = area, areaDescription = "STRATA") |>
  dplyr::filter(STRATUM %in% NYBstrata)
saveRDS(areas, here::here("NYBight-XYang/NYBstrata_areas.rds"))
