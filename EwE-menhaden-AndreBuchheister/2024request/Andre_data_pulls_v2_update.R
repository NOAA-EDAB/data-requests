#' Code split up into several parts
#'
#' 1. Reads in and wrangles the survey data
#' 2. Estimates swept are biomass
#'
#'
#'

# this is seans code, slightly tweaked.
source(here::here("EwE-menhaden-AndreBuchheister/2024request/Andre_data_pulls_v2_update_pulldata.r"))
# this is new using survdat
source(here::here("EwE-menhaden-AndreBuchheister/2024request/Andre_data_pulls_v2_update_sweptarea.r"))

writeToFile <- F

# reads in data (Seans code)
a <- andreprep()

regions <- a$regions
andre <- a$andre
stratum <- a$stratum
ewe.spp <- a$ewe.spp
andrePolygon <- a$polygon
surveyPolygon <- a$surveyPolygon

# use survdat to calculate swept are in 3 different ways,
# use fall, spring, and a combination
sweptarea <- calc_swept_area()


if(writeToFile) {
  write.csv(sweptarea$springDat,file = here::here("EwE-menhaden-AndreBuchheister/2024request/Buchheister_request_biomass_2024_spring.csv"), row.names = F)
  write.csv(sweptarea$fallDat,file = here::here("EwE-menhaden-AndreBuchheister/2024request/Buchheister_request_biomass_2024_fall.csv"), row.names = F)
  write.csv(sweptarea$comboDat,file = here::here("EwE-menhaden-AndreBuchheister/2024request/Buchheister_request_biomass_2024_combo.csv"), row.names = F)
}
