#' standard survey pull
#'
#'

# channel <-
s <- survdat::get_survdat_data(
  channel,
  filterByYear = NA,
  all.season = F,
  shg.check = T,
  conversion.factor = F,
  use.SAD = F,
  getBio = F,
  getLengths = F,
  getWeightLength = F
)


saveRDS(s, here::here("GMRI/DiFiore_survey/survdat.rds"))

## updated pull with conversion factors
# Needed for Fish traits indicator

s2 <- survdat::get_survdat_data(
  channel,
  filterByYear = NA,
  all.season = F,
  shg.check = T,
  conversion.factor = T,
  use.SAD = F,
  getBio = F,
  getLengths = F,
  getWeightLength = F
)
saveRDS(s2, here::here("GMRI/DiFiore_survey/survdat_with_conversion.rds"))

strata_epu <- SOEworkflows::epu_strata |> dplyr::as_tibble()
saveRDS(strata_epu, here::here("GMRI/DiFiore_survey/strata_epu.rds"))
