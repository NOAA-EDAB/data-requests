#' standard survey pull
#'
#'

# channel <-
s <- survdat::get_survdat_data(channel,
                          filterByYear = NA,
                          all.season = F,
                          shg.check = T,
                          conversion.factor = F,
                          use.SAD = F,
                          getBio = F,
                          getLengths = F,
                          getWeightLength = F
                          )


saveRDS(s,here::here("GMRI/DiFiore_survey/survdat.rds"))
