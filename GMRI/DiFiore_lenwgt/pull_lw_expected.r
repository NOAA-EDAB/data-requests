#' lengths and expected weight
#'


#channel <-

s <- survdat::get_survdat_data(channel = channel,
                               filterByYear = NA,
                               all.season = F,
                               shg.check = T,
                               conversion.factor = T,
                               use.SAD = F,
                               getBio = F,
                               getLengths = T,
                               getWeightLength = T)

saveRDS(s,here::here("GMRI/DiFiore_lenwgt/survdat_lw.rds"))
