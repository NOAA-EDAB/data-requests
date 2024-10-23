#' Run comlandr for Rpath fleet designations
#'
#'
#'
#'

rpathGears <- comlandr::mskeyGears

library(data.table)
gom <- data.table(AREA = c(500, 510, 512:515), EPU = 'GOM')
gb  <- data.table(AREA = c(521:526, 551, 552, 561, 562), EPU = 'GB')
mab <- data.table(AREA = c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632),
                  EPU = 'MAB')
ss  <- data.table(AREA = c(463:467, 511), EPU = 'SS')

epuAreas <- rbindlist(list(gom, gb, mab, ss))
epuAreas[, NESPP3 := 1]
epuAreas[, MeanProp := 1]

# pull aggregated data
dataPull <- comlandr::get_comland_data(channel,
                                filterByYear = NA,
                                aggArea = T,
                                userAreas = epuAreas,
                                applyProp = F,
                                areaDescription = 'EPU',
                                propDescription = 'MeanProp',
                                aggGear = T,
                                refYear = 2022,
                                refMonth = 1,
                                userGears = rpathGears,
                                fleetDescription = 'Fleet',
                                unkVar = c('MONTH','NEGEAR','AREA'),
                                knStrata = c('HY', 'QY','MONTH','NEGEAR', 'TONCL2', 'AREA')
)

# filter out NAs for EPUs (outside NEUS domain)
# Relabel NA fleets as "Other" (Not sure which NEGEAR codes are not mapped to a fleet
# Probably should check this and quantify associated landings )
dataPull$comland <- dataPull$comland |>
  dplyr::filter(!is.na(EPU)) |>
  dplyr::mutate(Fleet = dplyr::case_when(is.na(Fleet) ~ "Other",
                                         .default = Fleet))



saveRDS(dataPull,here::here("Rpath","comlandRpath.rds"))
rpathDiscards <- comlandr::get_comdisc_data(channel,
                                         comland=dataPull,
                                         aggArea=T,
                                         aggGear=T,
                                         areaDescription = "EPU",
                                         fleetDescription = "Fleet",
                                         extendTS = T)

saveRDS(rpathDiscards,here::here("Rpath","comdiscRpath.rds"))
