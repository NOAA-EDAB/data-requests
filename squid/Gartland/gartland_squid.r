#' Squid data for Gartland
#'
#' Working off Seans previous script
#'
#'
#'

library(survdat); library(data.table); library(here)

channel <- dbutils::connect_to_database('NEFSC_USEERS', 'user')

survdat <- survdat::get_survdat_data(channel)

#Catch data for illex (SVSPP == 502) and loligo (503) - including zero catches
all.stations <- unique(survdat$survdat[, list(YEAR, SEASON, CRUISE6, STATION, STRATUM, TOW), ])

illex <- unique(survdat$survdat[SVSPP == 502 , list(CRUISE6, STATION, STRATUM, TOW, BIOMASS, ABUNDANCE)])
data.table::setnames(illex, c('BIOMASS', 'ABUNDANCE'), c('ILLEX_BIO', 'ILLEX_ABUND'))

loligo <- unique(survdat$survdat[SVSPP == 503 , list(CRUISE6, STATION, STRATUM, TOW, BIOMASS, ABUNDANCE)])
data.table::setnames(loligo, c('BIOMASS', 'ABUNDANCE'), c('LOLIGO_BIO', 'LOLIGO_ABUND'))

all.catch <- data.table::merge.data.table(all.stations, illex,  all = T)
all.catch <- data.table::merge.data.table(all.catch,    loligo, all = T)

data.table::setcolorder(all.catch, c('YEAR', 'SEASON'))

save(all.catch, file = here::here('data/squid/Gartland', 'Gartland_squid_catch2024.RData'))

#Length data
squid.length <- survdat$survdat[SVSPP %in% c(502, 503), ]
save(squid.length, file = here::here('data', 'Gartland_squid_length.RData'))
