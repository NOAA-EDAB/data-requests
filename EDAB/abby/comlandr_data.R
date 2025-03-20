#' Data to be used in comlandr dev
#'
#' herring, comlandr
#'
#'

channel <- dbutils::connect_to_database("server","user")

# herring data pull
sql <- "select year, month, category, stock_area, negear, gearname,
keptmt, discmt from NEFSC_GARFO.maine_herring_catch"

sqlall <- "select * from NEFSC_GARFO.maine_herring_catch"

herring <- list()
herring$data <- DBI::dbGetQuery(channel, sql)
herring$sql <- sql

herringAll <- list()
herringAll$data <- DBI::dbGetQuery(channel, sqlall)
herringAll$sql <- sqlall


saveRDS(herring, here::here("EDAB/abby/herring.rds"))
saveRDS(herringAll, here::here("EDAB/abby/herringAll.rds"))

# comlandr pull
# no aggregation of fleets, or areas, no imputation
comlandData <- comlandr::get_comland_data(channel,
                                          aggArea = F,
                                          aggGear = F,
                                          disagSkatesHakes = F,
                                          refMonth = 10,
                                          refYear = 2024,
                                          unkVar = NULL)

saveRDS(comlandData,here::here("EDAB/abby/comlandNoAggregationNoImputation.rds"))
# + missing data imputed
comlandData <- comlandr::get_comland_data(channel,
                                          aggArea = F,
                                          aggGear = F,
                                          disagSkatesHakes = F,
                                          refMonth = 10,
                                          refYear = 2024,
                                          unkVar = c("MONTH", "NEGEAR", "AREA"))

saveRDS(comlandData,here::here("EDAB/abby/comlandNoAggregation.rds"))
# + aggreation of area and gear
# NEED TO SET UP DATA FRAME FOR EPU AREAS AND GEARS
library(data.table)
gom <- data.table(AREA = c(500, 510, 512:515), EPU = 'GOM')
gb  <- data.table(AREA = c(521:526, 551, 552, 561, 562), EPU = 'GB')
mab <- data.table(AREA = c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632),
                  EPU = 'MAB')
ss  <- data.table(AREA = c(463:467, 511), EPU = 'SS')

epuAreas <- rbindlist(list(gom, gb, mab, ss))
epuAreas[, NESPP3 := 1]
epuAreas[, MeanProp := 1]

comlandData <- comlandr::get_comland_data(channel,
                                          aggArea = T,
                                          userAreas = epuAreas,
                                          aggGear = T,
                                          userGears = comlandr::mskeyGears,
                                          disagSkatesHakes = F,
                                          refMonth = 10,
                                          refYear = 2024,
                                          unkVar = c("MONTH", "NEGEAR", "AREA"))

saveRDS(comlandData,here::here("EDAB/abby/comland.rds"))
