#' Aggregated Commercial landings
#'
#' We are interested in a single file with YEAR, AREA, METRIC TONS,
#' where area is statistical area, and metric tons represents the total landings
#' across all species in the list. I realize that the majority of species we are
#' examining are not commercially important. However, we are trying to better
#' understand how landings (generally) could shift the distribution of traits
#'  across all species.
#'
#'
#'

channel <- dbutils::connect_to_database("NEFSC_USERS","user")

# species list from Bart. Many species requested have Na nespp3A
speciesFromBart <- readr::read_csv(here::here("GMRI/Mills_DiFiore/species_list_forAB.csv"))
nespp3s <- speciesFromBart |>
  dplyr::filter(!is.na(NESPP3)) |>
  dplyr::distinct(NESPP3) |>
  dplyr::pull()

## INSTEAD select list of species included in the database (STOCKEFF)
# emailed this list as a reference to the data pull
ss <- DBI::dbGetQuery(channel,"select * from stockeff.mv_cf_species_c")
speciesCodes <- ss |>
  dplyr::select(SPECIES_ITIS,NESPP4,COMMON_NAME,SCIENTIFIC_NAME) |>
  dplyr::mutate(NESPP3 = substr(NESPP4,1,3)) |>
  dplyr::distinct(SPECIES_ITIS,NESPP3,COMMON_NAME,SCIENTIFIC_NAME) |>
  dplyr::mutate(NESPP3 = as.numeric(NESPP3)) |>
  dplyr::filter(NESPP3>0) |>
  dplyr::as_tibble()

readr::write_csv(speciesCodes,file=here::here("GMRI/Mills_DiFiore/nespp3codes.csv"))


# pull data. ignore foreign landings
comData <- comlandr::get_comland_data(channel,
                                      useForeign = F,
                                      filterByYear = 1964:2023,
                                      aggArea = F,
                                      aggGear = F,
                                      unkVar = c("MONTH", "NEGEAR", "AREA"),
                                      knStrata = c("HY", "QY", "MONTH", "NEGEAR", "TONCL2", "AREA"))

saveRDS(comData,here::here("GMRI/Mills_DiFiore/comdat.rds"))
comData <- readRDS(here::here("GMRI/Mills_DiFiore/comdat.rds"))

# filter data by stat areas used in SOE for GB,GOM,MAB,SS
gom <- data.table::data.table(AREA = c(500, 510, 512:515), EPU = 'GOM')
gb  <- data.table::data.table(AREA = c(521:526, 551, 552, 561, 562), EPU = 'GB')
mab <- data.table::data.table(AREA = c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632),
                  EPU = 'MAB')
ss  <- data.table::data.table(AREA = c(463:467, 511), EPU = 'SS')
epuAreas <- data.table::rbindlist(list(gom, gb, mab, ss)) |>
  dplyr::as_tibble()

# create a table mapping Barts selected species with what is in the data
# fist join nespp3 codes found in data with species names
speciesInData <- comData$comland |>
  dplyr::filter(AREA %in% epuAreas$AREA) |>
  dplyr::select(NESPP3) |>
  dplyr::distinct(NESPP3) |>
  dplyr::as_tibble() |>
  dplyr::left_join(speciesCodes, by ="NESPP3")

# join this with Barts list to determine other species he might want to include
# this was emailed to refine list.
speciesFromBart |>
  dplyr::filter(!is.na(NESPP3)) |>
  dplyr::full_join(speciesInData,by="NESPP3") |>
  dplyr::distinct() |>
  readr::write_csv(here::here("GMRI/Mills_DiFiore/mapCodes.csv"))








