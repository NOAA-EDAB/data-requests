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
species <- readr::read_csv(here::here("GMRI/Mills_DiFiore/species_list_forAB.csv"))
nespp3s <- species |>
  dplyr::filter(!is.na(NESPP3)) |>
  dplyr::distinct(NESPP3) |>
  dplyr::pull()

## INSTEAD select list of species included in the database (STOCKEFF)
# emailed this list as a reference to the data pull
ss <- DBI::dbGetQuery(channel,"select * from stockeff.mv_cf_species_c")
ss |>
  dplyr::select(SPECIES_ITIS,NESPP4,COMMON_NAME,SCIENTIFIC_NAME) |>
  dplyr::mutate(NESPP3 = substr(NESPP4,1,3)) |>
  dplyr::distinct(SPECIES_ITIS,NESPP3,COMMON_NAME,SCIENTIFIC_NAME) |>
  dplyr::mutate(NESPP3 = as.numeric(NESPP3)) |>
  dplyr::filter(NESPP3>0) |>
  readr::write_csv(file=here::here("GMRI/Mills_DiFiore/nespp3codes.csv"))


# pull data. ignore foregin landings
a <- comlandr::get_comland_data(channel,useForeign = F)


