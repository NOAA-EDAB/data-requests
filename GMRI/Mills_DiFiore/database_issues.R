## potential databse issues


channel <- dbutils::connect_to_database("NEFSC_USERS","user")
# pull data. ignore foreign landings
comData <- comlandr::get_comland_data(channel,
                                      useForeign = F,
                                      filterByYear = 1964:2023,
                                      aggArea = F,
                                      aggGear = F,
                                      unkVar = c("MONTH", "NEGEAR", "AREA"),
                                      knStrata = c("HY", "QY", "MONTH", "NEGEAR", "TONCL2", "AREA"))

comData <- readRDS(here::here("GMRI/Mills_DiFiore/comdat.rds"))

## pull list of species included in the database (STOCKEFF)
ss <- DBI::dbGetQuery(channel,"select * from stockeff.mv_cf_species_c")
speciesCodes <- ss |>
  dplyr::select(SPECIES_ITIS,NESPP4,COMMON_NAME,SCIENTIFIC_NAME) |>
  dplyr::mutate(NESPP3 = substr(NESPP4,1,3)) |>
  dplyr::distinct(SPECIES_ITIS,NESPP3,COMMON_NAME,SCIENTIFIC_NAME) |>
  dplyr::mutate(NESPP3 = as.numeric(NESPP3)) |>
  dplyr::filter(NESPP3>0) |>
  dplyr::as_tibble()

# create a table mlisting all species found in landings data
speciesInData <- comData$comland |>
  dplyr::select(NESPP3) |>
  dplyr::distinct(NESPP3) |>
  dplyr::as_tibble() |>
  dplyr::left_join(speciesCodes, by ="NESPP3")

# find species not listed in lookup but found in data
missingnespp3 <- speciesInData |>
  dplyr::filter(is.na(SPECIES_ITIS)) |>
  dplyr::pull(NESPP3)

# plot landings of the missing species
comData$comland |>
  dplyr::filter(NESPP3 %in% missingnespp3) |>
  ggplot2::ggplot(ggplot2::aes(x=YEAR,y = SPPLIVMT)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~NESPP3) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

