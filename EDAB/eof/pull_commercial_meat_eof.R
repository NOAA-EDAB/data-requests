#' Pull landings, discards, survey data for EOF
#'
#' Link request
#'
#' Commerical:
#' Live weight shellfish, to match NAFO
#' Add menhaden
#'

channel <- dbutils::connect_to_database("server", "user")
library(data.table)
library(sf)

# Define the stat areas for EPUs
gom <- data.table(AREA = c(500, 510, 512:515), EPU = 'GOM')
gb <- data.table(AREA = c(521:526, 551, 552, 561, 562), EPU = 'GB')
mab <- data.table(
  AREA = c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632),
  EPU = 'MAB'
)
ss <- data.table(AREA = c(463:467, 511), EPU = 'SS')

epuAreas <- rbindlist(list(gom, gb, mab, ss))
epuAreas[, NESPP3 := 1]
epuAreas[, MeanProp := 1]

# plot the EPU areas from the shapefile

NEFSCspatial::Statistical_Areas_2010_withNames |>
  dplyr::left_join(epuAreas, by = c("Id" = "AREA")) |>
  #dplyr::filter(Id %in% c(gom$AREA, gb$AREA, mab$AREA, ss$AREA)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = EPU)) +
  ggplot2::coord_sf(xlim = c(-80, -60), ylim = c(35, 50)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "EPU Areas")

# MEAT weight aggregate areas to epus
land <- comlandr::get_comland_data(
  channel,
  filterByYear = NA,
  useLanded = T,
  aggArea = T,
  userAreas = epuAreas,
  useForeign = T,
  useHerringMaine = T,
  applyProp = F,
  areaDescription = 'EPU',
  propDescription = 'MeanProp',
  aggGear = F,
  refYear = NA,
  refMonth = NA,
  userGears = comlandr::mskeyGears,
  fleetDescription = 'Fleet',
  unkVar = c('MONTH', 'NEGEAR', 'AREA'),
  knStrata = c('HY', 'QY', 'MONTH', 'NEGEAR', 'TONCL2', 'AREA')
)

saveRDS(land, here::here("EDAB/eof/data_pulls/comlandEOFMeat.rds"))
land <- readRDS(here::here("EDAB/eof/data_pulls/comlandEOFMeat.rds"))
# Remove menhaden NESPP3 221 from comland. We have catch separate
land$comland <- land$comland |>
  dplyr::filter(NESPP3 != 221)

# plot total by year
land$comland |>
  dplyr::filter(EPU %in% c("GOM", "GB", "MAB")) |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(mt = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop") |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = YEAR, y = mt)) +
  ggplot2::labs(y = "Landings mt") +
  ggplot2::ggtitle("Total landings by year")

# MEAT weight aggregation gears to fleets and areas to epus
land <- comlandr::get_comland_data(
  channel,
  filterByYear = NA,
  useLanded = T,
  aggArea = T,
  userAreas = epuAreas,
  useForeign = T,
  useHerringMaine = T,
  applyProp = F,
  areaDescription = 'EPU',
  propDescription = 'MeanProp',
  aggGear = T,
  refYear = NA,
  refMonth = NA,
  userGears = comlandr::mskeyGears,
  fleetDescription = 'Fleet',
  unkVar = c('MONTH', 'NEGEAR', 'AREA'),
  knStrata = c('HY', 'QY', 'MONTH', 'NEGEAR', 'TONCL2', 'AREA')
)

saveRDS(land, here::here("EDAB/eof/data_pulls/comlandEOFMeatAggGear.rds"))
land <- readRDS(here::here("EDAB/eof/data_pulls/comlandEOFMeatAggGear.rds"))

# Remove menhaden NESPP3 221 from comland. We have catch separate
land$comland <- land$comland |>
  dplyr::filter(NESPP3 != 221)

# plot total by year
land$comland |>
  dplyr::filter(EPU %in% c("GOM", "GB", "MAB")) |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(mt = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop") |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = YEAR, y = mt)) +
  ggplot2::labs(y = "Landings mt") +
  ggplot2::ggtitle("Total landings by year")

# pull discards for MEAT weight
discs <- comlandr::get_comdisc_data(
  channel,
  land,
  aggArea = T,
  aggGear = T,
  extendTS = T,
  areaDescription = "EPU",
  fleetDescription = "Fleet",
)
saveRDS(
  discs,
  here::here("EDAB/eof/data_pulls/discardscomlandEOFMeatAggGear.rds")
)
discs <- readRDS(here::here(
  "EDAB/eof/data_pulls/discardscomlandEOFMeatAggGear.rds"
))

# only keep species that exist in the landings data
landing_species <- unique(land$comland$NESPP3)
landed_species_discards <- discs$comdisc |>
  dplyr::filter(NESPP3 %in% landing_species)
# seperate discards by species groups. use RPATH groups
groupings <- ecodata::species_groupings |>
  dplyr::select(NESPP3, RPATH) |>
  dplyr::filter(!is.na(NESPP3)) |>
  dplyr::distinct() |>
  dplyr::mutate(
    RPATH = dplyr::case_when(
      grepl("Demersals", RPATH) ~ "Demersals",
      TRUE ~ RPATH
    )
  ) |>
  dplyr::distinct() |>
  dplyr::filter(!(NESPP3 == 363 & RPATH == "Demersals"))

# join RPATH groupings to discards
landed_species_discards <- landed_species_discards |>
  dplyr::left_join(groupings, by = "NESPP3")

nonlanded_species_discards <- discs$comdisc |>
  dplyr::filter(!(NESPP3 %in% landing_species))


#Add Menhaden directly from data provided by ASMFC
menhaden <- data.table::as.data.table(readRDS(here::here(
  "EDAB/eof",
  "menhadenEOF_2025.rds"
)))

# plot menhaden data by EPU
menhaden |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = year, y = MABcatch, color = "MAB")) +
  ggplot2::geom_line(ggplot2::aes(x = year, y = GOMcatch, color = "GOM")) +
  ggplot2::geom_line(ggplot2::aes(x = year, y = NEUScatch, color = "Combined"))

#Get in same format as comland
#Mid-Atlantic
mid.men <- menhaden |>
  dplyr::select(year, MABcatch) |>
  dplyr::rename(YEAR = year, SPPLIVMT = MABcatch) |>
  dplyr::mutate(
    MONTH = 1,
    NESPP3 = 221,
    Fleet = "Pelagic",
    TONCL2 = NA,
    EPU = 'MAB',
    UTILCD = 9,
    MARKET_CODE = 'UN',
    SPPVALUE = 0,
    US = T
  ) |>
  dplyr::relocate(
    YEAR,
    MONTH,
    TONCL2,
    NESPP3,
    UTILCD,
    MARKET_CODE,
    US,
    EPU,
    Fleet,
    SPPLIVMT,
    SPPVALUE
  )

land$comland <- data.table::rbindlist(
  list(land$comland, mid.men),
  use.names = T
)

#GOM
gom.men <- menhaden |>
  dplyr::select(year, GOMcatch) |>
  dplyr::rename(YEAR = year, SPPLIVMT = GOMcatch) |>
  dplyr::mutate(
    MONTH = 1,
    NESPP3 = 221,
    Fleet = "Pelagic",
    TONCL2 = NA,
    EPU = 'GOM',
    UTILCD = 7,
    MARKET_CODE = 'UN',
    SPPVALUE = 0,
    US = T
  ) |>
  dplyr::relocate(
    YEAR,
    MONTH,
    TONCL2,
    NESPP3,
    UTILCD,
    MARKET_CODE,
    US,
    EPU,
    Fleet,
    SPPLIVMT,
    SPPVALUE
  )

land$comland <- data.table::rbindlist(
  list(land$comland, gom.men),
  use.names = T
)

# plot total by year with menhaden added
land$comland |>
  dplyr::filter(EPU %in% c("GOM", "GB", "MAB")) |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(mt = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop") |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = YEAR, y = mt)) +
  ggplot2::labs(y = "Landings mt") +
  ggplot2::ggtitle("Total landings by year")


# write true landings to file
saveRDS(land, here::here("EDAB/eof/data_output/true_landings_meat_EOF.rds"))

# write true discards to file
saveRDS(
  landed_species_discards,
  here::here("EDAB/eof/data_output/true_discards_meat_EOF.rds")
)
