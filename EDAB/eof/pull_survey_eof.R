#' Pull survey data for EOF
#'
#' Link request
#'
#' Survey:
#' Swept area biomass
#'

channel <- dbutils::connect_to_database("server", "user")

#Calculate stratified means
#Strata sets
EPU <- c('MAB', 'GB', 'GOM')
MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
GB <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)
NEUSStrata <- c(MAB, GB, GOM)

# plot the EPU areas from the shapefile
library(sf)
ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = NEFSCspatial::BTS_Strata,
    color = "grey",
    alpha = 0.5
  ) +
  ggplot2::geom_sf(
    data = NEFSCspatial::BTS_Strata %>% dplyr::filter(STRATA %in% GOM),
    color = "green"
  ) +
  ggplot2::geom_sf(
    data = NEFSCspatial::BTS_Strata %>% dplyr::filter(STRATA %in% GB),
    color = "red"
  ) +
  ggplot2::geom_sf(
    data = NEFSCspatial::BTS_Strata %>% dplyr::filter(STRATA %in% MAB),
    color = "blue"
  ) +
  ggplot2::coord_sf(xlim = c(-80, -60), ylim = c(35, 50)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = paste0("EPU Areas, (", paste0(EPU, collapse = ", "), ")")
  )

# Pull survey data
survey_data <- survdat::get_survdat_data(channel, getLengths = FALSE)
# calculate swept area biomass for MAB
biomass_MAB <- survdat::calc_swept_area(
  survey_data$survdat,
  filterByArea = MAB,
  filterBySeason = "all",
  tidy = TRUE
)
biomass_MAB <- biomass_MAB |>
  dplyr::filter(variable == "tot.biomass") |>
  dplyr::mutate(EPU = "MAB")
# calculate swept area biomass for GB
biomass_GB <- survdat::calc_swept_area(
  survey_data$survdat,
  filterByArea = GB,
  filterBySeason = "all",
  tidy = TRUE
)
biomass_GB <- biomass_GB |>
  dplyr::filter(variable == "tot.biomass") |>
  dplyr::mutate(EPU = "GB")
# calculate swept area biomass for GOM
biomass_GOM <- survdat::calc_swept_area(
  survey_data$survdat,
  filterByArea = GOM,
  filterBySeason = "all",
  tidy = TRUE
)
biomass_GOM <- biomass_GOM |>
  dplyr::filter(variable == "tot.biomass") |>
  dplyr::mutate(EPU = "GOM")

# combine all EPUS and convert to metric tons
biomass <- dplyr::bind_rows(biomass_MAB, biomass_GB, biomass_GOM) |>
  dplyr::group_by(YEAR, SVSPP, EPU) |>
  dplyr::summarise(
    mt = sum(value * .001, na.rm = TRUE),
    .groups = "drop"
  )

# grab species and link to RPATH groups
groupings <- ecodata::species_groupings |>
  dplyr::select(SVSPP, RPATH) |>
  dplyr::filter(!is.na(SVSPP)) |>
  dplyr::filter(SVSPP != 0) |>
  dplyr::distinct() |>
  dplyr::mutate(
    RPATH = dplyr::case_when(
      grepl("Demersals", RPATH) ~ "Demersals",
      TRUE ~ RPATH
    )
  ) |>
  dplyr::distinct() |>
  dplyr::filter(!(SVSPP == 413 & RPATH == "Macrobenthos"))

biomass_save <- biomass |>
  dplyr::left_join(groupings, by = "SVSPP") |>
  dplyr::filter(SVSPP != 0) |>
  dplyr::filter(!is.na(RPATH))

saveRDS(
  biomass_save,
  here::here("EDAB/eof/swept_area_biomass_species_EOF.rds")
)

biomass_save |>
  dplyr::group_by(YEAR, EPU) |>
  dplyr::summarise(mt = sum(mt, na.rm = TRUE), .groups = "drop") |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = YEAR, y = mt, color = EPU)) +
  ggplot2::labs(y = "Swept Area Biomass mt") +
  ggplot2::ggtitle("Total swept area biomass by EPU")
