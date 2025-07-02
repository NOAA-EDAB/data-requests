#' Demersal species biomass and landings for ROB surplus production model
#'
#'
terminal_year <- 2024
survey <- readRDS(here::here(
  "EDAB/eof/data_output/swept_area_biomass_species_EOF.rds"
))
commercial <- readRDS(here::here(
  "EDAB/eof/data_output/true_landings_meat_EOF.rds"
))$comland |>
  dplyr::filter(EPU %in% c("GOM", "GB", "MAB"))


# RPATH names as defined in ecdata::species_groupings
demersal_species <- c(
  "Demersals",
  "SpinyDogfish",
  "OtherSkates",
  "Barndoor",
  "WinterSkate",
  "LittleSkate",
  "Cod",
  "Haddock",
  "Pollock",
  "AmPlaice",
  "SummerFlounder",
  "Fourspot",
  "YTFlounder",
  "WinterFlounder",
  "WitchFlounder",
  "Windowpane",
  "SmFlatfishes",
  "BlackSeaBass",
  "Goosefish",
  "Redfish",
  "SmoothDogfish",
  "OtherFlatfish"
)

demersal_nespp3 <- ecodata::species_groupings |>
  dplyr::filter(RPATH %in% demersal_species) |>
  dplyr::distinct(NESPP3) |>
  dplyr::pull()


sd <- survey |>
  dplyr::filter(RPATH %in% demersal_species) |>
  dplyr::group_by(YEAR, EPU) |>
  dplyr::summarise(mt = sum(mt, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(source = "survey")

cd <- commercial |>
  dplyr::filter(NESPP3 %in% demersal_nespp3) |>
  dplyr::group_by(YEAR, EPU) |>
  dplyr::summarise(mt = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(source = "commercial")

demersal_data <- rbind(sd, cd) |>
  dplyr::filter(YEAR <= terminal_year)

demersal_data |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = YEAR, y = mt, color = source)) +
  ggplot2::facet_wrap(~EPU, scales = "free")
