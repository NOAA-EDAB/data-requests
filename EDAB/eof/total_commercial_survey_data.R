#' Write out final totals for commercial landings and discards, survey data
#'
#' Link request
#'
#' See data_notes.rmd for breakdown
terminalYear <- 2024

#read in landings and discards
landings <- readRDS(here::here("EDAB/eof/comlandEOFLiveAggGear.rds"))$comland |>
  dplyr::filter(YEAR <= terminalYear)
discards <- readRDS(here::here("EDAB/eof/otherfish_discards_EOF.rds")) |>
  dplyr::filter(YEAR <= terminalYear)
discards_elasmobranchs <- readRDS(here::here(
  "EDAB/eof/elasmobranch_discards_EOF.rds"
)) |>
  dplyr::filter(YEAR <= terminalYear)

# aggregate landings and discards by year and type
ll <- landings |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(mt = sum(SPPLIVMT), .groups = "drop") |>
  dplyr::mutate(type = "landings")

dd <- discards |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(mt = sum(DISMT, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "fish_discards")

ee <- discards_elasmobranchs |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(mt = sum(DISMT, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "elasmobranch_discards")

# Combine landings and discards
total_landings_discards <- rbind(ll, dd, ee) |>
  dplyr::group_by(YEAR, type) |>
  dplyr::summarise(mt = sum(mt, na.rm = TRUE), .groups = "drop")

# Plot total landings and discards
total_landings_discards |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = YEAR, y = mt, color = type)) +
  ggplot2::labs(y = "Landings mt") +
  ggplot2::ggtitle("Total landings and discards combined")

# Save the final totals
saveRDS(
  total_landings_discards,
  here::here("EDAB/eof/total_landings_discards.rds")
)
