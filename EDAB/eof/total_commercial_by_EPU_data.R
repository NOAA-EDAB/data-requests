#' Write out final totals for commercial landings and discards, survey data
#' Note: since we aren't using shellfish discards meat and live weight will be the same
#' for all other species
#'
#'
#' Link request
#'
#' See data_notes.rmd for breakdown
#'
#' dependent on pull_commercial_eof.R and
#'
terminalYear <- 2024

#read in landings and discards
landings_meat <- readRDS(here::here(
  "EDAB/eof/data_output/true_landings_meat_EOF.rds"
))$comland |>
  dplyr::filter(YEAR <= terminalYear) |>
  dplyr::filter(EPU %in% c("GOM", "GB", "MAB"))

landings_live <- readRDS(here::here(
  "EDAB/eof/data_output/true_landings_live_EOF.rds"
))$comland |>
  dplyr::filter(YEAR <= terminalYear) |>
  dplyr::filter(EPU %in% c("GOM", "GB", "MAB"))

discards <- readRDS(here::here(
  "EDAB/eof/data_output/otherfish_discards_EOF.rds"
)) |>
  dplyr::filter(YEAR <= terminalYear)
discards_elasmobranchs <- readRDS(here::here(
  "EDAB/eof/data_output/elasmobranch_discards_EOF.rds"
)) |>
  dplyr::filter(YEAR <= terminalYear)
discards_shellfish <- readRDS(here::here(
  "EDAB/eof/data_output/shellfish_discards_EOF.rds"
)) |>
  dplyr::filter(YEAR <= terminalYear)
discards_shellfish_live <- readRDS(here::here(
  "EDAB/eof/data_output/shellfish_discards_live_EOF.rds"
)) |>
  dplyr::filter(YEAR <= terminalYear)

# aggregate landings and discards by year and type
ll_meat <- landings_meat |>
  dplyr::group_by(YEAR, EPU) |>
  dplyr::summarise(mt = sum(SPPLIVMT), .groups = "drop") |>
  dplyr::mutate(type = "landings_meat")

# aggregate landings and discards by year and type
ll_live <- landings_live |>
  dplyr::group_by(YEAR, EPU) |>
  dplyr::summarise(mt = sum(SPPLIVMT), .groups = "drop") |>
  dplyr::mutate(type = "landings_live")

dd <- discards |>
  dplyr::group_by(YEAR, EPU) |>
  dplyr::summarise(mt = sum(DISMT, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "fish_discards")

ee <- discards_elasmobranchs |>
  dplyr::group_by(YEAR, EPU) |>
  dplyr::summarise(mt = sum(DISMT, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "elasmobranch_discards")

# aggregate shellfish discards by year and type
ss_meat <- discards_shellfish |>
  dplyr::group_by(YEAR, EPU) |>
  dplyr::summarise(mt = sum(DISMT, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "shellfish_discards_meat")
ss_live <- discards_shellfish_live |>
  dplyr::group_by(YEAR, EPU) |>
  dplyr::summarise(mt = sum(DISMT, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "shellfish_discards_live")

# Combine landings and discards
total_landings_discards <- rbind(ll_meat, ll_live, dd, ee, ss_meat, ss_live) |>
  dplyr::group_by(YEAR, EPU, type) |>
  dplyr::summarise(mt = sum(mt, na.rm = TRUE), .groups = "drop")

# Plot total landings and discards
total_landings_discards |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = YEAR, y = mt, color = type)) +
  ggplot2::facet_wrap(~EPU) +
  ggplot2::labs(y = "Landings mt") +
  ggplot2::ggtitle("Total landings and discards combined")

# Save the final totals
saveRDS(
  total_landings_discards,
  here::here("EDAB/eof/data_output/total_landings_discards_by_EPU.rds")
)
