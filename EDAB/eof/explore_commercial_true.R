#' Parses discards. Elasmobranchs, lobster, otherfish
#'
#'  Dependent on  pull_commercial_eof.R
#'
#' Discards are only species from landings
#'

# read in the live landings data for the 3 epus of interest
landings <- readRDS(here::here("EDAB/eof/true_landings_EOF.rds"))$comland |>
  dplyr::filter(EPU %in% c("GOM", "GB", "MAB"))
# read in discards and plot together
discards <- readRDS(here::here("EDAB/eof/true_discards_EOF.rds")) |>
  dplyr::filter(EPU %in% c("GOM", "GB", "MAB"))

ll <- landings |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(mt = sum(SPPLIVMT), .groups = "drop") |>
  dplyr::mutate(type = "landings")

dd <- discards |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(mt = sum(DISMT, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "discards")

rbind(ll, dd) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = YEAR, y = mt, color = type))

# plot herring discards and landings
ll <- landings |>
  dplyr::filter(NESPP3 == 168) |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(mt = sum(SPPLIVMT), .groups = "drop") |>
  dplyr::mutate(type = "landings")

dd <- discards |>
  dplyr::filter(NESPP3 == 168) |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(mt = sum(DISMT, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "discards")

rbind(ll, dd) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = YEAR, y = mt, color = type))

# look at which species comprise the most discards
discards |>
  dplyr::group_by(NESPP3, RPATH) |>
  dplyr::summarise(mt = sum(DISMT, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(mt)) |>
  dplyr::slice_head(n = 20) |>
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = factor(RPATH), y = mt)) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "NESPP3", y = "Discard mt")

# split discards by groups
elasmobranchs <- c(
  "Skates",
  "Rays",
  "Sharks",
  "SpinyDogfish",
  "OtherSkates",
  "LittleSkate",
  "SmoothDogfish",
  "Barndoor",
  "WinterSkate"
)
lobster <- "AmLobster"

all_species <- discards |> dplyr::pull(RPATH) |> unique()

otherfish <- setdiff(all_species, c(elasmobranchs, lobster))

# elasmobranch discards
elasmobranch_discards <- discards |>
  dplyr::filter(RPATH %in% elasmobranchs)
#other fish discards
otherfish_discards <- discards |>
  dplyr::filter(RPATH %in% otherfish)
# lobster dicards
lobster_discards <- discards |>
  dplyr::filter(RPATH == lobster)

saveRDS(
  elasmobranch_discards,
  here::here("EDAB/eof/elasmobranch_discards_EOF.rds")
)
saveRDS(otherfish_discards, here::here("EDAB/eof/otherfish_discards_EOF.rds"))
saveRDS(lobster_discards, here::here("EDAB/eof/lobster_discards_EOF.rds"))

# look at which species comprise the most discards
otherfish_discards |>
  dplyr::group_by(RPATH) |>
  dplyr::summarise(mt = sum(DISMT, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(mt)) |>
  dplyr::slice_head(n = 30) |>
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = factor(RPATH), y = mt)) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "NESPP3", y = "Discard mt")
