#' Determine the major fleets/ gear codes catching menhaden from cfdbs

noagg <- readRDS(here::here("EDAB/eof/data_pulls/comlandEOFLive.rds"))


dd <- noagg$comland |>
  dplyr::filter(NESPP3 == 221, EPU %in% c("GB", "MAB", "GOM"), US == TRUE) |>
  dplyr::mutate(NEGEAR2 = as.numeric(substr(NEGEAR, 1, nchar(NEGEAR) - 1))) |>
  dplyr::group_by(YEAR, EPU, NEGEAR2) |>
  dplyr::summarise(mt = sum(SPPLIVMT, na.rm = TRUE))


gear_table <- comlandr::mskeyGears |>
  dplyr::select(-MESHCAT) |>
  dplyr::distinct() |>
  dplyr::slice(-1)

newdd <- dd |>
  dplyr::left_join(gear_table, by = "NEGEAR2")

newdd |>
  ggplot2::ggplot(ggplot2::aes(x = YEAR, y = mt, color = as.factor(Fleet))) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~EPU) +
  ggplot2::labs(
    title = "NEGEAR 221 in US EPU regions",
    x = "Year",
    y = "Metric Tons"
  )
