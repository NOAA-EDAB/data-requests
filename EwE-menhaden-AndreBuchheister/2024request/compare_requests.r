#' plot andre 2014 and 2024 data sets
#'
#' See how well they align
#'
#'@params season. character. the season used to estimate swept area biomass

compare_requests <- function(season = "fall") {

  # this is attempt at running script again
  data2024 <- readr::read_csv(here::here("EwE-menhaden-AndreBuchheister/2024request/Buchheister_request_biomass.csv"),
                              show_col_types = F) |>
    dplyr::mutate(source = as.factor(2024))
  # this is data previously sent
  data2014 <- readr::read_csv(here::here("EwE-menhaden-AndreBuchheister/2014request/Buchheister_request_biomass.csv"),
                              show_col_types = F) |>
    dplyr::mutate(source = as.factor(2014)) |>
    dplyr::rename(tot.biomass = Tot.biomass)

  # uses survdat spring q's
  data2024_spring <- readr::read_csv(here::here("EwE-menhaden-AndreBuchheister/2024request/Buchheister_request_biomass_2024_spring.csv"),
                                     show_col_types = F) |>
    dplyr::mutate(source = as.factor("2024_spring"))
  # uses survdat with fall qs
  data2024_fall <- readr::read_csv(here::here("EwE-menhaden-AndreBuchheister/2024request/Buchheister_request_biomass_2024_fall.csv"),
                                   show_col_types = F) |>
    dplyr::mutate(source = as.factor("2024_fall"))
  # uses survdat with fall qs but combined seasons
  data2024_combo <- readr::read_csv(here::here("EwE-menhaden-AndreBuchheister/2024request/Buchheister_request_biomass_2024_combo.csv"),
                                    show_col_types = F) |>
    dplyr::mutate(source = as.factor("2024_combo"))


  #allData <-  rbind(data2014,data2024,data2024_spring,data2024_fall)
  if (season == "fall") {
    allData <-  rbind(data2014,data2024_fall)
  } else if (season == "spring") {

    allData <-  rbind(data2014,data2024_spring)
  } else {
    # combination
    allData <-  rbind(data2014,data2024_combo)
  }


  regions <- unique(allData$Region)

  # plot total biomass 2014 vs 2024 by species/region
  for (aregion in regions) {

    regionalData <- allData |>
      dplyr::filter(Region == aregion)

    p <- ggplot2::ggplot(data = regionalData,
                         mapping=ggplot2::aes(x=YEAR,y=tot.biomass,color = source)) +
      ggplot2::geom_point(size = 0.5)+
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~EWE, scales = "free_y") +
      ggplot2::ggtitle(paste0(aregion,"-",season)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7,angle = 90, vjust = 0.5, hjust=1),
                     axis.text.y = ggplot2::element_text(size = 7),
                     strip.text.x = ggplot2::element_text(size = 5))+
      ggplot2::theme(legend.position="bottom")

    print(p)


  }
  #
  # allData |>
  #   dplyr::select(Region,AREA,source) |>
  #   dplyr::distinct() |>
  #   tidyr::pivot_wider(id_cols=Region,names_from = "source",values_from = AREA)
  #
  # ## plot biomass/km2 2014 vs 2024 by species/region
  # for (aregion in regions[1]) {
  #
  #   regionalData <- allData |>
  #     dplyr::filter(Region == aregion)
  #
  #   p <- ggplot2::ggplot(data = regionalData) +
  #     ggplot2::geom_line(ggplot2::aes(x=YEAR,y=kg.km2,color=source))+
  #     ggplot2::facet_wrap(~EWE, scales = "free_y") +
  #     ggplot2::ggtitle(aregion) +
  #     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  #
  #   print(p)


}

