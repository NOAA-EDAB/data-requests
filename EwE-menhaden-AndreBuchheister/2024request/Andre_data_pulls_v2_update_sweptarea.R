#' Calculate the swept area biomass
#'
#'
#'
#'
#'


#Calculate stratified mean/ swept area estimate
#Need to do this on a region by region basis
# define q's by season

calc_swept_area <- function(){

  spring <- ewe.spp |>
    dplyr::select(SVSPP,Spring.q) |>
    dplyr::rename(q = Spring.q) |>
    dplyr::distinct()

  fall <- ewe.spp |>
    dplyr::select(SVSPP,Fall.q) |>
    dplyr::rename(q = Fall.q) |>
    dplyr::distinct()

  sptable <- ewe.spp |>
    dplyr::select(SVSPP,EWE)

  springDat <- NULL
  fallDat <- NULL
  comboDat <- NULL
  for (aregion in regions){
    print(aregion)
    regionalData <- andre |>
      dplyr::filter(SVSPP %in% ewe.spp$SVSPP) |>
      dplyr::filter(Region == aregion)
    regionalPolygon <- surveyPolygon |>
      dplyr::filter(eco == aregion)

    biospring <- survdat::calc_swept_area(surveyData = regionalData,
                                          areaPolygon = regionalPolygon,
                                          areaDescription="STRATA",
                                          filterBySeason = "SPRING",
                                          groupDescription = "SVSPP",
                                          tidy = T,
                                          q = spring)
    sprDat <- biospring |>
      dplyr::left_join(sptable, by = "SVSPP") |>
      dplyr::filter(!is.na(EWE),
                    variable == "tot.biomass") |>
      dplyr::group_by(YEAR,EWE) |>
      dplyr::summarise(tot.biomass = sum(value),
                       .groups="drop") |>
      dplyr::mutate(Region = aregion,
                    AREA = survdat::get_area(regionalPolygon,"eco")$Area,
                    kg.km2 = tot.biomass/AREA) |>
      dplyr::relocate(Region,EWE,YEAR,tot.biomass,AREA,kg.km2)

    springDat <- rbind(springDat,sprDat)

    biofall <- survdat::calc_swept_area(surveyData = regionalData,
                                        areaPolygon = regionalPolygon,
                                        areaDescription="STRATA",
                                        filterBySeason = "FALL",
                                        groupDescription = "SVSPP",
                                        tidy = T,
                                        q = fall)

    fDat <- biofall |>
      dplyr::left_join(sptable, by = "SVSPP") |>
      dplyr::filter(!is.na(EWE),
                    variable == "tot.biomass") |>
      dplyr::group_by(YEAR,EWE) |>
      dplyr::summarise(tot.biomass = sum(value),
                       .groups="drop") |>
      dplyr::mutate(Region = aregion,
                    AREA = survdat::get_area(regionalPolygon,"eco")$Area,
                    kg.km2 = tot.biomass/AREA) |>
      dplyr::relocate(Region,EWE,YEAR,tot.biomass,AREA,kg.km2)

    fallDat <- rbind(fallDat,fDat)


    #combo
    #combine all months use fallqs
    biocombo <- survdat::calc_swept_area(surveyData = regionalData,
                                         areaPolygon = regionalPolygon,
                                         areaDescription="STRATA",
                                         filterBySeason = "all",
                                         groupDescription = "SVSPP",
                                         tidy = T,
                                         q = fall)
    combDat <- biocombo |>
      dplyr::left_join(sptable, by = "SVSPP") |>
      dplyr::filter(!is.na(EWE),
                    variable == "tot.biomass") |>
      dplyr::group_by(YEAR,EWE) |>
      dplyr::summarise(tot.biomass = sum(value),
                       .groups="drop") |>
      dplyr::mutate(Region = aregion,
                    AREA = survdat::get_area(regionalPolygon,"eco")$Area,
                    kg.km2 = tot.biomass/AREA) |>
      dplyr::relocate(Region,EWE,YEAR,tot.biomass,AREA,kg.km2)

    comboDat <- rbind(comboDat,combDat)


  }

  return(list(springDat=springDat,fallDat=fallDat,comboDat=comboDat))

}
