#' Swept area biomass - Biodiversity Research Institute
#'
#' I am a researcher at the Biodiversity Research Institute conducting
#' a study that wants to utilize biomass estimates from the bottom
#' trawl survey in the northwest Atlantic. I am trying to use the
#' 'survdat' package in R to collect biomass estimates for certain
#'  study areas, but it seems like I need permission to access the
#'  required server (username and password needed). Is this something
#'   you could help me with? Sarah Weisberg from Stony Brook
#'   and Philip Politis referred me to you for this.

channel <- dbutils::connect_to_database("NEFSC_USERS","user")

## pull survey data
data <- survdat::get_survdat_data(channel)

# select unique lat and longs for plotting
points <- data$survdat |>
  dplyr::select("LAT","LON") |>
  dplyr::distinct()


# specify spatial footprint
area1 <- sf::st_read(dsn = here::here("BRI/KimLato/gis/Empire_60km_rounded.shp"),quiet=T)
area2 <- sf::st_read(dsn = here::here("BRI/KimLato/gis/MassRI_60km_rounded.shp"),quiet=T)

# plot stations on polygon map
survdat::plot_data_area(points=points, polygons=area1)

# calculate swept area biomass by season for each area
# spring area1
spr1 <- survdat::calc_swept_area(surveyData=data$survdat,
                areaPolygon=area1,
                areaDescription="Id",
                filterByArea="all",
                filterBySeason = "SPRING",
                tidy=T) |>
  dplyr::mutate(SEASON = "SPRING",
                AREA = "Empire")
# fall area1
fall1 <- survdat::calc_swept_area(surveyData=data$survdat,
                areaPolygon=area1,
                areaDescription="Id",
                filterByArea="all",
                filterBySeason = "FALL",
                tidy=T)   |>
  dplyr::mutate(SEASON = "FALL",
                AREA = "Empire")

# spring area2
spr2 <- survdat::calc_swept_area(surveyData=data$survdat,
                                 areaPolygon=area2,
                                 areaDescription="Shape_Leng",
                                 filterByArea="all",
                                 filterBySeason = "SPRING",
                                 tidy=T)   |>
  dplyr::mutate(SEASON = "SPRING",
                AREA = "MassRI")

# fall area2
fall2 <- survdat::calc_swept_area(surveyData=data$survdat,
                                  areaPolygon=area2,
                                  areaDescription="Shape_Leng",
                                  filterByArea="all",
                                  filterBySeason = "FALL",
                                  tidy=T)   |>
  dplyr::mutate(SEASON = "FALL",
                AREA = "MassRI")


# concatenate all and select total biomass variable + variance
d <- rbind(spr1,spr2,fall1,fall2) |>
  dplyr::filter(variable %in% c("tot.biomass","tot.bio.var"))

# pull species from survey and join by SVSPP
species <- survdat::get_species(channel)$data |>
  dplyr::select(SVSPP,COMNAME,SCINAME) |>
  dplyr::filter(!is.na(SVSPP)) |>
  dplyr::distinct()

finalData <- d |>
  dplyr::left_join(species,by = "SVSPP") |>
  dplyr::as_tibble() |>
  dplyr::filter(!(SVSPP == 122))

#save output aas an rds file
saveRDS(finalData,here::here("BRI/KimLato/dataRequest.rds"))

