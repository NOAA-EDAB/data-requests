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

channel <- dbutils::connect_to_database("NEFSC_USERS","abeet")

## pull survey data
data <- survdat::get_survdat_data(channel)

# specify spatial footprint
area1 <- sf::st_read(dsn = here::here("BRI/KimLato/gis/"),quiet=T)
area2 <- sf::st_read(dsn = here::here("BRI/KimLato/gis/"),quiet=T)

# calculate swept area biomass
calc_swept_area(surveyData=data$survdat, areaPolygon=area, areaDescription="EPU", filterByArea="all",filterBySeason = "all",tidy=T)


##
