#' survdat bio pull
#'
#' Ryan request for fish production from aquaculture gear project
#' (BSB, cunner, and tog)
#'

channel <- dbutils::connect_to_database("NEFSC_USERS","user")
ryan <- survdat::get_survdat_data(channel,getLengths = T, getBio = T,getWeightLength = T)
saveRDS(ryan,here::here("EDAB/survey/survdatBio.rds"))
