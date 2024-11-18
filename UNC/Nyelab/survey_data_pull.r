#' Janet Nye request for survey data.
#' Last received prior to survdat development
#' Old Code: (https://github.com/NOAA-EDAB/survdat/blob/eb23a5f45b1086331494050532ee51cb32cf5b54/other/Survdat_Nye.r)
#'
#' Reproduced using survdat package
#'
#'

channel <- dbutils::connect_to_database("server","id")

nye <- survdat::get_survdat_data(channel,
                          shg.check = T,
                          use.SAD = F,
                          all.season = T,
                          conversion.factor = T,
                          getLengths = T)

saveRDS(nye, here::here("UNC/Nyelab/surveyData.rds"))
