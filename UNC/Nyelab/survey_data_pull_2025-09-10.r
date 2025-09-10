#' Laura Gruenburg request for survey data.
#'

channel <- dbutils::connect_to_database("server", "id")

nye <- survdat::get_survdat_data(
  channel,
  shg.check = T,
  use.SAD = F,
  all.season = T,
  conversion.factor = T,
  getLengths = T
)

saveRDS(nye, here::here("UNC/Nyelab/surveyData_2025-09-10.rds"))
