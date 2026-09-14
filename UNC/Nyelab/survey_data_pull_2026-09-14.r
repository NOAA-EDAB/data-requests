#' Laura Gruenburg request for survey data.
#' 2026-09-10. We are once again writing our New York specific ocean indicators report
#' and I was hoping you might be able to help us get some bottom trawl data.  Any chance that the NEFSC bottom trawl data from fall of 2025 and spring of 2026 are available?

channel <- dbutils::connect_to_database("server", "id")

nye <- survdat::get_survdat_data(
  channel,
  shg.check = T,
  use.SAD = F,
  all.season = T,
  conversion.factor = T,
  getLengths = T
)

saveRDS(nye, here::here("UNC/Nyelab/surveyData_2026-09-14.rds"))
