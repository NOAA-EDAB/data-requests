# get survey estimated for NYbight strata

# library(ROracle)
# driver <- ROracle::Oracle()
# server <- "(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=nefscdb1.nmfs.local)(PORT=1526))(CONNECT_DATA=(SERVICE_NAME = NEFSC_BATCH)))"
# uid <- "SGAICHAS"
# pwd <- getPass::getPass(msg=paste0("Enter the password for user ",uid," on server (",server,"):"),forcemask = FALSE)
# 
# con <- ROracle::dbConnect(driver, dbname=server, username=uid,password=pwd)

channel <- dbutils::connect_to_database(server=server,uid="SGAICHAS")

data_nolength <- survdat::get_survdat_data(channel = channel, 
                                  getLengths = F)

NYBstrata <- c(3010:3200, 1010:1080, 1730:1760)


NYBspring <- survdat::calc_swept_area(surveyData = data$survdat,
                                      filterByArea = NYBstrata,
                                      filterBySeason = "SPRING")

NYBspring_nolength <- survdat::calc_swept_area(surveyData = data_nolength$survdat,
                                      filterByArea = NYBstrata,
                                      filterBySeason = "SPRING")


NYBfall <- survdat::calc_swept_area(surveyData = data$survdat,
                                      filterByArea = NYBstrata,
                                      filterBySeason = "FALL")


NYBspring_mean <- survdat::calc_stratified_mean(surveyData = data$survdat,
                                                filterByArea = NYBstrata,
                                                filterBySeason = "SPRING")

NYBfall_mean <- survdat::calc_stratified_mean(surveyData = data$survdat,
                                    filterByArea = NYBstrata,
                                    filterBySeason = "FALL")

species <- survdat::get_species(channel = channel)

spplist <- species$data |>
  dplyr::filter(!is.na(SVSPP)) |>
  dplyr::select("SCINAME", "COMNAME", "SVSPP") |>
  dplyr::distinct()

saveRDS(spplist, here::here("VAST_indices/testsurvdat/spplist.rds"))

saveRDS(NYBspring, here::here("VAST_indices/testsurvdat/NYBspring.rds"))
saveRDS(NYBfall, here::here("VAST_indices/testsurvdat/NYBfall.rds"))
saveRDS(NYBspring_mean, here::here("VAST_indices/testsurvdat/NYBspring_mean.rds"))
saveRDS(NYBfall_mean, here::here("VAST_indices/testsurvdat/NYBfall_mean.rds"))
