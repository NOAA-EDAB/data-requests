#' Length and weight for all species (Bottom Trawl Survey)
#'
#' Note individual weights of species caught in the bottom trawl survey began
#' being recorded in 1991/1992
#'
#'

channel <- dbutils::connect_to_database("server","user")

dat <- NULL
for (iyr in 1963:2023){
  message(iyr)
  # query to pull data
  # should use survddat::get_length_weight_data but data type inconsistencies in data base for SEX
  # 0, 1, 2, "I", "M", "F, "m", "f"
  sql <- paste0("select m.cruise6, m.stratum, m.tow, m.station, m.svspp, m.sex, m.indid, m.length, m.indwt, m.maturity, m.stom_wgt, m.stom_volume, s.season
                from svdbs.union_fscs_svbio m LEFT JOIN svdbs.svdbs_cruises s ON m.cruise6 = s.cruise6 where ( (m.cruise6 like ('",iyr,"%'))) ")

  # remove some unneeded columns, filter out individuals without weights
  a <- DBI::dbGetQuery(channel,sql) |>
    dplyr::select(-c(STOM_WGT,STOM_VOLUME,MATURITY,INDID))|>
    dplyr::filter(SEX %in% c(0,1,2) & !is.na(INDWT)) |>
    dplyr::mutate(YEAR = substr(CRUISE6,1,4)) |>
    dplyr::as_tibble()
  dat <- rbind(dat,a)
}

# find species list
species <- survdat::get_species(channel)$data |>
  dplyr::select(SCINAME,COMNAME,SVSPP)

# join data
newDat <- dat |>
  dplyr::mutate(SVSPP = as.numeric(SVSPP),
                SEX = as.numeric(SEX),
                YEAR = as.numeric(YEAR)) |>
  dplyr::left_join(species, by = "SVSPP")

saveRDS(newDat, here::here("GMRI/DiFiore_lenwgt/bottomTrawlSurvey_indLengthWeight.rds"))

# pull current length weight coefficients

lwcoeffs <- survdat::get_length_weight(channel)$data |>
  dplyr::left_join(species, by = "SVSPP")

saveRDS(lwcoeffs, here::here("GMRI/DiFiore_lenwgt/lw_coeffs.rds"))
