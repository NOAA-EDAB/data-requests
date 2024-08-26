#' Calculates stratified mean by species for species in SOE.24
#'
#' get files with the survey data for Georges Bank and the Gulf of Maine
#' used in the 2024 SOE report for New England.
#' I'm  hoping to get the species-level data used to form the functional groups in the SOE.
#' CSV files would work best f
#'
#'
#'Rework code from SML
#'
#'

#Required packages----
library(data.table); library(survdat); library(here)
#Pull Data----
# Connect to VPN then
#channel <- dbutils::connect_to_database(server="server",uid="user")

create_species_biomass <- function(channel,saveToFile = F) {



  survey <- survdat::get_survdat_data(channel, getLengths = F)
  survdat <- survey$survdat

  #Aggregate species----
  #Grab species list
  load(here::here('EDAB/Fogarty/SOE_species_list_24.RData'))

  #Merge to get species group and fed managed status
  survdat <- merge(survdat, unique(species[, list(SVSPP, SOE.24, Fed.Managed)]),
                   by = 'SVSPP', all.x = T)
  #Combine agg group and manage status
  survdat[, SOE.Managed := paste(SOE.24, Fed.Managed)]

  #Change seasons from all caps
  survdat[SEASON == 'FALL',   SEASON := 'Fall']
  survdat[SEASON == 'SPRING', SEASON := 'Spring']

  #Calculate stratified means
  #Strata sets
  EPU <- c('MAB', 'GB', 'GOM', 'SS')
  MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
  GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
  GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)
  SS  <- c(1300:1352, 3840:3990)



  # filteres species table
  sp <- species |>
    dplyr::filter(!is.na(SVSPP)) |>
    dplyr::select(SVSPP,COMNAME,SCINAME,SOE.24) |>
    dplyr::distinct()
  survey.data <- c()
  for(iepu in 1:length(EPU)){
    epu.strata <- get(EPU[iepu])
    #Calculate stratified means
    allsp <- survdat::calc_stratified_mean(survdat,
                                         filterByArea = epu.strata,
                                         filterBySeason = c('Fall', 'Spring'),
                                         groupDescription = 'SVSPP',
                                         tidy = T)

    # join with SOE.24 to assign svspp with group
    all <- allsp |>
      dplyr::left_join(sp,by="SVSPP") |>
      dplyr::filter(!is.na(SOE.24))

    #Combine into one dataset
    epu.all <- rbindlist(list(all), use.names = F)
    epu.all[, Region := EPU[iepu]]

    #Combine with other EPUs
    survey.data <- rbindlist(list(survey.data, epu.all))
  }

  final <- survey.data |>
    dplyr::mutate(Var = dplyr::case_when(variable == "strat.biomass" ~ "Biomass Index",
                                         variable == "biomass.SE" ~ "Biomass Standard Error",
                                         .default = NA)) |>
    dplyr::filter(!is.na(Var)) |>
    dplyr::select(-variable)


  if (saveToFile){
    # GB
    readr::write_csv(final |> dplyr::filter(Region == "GB"),here::here("EDAB/Fogarty/FogartySpeciesIndex_GB.csv"))
    # GOM
    readr::write_csv(final |> dplyr::filter(Region == "GOM"),here::here("EDAB/Fogarty/FogartySpeciesIndex_GOM.csv"))

  }

  return(final)
}
