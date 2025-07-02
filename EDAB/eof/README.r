#' Explanation of what was done ad how to reproduce
#'
#' ## Survey data
#' 1. pull_survey_eof.r to get all survey related data
#' 2. Run data_notes_survey.rmd
#'
#' ## commercial data: Meat weight
#' 1. pull_commercial_meat_eof.r to get all commercial meat weight (Only shellfish are meat weight)
#' 2. split_commercial_discards.r to split commercial discards by elasmobranchs, other fish, lobster, shellfish
#' 3. total_commercial_by_EPU_data.r to get total commercial meat weight by EPU (landings and discards)
#' 4. Run data_notes_meat.rmd
#'
#' #' ## commercial data: Live weight
#' 1. pull_commercial_live_eof.r to get all commercial meat weight (Only shellfish are meat weight)
#' 2. split_commercial_discards.r to split commercial  discards by elasmobranchs, other fish, lobster, shellfish
#' 3. total_commercial_by_EPU_data.r to get total commercial meat weight by EPU (landings and discards)
#' 4. Run data_notes_live.rmd
#'
#' Files of interest
#' 1. true_landings_meat_EOF.rds - meat weight landings (shellfish meat weight only)
#' 2. true_discards_meat_EOF.rds - meat weight discards (based on shellfish meat weight)
#' 3. true_landings_live_EOF.rds - live weight landings
#' 4. true_discards_live_EOF.rds - live weight discards
#' 5. otherfish_discards_EOF.rds - other fish discards (live weight)
#' 6. elasmobranch_discards_EOF.rds - elasmobranch discards (live weight)
#' 7. lobster_discards_EOF.rds - lobster discards (live weight)
#' 8. shellfish_discards_live_EOF.rds - shellfish discards (live weight)
#' 8. shellfish_discards_meat_EOF.rds - shellfish discards (meat weight)
#' 9. total_landings_discards_by_EPU.rds - total landings and discards by EPU (meat, live, discards by type)
#' swept_area_biomass_species_EOF.rds - all species linked to RPATH groups
#'
