# create set of conversion factors

cf <- survdat::get_conversion_factors(channel)$data |>
  dplyr::mutate(SVSPP = as.integer(SVSPP))

species <- survdat::get_species(channel)$data |>
  dplyr::select(SVSPP,SCINAME,COMNAME) |>
  dplyr::filter(!is.na(SVSPP)) |>
  dplyr::distinct()

cfspecies <- cf |>
  dplyr::left_join(species,by = "SVSPP")

saveRDS(cfspecies,here::here("EwE-menhaden-AndreBuchheister/2024request/cf.rds"))
