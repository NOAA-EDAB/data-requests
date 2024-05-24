#' ecodata commercial revenue (Megan Ware)
#'
#'My name is Megan and I am a New England Council member from Maine chairing
#'our Risk Policy Workgroup. The New England Council is currently revising our
#'risk policy and one of the factors we are considering in our revised risk
#' policy is “economic and community importance”. A potential metric of this
#' is to look at trends in a species commercial revenue compared to total
#'  revenue for the New England region. I saw in the State of the Ecosystem
#'  report and on the Northeast US Ecosystem Indicator Catalog that total
#'  revenue (GOM, GB) for a region is being calculated. Would it be possible
#'  to get the numbers that underly these figures (Figure 9 in the New England
#'  SOE report or Tab 57 in the Ecosystem Indicator Catalog)? I saw your name
#'  listed as a data contact on the Ecosystem Indicator Catalog but if there
#'  is a different person I should contact, just let me know. I’ve cc’d
#'  Jonathon Peros who works for the New England Council and is helping
#'  on the revised risk policy.
#'
#'
#'

# pulled code from ecodata::plot_comdat
# For MAB
rev_managed <- ecodata::comdat |>
  dplyr::filter(Var %in% c("Piscivore MAFMC managed species - Revenue",
                           "Planktivore MAFMC managed species - Revenue",
                           "Benthivore MAFMC managed species - Revenue",
                           "Benthos MAFMC managed species - Revenue",
                           "Piscivore NEFMC managed species - Revenue",
                           "Planktivore NEFMC managed species - Revenue",
                           "Benthivore NEFMC managed species - Revenue",
                           "Benthos NEFMC managed species - Revenue",
                           "Piscivore JOINT managed species - Revenue",
                           "Planktivore JOINT managed species - Revenue",
                           "Benthivore JOINT managed species - Revenue",
                           "Benthos JOINT managed species - Revenue"),
                Time >= 1982) |>
  #rbind(apex) |>
  dplyr::filter(stringr::str_detect(Var, paste0("JOINT|", "MAFMC"))) |>
  dplyr::mutate(Status = c("Council Managed"))  #Create groups for

rev_guild <- rev_managed |>
  tidyr::separate(Var, into = c("feeding.guild"), sep = " ") |>
  dplyr::group_by(feeding.guild, EPU, Time) |>
  dplyr::summarise(Value = sum(Value)/1000) |>
  dplyr::mutate(grouping = c("Council Managed"),
                Units = c("10^3 US dollars")) |>
  dplyr::ungroup()

rev_agg <- rev_managed |>
  dplyr::group_by(Status, Time, EPU) |>
  dplyr::summarise(Total = sum(Value)/1000000) |>
  dplyr::group_by(Status, EPU)

rev_tot <- ecodata::comdat  |>
  dplyr::filter(Var %in% c("Planktivore Revenue",
                           "Piscivore Revenue",
                           "Benthivore Revenue",
                           "Apex Predator Revenue",
                           "Benthos Revenue",
                           "Other Revenue"),
                Time >= 1982) |>
  dplyr::mutate(grouping = c("Total")) |>
  tidyr::separate(Var, into = c("feeding.guild"), sep = " ") |>
  dplyr::mutate(Value = Value/1000)

rev_total<- ecodata::comdat |>
  dplyr::filter(Var == "Revenue",
                Time >= 1982) |>
  dplyr::mutate(Status = c("Total")) |>
  dplyr::group_by(Status, Time, EPU) |>
  dplyr::summarise(Total = sum(Value)/1000000) |>
  dplyr::group_by(Status, EPU)

guilddat <- rbind(rev_guild, rev_tot) |>
  dplyr::filter(!stringr::str_detect(feeding.guild, "Apex|Other")) |>
  #tidyr::separate(Var, into = c("feeding.guild", "a", "grouping"), sep = " ") |>
  dplyr::mutate(#feeding.guild = stringr::str_extract(Var,c(feeding.guilds)),
    #grouping = recode(grouping, "Landings" = "total"),
    Var = paste(feeding.guild, grouping),
    Value = Value * 1000,
    Units = "USD") |>
  dplyr::mutate(feeding.guild = factor(feeding.guild, levels = c("Apex Predator","Piscivore","Planktivore","Benthivore","Benthos"))) |>
  dplyr::group_by(Var, EPU) |>
  dplyr::filter(Time >1982) |>
  dplyr::filter(EPU %in% c("MAB")) |>
  dplyr::ungroup()

totdat <- rbind(rev_agg, rev_total) |>
  dplyr::mutate(Value = Total * 10^6,
                Var = Status,
                Units = "USD") |>
  dplyr::filter(Time >1982) |>
  dplyr::filter(EPU %in% c("MAB")) |>
  dplyr::ungroup() |>
  dplyr::select(-Status)


readr::write_csv(totdat,here::here("NECouncil/MeganWare/totalRevenueMAB.csv"))
readr::write_csv(guilddat,here::here("NECouncil/MeganWare/guildRevenueMAB.csv"))
