# remotes::install_github("NEFSC/READ-EDAB-CVA2.0")
source("https://raw.githubusercontent.com/NEFSC/READ-EDAB-CVA2.0/refs/heads/main/R/make_sensitivity_barplots.R")
library(dplyr)
library(stringi)

dat <- read.csv(here::here("EDAB/CVA2.0", "export_stocks_csv__.csv")) %>% 
  group_by(Scorer) %>% 
  mutate(Scorer = stri_rand_strings(length = 5, n = 1, pattern = "[A-Z]"))
  



#  data = dat
# species = unique(dat$Stock.Name)
# plots_name = here::here("EDAB/CVA2.0/figures/prelim_sensitivity")
# unique(dat$Stock.Name)


make_sensitivity_barplots(data = dat, species = unique(dat$Stock.Name),
                          plots_name =  here::here("EDAB/CVA2.0/figures/prelim_sensitivity"),
                          sensitivity = TRUE, 
                          scorer = TRUE, 
                          plot_data_quality = TRUE,
                          scorer_dir = here::here("EDAB/CVA2.0/figures"))
