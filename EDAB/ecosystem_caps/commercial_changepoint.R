library(googlesheets4)
library(EnvCpt)
library(dplyr)
library(ggplot2)

raw_dat <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1e95ux60IV1rOW88SEFmjr_GnZVeHwJ87etMpYBaPKXY/edit?usp=drive_link", sheet = "Commercial Landings", 
                          range = "A3:BW16")

long_dat <- raw_dat %>% 
  tidyr::pivot_longer(cols = !`Million Metric Tons`,
                      names_to = "year", values_to = "value") %>% 
  dplyr::rename("region" = `Million Metric Tons`) %>%
  filter(!region %in% c("Chukchi Sea", "Beaufort Sea", "Pacific Tropics 1", "Pacific Tropics 2", "Hawaii")) %>%
  na.omit(value)


td <- long_dat %>% 
  group_by(region) %>%
  tidyr::nest() %>% 
  mutate(
    # 1. Apply envcpt for each region
    cpt_model = purrr::map(data, ~ envcpt(.x %>% arrange(year) %>% pull(value))),
    # 2. Extract changepoint years from the best model using AIC
    changepoint_years = purrr::map2(data, cpt_model, function(df, model) {

      aic_scores <- AIC(model)
      best_model_name <- names(aic_scores)[which.min(aic_scores)]
      
      # If a changepoint model is best, get all changepoint indices
      if(grepl("cpt", best_model_name)) {
        cpt_indices <- cpts(model[[best_model_name]])
        changepoint_years <- (df %>% arrange(year) %>% pull(year))[cpt_indices]
      } else {
        # Return an empty numeric vector if no changepoints are found
        changepoint_years <- numeric(0)
      }
      tibble(
        best_model = best_model_name,
        years = list(changepoint_years)
      )
    })
  )

tidy_results <- td %>% 
  ungroup() %>%
  dplyr::select(region, changepoint_years) %>%
  tidyr::unnest(changepoint_years) %>% 
  tidyr::unnest(years) %>%  
  left_join(long_dat, by = join_by(region, years == year), relationship = "many-to-many")

write.csv(tidy_results, file = here::here("EDAB/ecosystem_caps/commercial_changepoint.csv"), row.names = FALSE)
