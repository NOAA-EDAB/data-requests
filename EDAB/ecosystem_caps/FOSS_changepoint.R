library(googlesheets4)
library(EnvCpt)
library(dplyr)
library(ggplot2)
library(tidyverse)


raw_dat <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1c_yx0YENpbZhUFCt5ZwCj4Se3_uoTRL5FOx7STH3-oU/edit?usp=sharing", sheet = "FOSS Total Landings",
                                range = "A4:BX19")

clean_dat <- raw_dat %>%
  mutate(across(-1, ~ {
    # Check if the column is a list
    if (is.list(.)) {
      # Replace empty cells (NULL) with NA so the column doesn't shrink
      .[lengths(.) == 0] <- NA
      # Flatten the list into a standard vector
      . <- unlist(.)
    }
    # Convert to numeric (suppressing warnings about text turning into NAs)
    suppressWarnings(as.numeric(.))
  }))


long_dat <- clean_dat %>% 
  tidyr::pivot_longer(cols = !`Million metric Tons per year`,
                      names_to = "year", values_to = "value") %>% 
  dplyr::rename("region" = `Million metric Tons per year`) %>%
  group_by(region) %>%
  # Filter to keep groups where it is NOT TRUE that all values are NA
  filter(!all(is.na(value))) %>%
  # Remove grouping structure for future operations
  ungroup() %>%
  # mutate(year = as.numeric(year)) %>% 
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
  # Force all items in the list to be character type
  mutate(years = purrr::map(years, as.character)) %>% 
  # Unnest, keeping regions that had 0 years
  tidyr::unnest(years, keep_empty = TRUE) %>%  
  left_join(long_dat, by = join_by(region, years == year), relationship = "many-to-many") %>% 
  mutate(years = as.numeric(years))

write.csv(tidy_results, file = here::here("EDAB/ecosystem_caps/FOSS_changepoint.csv"), row.names = FALSE)


# Convert year to numeric so it matches tidy_results
long_dat_numeric <- long_dat %>%
  mutate(year = as.numeric(year))

region_models <- tidy_results %>%
  select(region, best_model) %>%
  distinct()

valid_cpts <- tidy_results %>%
  filter(!is.na(years)) %>%
  select(region, year = years) %>%
  mutate(is_changepoint = 1) # Flag column for the join

plot_dat <- long_dat_numeric %>%
  left_join(valid_cpts, by = c("region", "year")) %>%
  arrange(region, year) %>%
  group_by(region) %>%
  mutate(
    regime = cumsum(replace_na(is_changepoint, 0))
  ) %>%
  ungroup()

plot_dat_fitted <- plot_dat %>%
  left_join(region_models, by = "region") %>%
  # Group by each specific regime block within each region
  group_by(region, regime) %>%
  mutate(
    fitted_value = case_when(
      grepl("mean", best_model) ~ mean(value, na.rm = TRUE),
      grepl("trend", best_model) ~ predict(lm(value ~ year)),
      TRUE ~ NA_real_ 
    )
  ) %>%
  ungroup()

ggplot(plot_dat_fitted, aes(x = year)) +
  geom_line(aes(y = value), color = "grey60", linewidth = 0.6) +
  geom_line(
    aes(y = fitted_value, group = regime), 
    color = "blue", 
    linewidth = 1
  ) +
  geom_vline(
    data = valid_cpts, 
    aes(xintercept = year), 
    color = "red", 
    linetype = "dashed", 
    alpha = 0.7
  ) +
  
  facet_wrap(~ region, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Million Metric Tons",
  ) +
  theme(
    panel.grid.minor = element_blank()
  )
