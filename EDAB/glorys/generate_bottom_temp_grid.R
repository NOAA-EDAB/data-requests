library(sf)
library(dplyr)
# remotes::install_github("NEFSC/NEFSC-Spatial")
library(NEFSCspatial)
library(ggplot2)
library(lubridate)
library(ggridges)
library(glue)

### Functions -----
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


### ### ### Spatial stuff -----
wg_crs <- sf::st_crs(4269)

# Extract NEUS shapefile
region <- NEFSCspatial::epu_sf %>%
  sf::st_transform(wg_crs) %>%
  sf::st_make_valid()

# make extrapolation-grid
sf_grid = sf::st_make_grid( region, cellsize=c(0.2,0.2) )
sf_grid = sf::st_intersection( sf_grid, region )
sf_grid = sf::st_make_valid( sf_grid )
n_g = length(sf_grid)
# saveRDS(sf_grid, file = here::here("data-raw/neus_grid.rds"))

grid_coords = sf::st_coordinates( sf::st_centroid(sf_grid) )
areas_km2 = sf::st_area( sf_grid ) / 1e6


### Survey stuff -----
survdat <- read.csv(here::here("EDAB/glorys/survdat_datapull.csv"))  %>% 
  mutate(date = as.Date(EST_TOWDATE),
         jdate = as.numeric(format(date, "%j"))) %>% 
  filter(SEASON == "FALL",
         YEAR >= 1990) %>% 
  select(station = STATION,
         tow = TOW,
         lat = LAT,
         lon = LON,
         year = YEAR,
         date, 
         jdate) %>% 
  dplyr::distinct(.keep_all = TRUE) %>% 
  sf::st_as_sf(coords = c("lon","lat"), crs = wg_crs) %>%
  sf::st_join(region) %>%
  sfc_as_cols(names = c("lon", "lat")) %>%
  sf::st_drop_geometry() %>% 
  mutate(EPU = ifelse(is.na(EPU), "none", EPU )) %>% 
  filter(!EPU %in% c("none", "SS")) 


survdat$EPU <- factor(survdat$EPU, levels = c( "MAB", "GB", "GOM"))
survdat_years <- min(survdat$year):max(survdat$year)

# 
survdat_median <- survdat %>%
  mutate(EPU = factor(EPU, levels = c("MAB", "GB", "GOM"))) %>% 
  group_by(EPU) %>%
  summarize(median = median(jdate, na.rm = TRUE))

subtitle_string <- survdat_median %>%
  mutate(date_formatted = format(as.Date("2023-01-01") + median - 1, "%B %d")) %>%
  mutate(full_label = glue("{EPU}: {date_formatted} ({median})")) %>%
  pull(full_label) %>%
  paste(collapse = " | ")

## Grid for bottom temperature
bt_grid <- data.frame(grid_coords) %>%
  rename(lon = Y, lat = X) %>% 
  tidyr::crossing(year = survdat_years, 
                  EPU = c("MAB", "GB", "GOM")) %>% 
  left_join(survdat_median, by = join_by(EPU)) %>% 
  mutate(date = as.Date(paste0(year, "-01-01")) + median - 1) %>% 
  select(lat,
         lon,
         EPU,
         year,
         date) %>% 
  arrange(year, EPU)

write.csv(x = bt_grid, file = here::here("EDAB/glorys/bottom_temp_grid.csv"), row.names = FALSE)

## Plots -----
p_survdate <- ggplot(survdat, aes(x = jdate, y = as.factor(year), fill = EPU)) +
  stat_density_ridges(
    geom = "density_ridges", 
    bandwidth = 7,
    quantile_lines = TRUE,
    quantiles = 2, # median
    scale = .9,
    rel_min_height = 0.01,
    alpha = 0.75,
    linewidth = 0.25,
    color = "grey30"
  ) +
  geom_vline(data = survdat_median,
             aes(xintercept = median,
                 color = EPU),
             alpha = 1,
             linewidth = 1
  ) +
  theme_ridges(grid = FALSE, font_size = 8, line_size = .25) +
  theme(legend.position = "bottom") +
  scale_y_discrete(limits = rev(as.character(survdat_years))) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Distribution of Annual Fall Survey Dates",
    subtitle = subtitle_string,
    x = "day of year",
    y = "year"
  ) +
  NULL

ggsave(
  filename = here::here("EDAB/glorys/survey_dates.tiff"),
  plot = p_survdate,
  device = grDevices::tiff,
  width = 120,
  # height = 220,
  bg = "white",
  units = "mm",
  dpi = 300,
  compression = "lzw"
)
