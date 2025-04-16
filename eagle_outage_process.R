library(dplyr)
library(tmap)
library(tigris)
library(ggplot2)
library(geojsonsf)
library(sf)


`%notin%` <- Negate(`%in%`)

options(tigris_use_cache = TRUE)


setwd("~/Downloads")
### Load and Process EAGLE-I Data

eaglei.2021 <- readr::read_csv("eaglei_outages_2021.csv") %>% rename(datetime = run_start_time)

eaglei.2021.agg <- eaglei.2021 %>%
  filter(customers_out >= 1) %>%
  group_by(fips_code) %>%
  summarise(number_events = n())


rm(eaglei.2021)
gc()

#load fips_geo from census
fips_geo <- st_read('/Users/hume766/Downloads/tl_2021_us_county/tl_2021_us_county.shp')

fips_geo <- fips_geo %>%
  select(GEOID, NAME, geometry) %>%
  rename(FIPS = GEOID)  # Rename to match outage dataset

test_merge <- left_join(fips_geo,eaglei.2021.agg, by = c("FIPS" = "fips_code"))


ggplot(test_merge) +
  geom_sf(aes(fill = number_events), color = "black") +  # Replace 'outage_count' with your column
  scale_fill_viridis_c() + 
  theme_minimal() +
  ggtitle("Utility Outages by FIPS")

 
st_write(test_merge, "grid_outages_with_fips.geojson", driver = "GeoJSON")


### Load and Process Modeled County Customer Data

mcc <-readr::read_csv("MCC.csv") %>%
  dplyr::rename(GEOID = County_FIPS) %>%
  dplyr::group_by(GEOID) %>% 
  dplyr::summarise(mcc = sum(Customers, na.rm = TRUE)) %>%
  mutate(GEOID = stringr::str_pad(as.character(GEOID), width = 5, pad = "0", side = "left")) 

mcc <- mcc %>% filter(GEOID != "Grand Total") %>% mutate(has.mcc = 1)

### Load Geographic information data

usa.counties = tigris::counties(year = 2022, resolution = "500k", cb = TRUE) %>% 
  dplyr::select(one_of("GEOID", "NAME")) %>% 
  dplyr::filter(substr(GEOID,1,2) %notin% c("60", "66", "69", "78")) %>%
  tigris::shift_geometry(position = "below")

### Merge Data

events_with_geo <- dplyr::left_join(eaglei.2021.agg, usa.counties, by = c('fips_code' = 'GEOID'))

events_with_geo$geometry <- sf::st_transform(events_with_geo$geometry,crs = 4326)

write.csv(events_with_geo, "outage_events_test.csv", row.names = F)

# counties <- dplyr::full_join(counties,uri.slice, by = c("GEOID" = "fips_code")) %>% 
#   dplyr::mutate(max_out = dplyr::if_else(is.na(max_out), 0, max_out),
#                 cust_hours_out = dplyr::if_else(is.na(cust_hours_out), 0, cust_hours_out),
#                 max_pct_out = dplyr::if_else(max_out/mcc> 1, 1, max_out/mcc),
#                 max_pct_out = dplyr::if_else(is.na(max_pct_out), 0, max_pct_out)) 





