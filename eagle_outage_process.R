library(dplyr)
library(tmap)
library(tigris)
library(ggplot2)
library(geojsonsf)
library(sf)
library(purrr)
library(stringr)
library(readr)

#---- data sources ----
#https://figshare.com/s/417a4f147cf1357a5391?file=44574907


`%notin%` <- Negate(`%in%`)

options(tigris_use_cache = TRUE)

#----Read EAGLE-I Data ----
setwd("~/Downloads")

# 1) Find all your CSVs
files <- list.files(pattern = "^eaglei_outages_\\d{4}\\.csv$")

# 2) Name your summary‐output file (overwrites if it already exists)
out_file <- "eaglei_outages_summary.csv"
if (file.exists(out_file)) file.remove(out_file)

# 3) Loop: read → summarise → append → clean up
for (f in files) {
  year_val <- str_extract(f, "\\d{4}") %>% as.integer()
  
  # a) read the big CSV
  print(paste0("reading file", f))
  tmp <- read_csv(f)
  
  # b) if this file calls it "sum" instead of "customers_out", rename it
  if ("sum" %in% names(tmp) && !("customers_out" %in% names(tmp))) {
    tmp <- tmp %>% rename(customers_out = sum)
  }
  
  # c) do your filter/group/count and tag the year
  summary_tbl <- tmp %>%
    rename(datetime = run_start_time) %>%
    filter(customers_out >= 1) %>%
    count(fips_code, name = "number_events") %>%
    mutate(year = year_val)
  
  # d) write or append to the single output CSV
  write_csv(
    summary_tbl,
    out_file,
    append    = file.exists(out_file),
    col_names = !file.exists(out_file)
  )
  
  # e) free memory before the next iteration
  rm(tmp)
  gc()
}

summary_tbl <- read_csv("eaglei_outages_summary.csv")

summary_tbl_all_years <- summary_tbl %>%
  group_by(fips_code) %>%
  summarise(number_events = sum(number_events)) %>%
  # mutate(
  #   log_ne = log1p(number_events),
  #   z_log  = (log_ne - mean(log_ne)) / sd(log_ne),
  #   is_outlier_log = abs(z_log) > 2
  # ) %>%
  mutate(
    pct_rank = percent_rank(number_events),
    is_extreme =  pct_rank > 0.9
  )
  #mutate(z_score = (number_events - mean(number_events)) / sd(number_events))

#----Normal Tests on Outage Data ----
hist(summary_tbl_all_years$log_ne,
     breaks = 30,
     probability = TRUE,
     main = "Histogram of Number of Events",
     xlab = "number_events")
curve(dnorm(x, mean(summary_tbl_all_years$log_ne), sd(summary_tbl_all_years$log_ne)),
      add = TRUE, col = "red", lwd = 2)

qqnorm(summary_tbl_all_years$log_ne, main = "Q–Q Plot of Number of Events")
qqline(summary_tbl_all_years$log_ne, col = "blue", lwd = 2)

shapiro.test(summary_tbl_all_years$log_ne)


#----

#---- Add GEO Data ----

#load fips_geo from census
fips_geo <- st_read('/Users/hume766/Downloads/tl_2021_us_county/tl_2021_us_county.shp')

fips_geo <- fips_geo %>%
  select(GEOID, NAME, geometry) %>%
  rename(FIPS = GEOID)  # Rename to match outage dataset

outage_fips <- left_join(fips_geo,summary_tbl_all_years, by = c("FIPS" = "fips_code"))

outage_fips_10per <- outage_fips %>%
  filter(!is.na(number_events)) %>%
  filter(is_extreme)




ggplot(outage_fips_10per) +
  geom_sf(aes(fill = number_events), color = "black") +  # Replace 'outage_count' with your column
  scale_fill_viridis_c() + 
  theme_minimal() +
  ggtitle("Utility Outages by FIPS")

 
st_write(outage_fips_10per, "grid_outages_10per_with_fips.geojson", driver = "GeoJSON")


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





