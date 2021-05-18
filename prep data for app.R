library(tidyverse)
library(geographr)
library(sf)

# Save datasets for Shiny app
imd_wards %>% 
  rename(ward_code = WD20CD) %>% 
  filter(str_detect(ward_code, "^E")) %>% 
  write_rds("app/data/imd_wards.rds")

imd_lsoa %>% 
  rename(lsoa_code = LSOA) %>% 
  filter(str_detect(lsoa_code, "^E")) %>% 
  write_rds("app/data/imd_lsoa.rds")

geographr::boundaries_wards %>% 
  filter(str_detect(ward_code, "^E")) %>% 
  write_rds("app/data/wards.rds")

geographr::boundaries_lsoa %>% 
  filter(str_detect(lsoa_code, "^E")) %>% 
  left_join(
    geographr::lookup_lsoa_ward %>% select(lsoa_code, ward_code), 
    by = "lsoa_code"
  ) %>% 
  relocate(geometry, .after = ward_code) %>% 
  write_rds("app/data/lsoa.rds")
