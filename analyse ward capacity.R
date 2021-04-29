library(tidyverse)
library(geographr)
library(readxl)

wards <- read_excel("data/Priority and exemplar wards - Healthier Lancashire and South Cumbria.xlsx")

# Load Community Needs Index ranks from British Red Cross Vulnerability Index
cni <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/community-needs-ward.csv")

# Source: https://github.com/drkane/geo-lookups/
ward_all_codes <- read_csv("https://github.com/drkane/geo-lookups/raw/master/ward_all_codes.csv")

# Ward 2017 names and codes from https://geoportal.statistics.gov.uk/datasets/wards-december-2017-names-and-codes-in-the-united-kingdom
ward_names <- read_csv("https://opendata.arcgis.com/datasets/63773bdd52e34745be3db659663d5662_0.csv") %>% 
  select(WD17CD, WD17NM)

# Rural-urban classifications in wards
ruc_wards <- geographr::ruc_england_wales_ward %>% 
  filter(str_detect(ward_code, "^E"))

# IMD in wards
imd_wards <- read_csv("https://github.com/matthewgthomas/IMD/raw/master/data/English%20IMD%20-%20Ward%202020.csv")

cni <- 
  cni %>% 
  left_join(ward_names, by = c("Code" = "WD17CD"))

wards %>% 
  left_join(cni, by = c("2019 ward code" = "Code"))

wards_imd_ruc <- wards %>% 
  left_join(imd_wards, by = c("2019 ward code" = "WD20CD")) %>% 
  left_join(ruc_wards, by = c("2019 ward code" = "ward_code"))

wards_imd_ruc %>% 
  select(`2019 CCG code`:Extent, starts_with("Health"), `Proportion of urban LSOAs`:Classification) %>% 
  write_csv("output/Priority and exemplar wards - Healthier Lancashire and South Cumbria.csv")

# Compare deprivation extent/proportion in priority vs exemplar wards
wardsimd_ruc %>% 
  ggplot(aes(x = Status, y = Extent)) +
  geom_boxplot()

wardsimd_ruc %>% 
  ggplot(aes(x = Status, y = Health_Extent)) +
  geom_boxplot()



