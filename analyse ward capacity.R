library(tidyverse)
library(readxl)

wards <- read_excel("data/Priority and exemplar wards - Healthier Lancashire and South Cumbria.xlsx")

# Load Community Needs Index ranks from British Red Cross Vulnerability Index
cni <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/community-needs-ward.csv")

# Source: https://github.com/drkane/geo-lookups/
ward_all_codes <- read_csv("https://github.com/drkane/geo-lookups/raw/master/ward_all_codes.csv")

# Ward 2017 names and codes from https://geoportal.statistics.gov.uk/datasets/wards-december-2017-names-and-codes-in-the-united-kingdom
ward_names <- read_csv("https://opendata.arcgis.com/datasets/63773bdd52e34745be3db659663d5662_0.csv") %>% 
  select(WD17CD, WD17NM)

cni <- 
  cni %>% 
  left_join(ward_names, by = c("Code" = "WD17CD"))

wards %>% 
  left_join(cni, by = c("2019 ward code" = "Code"))
