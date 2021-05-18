library(tidyverse)
library(geographr)
library(readxl)

#' invert deciles
#' @param x Deciles
#' 
reverse_decile = function(x) 11 - x

wards <- read_excel("data/Priority and exemplar wards - Healthier Lancashire and South Cumbria.xlsx")

wards <- 
  wards %>% 
  fill(`2019 CCG code`, `2019 CCG name`)

# Source: https://github.com/drkane/geo-lookups/
# ward_all_codes <- read_csv("https://github.com/drkane/geo-lookups/raw/master/ward_all_codes.csv")

# Ward 2017 names and codes from https://geoportal.statistics.gov.uk/datasets/wards-december-2017-names-and-codes-in-the-united-kingdom
# ward_names <- read_csv("https://opendata.arcgis.com/datasets/63773bdd52e34745be3db659663d5662_0.csv") %>% 
#   select(WD17CD, WD17NM)

# Rural-urban classifications in wards
ruc_wards <- geographr::ruc_england_wales_ward %>% 
  filter(str_detect(ward_code, "^E"))

# IMD
imd_wards <- read_csv("https://github.com/matthewgthomas/IMD/raw/master/data/English%20IMD%20-%20Ward%202020.csv")
imd_lsoa <- read_csv("https://github.com/matthewgthomas/IMD/raw/master/data/UK%20IMD%20domains.csv")

# ---- Community Needs Index ----
# Commenting this section out because CNI is currently for 2017 ward codes and doesn't align to 2019/2020 codes

# Load Community Needs Index ranks from British Red Cross Vulnerability Index
# cni <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/community-needs-ward.csv")
# 
# cni <- 
#   cni %>% 
#   left_join(ward_names, by = c("Code" = "WD17CD"))
# 
# wards %>% 
#   left_join(cni, by = c("2019 ward code" = "Code"))

# ---- Digital exclusion ----
digital_ward <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/CACI/digital-exclusion-ward.csv")

digital_ward <- 
  digital_ward %>% 
  select(WD20CD, `Digital Vulnerability decile`) %>% 
  
  # Reverse deciles so 1 is most vulnerable, in line with IMD
  mutate(`Digital Vulnerability decile` = reverse_decile(`Digital Vulnerability decile`))

# ---- Financial insecurity ----
financial_ward <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/CACI/financial-ward.csv")

financial_ward <- 
  financial_ward %>% 
  select(WD20CD, `Financial Vulnerability decile`) %>% 
  
  # Reverse deciles so 1 is most vulnerable, in line with IMD
  mutate(`Financial Vulnerability decile` = reverse_decile(`Financial Vulnerability decile`))

# ---- Living alone ----
alone_ward <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/CACI/living-alone-ward.csv")

# ---- Capacity from Resilience Index ----
capacity_la <- read_csv("https://github.com/britishredcrosssociety/resilience-index/raw/main/depreciated/data/processed/capacity%20index.csv")

# Ward to Local Authority District to County to Region to Country (December 2019) Lookup in United Kingdom
# from https://geoportal.statistics.gov.uk/datasets/ward-to-local-authority-district-to-county-to-region-to-country-december-2019-lookup-in-united-kingdom
ward_lad <- read_csv("https://opendata.arcgis.com/datasets/cdcc46d656e84e3d997e4ab2cd77881a_0.csv") %>% 
  select(WD19CD, LAD19CD)

capacity_ward <- 
  ward_lad %>% 
  filter(str_detect(LAD19CD, "^E")) %>% 
  left_join(capacity_la %>% select(LAD19CD, `Capacity decile`), 
            by = "LAD19CD") %>%
  
  select(WD19CD, `Capacity decile`) %>% 
  # Reverse deciles so 1 is most vulnerable, in line with IMD
  mutate(`Capacity decile` = reverse_decile(`Capacity decile`))

# ---- Check by jowl analysis of deprivation in wards ----
cheek_by_jowl <- 
  imd_lsoa %>% 
  filter(str_detect(LSOA, "^E")) %>% 
  select(LSOA, IMD_decile) %>% 
  
  left_join(geographr::lookup_lsoa_ward, by = c("LSOA" = "lsoa_code")) %>% 
  
  mutate(highest_deprivation = ifelse(IMD_decile <= 2, 1, 0),
         lowest_deprivation  = ifelse(IMD_decile >= 9, 1, 0)) %>%
  
  group_by(ward_code) %>% 
  summarise(highest_deprivation = sum(highest_deprivation),
            lowest_deprivation = sum(lowest_deprivation)) %>% 
  ungroup() %>% 
  
  mutate(`Cheek by jowl?` = ifelse(highest_deprivation > 0 & lowest_deprivation > 0, "Yes", "No")) %>% 
  select(ward_code, `Cheek by jowl?`)


# ---- Join everything ----
wards_analysis <- wards %>% 
  left_join(imd_wards, by = c("2019 ward code" = "WD20CD")) %>% 
  left_join(cheek_by_jowl, by = c("2019 ward code" = "ward_code")) %>%
  left_join(digital_ward, by = c("2019 ward code" = "WD20CD")) %>% 
  left_join(financial_ward, by = c("2019 ward code" = "WD20CD")) %>% 
  left_join(alone_ward, by = c("2019 ward code" = "WD20CD")) %>% 
  left_join(capacity_ward, by = c("2019 ward code" = "WD19CD")) %>% 
  left_join(ruc_wards, by = c("2019 ward code" = "ward_code")) %>% 
  
  select(-ends_with("Score"), -ends_with("Proportion"))  %>% 
  rename(
    `IMD: % of population living in areas rated as among the nation's most deprived` = Extent, 
    `Income: % of population living in areas rated as among the nation's most deprived` = Income_Extent,
    `Employment: % of population living in areas rated as among the nation's most deprived` = Employment_Extent, 
    `Education: % of population living in areas rated as among the nation's most deprived` = Education_Extent,
    `Health Deprivation & Disability: % of population living in areas rated as among the nation's most deprived` = Health_Extent, 
    `Crime: % of population living in areas rated as among the nation's most deprived` = Crime_Extent, 
    `Barriers to Housing & Services: % of population living in areas rated as among the nation's most deprived` = Housing_and_Access_Extent, 
    `Living Environment: % of population living in areas rated as among the nation's most deprived` = Environment_Extent
    )

# Save
wards_analysis %>% 
  write_csv("output/Priority and exemplar wards - BRC analysis.csv")

# ---- Compare deprivation extent/proportion in priority vs exemplar wards ----
wards_analysis %>% 
  ggplot(aes(x = Status, y = `IMD: % of population living in areas rated as among the nation's most deprived`)) +
  geom_boxplot()

wards_analysis %>% 
  ggplot(aes(x = Status, y = `Health Deprivation & Disability: % of population living in areas rated as among the nation's most deprived`)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Health Deprivation & Disability") +
  theme_minimal()

ggsave("output/health deprivation.png", width = 75, height = 75, units = "mm")

wards_analysis %>% 
  ggplot(aes(x = Status, y = `Barriers to Housing & Services: % of population living in areas rated as among the nation's most deprived`)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Barriers to Housing and Services") +
  theme_minimal()

ggsave("output/housing deprivation.png", width = 75, height = 75, units = "mm")

wards_analysis %>% 
  ggplot(aes(x = Status, y = `Cheek by jowl?`)) +
  geom_boxplot() +
  # scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Barriers to Housing and Services") +
  theme_minimal()

wards_analysis %>% 
  janitor::tabyl(`Cheek by jowl?`, Status)

geographr::lookup_lsoa_ward %>% 
  count(ward_code) %>% 
  
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1)


