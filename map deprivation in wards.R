library(tidyverse)
library(geographr)
library(sf)
library(viridis)
library(Hmisc)
library(readxl)

# ---- Load data ----
wards <- read_excel("data/Priority and exemplar wards - Healthier Lancashire and South Cumbria.xlsx") %>% 
  pull(`2019 ward code`)

# IMD
imd_wards <- read_csv("https://raw.githubusercontent.com/matthewgthomas/IMD/master/data-raw/imd_england_ward.csv")
imd_lsoa <- read_csv("https://raw.githubusercontent.com/matthewgthomas/IMD/master/data-raw/imd_england_lsoa.csv")

# Calculate IMD deciles for wards
imd_wards_subset <- 
  imd_wards %>% 
  select(ward_code, Score, Extent) %>% 
  mutate(Score_rank = rank(Score)) %>% 
  mutate(Score_decile = 11 - as.integer(cut2(Score_rank, g = 10)))

# hist(imd_wards_subset$Score_decile)

ward_imd <- 
  geographr::boundaries_wards %>% 
  filter(str_detect(ward_code, "^E")) %>% 
  left_join(imd_wards_subset, by = "ward_code")

lsoa_imd <- 
  geographr::boundaries_lsoa %>% 
  filter(str_detect(lsoa_code, "^E")) %>% 
  left_join(imd_lsoa, by = "lsoa_code")

n_lsoas_in_wards <- 
  geographr::lookup_lsoa_ward %>% 
  count(ward_code)

# ---- Custom map theme ----
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "FiraCode-Retina", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),  # element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_blank(),
      # Add labs elements
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(
        size = 10, hjust = 0.5,
        margin = margin(
          b = 0.2,
          t = 0.2,
          l = 2,
          unit = "cm"
        ),
        debug = F
      ),
      # captions
      plot.caption = element_text(
        size = 7,
        hjust = .5,
        margin = margin(
          t = 0.2,
          b = 0,
          unit = "cm"
        ),
        color = "#939184"
      ),
      ...
    )
}

# ---- Plot maps for all wards we're interested in ----

# ward_of_interest <- "E05009549"
wards_of_interest <- c(
  "E05012226",  # Neston
  "E05011376",  # Piccadilly
  "E05000655",  # Farnworth
  "E05009549",  # East Downs
  "E05012429",  # Lynton & Lynmouth
  "E05003527",  # Taw Vale
  "E05012433",  # North Molton
  "E05012436",  # Witheridge
  "E05009882",  # Hagworthingham
  "E05005648",  # Holbeach Hurn
  "E05009888",  # Mablethorpe
  "E05008894",  # Berrow
  "E05006776",  # Glastonbury St Edmund's
  "E05009230",  # Penzance Promenade
  "E05009159",  # Altarnun
  "E05011945",  # Harwich & Kingsway
  "E05004170"   # Passingford
)

for (ward_of_interest in wards) {
  current_ward <- 
    ward_imd %>% 
    filter(ward_code == ward_of_interest)
  
  current_ward_name <- 
    geographr::boundaries_wards %>% 
    filter(ward_code == ward_of_interest) %>% 
    st_drop_geometry() %>% 
    pull(ward_name)
  
  lsoas_in_ward <- 
    geographr::lookup_lsoa_ward %>% 
    filter(ward_code == ward_of_interest) %>% 
    pull(lsoa_code)
  
  current_lsoas <- 
    lsoa_imd %>% 
    filter(lsoa_code %in% lsoas_in_ward)
  
  # imd_wards %>% 
  #   filter(WD20CD == ward_of_interest) %>% 
  #   select(ends_with("Extent"))
  # 
  # imd_lsoa %>% 
  #   filter(LSOA %in% lsoas_in_ward)
  
  # - Some descriptives -
  # Extent of the current ward
  ward_imd_extent <- 
    imd_wards_subset %>% 
    filter(ward_code == ward_of_interest) %>% 
    pull(Extent)
  
  ward_imd_extent <- round(ward_imd_extent * 100, 0)
  
  # IMD decile of the current ward
  ward_imd_decile <- 
    imd_wards_subset %>% 
    filter(ward_code == ward_of_interest) %>% 
    pull(Score_decile)
  
  # Number of LSOAs in this ward
  n_lsoas <- 
    n_lsoas_in_wards %>% 
    filter(ward_code == ward_of_interest) %>% 
    pull(n)
  
  plt_map <- 
    current_lsoas %>% 
    ggplot() +
    geom_sf(
      color = "white",
      size = 1,
      mapping = aes(fill = factor(IMD_decile, levels = 1:10))
    ) +
    scale_fill_viridis(
      na.value = "transparent",
      option = "D",
      name = expression("IMD decile\n(1 = most deprived)"),
      alpha = 0.8, 
      discrete = TRUE, 
      direction = 1,
      drop = FALSE
    ) +
    
    theme_map() +
    labs(
      title = paste0("Deprivation in ", current_ward_name),
      subtitle = paste0("Areas show the ", n_lsoas, " LSOA(s) within this ward coloured by their deprivation decile\n",
                        "The ward has an IMD decile of ", ward_imd_decile, " and ", ward_imd_extent, "% of people live in the most deprived areas"),
      caption = "Source: British Red Cross analysis of MHCLG data"
    )
  
  ggsave(plt_map, file = paste0("maps/", current_ward_name, ".png"), height = 200, width = 220, units = "mm")
  print(paste0("Finished ", current_ward_name))
}
