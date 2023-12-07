## This script imports basic site information about StreamPULSE sites downloaded
## from https://data.streampulse.org/download_bulk ("All basic site data")
##
## Goal: create an sf object that plays nicely with MTBS
##
## 2023-12-06
## Peter Regier
## 
# ########### #

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")


# 2. Site metadata -------------------------------------------------------------

## Read in basic metadata and convert to spatial object
sp_sites_all <- read_csv("data/streampulse/all_basic_site_data.csv") %>% 
  clean_names() %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% #some missing values stop conversion to sf
  st_as_sf(coords = c("longitude", "latitude"), crs = common_crs)

## Visualize it
ggplot() + 
  geom_sf(data = conus_map) + 
  geom_sf(data = sp_sites_all, aes(color = data_source))

## Since we're focused on CONUS, remove sites outside conus and replot
sp_sites_conus <- sp_sites_all %>% 
  st_crop(., conus_map) %>% 
  mutate(source = case_when(grepl("neon", data_source) ~ "NEON", 
                            grepl("nature", data_source) ~ "USGS", 
                            TRUE ~ "StreamPULSE"))
  
ggplot() + 
  geom_sf(data = conus_map) + 
  geom_sf(data = sp_sites_conus, aes(color = data_source))
ggsave("figures/231206_conus_streampulse_sites.png", 
       width = conus_width, height = conus_height)





