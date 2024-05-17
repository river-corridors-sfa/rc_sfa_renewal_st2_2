## Make map for the first task (see readme)
##
## 2023-12-07
## Peter Regier
## 
# ########### #

# 1. Setup ---------------------------------------------------------------------


## Call scripts to prep data (call first to load packages)
source("scripts/0_setup.R")

tic("prep") #104s
#source("scripts/1_streampulse_prep.R")
#source("scripts/1_mtbs_prep.R")
toc()

# 2. Make plot -----------------------------------------------------------------

## https://www.epa.gov/eco-research/level-iii-and-iv-ecoregions-continental-united-states
ecoregions <- read_sf("data/us_eco_l3/us_eco_l3.shp") %>% 
  st_transform(crs = common_crs) %>% 
  st_make_valid() %>% 
  clean_names() %>% 
  mutate(na_l2name = case_when(na_l2name == "UPPER GILA MOUNTAINS (?)" ~ "UPPER GILA MOUNTAINS", 
                               TRUE ~ na_l2name))

tic("make plot") #200s
ggplot() + 
  geom_sf(data = conus_map, fill = "gray", alpha = 0.01) + 
  geom_sf(data = ecoregions, 
          aes(fill = na_l2name), alpha = 0.3) + 
  geom_sf(data = mtbs_conus,
          #%>% slice(1:3000),  # use this to tweak plot for faster rendering
          color = "red", fill = "orange", alpha = 0.8) + 
  geom_sf(data = sp_sites_conus, alpha = 0.5) + 
  labs(fill = "Ecoregion", color = "", 
       title = "StreamPULSE sites, MTBS wildfires and EPA ecoregions") + 
  coord_sf(crs = coord_sf_crs)
ggsave("figures/240123_1_conus_map.png", width = 15, height = 7.5)
toc()


# tic("make plot") #200s
# ggplot() + 
#   geom_sf(data = conus_map, fill = "gray10", alpha = 0.95) + 
#   geom_sf(data = sp_sites_conus, aes(pch = source, color = source), alpha = 0.8) + 
#   geom_sf(data = mtbs_conus,
#           #%>% slice(1:100),  # use this to tweak plot for faster rendering
#           color = NA, fill = "orange", alpha = 0.6) + 
#   labs(pch = "", color = "", title = "StreamPULSE sites and wildfires") + 
#   coord_sf(crs = coord_sf_crs)
# ggsave("figures/1_conus_map_dark.png", width = 8, height = 4)
# toc()

