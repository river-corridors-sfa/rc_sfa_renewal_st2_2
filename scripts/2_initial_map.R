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
source("scripts/1_streampulse_prep.R")
source("scripts/1_mtbs_prep.R")
toc()

# 2. Make plot -----------------------------------------------------------------

tic("make plot") #200s
ggplot() + 
  geom_sf(data = conus_map, fill = "gray", alpha = 0.05) + 
  geom_sf(data = sp_sites_conus, aes(pch = source, color = source), alpha = 0.8) + 
  geom_sf(data = mtbs_conus,
          #%>% slice(1:100),  # use this to tweak plot for faster rendering
          color = "orange", fill = "orange", alpha = 0.6) + 
  labs(pch = "", color = "", title = "StreamPULSE sites and wildfires") + 
  coord_sf(crs = coord_sf_crs)
ggsave("figures/1_conus_map.png", width = 8, height = 4)
toc()


tic("make plot") #200s
ggplot() + 
  geom_sf(data = conus_map, fill = "gray10", alpha = 0.95) + 
  geom_sf(data = sp_sites_conus, aes(pch = source, color = source), alpha = 0.8) + 
  geom_sf(data = mtbs_conus,
          #%>% slice(1:100),  # use this to tweak plot for faster rendering
          color = "orange", fill = "orange", alpha = 0.6) + 
  labs(pch = "", color = "", title = "StreamPULSE sites and wildfires") + 
  coord_sf(crs = coord_sf_crs)
ggsave("figures/1_conus_map_dark.png", width = 8, height = 4)
toc()

