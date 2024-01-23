## Initial plots for ST2-2 as of 1/17/23. There will be several outputs, focused 
## on the first two tasks for ST2-2: 
### 1. Map StreamPulse sites w outlines of MTBS wildfires
### 2. Table w of watershed burned, size of watershed, dominant biome, and climate
## P1: map for task 1 of CONUS
## P2: map for task 2 w USGS sites, Katie watersheds, MTBS wildfires, and ecoregions
## P3: density plots for size of watershed, % burned, biome, MAT, and MAP
##
## Peter Regier
## 2024-01-17
##
# ########## #
# ########## #

# 1. Setup ---------------------------------------------------------------------

## Call scripts to prep data (call first to load packages)
source("scripts/0_setup.R") # already called in 1_mtbs_prep
source("scripts/1_mtbs_prep.R") # This takes 106s to run...
source("scripts/1_streampulse_prep.R")


# 2. Make initial map -------------------------------------------------------

tic("make plot") #200s
ggplot() + 
  geom_sf(data = conus_map, fill = "white") + 
  geom_sf(data = sp_sites_conus, aes(pch = source), alpha = 0.9) + 
  geom_sf(data = mtbs_conus,
          #%>% slice(1:100),  # use this to tweak plot for faster rendering
          color = "red", fill = "red", alpha = 0.2) + 
  labs(pch = "", color = "", title = "656 StreamPULSE sites, 28k MTBS burn-scars") + 
  coord_sf(crs = coord_sf_crs)
ggsave("figures/1_conus_map.png", width = 8, height = 5)
toc()


# 3. Read in P2 datasets -------------------------------------------------------

characteristics <- read_csv("data/240115_st2_characteristics.csv")

boundaries <- read_sf("data/katie_willie/StreamPULSE.shp", crs = common_crs)

ggplot() + 
  geom_sf(data = conus_map, fill = "white") + 
  geom_sf(data = sp_sites_conus, alpha = 0.9) + 
  geom_sf(data = boundaries)


# 4. Read in watersheds from Alan ----------------------------------------------







