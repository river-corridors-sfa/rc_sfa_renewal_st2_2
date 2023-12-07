## This script prepares MTBS burned boundary area data for combining with
## StreamPULSE
## 
## 2023-12-06
## Peter Regier
## 
# ########### #

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")


# 2. Prep MTBS layer -----------------------------------------------------------

## Import the burn parameter data
tic("import and prep mtbs")
mtbs_raw <- read_sf("data/mtbs/mtbs_perimeter_data/mtbs_perims_DD.shp") %>% 
  st_transform(crs = common_crs) %>% 
  st_make_valid() # required because there are overlapping polygons

## Crop to CONUS
mtbs_conus <- mtbs_raw %>% 
  st_crop(., conus_map)
toc()





## Moving forward, it might be nice to compartmentalize things by state then
## future_map() across so it gets somewhat more managable working with big data.

mtbs_conus_clean <- mtbs_conus %>% 
  clean_names() %>% 
  select()


x <- mtbs_conus %>% 
  slice(1:100)




ggplot() + 
  geom_sf(data = conus_map) + 
  geom_sf(data = x)

tic("intersect sites and burn-scars")
## nrow(x) = 300 - 29s (0 overlaps)
## nrow(x) = 500 - 39s (0 overlaps)
## nrow(x = 1000) - 66s (0 overlaps)
spatial_overlaps <- st_intersection(sp_sites_conus, x)
toc()



