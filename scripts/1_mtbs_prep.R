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

