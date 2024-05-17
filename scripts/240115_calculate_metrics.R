## This script takes boundaries determined by Katie Willie's code to figure out
## key watershed and burn characteristics for selected streampulse sites
## 1. fraction of watershed burned
## 2. size of watershed
## 3. dominant biome / vegetation
## 4. climate variables (MAT, MAP)

# 1. Setup ---------------------------------------------------------------------

## Call scripts to prep data (call first to load packages)
#source("scripts/0_setup.R") # already called in 1_mtbs_prep
source("scripts/1_mtbs_prep.R") # This takes 106s to run...

## Needed for Metric 4
p_load(nasapower)

# 2. Pull in USGS gauges dataset -----------------------------------------------

matched_sites <- read_csv("data/240108_matched_sites_for_alan_trim.csv")

## This removes 6 rows of duplicate (except for ign_date) rows, and then
## removes ~500 rows without a USGS gauge value
matched_sites_trimmed <- matched_sites %>% 
  group_by(site_id, usgs_gage, sp_start, lat, long, fire_id) %>%
  arrange(ign_date) %>%
  slice(1) %>% 
  ungroup() %>% 
  filter(!is.na(usgs_gage))

sites_sf <- matched_sites %>% 
  st_as_sf(coords = c("lat", "long"), crs = common_crs)

# 4. Read in watersheds from Alan ----------------------------------------------

boundaries <- read_sf("data/katie_willie/StreamPULSE.shp", crs = common_crs)

ggplot() + 
  geom_sf(data = conus_map, fill = NA) + 
  geom_sf(data = boundaries) + 
  geom_sf(data = sites_sf)


# 5. Read in ecoregion data ----------------------------------------------------

## To pull biome, we're going to use Level III ecoregions from EPA read in above: 
## https://www.epa.gov/eco-research/level-iii-and-iv-ecoregions-continental-united-states
ecoregions <- read_sf("data/us_eco_l3/us_eco_l3.shp") %>% 
  st_transform(crs = common_crs) %>% 
  st_make_valid() %>% 
  clean_names() #%>% 
 #select(na_l3name)


# 6. Pull precip and temp data 

map <- rnoaa::cpc_prcp(date = "2023-01-01", 
                       us = T, 
                       drop_undefined = T)

# 6. Pull metrics --------------------------------------------------------------
mtbs_prepped <- mtbs_conus %>% 
  clean_names() %>% 
  select(event_id, burn_bnd_ac)

pull_watershed_and_burn_characteristics_by_row <- function(i){
  
  message(paste("processing", i, "of", nrow(sites_sf)))
  ## Assemble steps for a single row, then set up as map() or for() to loop
  ## 1. Pull a single row from sites_sf
  x <- sites_sf %>% slice(i)
  gage <- x$usgs_gage
  
  fire_id = x$fire_id
  mtbs <- mtbs_prepped %>% filter(event_id == fire_id)
  watershed <- boundaries %>% 
    st_make_valid() %>% 
    st_intersects(., x, sparse = FALSE)[,1]
  
  ## Metric 1 - watershed area
  watershed_area <- st_area(watershed)
  
  ## Now there's an issue here, because the sites were originally intersected with
  ## HUC8s, but watersheds are now Katie Willie. So for each one we need to test
  ## if the current watershed overlaps with the fire in question. See slice(1) vs
  ## slice(2) as examples of non-overlapping / overlapping. This will be easy to 
  ## deal with, since we can just filter out burned_area == 0 later.
  
  ## Metric 2 - area of watershed that was burned
  burned_area_raw <- as.numeric(st_area(st_intersection(mtbs, watershed)))
  
  ## Need to reformat so it will read into the tibble
  burned_area <- ifelse(is.numeric(burned_area_raw), burned_area_raw, NA)
  
  ## Metric 3 - dominant ecoregion (we're going to use L2 for now)
  ecoregion <- ecoregions %>% 
    st_intersects(., x, sparse = FALSE)[,1] %>% 
    select(na_l2name)
  
  ## Metric 4 - 
  pull_climate <- function(site){
    
    coordinates <- st_coordinates(site) %>% 
      as_tibble()
    
    climate <- get_power(
      community = "ag",
      lonlat = c(coordinates$X, coordinates$Y),
      pars = c("T2M", "PRECTOTCORR"),
      temporal_api = "climatology") %>% 
      select(-c(LON, LAT, ANN)) 
    
    mat_c <- climate %>%
      filter(PARAMETER == "T2M") %>%
      as.numeric() %>% mean(., na.rm = T)
    
    map_mm_yr <- climate %>%
      filter(PARAMETER == "PRECTOTCORR") %>%
      as.numeric() %>% sum(., na.rm = T) * 365.25
    
    tibble(mat_c = mat_c, 
           map_mm_yr = map_mm_yr)
  }
  
  climate <- pull_climate(x)
  
  tibble(fire_id = fire_id, 
         watershed_area_m2 = as.numeric(watershed_area), 
         burned_area_m2 = as.numeric(burned_area), 
         usgs_gage = gage,
         ecoregion = ecoregion) %>% 
    bind_cols(climate)
}

## Set mid-point
mid = round(nrow(sites_sf)/2,0)

tic("pull characteristics 1")
st2_characteristics1 <- c(1:mid) %>%  
  #c(1:5) %>% 
  map(pull_watershed_and_burn_characteristics_by_row) %>% 
  bind_rows()
toc()

st2_clean1 <- st2_characteristics1 %>% 
  drop_na() %>% 
  unnest() %>% 
  select(fire_id, usgs_gage, watershed_area_m2, burned_area_m2, na_l2name, mat_c, map_mm_yr) %>% 
  st_drop_geometry()
write_csv(st2_clean1, "data/240117_st2_characteristics_1.csv")

rm(st2_characteristics1, st2_clean1)

tic("pull characteristics 2")
st2_characteristics2 <- c(mid:nrow(sites_sf)) %>% 
  #c(nrow(sites_sf)-5:nrow(sites_sf)) %>% 
  map(pull_watershed_and_burn_characteristics_by_row) %>% 
  bind_rows()
toc()

st2_clean2 <- st2_characteristics2 %>% 
  drop_na() %>% 
  unnest() %>% 
  select(fire_id, usgs_gage, watershed_area_m2, burned_area_m2, na_l2name, mat_c, map_mm_yr) %>% 
  st_drop_geometry()
write_csv(st2_clean2, "data/240117_st2_characteristics_2.csv")


# 
# p_load(devtools)
# 
# devtools::install_github("jayverhoef/SSN")




