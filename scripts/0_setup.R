## Setup script for things standardized across analyses

## Load packages
require(pacman)
p_load(tidyverse,
       janitor,
       future,
       ggthemes,
       sf, 
       tictoc, # time stuff
       rnaturalearth, # US map baselayer
       nhdplusTools)

## Set a common crs
common_crs = 4326

## Coordinate projection for coord_sf to make things look cool
coord_sf_crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100"

## US maps
us_map <- ne_states(country = "united states of america", 
                    returnclass = "sf")

conus_map <- us_map %>% 
  filter(gn_name != "Hawaii" & gn_name != "Alaska") %>% 
  mutate(st_abb = str_sub(gn_a1_code, 4,5)) %>% 
  select(gn_name, st_abb)

## Set ggplot theme
theme_set(theme_bw())

## Conus map presets
conus_width = 8
conus_height = 6
