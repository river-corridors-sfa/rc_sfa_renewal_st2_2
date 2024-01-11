## This script serves two purposes: 1) find interesections between streampulse and
## MTBS datasets and 2) Pull datasets and prep for Alan to analyze. For Alan:  
## Streampulse: site names, lat-longs, intersects spatially with wildfire
## Wildfires: fire names, date burned, upstream of streampulse gauge?

# 1. Setup ---------------------------------------------------------------------

## Call scripts to prep data (call first to load packages)
source("scripts/0_setup.R")

tic("prep") #104s
source("scripts/1_streampulse_prep.R")
source("scripts/1_mtbs_prep.R")
toc()


# 2. Find HUC-10 watershed downstream of wildfire ------------------------------

match_fires_and_sp_huc8 <- function(fire_id){ 
  
  ## Pull single fire outline
  fire = mtbs_conus %>% filter(Event_ID == fire_id)
  
  ## Pull the associated watershed
  watershed <- get_huc(AOI = fire, type = "huc08") %>% 
    st_union()
  
  ## ID streampulse sites in 
  x <- sp_sites_conus %>% 
    st_intersects(., watershed, sparse = FALSE)[,1] %>% 
    extract(geometry, c('lat', 'long'), '\\((.*), (.*)\\)', convert = TRUE) %>%
    rename("usgs_gage" = usg_sgage_id, 
           "sp_start" = first_record) %>% 
    select(site_id, usgs_gage, sp_start, lat, long) %>% 
    add_column(fire_id = fire$Event_ID, 
               ign_date = fire$Ig_Date)
  
  if (nrow(x) == 0) {
    # If the tibble is empty, return a tibble with blank rows
    x <- tibble::tibble(site_id = NA, usgs_gage = NA, sp_start = NA, 
                        lat = NA, long = NA) %>% 
      add_column(fire_id = fire$Event_ID, 
                 ign_date = fire$Ig_Date)
  } else {
    # If the tibble is not empty, process it
    x <- x
  }
  return(x)
}

matched_sp_and_mtbs <- mtbs_conus %>% 
  st_drop_geometry() %>% 
  select(Event_ID) %>% 
  #slice(1:100) %>% 
  pull() %>% 
  map(match_fires_and_sp_huc8) %>% 
  bind_rows()

matched_sp_and_mtbs_trim <- matched_sp_and_mtbs %>% 
  filter(!is.na(site_id)) %>% 
  filter(sp_start < ign_date)

write_csv(matched_sp_and_mtbs, "data/240108_matched_sites_for_alan.csv")
write_csv(matched_sp_and_mtbs_trim, "data/240108_matched_sites_for_alan_trim.csv")

