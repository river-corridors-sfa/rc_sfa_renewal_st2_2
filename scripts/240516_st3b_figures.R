## This script is supposed to make some small figures for use on renewal poster
## for ST-3B
##
## 2024-05-16
## Peter Regier
## 
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Call scripts to prep data (call first to load packages)
source("scripts/0_setup.R")

p_load(ggsflabel, 
       cowplot, 
       tmaptools)

figure_path = "figures/st3b_poster/"


# 2. Prep data -----------------------------------------------------------------

## I'm recycling code from the ST-3 modex figure watersheds to keep things 1)
## visually consistent and 2) avoid a lengthy rabbit-hole of finding the "right"
## watershed

## Custom watersheds drawn by Micah that were used in proposal figure
custom_yrb_boundaries <- read_sf("/Users/regi350/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/RC/rcsfa-ST2-2-streampulse/data/Yakama_Basin_Custom_Boundaries/Yakama_Basin_Custom_Boundaries.shp")

## Let's pull in Schneider Springs as well


initial_plots <- function(i){
  boundary <- custom_yrb_boundaries %>% slice(i)
  
  flowlines <- nhdplusTools::get_nhdplus(AOI = boundary) %>% 
    filter(fcode %in% c(46006, 46003)) %>% 
    mutate(fcode_fct = case_when(fcode == 46006 ~ "perennial", 
                                 fcode == 46003 ~ "intermittent"))
  
  ggplot() + 
    geom_sf(data = boundary, fill = "forestgreen", alpha = 0.2) + 
    #geom_sf(data = boundary, fill = "forestgreen") + 
    #geom_sf(data = flowlines, aes(color = fcode_fct, lwd = streamorde), 
    #        color = "gray") +
    geom_sf(data = flowlines, aes(color = fcode_fct)) + 
    geom_sf_text(data = flowlines, aes(label = comid), size = 2) + 
    scale_color_manual(values = c("lightblue", "darkblue")) + 
    labs(color = "") + 
    theme_map()
}


initial_plots(2)
ggsave(paste0(figure_path, "satus_comids.png"),
       width = 15, height = 20)

nhdplusTools::get_nhdplus(AOI = custom_yrb_boundaries %>% slice(2)) %>% 
  filter(fcode %in% c(46006, 46003)) %>% 
  mutate(fcode_fct = case_when(fcode == 46006 ~ "perennial", 
                               fcode == 46003 ~ "intermittent")) %>% 
  filter(comid == 23100244) %>% 
  st_centroid()


## Manually pick Satus sites based on stream order and non-perenniality
alt1_comids <- c(23100244, 23100436, 23104628, 23104408, 23105168, 
                 23100314, 23100320, 23100272)

second_plots <- function(i, alt_comids){
  boundary <- custom_yrb_boundaries %>% slice(i)
  
  flowlines <- nhdplusTools::get_nhdplus(AOI = boundary) %>% 
    filter(fcode %in% c(46006, 46003)) %>% 
    mutate(fcode_fct = case_when(fcode == 46006 ~ "perennial", 
                                 fcode == 46003 ~ "intermittent"))
  
  sites <- flowlines %>% filter(comid %in% alt_comids) %>% 
    st_centroid()
  
  ggplot() + 
    geom_sf(data = boundary, fill = "white") + 
    geom_sf(data = boundary, fill = "forestgreen", alpha = 0.2) + 
    geom_sf(data = flowlines, aes(color = fcode_fct)) + 
    geom_sf(data = sites, color = "black", size = 5) + 
    geom_sf(data = sites, color = "white", size = 3) + 
    scale_color_manual(values = c("lightblue", "darkblue")) + 
    #scale_color_manual(values = c("gray95", "white")) + 
    labs(color = "") + 
    theme_map() + 
    theme(#legend.position = "bottom", 
          legend.position  = c(0.3, 0),
          legend.direction = 'horizontal',
          legend.background = element_blank()) 
}

second_plots(2, alt1_comids)
ggsave(paste0(figure_path, "alt1_watershed_and_sites.png"),
       width = 4, height = 4)


## Bring in respiration and create watershed plots -----------------------------

scaling_analysis_dat <- read_csv("/Users/regi350/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/peterregier/rc_wrb_yrb_scaling/data/231008_scaling_analysis_dat.csv") %>% 
  filter(basin == "yakima") %>% 
  dplyr::select(comid, 
                wshd_area_km2, 
                accm_hzt_cat,
                totco2_o2g_day,
                accm_totco2_o2g_day, 
                longitude,
                latitude)

## Prep is specifically for Satus now
satus_boundary <- custom_yrb_boundaries %>% slice(2)
  
satus_flowlines <- nhdplusTools::get_nhdplus(AOI = satus_boundary) %>% 
    filter(fcode %in% c(46006, 46003)) %>% 
    mutate(fcode_fct = case_when(fcode == 46006 ~ "perennial", 
                                 fcode == 46003 ~ "intermittent"))
  
satus_sites <- satus_flowlines %>% filter(comid %in% alt1_comids) %>% 
    st_centroid() %>% 
  inner_join(scaling_analysis_dat, by = "comid")

satus_resp <- inner_join(satus_flowlines, 
           scaling_analysis_dat, 
           by = "comid")

satus_tot_resp <- ggplot() + 
  geom_sf(data = satus_boundary, fill = "white") + 
  geom_sf(data = satus_boundary, fill = "forestgreen", alpha = 0.2) + 
  geom_sf(data = satus_resp, aes(color = totco2_o2g_day)) + 
  geom_sf(data = satus_sites, color = "black", size = 5) + 
  geom_sf(data = satus_sites, aes(color = totco2_o2g_day), size = 3) + 
  geom_sf_text_repel(data = satus_sites, aes(label = round(totco2_o2g_day, 0)), 
                     size = 2.5, fontface = "bold", nudge_x = -0.03, nudge_y = -0.03) + 
  scale_color_viridis_c(trans = "log10", option = "D") + 
  theme_map() + 
  labs(title = "Reach-scale respiration", color = "g CO2/d") + 
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5), 
        legend.text = element_text(size = 8), 
        legend.key.width = unit(1, "cm"))

satus_accm_resp <- ggplot() + 
  geom_sf(data = satus_boundary, fill = "white") + 
  geom_sf(data = satus_boundary, fill = "forestgreen", alpha = 0.2) + 
  geom_sf(data = satus_resp, aes(color = accm_totco2_o2g_day)) + 
  geom_sf(data = satus_sites, color = "black", size = 5) + 
  geom_sf(data = satus_sites, aes(color = accm_totco2_o2g_day), size = 3) + 
  geom_sf_text_repel(data = satus_sites, aes(label = round(accm_totco2_o2g_day, 0)), 
               size = 2.5, fontface = "bold", nudge_x = -0.03, nudge_y = -0.03) + 
  scale_color_viridis_c(trans = "log10", option = "D") + 
  theme_map() + 
  labs(title = "Cumulative respiration", color = "g CO2/d") + 
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5), 
        legend.text = element_text(size = 8), 
        legend.key.width = unit(1, "cm"))

plot_grid(satus_tot_resp, satus_accm_resp, nrow = 1)
ggsave(paste0(figure_path, "satus_respiration.png"),
       width = 8, height = 4)










nhdplusTools::get_nhdplus(AOI = custom_yrb_boundaries %>% slice(1)) %>% 
  filter(fcode %in% c(46006, 46003)) %>% 
  mutate(fcode_fct = case_when(fcode == 46006 ~ "perennial", 
                               fcode == 46003 ~ "intermittent")) %>% 
  group_by(fcode_fct) %>% 
  summarize(length = sum(lengthkm)) %>% 
  st_drop_geometry() %>% 
  mutate(perc_length = length / sum(length)) %>% 
  filter(fcode_fct == "perennial") %>% 
  pull(perc_length)







# 
# ## https://www.epa.gov/eco-research/level-iii-and-iv-ecoregions-continental-united-states
# ecoregions <- read_sf("data/us_eco_l3/us_eco_l3.shp") %>% 
#   st_transform(crs = common_crs) %>% 
#   st_make_valid() %>% 
#   clean_names() %>% 
#   mutate(na_l2name = case_when(na_l2name == "UPPER GILA MOUNTAINS (?)" ~ "UPPER GILA MOUNTAINS", 
#                                TRUE ~ na_l2name))
# 
# ## Bring in info on wildfires overlapping USGS gauges in time and space
# matched_sp_and_mtbs_trim <- read_csv("data/240108_matched_sites_for_alan_trim.csv") %>% 
#   st_as_sf(coords = c("lat", "long"), crs = common_crs) %>% 
#   mutate(index = row_number()) #%>% 
#   #slice(1:100)
# 
# ## Add MAP
# st2_clean2 <- read_csv("data/240117_st2_characteristics_2.csv") %>% 
#   group_by(usgs_gage) %>% 
#   summarize(map_mm_yr = first(map_mm_yr))
# 
# ## Alt1 dataset
# sites_df <- inner_join(matched_sp_and_mtbs_trim, 
#            st2_clean2, 
#            by = "usgs_gage")
# 
# ## 5 sites spanning arid-to-mesic
# alt1_sites <- sites_df %>% 
#   filter(index %in% c(36, 2668, 3052, 3094, 5459))
# 
# ggplot() + 
#   geom_sf(data = conus_map, fill = "white") + 
#   geom_sf(data = alt1_sites, aes(size = map_mm_yr * 1.5), 
#           color = "black", show.legend = F) + 
#   geom_sf(data = alt1_sites, aes(size = map_mm_yr), color = "forestgreen") + 
#   scale_color_viridis_c() + 
#   labs(color = "Mean annual precip (mm/yr)", 
#        title = "Sites span 77-1645 cm of precipitation per year") + 
#   coord_sf(crs = coord_sf_crs) + 
#   theme_map() + 
#   theme(legend.position = "none", 
#         plot.title = element_text(hjust = 0.5))
# ggsave(paste0(figure_path, "1_alt1_conus.png"), 
#        width = 5, height = 4)
# 
# ## Select example watershed, pull from NHDPlus, ID sites along gradient of VI
# sites_df %>% filter(grepl("AZ", fire_id))
# 
# sites_subset <- sites_df %>% 
#   slice(1:5)
#  #filter(index %in% c(1:4))
# 
# sites_subset %>% 
#   map(get_huc(AOI = alt1_ex_site, type = "huc10"))
# 
# 
# 
# 
# 
# plot_huc_by_row <- function(row){
#   
#   x <- sites_subset %>% 
#     slice(row)
#   
#   huc = get_huc(AOI = x, type = "huc10")
#   flowlines <- nhdplusTools::get_nhdplus(AOI = huc) %>% 
#     mutate(fcode_fct = case_when(fcode == 46006 ~ "perennial", 
#                                  fcode == 46003 ~ "intermittent", 
#                                  fcode == 46007 ~ "ephemeral", 
#                                  TRUE ~ "other"))
#   
#   ggplot() + 
#     geom_sf(data = huc, fill = "white") + 
#     geom_sf(data = flowlines, 
#             aes(color = fcode_fct), 
#             show.legend = F) + 
#     geom_sf(data = x) + 
#     scale_color_manual(values = c("lightblue", "red", "blue")) + 
#     facet_wrap(~index)
# }
# 
# 
# 
# make_plots_of_rows <- function(rows){
#   
#   min_row = min(rows)
#   max_row = max(rows)
#   
#   plots <- rows %>% 
#     map(plot_huc_by_row)
#   
#   plot_grid(plotlist = plots)
#   ggsave(paste0(figure_path, "grid_plots", min_row, "_", max_row, ".png"), 
#          width = 15, height = 12)
# }
# 
# 
# make_plots_of_rows(c(1:4))
# make_plots_of_rows(7)
# make_plots_of_rows(c(9:12))
# 
# 
# first_row = min(rows)
# last_row = max(rows)
# 
# 
# plots <- rows %>% 
#   map(plot_huc_by_row)
# 
# plot_grid(plotlist = plots)
# 
# ggsave(grid_plot, "grid_plot.png")
# ggsave(paste0(figure_path, "grid_plots.png"), 
#        width = 15, height = 12)
# 
# 
# 
# plot_huc_by_row(3)
# 
# 
# alt1_ex_site = sites_df %>% 
#   slice(3:5)
#   #filter(index == 5999)
# alt1_huc <- get_huc(AOI = alt1_ex_site, type = "huc10")
# alt1_flowlines <- nhdplusTools::get_nhdplus(AOI = alt1_huc) %>% 
#   mutate(fcode_fct = case_when(fcode == 46006 ~ "perennial", 
#                                fcode == 46003 ~ "intermittent", 
#                                fcode == 46007 ~ "ephemeral", 
#                                TRUE ~ "other"))
# 
# ## Fcodes from 
# ## 46006: perennial, 46003: intermittent, 46007: ephemeral
# unique(alt1_flowlines$fcode)
# 
# ggplot() + 
#   geom_sf(data = alt1_huc, fill = "white") + 
#   geom_sf(data = alt1_flowlines, 
#           aes(color = fcode_fct), 
#           show.legend = F) + 
#   geom_sf(data = alt1_ex_site) + 
#   scale_color_manual(values = c("lightblue", "red", "blue")) + 
#   facet_wrap(~index)
# 
# 
# 
# 
# ggplot() + 
#   geom_sf(data = conus_map, fill = "white") + 
#   geom_sf(data = sites_df, color = "red", size = 1) + 
#   geom_sf_text(data = sites_df, aes(label = index, color = map_mm_yr), size = 4)
# ggsave(paste0(figure_path, "pick_gauges.png"), 
#        width = 15, height = 12)
# 
# 
# # 4. CONUS plot ----------------------------------------------------------------
# 
#   
#   
#   
#   geom_sf(data = ecoregions %>% slice(1:20), 
#           aes(fill = na_l2name), 
#           alpha = 0.5, show.legend = F) + 
#   geom_sf(data = matched_sp_and_mtbs_trim, color = "gray", size = 0.5) + 
#   geom_sf(data = selected_fires, size = 3) + 
#   coord_sf(crs = coord_sf_crs) + 
#   theme_map()
# ggsave(paste0(figure_path, "conus.png"), 
#        width = 5, height = 4)
# 
# 
# 
# 
# 
# 
