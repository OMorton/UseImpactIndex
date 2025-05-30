pacman::p_load("tidyverse", "sf", "terra", "tidyterra")
data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_eBird_Index/Analysis/"

sf_use_s2(FALSE)

source("functions.r")


## Purpose ---------------------------------------------------------------------

# Crop points by species ranges, to identify which points are within species 
# ranges (plus a small buffer) and thus that they could feasibly be detected in.
# Then crop these species, point combinations by point elevation if the point 
# is outside the species elevational range remove.

## Data ------------------------------------------------------------------------
All_IUCN_AVO_eBird_taxa <- read.csv(paste0(data.path, "Data/Taxonomy/eBird_IUCN_AVONET_ID.MY_match.csv")) %>%
  select(-n, -X)
all.bird.sf <- read_sf(paste0(data.path, "I:/Data/IUCN/Ranges/Birds/all.birds_08.10.24/all.birds.fixed.gpkg"))
asia_elev <- rast(paste0(data.path, "I:/Data/Elevation_SRTM90v4/SEA_AUS_elevation_final.tif"))

# MY data
MY_sf <- read_sf(paste0(data.path, "Data/GADM/gadm41_MYS_shp/gadm41_MYS_0.shp"))
MY_sp <- read.csv(paste0(data.path, "Data/eBird/Obs_species/MY_filt_all.csv"))
MY_sites <- read.csv(paste0(data.path, "Data/eBird/Obs_sites/MY_filt_all_sites.csv"))

# ID data
ID_sf <- read_sf(paste0(data.path, "Data/GADM/gadm41_MYS_shp/gadm41_IDN_0.shp"))
ID_sp <- read.csv(paste0(data.path, "Data/eBird/Obs_species/ID_filt_all.csv"))
ID_sites <- read.csv(paste0(data.path, "Data/eBird/Obs_sites/ID_filt_all_sites.csv"))

## Get intersections -----------------------------------------------------------
MY_sites_intersect <- site.range.intersect(ebird.nat.list = MY_sp, country.sf = MY_sf, 
                     samp.sites = MY_sites, buff.km = 25)
write.csv(MY_sites_intersect, paste0(data.path, "Data/Ranges/MY_species_pts_overlaps.csv"))


ID_sites_intersect <- site.range.intersect(ebird.nat.list = ID_sp, country.sf = ID_sf, 
                                            samp.sites = ID_sites, buff.km = 25)
write.csv(ID_sites_intersect, paste0(data.path, "Data/Ranges/ID_species_pts_overlaps.csv"))

## Get elevations --------------------------------------------------------------
MY_elev <- get.elevation(samp.sites = MY_sites, elev.raster = asia_elev)

ID_elev <- get.elevation(samp.sites = ID_sites, elev.raster = asia_elev)

## Compile ---------------------------------------------------------------------
elev_buff <- read.csv(paste0(data.path, "Data/IUCN/tidy_elev_Sept2024.csv")) %>% 
  select(sn, elev_min, elev_max) %>% mutate(elev_min = elev_min -200,
                                    elev_max = elev_max +200)

MY_sites_elev_df <- MY_sites_intersect %>% 
  left_join(MY_elev, by = c("pt" = "locality_id")) %>%
  left_join(elev_buff, by = c("IUCN_name" = "sn")) %>%
  mutate(in_range = ifelse(elev <= elev_max & elev >= elev_min, TRUE, FALSE)) %>%
  filter(in_range %in% c(TRUE, NA))
write.csv(MY_sites_elev_df, paste0(data.path, "Data/Ranges/MY_within_range_pts.csv"))


ID_sites_elev_df <- ID_sites_intersect %>% 
  left_join(ID_elev, by = c("pt" = "locality_id")) %>%
  left_join(elev_buff, by = c("IUCN_name" = "sn")) %>%
  mutate(in_range = ifelse(elev <= elev_max & elev >= elev_min, TRUE, FALSE)) %>%
  filter(in_range %in% c(TRUE, NA))
write.csv(ID_sites_elev_df, paste0(data.path, "Data/Ranges/ID_within_range_pts.csv"))
