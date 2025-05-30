
pacman::p_load("tidyverse", "auk", "data.table")

## ID not updated as likely to be removed......
## MY Updated
## NEW EU data not added 

## Data ------------------------------------------------------------------------
# Load from script 2, 3 and 4
data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_eBird_Index/Analysis/"

# ID
ID_obs_filt <- read.csv(paste0(data.path, "Data/eBird/Obs_datasets/ID_obs_filt_all.csv"))
ID_check_filt <- read.csv(paste0(data.path, "Data/eBird/Obs_datasets/ID_checkls_filt_all.csv"))
ID_sites_elev_df <- read.csv(paste0(data.path, "Data/Ranges/ID_within_range_pts.csv")) %>% select(-V1,-X)
ID_lst_length <- read.csv(paste0(data.path, "Data/eBird/Obs_datasets/ID_lst_lenght_all.csv"))

# MY
MY_obs_filt <- fread(paste0(data.path, "Data/eBird/Obs_datasets/MY_obs_filt_all.csv"))
MY_check_filt <- fread(paste0(data.path, "Data/eBird/Obs_datasets/MY_checkls_filt_all.csv"))
MY_sites_elev_df <- fread(paste0(data.path, "Data/Ranges/MY_within_range_pts.csv")) %>% select(-V1, -X)
MY_lst_length <- fread(paste0(data.path, "Data/eBird/Obs_datasets/MY_lst_lenght_all.csv")) %>% select(-V1)

master_taxonomy <- fread(paste0(data.path, "Data/Taxonomy/eBird_IUCN_AVONET_ID.MY_match.csv"))%>% select(-V1)

## Filter the ZF data - ID -----------------------------------------------------
# make a unique site sp code.
ID_sites_elev_corr <- ID_sites_elev_df %>%
  left_join(master_taxonomy, by = "IUCN_name", relationship = "many-to-many") %>%
  filter(!is.na(scientific_name)) %>%
  mutate(pt_sp = paste(pt, scientific_name, sep = "_"))

# 52,971,516 zero filled obs - all species site combinations
ID_ebd_zf <- auk_zerofill(ID_obs_filt, ID_check_filt, collapse = TRUE)

# 8,806,093 after filtering to just including points within each species range
# and elevation
ID_ebd_zf_filt <- ID_ebd_zf %>%
  mutate(pt_sp = paste(locality_id, scientific_name, sep = "_")) %>% 
  filter(pt_sp %in% ID_sites_elev_corr$pt_sp)

# filter for time and year and site type.
# filtering for total number of records and checklist number is a trade off between
# maintaining the diversity of species covered by not attempting to include species
# with very poor coverage or record.
# see https://doi.org/10.1016/j.biocon.2018.02.027 for an example of how increasing 
# checklist coverage can effect estimates.

# 1,721,832 obs
ID_zf_clean <- ID_ebd_zf_filt %>%
              ## focus only on hotspots
              filter(locality_type == "H", 
                     ## Focus on obs in dry season/summer
                     month(observation_date) >= 4 & month(observation_date) <= 9) %>%
              group_by(scientific_name) %>% 
              filter(sum(species_observed) >= 50, length(unique(checklist_id)) >= 1000) %>%
  left_join(ID_lst_length)

length(unique(ID_ebd_zf_filt$scientific_name)) # sp.1510
length(unique(ID_zf_clean$scientific_name)) # sp.405

length(unique(ID_ebd_zf_filt$locality_id)) # 11817 sites
length(unique(ID_zf_clean$locality_id)) # 958 sites

length(unique(ID_ebd_zf_filt$checklist_id)) # 33826
length(unique(ID_zf_clean$checklist_id)) # 10014

saveRDS(ID_zf_clean, "Data/eBird/Obs_datasets/zf_ID_March25.rds")

## Filter the ZF data - MY -----------------------------------------------------
# make a unique site sp code.
MY_sites_elev_corr <- MY_sites_elev_df %>%
  left_join(master_taxonomy, by = "IUCN_name", relationship = "many-to-many") %>%
  filter(!is.na(scientific_name)) %>%
  mutate(pt_sp = paste(pt, scientific_name, sep = "_"))

# 40,140,912 zero filled obs - all species site combinations
MY_ebd_zf <- auk_zerofill(MY_obs_filt, MY_check_filt, collapse = TRUE)

# Do not do this - we directly specify this grid in the modelling step
# 20,173,025 after filtering to just including points within each species range
# and elevation.
# MY_ebd_zf_filt <- MY_ebd_zf %>%
#   mutate(pt_sp = paste(locality_id, scientific_name, sep = "_")) %>% 
#   filter(pt_sp %in% MY_sites_elev_corr$pt_sp)

# Do create a reference col for if that point is in that species range
MY_ebd_zf_filt <- MY_ebd_zf %>%
  mutate(pt_sp = paste(locality_id, scientific_name, sep = "_"),
         pt.in.range = ifelse(pt_sp %in% MY_sites_elev_corr$pt_sp, 1,0),
         ch.ls.in.range = ifelse(pt.in.range == 1, checklist_id, NA),
         obs.in.range = ifelse(pt.in.range == 1, species_observed, NA))

# filter for time and year and site type.
# filtering for total number of records and checklist number is a trade off between
# maintaining the diversity of species covered by not attempting to include species
# with very poor coverage or record.
# see https://doi.org/10.1016/j.biocon.2018.02.027 for an example of how increasing 
# checklist coverage can effect estimates.


# 40,140,912 to 7,321,792 obs
MY_zf_clean <- MY_ebd_zf_filt %>%
  ## focus only on hotspots
  filter(locality_type == "H", 
         ## Focus on obs in dry season/summer
         lubridate::month(observation_date) >= 4 & lubridate::month(observation_date) <= 9) %>%
  group_by(scientific_name) %>% 
  # keep on species obs >50 times in range and have at least 1000 visits in range
  filter(sum(obs.in.range, na.rm = T) >= 50, 
         length(unique(ch.ls.in.range)) >= 1000)

length(unique(MY_ebd_zf_filt$scientific_name)) # sp.772
length(unique(MY_zf_clean$scientific_name)) # sp.466

length(unique(MY_ebd_zf_filt$locality_id)) # 11548 sites
length(unique(MY_zf_clean$locality_id)) # 548 sites

length(unique(MY_ebd_zf_filt$checklist_id)) # 51996
length(unique(MY_zf_clean$checklist_id)) # 15712

saveRDS(MY_zf_clean, paste0(data.path, "Data/eBird/Obs_datasets/zf_allsites_MY_Apr25.rds"))
