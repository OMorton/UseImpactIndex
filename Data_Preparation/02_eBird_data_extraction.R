
pacman::p_load("tidyverse", "auk")

source("functions.r")

## Purpose ---------------------------------------------------------------------
# - only stationary or travelling (10km or 6 hours)
# - remove escapees
# - 2010-2023
# - complete checklists
# - 10 or fewer observers
# - remove observers with less than 3 checklists
# - remove checklists with less than 3 species (reduce targeted rarity hunting)

## Data ------------------------------------------------------------------------
## MY
MY_filt <- filter.ebird.data(sed.path = "I:/Data/Ebird/Sep24/Subsets/ebd_MY_smp_relJan-2025/ebd_MY_smp_relJan-2025_sampling.txt", 
                  ebd.path = "I:/Data/Ebird/Sep24/Subsets/ebd_MY_smp_relJan-2025/ebd_MY_smp_relJan-2025.txt",
                  sens.ebd.path = "I:/Data/Ebird/Sep24/Sensitive/ebd_sensitive_relAug-2024_MY.txt/ebd_sensitive_relAug-2024_MY.txt")

## ID
ID_filt <- filter.ebird.data(sed.path = "I:/Data/Ebird/Sep24/Subsets/ebd_ID_smp_relAug-2024/ebd_ID_smp_relAug-2024_sampling.txt", 
                             ebd.path = "I:/Data/Ebird/Sep24/Subsets/ebd_ID_smp_relAug-2024/ebd_ID_smp_relAug-2024.txt",
                             sens.ebd.path = "I:/Data/Ebird/Sep24/Sensitive/ebd_sensitive_relAug-2024_ID.txt/ebd_sensitive_relAug-2024_ID.txt")

## IT
IT_filt <- filter.ebird.data(sed.path = "I:/Data/Ebird/Sep24/Subsets/ebd_IT_smp_relFeb-2025/ebd_IT_smp_relFeb-2025_sampling.txt", 
                             ebd.path = "I:/Data/Ebird/Sep24/Subsets/ebd_IT_smp_relFeb-2025/ebd_IT_smp_relFeb-2025.txt",
                             sens.ebd.path = "I:/Data/Ebird/Sep24/Sensitive/ebd_sensitive_relAug-2024_ID.txt/ebd_sensitive_relAug-2024_ID.txt")
## Write out data for subs steps -----------------------------------------------

## MY
MY_filt$obs_filt %>% 
  group_by(scientific_name, common_name) %>% 
  tally() %>% write.csv("Data/eBird/Obs_species/MY_filt_all.csv")

MY_filt$check_filt %>% group_by(latitude, longitude, locality_id) %>% 
  tally() %>% write.csv("Data/eBird/Obs_sites/MY_filt_all_sites.csv")

write.csv(MY_filt$obs_filt, "Data/eBird/Obs_datasets/MY_obs_filt_all.csv")
write.csv(MY_filt$check_filt, "Data/eBird/Obs_datasets/MY_checkls_filt_all.csv")
write.csv(MY_filt$lst_length, "Data/eBird/Obs_datasets/MY_lst_lenght_all.csv")

## ID
ID_filt$obs_filt %>% 
  group_by(scientific_name, common_name) %>% 
  tally() %>% write.csv("Data/eBird/Obs_species/ID_filt_all.csv")

ID_filt$check_filt %>% group_by(latitude, longitude, locality_id) %>% 
  tally() %>% write.csv("Data/eBird/Obs_sites/ID_filt_all_sites.csv")

write.csv(ID_filt$obs_filt, "Data/eBird/Obs_datasets/ID_obs_filt_all.csv")
write.csv(ID_filt$check_filt, "Data/eBird/Obs_datasets/ID_checkls_filt_all.csv")
write.csv(ID_filt$lst_length, "Data/eBird/Obs_datasets/ID_lst_lenght_all.csv")
