


## eBird filtering -------------------------------------------------------------
filter.ebird.data <- function(sed.path, ebd.path, sens.ebd.path) {
  ## read in
  check_ls <- read_sampling(sed.path)
  obs <- read_ebd(ebd.path)
  obs_sens <- read_ebd(sens.ebd.path)
  
  ## fitler
  check_filt <- check_ls %>% 
    mutate(effort_distance_km = ifelse(protocol_type == "Stationary", 
                                       0, effort_distance_km),
           year = year(observation_date)) %>%
    filter(observation_date >= "2010-01-01" & observation_date <= "2023-12-31",
           protocol_type %in% c("Traveling", "Stationary"),
           all_species_reported == TRUE, number_observers <= 10,
           duration_minutes < 60*6 +1,
           effort_distance_km <=10) %>%
    group_by(observer_id) %>% filter(n() >3)
  uni_checkls_id <- unique(check_filt$checklist_id)
  
  obs_filt <- obs %>% 
    mutate(effort_distance_km = ifelse(protocol_type == "Stationary", 
                                       0, effort_distance_km))%>%
    filter(observation_date >= "2010-01-01" & observation_date <= "2023-12-31",
           protocol_type %in% c("Traveling", "Stationary"),
           all_species_reported == TRUE, number_observers <= 10,
           duration_minutes < 60*6 +1,
           effort_distance_km <=10, exotic_code %in% c(NA, "", "N"),
           checklist_id %in% uni_checkls_id)
  
  
  obs_sens_filt <- obs_sens %>% 
    mutate(effort_distance_km = ifelse(protocol_type == "Stationary", 
                                       0, effort_distance_km))%>%
    filter(observation_date >= "2010-01-01" & observation_date <= "2023-12-31",
           protocol_type %in% c("Traveling", "Stationary"),
           all_species_reported == TRUE, number_observers <= 10,
           duration_minutes < 60*6 +1,
           effort_distance_km <=10 , exotic_code %in% c(NA, "", "N"),
           checklist_id %in% uni_checkls_id)
  
  obs_filt_all <- rbind(obs_filt, obs_sens_filt) %>% 
    filter(!grepl("undescribed", common_name))
  
  # calc list length
  lst_lngth <- obs_filt_all  %>% group_by(checklist_id) %>% 
    summarise(list.length = length(unique(scientific_name)))
  
  # Do filter out lists of 1-2 species to remove instances of targetted searching 
  # for specific species
  check_filt2 <- check_filt %>% left_join(lst_lngth) %>% 
    mutate(list.length = ifelse(is.na(list.length), 0, list.length)) %>%
    filter(list.length >= 3)
  
  obs_filt_all2 <- obs_filt_all %>% 
    filter(checklist_id %in% unique(check_filt2$checklist_id))
  
  return(list("check_filt" = check_filt2, "obs_filt" = obs_filt_all2,
              "lst_length" = lst_lngth))
  
}

## Sampling points and species overlap -----------------------------------------

site.range.intersect <- function(ebird.nat.list, country.sf, samp.sites,
                                 buff.km = 25) {

  # correct taxonomy
  ebird.sp.list <- ebird.sp.list %>% left_join(All_IUCN_AVO_eBird_taxa) %>% 
    filter(!grepl("undescribed", common_name))
  
  # filter to just ranges of target sp
  nat.ranges <- all.bird.sf %>% filter(SCI_NAME %in% ebird.sp.list$IUCN_name) %>%
    st_transform("+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  
  # check crs
  country.sf <-
    st_transform(country.sf, "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  
  box <- st_bbox(country.sf)
  nat.range.crop <- st_crop(nat.ranges, box)
  
  # get sites and buffer
  sites.dedup <- samp.sites %>% 
    group_by(longitude, latitude, locality_id) %>% tally()
  
  sites.buff <- sf::st_as_sf(sites.dedup, 
                              coords = c("longitude", "latitude"), 
                              crs="+proj=longlat +datum=WGS84 +no_defs") %>% 
    st_transform("+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") %>%
    st_buffer(buff.km*1000)
  
  
  # Get site and species range overlaps
  pts.sites.intersect <- st_intersects(sites.buff, nat.range.crop, sparse = T)
  
  pts_sites_df <- lapply(pts.sites.intersect,
                         FUN = function(x) {data.frame(IUCN_name = nat.range.crop$SCI_NAME[x],
                                                       PRESENCE = nat.range.crop$PRESENCE[x],
                                                       ORIGIN = nat.range.crop$ORIGIN[x],
                                                       SEASONAL = nat.range.crop$SEASONAL[x])}) %>% 
    bind_rows(.id = "id") %>% 
    mutate(id = as.numeric(id),
           pt = sites.buff$locality_id[id])
  
  return(pts_sites_df)
}

## Sampling points elevation ---------------------------------------------------

get.elevation <- function(samp.sites, elev.raster){
  
      # get sites
      sites.dedup <- samp.sites %>% 
        group_by(longitude, latitude, locality_id) %>% tally()
      
      sites.for.elev <- sf::st_as_sf(sites.dedup, 
                                      coords = c("longitude", "latitude"), 
                                      crs="+proj=longlat +datum=WGS84 +no_defs") %>% 
        st_transform(crs(elev.raster)) %>% as_spatvector()
      
      # extract site elevation
      sites.elev <- data.frame(elev = extract(elev.raster, sitesfor.elev, fun = "mean")$b1, 
                               locality_id = sites.for.elev$locality_id)
      
      return(sites.elev)
}