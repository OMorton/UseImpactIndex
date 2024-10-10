
library(tidyverse)
library(rredlist)
library(rvest)

# Get IUCN Threat Table data ---------------------------------------------------

apikey <- "a3fc116c122aefc621329055aeae8f67483e575c518acc14fcb77709bd94f6a2"

IUCN_all <- read.csv("Data/IUCN/Taxonomy_Sept2024.csv")
length(unique(IUCN_all$scientificName)) # 11195

# Faster alternate way of getting iucn data but doesnt debug as easily for nomenclature errors
# redlist_status <- do.call(rbind,lapply(spp,FUN=function(sp){rl_search(sp, key= MY_IUCN_REDLIST_KEY)$result}))

# Dummy data frame for the loops
df <- data.frame(scientificName = character(),
                 code = character(),
                 title = character(),
                 timing = character(),
                 scope = character(),
                 severity = character(),
                 score = character(),
                 invasive = character())

for(i in 1:nrow(IUCN_all)){ # would have used for(sp in speciesList) but need i for progress bar?
  ## incorporate 2s delay between each query
  Sys.sleep(2)
  ## Progress update
  cat('Species=',i, '\n')
  ## get historical data from website
  sp <- IUCN_all$scientificName[i]
  iucnthreat <- rl_threats(name=sp, key=apikey)
  # IF species cannot be found
  if (length(iucnthreat$result) == 0){ 
    spDf <- data.frame(scientificName = sp,
                       code = NA,
                       title = NA,
                       timing = NA,
                       scope = NA,
                       severity = NA,
                       score = NA,
                       invasive = NA)
    df <- rbind(df, spDf)
    # cat('Check ', sp, '\n')
  } else { 
    spdf <- data.frame(scientificName = sp,
                       code = iucnthreat$result$code,
                       title = iucnthreat$result$title,
                       timing = iucnthreat$result$timing,
                       scope = iucnthreat$result$scope,
                       severity = iucnthreat$result$severity,
                       score = iucnthreat$result$score,
                       invasive = iucnthreat$result$invasive)
    df <- rbind(df, spdf)
  }
}

#write.csv(df, "Data/IUCN/IUCN_all_ThreatAssessments_Sept2024.csv")

# Make threat score matrix -----------------------------------------------------


thr_raw <- read.csv("Data/IUCN/IUCN_all_ThreatAssessments_Sept2024.csv")

# calc the threat score, incorporating the unknown scope
thr_raw <- thr_raw %>% mutate(timing_sc = case_when(timing == "Ongoing" ~ 3,
                                                    timing == "Past, Unlikely to Return" ~ 0, 
                                                    timing == "Past, Likely to Return" ~ 1,
                                                    timing == "Future" ~ 1,
                                                    timing == "Unknown" ~ 2),
                              scope_sc = case_when(scope == "Minority (<50%)" ~ 1,
                                                   scope == "Majority (50-90%)" ~ 2,
                                                   scope == "Whole (>90%)" ~ 3,
                                                   scope == "Unknown" ~ 2,
                                                   is.na(scope) & !is.na(code) ~2),
                              severity_sc = case_when(severity == "Very Rapid Declines" ~ 3,
                                                      severity == "Rapid Declines" ~ 2,
                                                      severity == "Slow, Significant Declines" ~ 1,
                                                      severity == "Causing/Could cause fluctuations" ~ 1,
                                                      severity == "Negligible declines" ~ 0,
                                                      severity == "No decline" ~ 0,
                                                      severity == "Unknown" ~ 2,
                                                      is.na(severity) & !is.na(code) ~ 2),
                              score_upd = timing_sc + scope_sc + severity_sc,
                              code_coarse = substring(code, 1, 2),
                              code_coarse = gsub("\\.","", code_coarse),
                              code_coarse = case_when(code %in% c("5.1", "5.1.1", "5.1.2", "5.1.3", "5.1.4") ~ "5.1",
                                                      code %in% c("5.2", "5.2.1", "5.2.2", "5.2.3", "5.2.4",
                                                                  "5.3", "5.3.1", "5.3.2", "5.3.3", "5.3.4",
                                                                  "5.3.5") ~ "5.2-5.3",
                                                      code %in% c("5.4", "5.4.1", "5.4.2", "5.4.3", "5.4.4",
                                                                  "5.4.5", "5.4.6") ~ "5.4",
                                                      .default = code_coarse))


# extract the highest threat score per broad category of threat
thr_sum <- thr_raw %>% group_by(scientificName, code_coarse) %>%
  summarise(score_upd = max(score_upd))

backbone <- expand.grid(scientificName = unique(thr_sum$scientificName),
                        code_coarse = c("1", "2", "3", "4","5.1", "5.2-5.3", "5.4",
                                        "6", "7", "8", "9", "10", "11", "12"))

# ref data of the IUCN threat classification 3.1
# https://www.iucnredlist.org/resources/threat-classification-scheme
thr_class <- data.frame(code_coarse = c("1", "2", "3", "4","5.1", "5.2-5.3", "5.4",
                                        "6", "7", "8", "9", "10", "11", "12"), 
                        name = c("residential_commercial_development",
                                 "agriculture_aquaculture",
                                 "energy_mining",
                                 "transportation_service_corridors",
                                 "biological_resource_use_hunting",
                                 "biological_resource_use_loggingplants",
                                 "biological_resource_use_fishing",
                                 "human_intrusions_disturbance",
                                 "natural_system_modifications",
                                 "invasives_diseases",
                                 "pollution",
                                 "geological_events",
                                 "climate_change",
                                 "other"))

# expanded version of the data frame with the absence of each threat  zero filled
# for each species where it is not given.
thr_exp <- left_join(backbone, thr_sum) %>% 
  left_join(thr_class) %>%
  mutate(score_upd = ifelse(is.na(score_upd), 0, score_upd))

# wide data version
thr_wide <- thr_exp %>% select(-code_coarse) %>% 
  pivot_wider(id_cols = scientificName, names_from = "name", values_from = "score_upd")

write.csv(thr_wide, "Data/IUCN/IUCN_wide_ThreatMatrix_Sept2024.csv")

df <- read.csv("Data/IUCN/IUCN_all_ThreatAssessments_Sept2024.csv")



# Get Birdlife ecology and use data --------------------------------------------

# Example 
web <- read_html("http://datazone.birdlife.org/species/factsheet/papuan-hornbill-rhyticeros-plicatus/details")
## extract table data 
tables <- html_elements(web, '.table') %>% 
  html_table()
## extract forest dep and habitat table
ForestDepTable <- tables[grep('Forest dependency', tables)]
HabTable <- tables[grep('Habitat', tables)][[1]] %>% slice(-n())
ElevTable <- tables[grep('Habitat', tables)][[1]] %>% slice_tail()
HistTable <- tables[grep('Category', tables)][[1]]
PurpTable <- tables[grep('Purpose', tables)][[1]] %>% janitor::clean_names()
EOOTable <- tables[grep('Extent of Occurrence', tables)][[1]]
EOOTable$Estimate[1]

# For all species
#BOTW <- read.csv("Data/Taxonomy/HBW/BOTW_Taxonomy_clean_Dec2023.csv")
#speciesList <- read.csv("Data/Taxonomy/HBW/BOTW_Taxonomy_clean_Dec2023.csv")

BOTW <- data.table::fread("Data/Taxonomy/HBW/BOTW_Taxonomy_v81.csv", encoding = "Latin-1") %>% 
  filter(!is.na(SISRecID)) %>%
  select(Order, Family, `Family name`, `Common name`, `Scientific name`, SISRecID,
         `2023 IUCN Red List category`) %>%
  rename("sn"=`Scientific name`, "cn"=`Common name`, "IUCN2023" = `2023 IUCN Red List category`) %>%
  mutate(cn = gsub("'", "", cn),
         cn = gsub('á', 'a', cn),
         cn = gsub('ç', 'c', cn),
         cn = gsub('ä', 'a', cn),
         cn = gsub('ü', 'u', cn),
         cn = gsub('ö','o', cn))


# function to scrape all species
scrape_birdlife <- function(speciesList){
  n = nrow(speciesList)
  ## add a new column to the current list for forest dependency
  df <- speciesList %>% 
    add_column(ForestDependency = NA,
               EOO = NA,
               MigrStatus = NA,
               Alt = NA,
               Alt_lim = NA)
  ## add df for habitat data
  df2 <- speciesList
  habstore <- data.frame()
  histstore <- data.frame()
  purpstore <- data.frame()
  
  print('Checking BirdLife Datazone:')
  ## run through the list
  for (i in 1:n){
    ## print a progress bar: https://stackoverflow.com/questions/26919787/r-text-progress-bar-in-for-loop
    #extra <- nchar('||100%')
    #width <- getOption('width')
    #step <- round(i / n * (width - extra))
    #text <- sprintf('|%s%s|% 3s%%', strrep('=', step),
    #               strrep(' ', width - step - extra), round(i / n * 100))
    #cat(text)
    #cat(if (i == n) '\n' else '\r') # '\014' clears the console
    
    ## incorporate 1s delay between each query
    Sys.sleep(1)
    
    ## apply trycatch to skip if run into error
    tryCatch({ # try to get data
      ## obtain the url
      sn <- speciesList$sn[i]
      cn <- speciesList$cn[i]
      
      ## print species name to check progress (alternative to the progress bar)
      cat(paste0(cn, ": ", i, " out of ", n, "\n"))
      urlname <- paste(c(strsplit(cn, ' ')[[1]], strsplit(sn, ' ')[[1]]), sep = '-', collapse = '-')
      theurl <- paste0('http://datazone.birdlife.org/species/factsheet/', urlname, '/details')
      # alternative way of getting url
      # bl_base <- "http://datazone.birdlife.org/species/factsheet"
      # bl_url <- function(x) sprintf("%s/%s/details", bl_base, x)
      # theurl <- bl_url(urlname)
      
      ## read html from website
      web <- read_html(theurl)
      
      tables <- NULL
      ## extract table data from html
      tables <- html_elements(web, '.table') %>% 
        html_table()
      
      ## find the table which lists forest dependency and hab
      fdTable <- tables[grep('Forest dependency', tables)]
      EOOTable <- tables[grep('Extent of Occurrence', tables)]
      
      HabTable <- tables[grep('(level 1)', tables)][[1]] %>% slice(-n()) %>% janitor::clean_names()
      HabTable$sn <- sn
      ElevTable <- tables[grep('(level 1)', tables)][[1]] %>% slice_tail()
      HistTable <- tables[grep('Category', tables)][[1]]
      HistTable$sn <- sn
      # not all sp have use and purpose recorded so need to handle those omissions
      PurpTable <- tryCatch({tables[grep('Primary form used', tables)][[1]] %>% 
        janitor::clean_names() %>% select(!c(primary_form_used,life_stage_used, source)) %>%
          mutate(sn = sn)},
      error = function(cond) {data.frame(purpose = NA, scale = NA, level = NA,
                                         timing = NA, sn = sn)})
      
      ## input forest dep, elevs, and migr status
      df$ForestDependency[i] <- fdTable[[1]]$X4[1]
      df$EOO[i] <- EOOTable[[1]]$Estimate[1]
      df$MigrStatus[i] <- fdTable[[1]]$X2[1]
      df$Alt[i] <- ElevTable[[1,2]]
      df$Alt_lim[i] <- ElevTable[[1,4]]
      
      habstore <- rbind(habstore, HabTable)
      purpstore <- rbind(purpstore, PurpTable)
      histstore <- rbind(histstore, HistTable)
      
    }, error=function(cond){ # optional error catching
      cat('Error at ', speciesList$Species[i], conditionMessage(cond), '\n')
    }, warning=function(cond){ # optional warning catching
      cat('Warning issued at ', speciesList$Species[i], conditionMessage(cond), '\n')
    })
  }
  return(list("FDep" = df, "Hab" = habstore, "Purp" = purpstore, "History" = histstore))
}

# test run
Out <- scrape_birdlife(BOTW[1:2,])
# full run
BL_all_scrape <- scrape_birdlife(BOTW)

write.csv(BL_all_scrape$FDep, "Data/IUCN/ForestDep_and_elev_Sept2024.csv")
write.csv(BL_all_scrape$Hab, "Data/IUCN/Habitatdetails_Sept2024.csv")
write.csv(BL_all_scrape$Purp, "Data/IUCN/UseandPurpose_Sept2024.csv")
write.csv(BL_all_scrape$History, "Data/IUCN/IUCNHistoricalAssessments_Sept2024.csv")

# Tidying elevation ------------------------------------------------------------
dep_elev <- read.csv("Data/IUCN/ForestDep_and_elev_Sept2024.csv")

elev_sh <- dep_elev %>% separate_wider_delim(Alt, delim = " - ", too_few = "align_start", 
                                  names = c("elev_min", "elev_max")) %>%
  mutate(elev_max = gsub(" m", "", elev_max),
         elev_max = as.numeric(elev_max),
         elev_min = as.numeric(elev_min)) %>%
  select(cn, sn, elev_min, elev_max)

write.csv(elev_sh, "Data/IUCN/tidy_elev_Sept2024.csv")

