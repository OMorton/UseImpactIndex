library(tidyverse)
library(rredlist)


apikey <- "a3fc116c122aefc621329055aeae8f67483e575c518acc14fcb77709bd94f6a2"

IUCN_all <- read.csv("Data/IUCN/Taxonomy_Sept2024.csv")
length(unique(IUCN_all$scientificName)) # 11195

## Faster alternate way of getting iucn data but doesnt debug as easily for nomenclature errors
#redlist_status <- do.call(rbind,lapply(spp,FUN=function(sp){rl_search(sp, key= MY_IUCN_REDLIST_KEY)$result}))

## Dummy data frame for the loops
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
df <- read.csv("Data/IUCN/IUCN_all_ThreatAssessments_Sept2024.csv")
