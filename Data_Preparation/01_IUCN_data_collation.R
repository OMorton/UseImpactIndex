
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


# For all species
BOTW <- read.csv("Data/Taxonomy/HBW/BOTW_Taxonomy_clean_Dec2023.csv")
#speciesList <- read.csv("Data/Taxonomy/HBW/BOTW_Taxonomy_clean_Dec2023.csv")

# function to scrape all species
scrape_birdlife <- function(speciesList){
  n = nrow(speciesList)
  ## add a new column to the current list for forest dependency
  df <- speciesList %>% 
    add_column(ForestDependency = NA)
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
      cn <- gsub("'", "", cn)
      cn <- gsub('á', 'a', cn)
      cn <- gsub('ç', 'c', cn)
      cn <- gsub('ä', 'a', cn)
      cn <- gsub('ü', 'u', cn)
      cn <- gsub('ö','o', cn)
      
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
      
      ## extract table data from html
      tables <- html_elements(web, '.table') %>% 
        html_table()
      
      ## find the table which lists forest dependency and hab
      fdTable <- tables[grep('Forest dependency', tables)]
      HabTable <- tables[grep('Habitat', tables)][[1]] %>% slice(-n()) %>% janitor::clean_names()
      HabTable$sn <- sn
      ElevTable <- tables[grep('Habitat', tables)][[1]] %>% slice_tail()
      HistTable <- tables[grep('Category', tables)][[1]]
      HistTable$sn <- sn
      # not all sp have use and purpose recorded so need to handle those omissions
      PurpTable <- tryCatch({tables[grep('Purpose', tables)][[1]] %>% 
        janitor::clean_names() %>% select(!primary_form_used, !life_stage_used, !source)
      PurpTable$sn <- sn},
      error = function(cond) {data.frame(purpose = NA, scale = NA, level = NA,
                                         timing = NA, sn = sn)})
      
      ## input forest dep, elevs, and migr status
      df$ForestDependency[i] <- fdTable[[1]]$X4[1]
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
Out <- scrape_birdlife(BOTW[7,])
# full run
BL_all_scrape <- scrape_birdlife(BOTW)

