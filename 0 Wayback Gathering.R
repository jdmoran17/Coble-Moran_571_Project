### ACJM Final Project
## 0 Wayback Gathering
# Last Updated: 4-15-21

### Preliminaries
pacman::p_load(tidyverse,rvest,xml2,xfun,stringr,lubridate,countytimezones,usmap)

# Install/load package `wayback` from github.
if(!require("wayback")){
  remotes::install_github("hrbrmstr/wayback",build_vignettes = TRUE)
  library(tidyverse)
}

### Step 1: Read in URLs to scrape.

# Read in list of schools and URLs.
dashboard_url <- readxl::read_xlsx("data/School_Reopening_Status_Dashboard_Links.xlsx",sheet="In Person",na=".")

# Clean schools list.
dashboard_url_df <- dashboard_url %>% 
  select(`COLLEGECOUNTY, STATE`,`PLAN`,`Corrected URL`,`Start Date`) %>% 
  rename("Dashboard_URL" = "Corrected URL","Start_Date" = "Start Date","College" = "COLLEGECOUNTY, STATE") %>% 
  
  # regex Step 1: Separate state abbrev. from college name.
  mutate(State = substr(College,regexpr("[,]\\s[[:upper:]][[:upper:]]$",College)+2,nchar(College))) %>% 
  mutate(College = substr(College,1,regexpr("[,]\\s[[:upper:]][[:upper:]]$",College)-1)) %>% 
  
  # regex Step 2a: Separate county name from college name (regular).
  mutate(College = substr(College,1,nchar(College)-nchar("County")-1)) %>% 
  mutate(County = gsub("(^.+[[:space:]][[:upper:]][[:lower:]]+)([[:upper:]].+$)","\\2",College)) %>% 
  mutate(College = gsub("(^.+[[:space:]][[:upper:]][[:lower:]]+)([[:upper:]].+$)","\\1",College)) %>% 
  
  # regex Step 2b: Separate county name from college name (college name ends in (<state>)).
  mutate(County = ifelse(County == College, gsub("(^.+)(\\(.+\\))([[:upper:]].+$)","\\3",College),County)) %>% 
  mutate(College = gsub("(^.+)(\\(.+\\))","\\1",College)) %>% 

  # regex Step 2c: Separate county name from college name (college name has hyphen).
  mutate(County = ifelse(County == College, gsub("(^.+\\-[[:upper:]][[:lower:]]+)([[:upper:]].+$)","\\2",College),County)) %>% 
  mutate(College = gsub("(^.+\\-[[:upper:]][[:lower:]]+)([[:upper:]].+$)","\\1",College)) %>% 
  
  # regex Step 2d: Separate county name from college name (college name ends in (<state>) pt 2).
  mutate(County = ifelse(County == College, gsub("(^.+\\(.\\..)([[:upper:]].+$)","\\2",College),County)) %>% 
  mutate(College = gsub("(^.+)(\\(.\\..)([[:upper:]].+$)","\\1",College)) %>% 
  
  # regex Step 2e: Separate county name from college name (college name ends in (<state>) pt 3).
  mutate(County = ifelse(County == College, gsub("(^.+\\([[:upper:]][[:lower:]]+)([[:upper:]].+$)","\\2",College),County)) %>% 
  mutate(College = gsub("(^.+)(\\([[:upper:]][[:lower:]]+)([[:upper:]].+$)","\\1",College)) %>% 
  
  # regex Step 2f: Separate county name from college name (college name ends in (<state>) pt 4).
  mutate(County = ifelse(County == College, gsub("(^.+)(\\-[[:upper:]]\\.[[:upper:]])([[:upper:]].+$)","\\3",College),County)) %>% 
  mutate(College = gsub("(^.+)(\\-[[:upper:]]\\.[[:upper:]])([[:upper:]].+$)","\\1",College)) %>% 
  
  # regex Step 2g: Separate county name from college name (college name ends in (<state>) pt 5).
  mutate(County = ifelse(County == College, gsub("(^.+)(\\([[:upper:]])([[:upper:]].+$)","\\3",College),County)) %>% 
  mutate(College = gsub("(^.+)(\\([[:upper:]])([[:upper:]].+$)","\\1",College))

# Replace errors in counties.
dashboard_url_df$County[dashboard_url_df$County == "Virginia Bea"] <- "Virginia Beach City"
dashboard_url_df$County[dashboard_url_df$County == "Radfo"] <- "Radford City"
dashboard_url_df$County[dashboard_url_df$County == "Richmo"] <- "Richmond City"
dashboard_url_df$County[dashboard_url_df$County == "Lynchbu"] <- "Lynchburg City"
dashboard_url_df$County[dashboard_url_df$County == "Norfo"] <- "Norfolk City"
dashboard_url_df$County[dashboard_url_df$County == "Winchest"] <- "Winchester City"
dashboard_url_df$County[dashboard_url_df$County == "Fairbanks North Star "] <- "Fairbanks North Star"
dashboard_url_df$County[dashboard_url_df$County == "St. Lou"] <- "St. Louis"

# Get FIPS code for each county.
dashboard_url_df <- dashboard_url_df %>% 
  rowwise %>% 
  mutate(FIPS = ifelse(State != "LA", ifelse(State != "AK",fips(State,County),fips(State,paste(County,"Borough"))),fips(State,paste(County,"Parish"))))

# Create filenames for easy merging later.
dashboard_url_df <- dashboard_url_df %>% 
  mutate(url_filename = paste(gsub("\\W","_",gsub("\\s","_",College)),"_url.rds",sep = "")) %>% 
  mutate(content_filename = paste(gsub("\\W","_",gsub("\\s","_",College)),"_content.rds",sep = ""))

# Create time zone variable.
tzs <- county_tzs %>% 
  mutate(FIPS = as.character(fips)) %>% 
  mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),FIPS)) %>% 
  select(tz,FIPS)

# Merge time zone to school list.
dashboard_url_df <- dashboard_url_df %>% 
  left_join(tzs,dashboard_url_df,by = "FIPS")

# Output updated list of schools for future use.
saveRDS(dashboard_url_df,file = "data/School_List_Clean.rds")

### Step 2: Download associated URLs from Wayback Machine.

# Read in saved clean list of schools.
dashboard_url_df <- readRDS("data/School_List_Clean.rds")

# Remove https and replace with http, slash at end (`wayback` doesn't like https or slash at end).
dashboard_url_df <- dashboard_url_df %>% 
  mutate(Dashboard_URL = gsub("^https://","http://",Dashboard_URL)) %>% #remove https:// at beginning and replace with http://
  mutate(Dashboard_URL = str_replace(Dashboard_URL,"/$","")) #remove slash at end 

# Filter out schools with no dashboards
dashboard_url_df_search <- dashboard_url_df %>% 
  filter(!is.na(Dashboard_URL))

# Create list of schools with mementos available in urls folder.
url_filename <- list.files("data/urls/")
url_list <- as.data.frame(url_filename)

# Remove schools already scraped from the list of mementos.
dashboard_url_df_search <- dashboard_url_df_search %>% 
  anti_join(url_list,dashboard_url_df_search,by = "url_filename")

# Create repository to hold URLs for which `wayback` cannot find a memento.
no_memento_df <- readRDS("data/No_Mementos.rds")

# Remove schools identified not to have mementos.
dashboard_url_df_search <- dashboard_url_df_search %>% 
  anti_join(no_memento_df,dashboard_url_df_search,by = "Dashboard_URL")

# Download URLs and save results to RDS.
for(k in 1:nrow(dashboard_url_df_search)){

  # Check whether `wayback` cannot identify mementos. If true, store URL w/o mementos in df and skip to next iteration of loop.
  if(tryCatch(
    sum(dim(get_mementos(dashboard_url_df_search$Dashboard_URL[k]))),
    error = function(msg){
      return(0)
    }
    ) == 0){
    no_memento_df[nrow(no_memento_df)+1,1] <- dashboard_url_df_search$Dashboard_URL[k]
    next
  }
  
  # Create dynamic filename to save RDS. (Remove any spaces or special characters.)
  memento_filename <- paste("data/urls/",gsub("\\W","_",gsub("\\s","_",dashboard_url_df_search$College[k])),"_url.rds",sep = "")
  
  # Check if RDS for school already exists; if not, save down memento.
  if(file.exists(memento_filename) == FALSE){
  
    # First, save down list of urls on memento.
    memento_data <- get_mementos(dashboard_url_df_search$Dashboard_URL[k])

    # Download list of links in the Wayback page for url.
    memento_url <- get_timemap(memento_data$link[2])
  
    # Save results to RDS.
    saveRDS(memento_url,file = memento_filename)
  }

}

# Save no_memento_df.
saveRDS(no_memento_df, file = "data/No_Mementos.rds")