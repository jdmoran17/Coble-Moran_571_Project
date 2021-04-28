### ACJM Final Project
## 2 Create Content Table
# Last Updated: 4-20-21

### Preliminaries
pacman::p_load(tidyverse,rvest,xml2,xfun,stringr,lubridate,countytimezones,
               usmap,fuzzyjoin,stringdist,readxl)

### Create long table of observations.

# Generate list of content files to loop over.
content_filename <- list.files("data/content/")

# Iteratively load and append RDS files in content list.
for(i in 1:length(content_filename)){
  content_load <- readRDS(paste("data/content/",content_filename[i],sep=""))
  content_load[,9] <- content_filename[i]
  
  ifelse(i == 1,
         content_table <- content_load,
         content_table <- rbind(content_table,content_load)
         )
}

# Load school data.
school_data <- readRDS("data/School_List_Clean.rds")

# Clean table, merge with school data.
content_table <- content_table %>% 
  rename("content_filename" = "...9") %>% 
  left_join(school_data,content_table,by = "content_filename") %>% 
  select(College,local_date,PLAN,Start_Date,State,County,FIPS,tz,nchars_text,n_links,
         nnodes_rich,nnodes_dynamic,nnodes_formatting,nnodes_dynchart,link,Dashboard_URL,url_filename,content_filename)

### Create match to IPEDS UnitID (unique identifier for school per IPEDS).

# Load IPEDS match key.
IPEDS_match <- read.csv("data/IPEDS_Match_Key.csv",stringsAsFactors = FALSE)

IPEDS_match <- IPEDS_match %>% 
  select(UnitID,Institution.Name,Fips.County.code..HD2019.) %>% 
  rename("College" = "Institution.Name","FIPS" = "Fips.County.code..HD2019.") %>% 
  mutate(FIPS = as.character(FIPS)) %>% 
  mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),FIPS))

# Generate data frame of potential matches.
school_names <- school_data %>% 
  select(College,FIPS) %>% 
  left_join(IPEDS_match,by = "FIPS")

# Calculate string distance between IPEDS school name and school_names.
school_names <- school_names %>% 
  mutate(distance = stringdist(College.x,College.y))

# Sort by string distance within school and keep lowest-distance match.
school_names <- school_names %>% 
  arrange(College.x,distance) %>%
  group_by(College.x) %>% 
  slice_min(order_by = distance)

# Write out match table for manual review and correction (identifies 30 schools / 5% incorrectly).
write.table(school_names,"data/school_names.csv",sep=",")

# Read corrected match table back in and correct names.
school_names <- read_excel("data/school_names.xlsx")

school_names <- school_names %>% 
  select(College.x,FIPS,UnitID) %>% 
  rename("College" = "College.x") %>% 
  mutate(FIPS = as.character(FIPS)) %>% 
  mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),FIPS))

# Merge corrected IPEDS match key to content_table.
content_table <- content_table %>% 
  mutate(College = str_trim(College,side = c("right")))%>% 
  left_join(school_names,by = c("College","FIPS"))

# Merge corrected IPEDS match key to school_data.
school_data <- school_data %>% 
  mutate(College = str_trim(College,side = c("right")))%>% 
  left_join(school_names,by = c("College","FIPS"))

### Export long content_table and merged school_data.
saveRDS(content_table,file = "data/content_table.rds")
saveRDS(school_data,file = "data/School_List_Clean_w_IPEDS.rds")