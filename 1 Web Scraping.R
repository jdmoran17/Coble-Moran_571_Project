### ACJM Final Project
## 1 Web Scraping
# Last Updated: 4-13-21

### Preliminaries
pacman::p_load(tidyverse,rvest,xml2,xfun,stringr,lubridate,countytimezones,usmap)

# Install/load package `wayback` from github.
if(!require("wayback")){
  remotes::install_github("hrbrmstr/wayback",build_vignettes = TRUE)
  library(tidyverse)
}

### Data Preparation

# Read in saved clean df of schools.
dashboard_url_df <- readRDS("data/School_List_Clean.rds")

# Create list of schools with mementos available in urls folder.
url_filename <- list.files("data/urls/")
url_list <- as.data.frame(url_filename)

# Merge list of schools to list of mementos available.
url_list <- url_list %>% 
  left_join(dashboard_url_df,url_list,by = "url_filename")

# Create list of schools which were already scraped.
content_filename <- list.files("data/content/")
content_list <- as.data.frame(content_filename)

# Create list of schools which were already identified to have no mementos.
no_content_df <- readRDS("data/No_Content.rds")

# Create list of schools which were already identified to have only mementos past the cutoff date.
only_2021_content_df <- readRDS("data/only_2021_Content.rds")

# Remove schools identified above from the list of mementos.
url_list <- url_list %>% 
  anti_join(content_list,url_list,by = "content_filename") %>% 
  anti_join(no_content_df,url_list,by = "url_filename") %>% 
  anti_join(only_2021_content_df,url_list,by = "url_filename")

### Web Scraping

for(f in 1:nrow(url_list)){ #only run 10 at a time for sake of your computer :)
  
  if(tryCatch(
    sum(dim(readRDS(paste("data/urls/",url_list$url_filename[f],sep="")))),
    error = function(msg){
      return(0)
    }
  ) <= 2){
    no_content_df[nrow(no_content_df)+1,] = url_list$url_filename[f]
    next
  }
  
  # Read in saved RDS.
  memento_raw <- readRDS(paste("data/urls/",url_list$url_filename[f],sep=""))
  
  # Append filename as variable to memento_raw (for merging in school data).
  memento_raw <- memento_raw %>% 
    mutate(url_filename = url_list$url_filename[f])
  
  # Clean data.
  memento_clean <- memento_raw %>% 
    filter(rel == "memento" | rel == "first memento") %>% #filter out rows that don't correspond to a single crawl.
    select(link,datetime,url_filename) #select link, datetime, and filename variables.
  
  memento_clean$datetime <- dmy_hms(memento_clean$datetime) #convert datetime to POSIXct.
  
  # Merge in school data.
  memento_cleaner <- memento_clean %>% 
    left_join(url_list,memento_clean,by = "url_filename")
  
  # Convert datetime to local time zone (Wayback outputs in GMT).
  memento_cleaner$local_time <- with_tz(memento_cleaner$datetime,tzone = memento_cleaner$tz[1])
  memento_cleaner$local_date <- date(memento_cleaner$local_time)
  
  # Keep first observation on each date.
  memento_cleaner <- memento_cleaner %>% 
    arrange(local_time) %>% 
    distinct(local_date,.keep_all = TRUE) %>% 
    select(link,local_date) %>% 
    filter(local_date <= date("2020-12-31")) %>% #keep dates in 2020 only.
    filter(local_date >= date("2020-01-01")) %>% #keep dates before cutoff (DISCUSS)
    mutate(nchars_text = 0,n_links = 0,nnodes_rich = 0,nnodes_dynamic = 0,nnodes_formatting = 0,nnodes_dynchart = 0)
  
  # Run for loop over rows in memento_cleaner
  for(j in 1:nrow(memento_cleaner)){
    
    ### Step 3: Scrape HTML from Wayback URLs.
    ifelse(nrow(memento_cleaner !=0),
      html_data <- read_html(memento_cleaner$link[j]),
      break)
    
    ### Step 4: Clean HTML data.
    
    # If we take Bradford's approach we need 5 measures: 
    # characters (need text); 
    # hyperlinks (need list of hyperlinks); 
    # rich, dynamic, and formatting tags (need list of tags).
    
    # 4a. Output number of characters.
    text_loop_vector <- c("p","li","h1","h2","h3","h4","h5","h6","table") #create vector of text elements to loop over.
    
    for(i in text_loop_vector){
      if(i == text_loop_vector[1]){
        nchars_text <-0 #if this is the first iteration, set nchars_text to zero, else keep nchars_text
      }
      
      list_elements <- html_data %>% 
        html_elements(i) %>% #select node from text_loop_vector
        html_text() %>% #extract text from node
        iconv("UTF-8", "UTF-8",sub='x') %>% #replace any invalid characters
        trimws() #remove excess whitespace
      
      nchars_text <- nchars_text + sum(nchar(list_elements)) #add number of characters from node to total.
    }
    
    # 4b. Output hyperlinks.
    links_elements <- html_data %>% 
      html_elements("a") %>%  #look for hyperlinks labeled a.
      as_list() #convert to list for easy counting.
    
    n_links <-  as.double(length(links_elements))
    
    # 4c. Output rich tags.
    rich_loop_vector <- c("audio","source","track","video","img","map","area","canvas","picture","svg") #create vector of rich elements to loop over.
    
    for(i in rich_loop_vector){
      if(i == rich_loop_vector[1]){
        nnodes_rich <-0 #if this is the first iteration, set nnodes_rich to zero, else keep nnodes_rich
      }
      
      list_elements <- html_data %>% 
        html_elements(i) %>% #select node from rich_loop_vector
        as_list() #convert to list for easy counting.
      
      nnodes_rich <- nnodes_rich + length(list_elements) #add count of nodes to total.
    }
    
    # 4d. Output dynamic tags.
    dynamic_loop_vector <- c("script","noscript","embed","object") #create vector of dynamic elements to loop over.
    
    for(i in dynamic_loop_vector){
      if(i == dynamic_loop_vector[1]){
        nnodes_dynamic <-0 #if this is the first iteration, set nnodes_dynamic to zero, else keep nnodes_dynamic
      }
      
      list_elements <- html_data %>% 
        html_elements(i) %>%  #select node from dynamic_loop_vector
        as_list() #convert to list for easy counting.
      
      nnodes_dynamic <- nnodes_dynamic + length(list_elements) #add count of nodes to total.
    }
    
    # 4e. Output formatting tags.
    formatting_loop_vector <- c("div","br","p","span","li","ul","table","tr","td","hr","tbody",
                                "header","center","section","th","article","ol","dl","dt","dd",
                                "nobr","aside","thread","main","col","colgroup","address",
                                "time","spacer") #create vector of formatting elements to loop over.
    
    for(i in formatting_loop_vector){
      if(i == formatting_loop_vector[1]){
        nnodes_formatting <-0 #if this is the first iteration, set nnodes_formatting to zero, else keep nnodes_formatting
      }
      
      list_elements <- html_data %>% 
        html_elements(i) %>% #select node from formatting_loop_vector
        as_list() #convert to list for easy counting.
      
      nnodes_formatting <- nnodes_formatting + length(list_elements) #add count of nodes to total.
    }
    
    # 4f. Output indicator for dynamic chart.
    
    ### TABLEAU
    
    dynchart_loop_vector_div <- c("tableau.") #create vector of "div" classes which indicate a dynamic chart to loop over.
    
    for(i in dynchart_loop_vector_div){
      if(i == dynchart_loop_vector_div[1]){
        nnodes_dynchart <-0 #if this is the first iteration, set nnodes_dynchart to zero, else keep nnodes_dynchart
      }
      
      list_elements <- html_data %>% 
        html_elements("script") %>% #select "div" node as these are containers for dynamic charts.
        html_attr("src") 
      
      list_elements <- as.character(list_elements)
      
      nnodes_dynchart <- nnodes_dynchart + length(grep(i,list_elements,value=TRUE)) #add count of nodes to total.
    }
    
    ### HIGHCHARTS
    
    dynchart_loop_vector_svg <- c(".highcharts-figure","#highcharts_js-js") #create vector of highcharts indicators to loop over. These are mutually exclusive.
    
    for(i in dynchart_loop_vector_svg){
      
      list_elements <- html_data %>% 
        html_elements(i)
      
      nnodes_dynchart <- nnodes_dynchart + length(list_elements) #add count of nodes to total.
    }
    
    ### JS CHARTS
    
    list_elements <- html_data %>% 
      html_elements("canvas") #select "canvas" node. "Canvas" nodes are used for real-time data plotting within JavaScript.
    
    nnodes_dynchart <- nnodes_dynchart + length(list_elements) #add count of nodes to total
    
    ### POWERBI, GOOGLE DATA STUDIO, GOOGLE DOCS, R SHINY
    
    dynchart_loop_vector_iframe <- c("app.powerbi.com","datastudio.google.com","shinyapps.io","docs.google.com") #create vector of hyperlinks which indicate a dynamic chart to loop over.
    
    for(i in dynchart_loop_vector_iframe){
      
      list_elements <- html_data %>% 
        html_nodes("iframe") %>% #select "iframe" node
        html_attr("src") #select "src" attribute from iframe node. This will contain a link to the dashboard.
      
      list_elements <- as.character(list_elements)
      
      nnodes_dynchart <- nnodes_dynchart + length(grep(i,list_elements,value=TRUE)) #add count of nodes to total.
    }
    
    # Write results to memento_cleaner.
    memento_cleaner[j,3] <- nchars_text
    memento_cleaner[j,4] <- n_links
    memento_cleaner[j,5] <- nnodes_rich
    memento_cleaner[j,6] <- nnodes_dynamic
    memento_cleaner[j,7] <- nnodes_formatting
    memento_cleaner[j,8] <- nnodes_dynchart
  }
  
  # Output results.
  if(nrow(memento_cleaner) !=0){
    saveRDS(memento_cleaner,file = paste("data/content/",url_list$content_filename[f],sep=""))}
  else{
    only_2021_content_df[nrow(only_2021_content_df)+1,] = url_list$url_filename[f]
  }
}

# Save no_content_df's.
saveRDS(no_content_df, file = "data/No_Content.rds")
saveRDS(only_2021_content_df, file = "data/only_2021_Content.rds")
