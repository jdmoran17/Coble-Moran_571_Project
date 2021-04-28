### ACJM Final Project
## 3 Data Cleaning - Event Study
# Last Updated: 4-27-21

### Preliminaries
pacman::p_load(tidyverse,rvest,xml2,xfun,stringr,lubridate,countytimezones,
               usmap,fuzzyjoin,stringdist,readxl,plm)

# Read in long table of observations.
content_table <- readRDS("data/content_table.rds")

### Generate clean content measures.

# Generate scaled content measures.
content_table <- content_table %>% 
  mutate(nchars_text_scaled = scale(nchars_text)) %>% 
  mutate(n_links_scaled = scale(n_links)) %>% 
  mutate(nnodes_rich_scaled = scale(nnodes_rich)) %>% 
  mutate(nnodes_dynamic_scaled = scale(nnodes_dynamic)) %>% 
  mutate(nnodes_formatting_scaled = scale(nnodes_formatting)) %>% 
  mutate(nnodes_dynchart_scaled = scale(nnodes_dynchart))

# Perform PCA on content measures.
content_table_content <- content_table %>% 
  select(nchars_text_scaled,n_links_scaled,nnodes_rich_scaled,nnodes_dynamic_scaled,nnodes_formatting_scaled)

content_table_PCA <- prcomp(content_table_content,scale = FALSE, center = FALSE)

# Report loadings.
knitr::kable(content_table_PCA$rotation)

# Convert proportion variance explained into data frame so we can use ggplot2.
content_table_PCA_PVE <- as.data.frame(summary(content_table_PCA)$importance[2,])

# Rename variables, create PC number variable and convert to factor.
content_table_PCA_PVE$PCA_Name <- rownames(content_table_PCA_PVE)
content_table_PCA_PVE$PCA_Name <- factor(content_table_PCA_PVE$PCA_Name, levels = c("PC1","PC2","PC3","PC4","PC5"))

# Plot scree plot
content_table_PCA_PVE %>% 
  ggplot(aes(x = PCA_Name, y = summary(content_table_PCA)$importance[2, ])) +
  geom_bar(stat = "identity") +
  ggtitle("Scree Plot, Content PCA")+
  xlab("Principal Component") +
  ylab("Percent Variance Explained")

# Check the eigenvalues of PCs.
knitr::kable((content_table_PCA$sdev)^2) ###PC 1 and PC2 have eigenvalues in excess of 1; however, PC2 is only marginally higher than 1. We'll keep PC1 (also confirmed by scree plot).

# Merge PC scores back to content_table.
PC_scores_table <- as.data.frame(content_table_PCA$x)

PC_scores_table <- PC_scores_table %>% 
  select(PC1,PC2) %>%  ###keeping the first two PC's.
  mutate(ID = rownames(PC_scores_table))

content_table <- content_table %>% 
  mutate(ID = rownames(content_table)) %>% 
  left_join(PC_scores_table, by="ID") %>% 
  select(-ID)

# Merge IPEDS data to covid_data_sub.
IPEDS_data <- read.csv("data/IPEDS_data_full.csv",stringsAsFactors = FALSE)

# Clean IPEDS data.
IPEDS_data_clean <- IPEDS_data %>% 
  select(-Institution.Name,-Institution..entity..name..HD2019.,-Institution.name.alias..HD2019.,-Response.status.of.institution....Fall.enrollment..FLAGS2019.,-X) %>% 
  mutate(Multi.institution.or.multi.campus.organization..HD2019. = ifelse(Multi.institution.or.multi.campus.organization..HD2019. == 2,0,1)) %>% #change to indicator taking 1 if college is multi-campus.
  mutate(Level.of.institution..HD2019. = ifelse(Level.of.institution..HD2019. == 1,1,0)) %>% #change to indicator for 4-year school.
  mutate(Control.of.institution..HD2019. = ifelse(Control.of.institution..HD2019. != 1,1,0)) %>% #change to indicator for private school.
  mutate(Carnegie.Classification.2018..Basic..HD2019. = factor(Carnegie.Classification.2018..Basic..HD2019.)) %>% #change from integer to factor.
  mutate(Institution.size.category..HD2019. = factor(Institution.size.category..HD2019.)) %>% #change from integer to factor.
  mutate(Continuing.professional..IC2019. = ifelse(Continuing.professional..IC2019. == -2,0,Continuing.professional..IC2019.)) %>% 
  mutate(Religious.affiliation..IC2019. = ifelse(Religious.affiliation..IC2019. != -2,1,0)) %>% #change to indicator for religious school.
  rename("Multi_campus_institution" = "Multi.institution.or.multi.campus.organization..HD2019.",
         "Four_year_school" = "Level.of.institution..HD2019.",
         "Private_school" = "Control.of.institution..HD2019.",
         "Carnegie_classification" = "Carnegie.Classification.2018..Basic..HD2019.",
         "Enrollment_category" = "Institution.size.category..HD2019.",
         "Occupational_programs_offered" = "Occupational..IC2019.",
         "Academic_programs_offered" = "Academic..IC2019.",
         "Continuing_professional_programs_offered" = "Continuing.professional..IC2019.",
         "Recreational_avocational_programs_offered" = "Recreational.or.avocational..IC2019.",
         "GED_offered" = "Adult.basic.remedial.or.high.school.equivalent..IC2019.",
         "Secondary_programs_offered" = "Secondary..high.school...IC2019.",
         "Religious_school" = "Religious.affiliation..IC2019.",
         "Percent_admitted_2019" = "Percent.admitted...total..DRVADM2019.",
         "Adult_undergrad_enrollment_2019" = "Adult.age..25.64..enrollment..undergraduate..DRVEF2019.",
         "Adult_grad_enrollment_2019" = "Adult.age..25.64..enrollment..graduate..DRVEF2019.",
         "First_time_in_state_enrollment_2019" = "Number.of.first.time.undergraduates...in.state..DRVEF2019.",
         "First_time_out_of_state_enrollment_2019" = "Number.of.first.time.undergraduates...out.of.state..DRVEF2019.",
         "First_time_foreign_enrollment_2019" = "Number.of.first.time.undergraduates...foreign.countries..DRVEF2019.",
         "Total_enrollment_2019" = "Total..enrollment..DRVEF2019.",
         "FTE_total_enrollment_2019" = "Full.time.equivalent.fall.enrollment..DRVEF2019.",
         "Full_time_total_enrollment_2019" = "Full.time.enrollment..DRVEF2019.",
         "Part_time_total_enrollment_2019" = "Part.time.enrollment..DRVEF2019.",
         "Full_time_retention_rate_2019" = "Full.time.retention.rate..2019..EF2019D.",
         "Part_time_retention_rate_2019" = "Part.time.retention.rate..2019..EF2019D.",
         "Total_new_undergrads_2019" = "Total.entering.students.at.the.undergraduate.level..fall.2019..EF2019D.",
         "Student_faculty_ratio_2019" = "Student.to.faculty.ratio..EF2019D.",
         "Graduation_rate_2019" = "Graduation.rate..total.cohort..DRVGR2019.",
         "Percent_enrollment_awarded_aid_2019" = "Percent.of.full.time.first.time.undergraduates.awarded.any.financial.aid..SFA1819.")

IPEDS_data_clean <- IPEDS_data_clean %>% 
  filter(FIPS.state.code..HD2019. <= 56) %>% 
  mutate(FIPS.state.code..HD2019. = as.character(FIPS.state.code..HD2019.)) %>% #convert 2-digit FIPS to state code (next 3 lines)
  mutate(FIPS.state.code..HD2019. = ifelse(nchar(FIPS.state.code..HD2019.)==1,paste("0",FIPS.state.code..HD2019.,sep=""),FIPS.state.code..HD2019.)) %>% 
  rowwise() %>% 
  mutate(FIPS.state.code..HD2019. = fips_info(FIPS.state.code..HD2019.)) %>% 
  rename("State" = "FIPS.state.code..HD2019.") %>% 
  mutate("State_abbr" = State$abbr) %>% 
  select(-State)

# Output cleaned IPEDS data.
saveRDS(IPEDS_data_clean,file = "data/IPEDS_data_clean.RDS")

# Merge cleaned IPEDS data to school data and output.
school_data <- readRDS("data/School_List_Clean_w_IPEDS.rds")

school_data <- school_data %>% 
  left_join(IPEDS_data_clean,by = "UnitID") %>% 
  mutate(dashboard_indicator = ifelse(is.na(Dashboard_URL) == TRUE,0,1))

saveRDS(school_data,"data/School_List_Merged_w_IPEDS.rds")

### Content Measurement (in preparation for event study).

# Generate lagged change variable.
content_table <- content_table %>% 
  select(-PC2) %>% 
  group_by(UnitID) %>% 
  mutate(PC1 = -1*PC1) %>% ###to make interpretation easier, flip sign on PC1 (all loadings are negative).
  mutate(PC1_change = PC1 - dplyr::lag(PC1)) %>% 
  #mutate(PC2_change = PC2 - lag(PC2)) %>% 
  ungroup()

content_table_filtered <- content_table %>% 
  filter(local_date >= ymd("2020-06-01")) %>% #drop all observations before June 1, 2020
  filter(!is.na(PC1_change)) %>% #drop all na observations - these correspond to the first observation for a school, which we don't need for our analysis.
  
  # Generate indicators for above-n percentile change.
  mutate(PC1_change_pctle = percent_rank(PC1_change)) %>% 
  #mutate(PC2_change_pctle = percent_rank(PC2_change)) %>% 
  mutate(PC1_top50 = ifelse(PC1_change_pctle >= .5,1,0)) %>% 
  mutate(PC1_top33 = ifelse(PC1_change_pctle >= (2/3),1,0)) %>% 
  mutate(PC1_top25 = ifelse(PC1_change_pctle >= .75,1,0)) %>% 
  mutate(PC1_top20 = ifelse(PC1_change_pctle >= .8,1,0)) %>% 
  mutate(PC1_top10 = ifelse(PC1_change_pctle >= .9,1,0)) %>% 
  mutate(PC1_low50 = ifelse(PC1_change_pctle <= .5,1,0)) %>% 
  mutate(PC1_low33 = ifelse(PC1_change_pctle <= (1/3),1,0)) %>% 
  mutate(PC1_low25 = ifelse(PC1_change_pctle <= .25,1,0)) %>% 
  mutate(PC1_low20 = ifelse(PC1_change_pctle <= .2,1,0)) %>% 
  mutate(PC1_low10 = ifelse(PC1_change_pctle <= .1,1,0))

### Perform COVID data cleaning.

# Load COVID data.
covid_data <- read.csv("data/covid_rates.csv",stringsAsFactors = FALSE)

# Convert case rates to per 100,000 and log per 100,000.
covid_data <- covid_data %>% 
  mutate(daily_cases_per100k = cum_cases / (TotalPopEst2019 / 100000)) %>% 
  mutate(log_daily_cases_per100k = log(daily_cases_per100k))

# Merge case rates to content_analysis.
covid_data_sub <- covid_data %>% 
  select(FIPS,date,daily_cases_per100k,log_daily_cases_per100k) %>% 
  mutate(FIPS = as.character(FIPS)) %>% 
  mutate(FIPS = ifelse(nchar(FIPS)==4,paste("0",FIPS,sep=""),FIPS)) %>% 
  mutate(date = ymd(date))

# Generate future dates (for analysis purposes).
content_table_analysis <- content_table_filtered %>% 
  mutate(local_date_7 = local_date + 7) %>% 
  mutate(local_date_14 = local_date + 14)

content_table_analysis <- content_table_analysis %>% ###merge cases for observation date.
  left_join(covid_data_sub,by = c("FIPS" = "FIPS","local_date" = "date"))

content_table_analysis <- rename(content_table_analysis,"daily_cases_per100k_0" = "daily_cases_per100k")
content_table_analysis <- rename(content_table_analysis,"log_daily_cases_per100k_0" = "log_daily_cases_per100k")

content_table_analysis <- content_table_analysis %>% ###merge cases for 7 days after observation date.
  left_join(covid_data_sub,by = c("FIPS" = "FIPS","local_date_7" = "date"))

content_table_analysis <- rename(content_table_analysis,"daily_cases_per100k_7" = "daily_cases_per100k")
content_table_analysis <- rename(content_table_analysis,"log_daily_cases_per100k_7" = "log_daily_cases_per100k")

content_table_analysis <- content_table_analysis %>% ###merge cases for 14 days after observation date.
  left_join(covid_data_sub,by = c("FIPS" = "FIPS","local_date_14" = "date"))

content_table_analysis <- rename(content_table_analysis,"daily_cases_per100k_14" = "daily_cases_per100k")
content_table_analysis <- rename(content_table_analysis,"log_daily_cases_per100k_14" = "log_daily_cases_per100k")

# Generate measures of case rate change.
content_table_analysis <- content_table_analysis %>% 
  mutate(change_7 = daily_cases_per100k_7 - daily_cases_per100k_0,
         change_14 = daily_cases_per100k_14 - daily_cases_per100k_0,
         log_change_7 = log_daily_cases_per100k_7 - log_daily_cases_per100k_0,
         log_change_14 = log_daily_cases_per100k_14 - log_daily_cases_per100k_0)

saveRDS(content_table_analysis,"data/content_table_analysis.RDS")

### DIFFERENCE IN DIFFERENCES

# Load list of colleges.
school_data <- readRDS("data/School_List_Clean_w_IPEDS.RDS")

# Subset school_data.
school_data_sub <- school_data %>% 
  select(UnitID,College,PLAN,Start_Date,FIPS,Dashboard_URL) %>% 
  mutate(Dashboard_Indicator = ifelse(is.na(Dashboard_URL) == TRUE, 0,1)) %>% 
  select(-Dashboard_URL)

# Merge school_data to covid_data_sub.
covid_data_analysis <- covid_data_sub %>% 
  left_join(school_data_sub,by = "FIPS") %>% 
  filter(!is.na(UnitID)) %>% ###drop counties without colleges - for purposes of our analysis, these are unnecessary.
  filter(date >= ymd("2020-05-31")) %>% ###drop observations before May 31 for calculating lagged cases.
  filter(date <= ymd("2020-11-24")) %>% ###drop observations after Thanksgiving - we drop after this date since most schools ended sessions post-Thanksgiving.
  mutate(Start_Date = as.Date(Start_Date),
         post = ifelse(date >= Start_Date,1,0)) %>% 
  mutate(PLAN = factor(PLAN,levels = c("Hybrid","Primarily in person","Fully in person"))) %>% 
  mutate(FIPSID = paste(FIPS,UnitID,sep="")) %>% 
  group_by(FIPSID) %>% 
  mutate(daily_new_cases_per100k = daily_cases_per100k - dplyr::lag(daily_cases_per100k,order_by = FIPSID)) %>% 
  filter(date >= ymd("2020-06-01")) %>% ###drop observations before June 1 (start of "pre" period - we know no schools are in session at this point)
  mutate(log_daily_new_cases_per100k = log(1 + daily_new_cases_per100k)) %>% 
  ungroup()

# Merge IPEDS_data_clean to covid_data_analysis.
covid_data_analysis <- covid_data_analysis %>% 
  left_join(IPEDS_data_clean,by = "UnitID")

# Create average content table with indicator for above-median content measure.
average_content_table <- content_table %>% 
  select(UnitID,local_date,PC1,nnodes_dynchart) %>% 
  group_by(UnitID) %>% 
  summarise(median_PC1 = median(PC1),chart_indicator = mean(nnodes_dynchart)) %>% 
  ungroup() %>% 
  mutate(chart_indicator = ifelse(chart_indicator != 0,1,0)) %>% 
  mutate(pctle_PC1 = percent_rank(median_PC1)) %>% 
  mutate(median_PC1_indicator = ifelse(pctle_PC1 >= .5,1,0)) %>% 
  mutate(below_median_PC1_indicator = ifelse(pctle_PC1 <= .5,1,0)) %>% 
  mutate(median_PC1_indicator = ifelse(is.na(median_PC1_indicator) == TRUE,0,median_PC1_indicator)) %>% 
  mutate(below_median_PC1_indicator = ifelse(is.na(below_median_PC1_indicator) == TRUE,0,below_median_PC1_indicator)) %>% 
  mutate(chart_indicator = ifelse(is.na(chart_indicator) == TRUE,0,chart_indicator))

# Merge average content table to covid_data_analysis.
covid_data_analysis_DiD <- covid_data_analysis %>% 
  left_join(average_content_table, by = "UnitID") %>% 
  mutate(month=factor(month(date),levels = c("8","6","7","9","10","11"))) %>% 
  mutate(median_PC1_indicator = ifelse(is.na(median_PC1_indicator) == TRUE,0,median_PC1_indicator)) %>% 
  mutate(below_median_PC1_indicator = ifelse(is.na(below_median_PC1_indicator) == TRUE,0,below_median_PC1_indicator)) %>% 
  mutate(chart_indicator = ifelse(is.na(chart_indicator) == TRUE,0,chart_indicator))

# Save Difference in Differences table.
saveRDS(covid_data_analysis_DiD,"data/DiDtable.RDS")

### EVENT STUDY

# Prepare list of above-qtile change in dashboard content indicators.
content_table_analysis_changes <- content_table_analysis %>% 
  select(UnitID,local_date,PC1_top50,PC1_top33,PC1_top25,PC1_top20,PC1_top10,
         PC1_low50,PC1_low33,PC1_low25,PC1_low20,PC1_low10)

# Prepare daily covid data with above-average change in dashboard content indicators.
covid_data_analysis_ES <- covid_data_analysis %>% 
  left_join(content_table_analysis_changes,by = c("UnitID" = "UnitID","date" = "local_date")) %>% 
  mutate(PC1_top50 = ifelse(is.na(PC1_top50) == TRUE,0,PC1_top50), ###replace missing observations with 0
         PC1_top33 = ifelse(is.na(PC1_top33) == TRUE,0,PC1_top33),
         PC1_top25 = ifelse(is.na(PC1_top25) == TRUE,0,PC1_top25),
         PC1_top20 = ifelse(is.na(PC1_top20) == TRUE,0,PC1_top20),
         PC1_top10 = ifelse(is.na(PC1_top10) == TRUE,0,PC1_top10),
         PC1_low50 = ifelse(is.na(PC1_low50) == TRUE,0,PC1_low50),
         PC1_low33 = ifelse(is.na(PC1_low33) == TRUE,0,PC1_low33),
         PC1_low25 = ifelse(is.na(PC1_low25) == TRUE,0,PC1_low25),
         PC1_low20 = ifelse(is.na(PC1_low20) == TRUE,0,PC1_low20),
         PC1_low10 = ifelse(is.na(PC1_low10) == TRUE,0,PC1_low10))

# Add indicators for event windows (7 and 14 days after an above-qtile change in dashboard content indicators).
covid_data_analysis_ES <- covid_data_analysis_ES %>% 
  group_by(UnitID) %>% 
  mutate(PC1_top50_7 = ifelse(PC1_top50 == 1 |
                                dplyr::lag(PC1_top50, n=1) == 1 |
                                dplyr::lag(PC1_top50, n=2) == 1 |
                                dplyr::lag(PC1_top50, n=3) == 1 |
                                dplyr::lag(PC1_top50, n=4) == 1 |
                                dplyr::lag(PC1_top50, n=5) == 1 |
                                dplyr::lag(PC1_top50, n=6) == 1, 
                              1,0)) %>% 
  mutate(PC1_top33_7 = ifelse(PC1_top33 == 1 |
                                dplyr::lag(PC1_top33, n=1) == 1 |
                                dplyr::lag(PC1_top33, n=2) == 1 |
                                dplyr::lag(PC1_top33, n=3) == 1 |
                                dplyr::lag(PC1_top33, n=4) == 1 |
                                dplyr::lag(PC1_top33, n=5) == 1 |
                                dplyr::lag(PC1_top33, n=6) == 1, 
                              1,0)) %>% 
  mutate(PC1_top25_7 = ifelse(PC1_top25 == 1 |
                                dplyr::lag(PC1_top25, n=1) == 1 |
                                dplyr::lag(PC1_top25, n=2) == 1 |
                                dplyr::lag(PC1_top25, n=3) == 1 |
                                dplyr::lag(PC1_top25, n=4) == 1 |
                                dplyr::lag(PC1_top25, n=5) == 1 |
                                dplyr::lag(PC1_top25, n=6) == 1, 
                              1,0)) %>% 
  mutate(PC1_top20_7 = ifelse(PC1_top20 == 1 |
                                dplyr::lag(PC1_top20, n=1) == 1 |
                                dplyr::lag(PC1_top20, n=2) == 1 |
                                dplyr::lag(PC1_top20, n=3) == 1 |
                                dplyr::lag(PC1_top20, n=4) == 1 |
                                dplyr::lag(PC1_top20, n=5) == 1 |
                                dplyr::lag(PC1_top20, n=6) == 1, 
                              1,0)) %>% 
  mutate(PC1_top10_7 = ifelse(PC1_top10 == 1 |
                                dplyr::lag(PC1_top10, n=1) == 1 |
                                dplyr::lag(PC1_top10, n=2) == 1 |
                                dplyr::lag(PC1_top10, n=3) == 1 |
                                dplyr::lag(PC1_top10, n=4) == 1 |
                                dplyr::lag(PC1_top10, n=5) == 1 |
                                dplyr::lag(PC1_top10, n=6) == 1, 
                              1,0)) %>% 
  
  mutate(PC1_top50_14 = ifelse(PC1_top50 == 1 |
                                dplyr::lag(PC1_top50, n=1) == 1 |
                                dplyr::lag(PC1_top50, n=2) == 1 |
                                dplyr::lag(PC1_top50, n=3) == 1 |
                                dplyr::lag(PC1_top50, n=4) == 1 |
                                dplyr::lag(PC1_top50, n=5) == 1 |
                                dplyr::lag(PC1_top50, n=6) == 1 |
                                dplyr::lag(PC1_top50, n=7) == 1 | 
                                dplyr::lag(PC1_top50, n=8) == 1 |
                                dplyr::lag(PC1_top50, n=9) == 1 |
                                dplyr::lag(PC1_top50, n=10) == 1 |
                                dplyr::lag(PC1_top50, n=11) == 1 |
                                dplyr::lag(PC1_top50, n=12) == 1 |
                                dplyr::lag(PC1_top50, n=13) == 1, 
                              1,0)) %>% 
  mutate(PC1_top33_14 = ifelse(PC1_top33 == 1 |
                                 dplyr::lag(PC1_top33, n=1) == 1 |
                                 dplyr::lag(PC1_top33, n=2) == 1 |
                                 dplyr::lag(PC1_top33, n=3) == 1 |
                                 dplyr::lag(PC1_top33, n=4) == 1 |
                                 dplyr::lag(PC1_top33, n=5) == 1 |
                                 dplyr::lag(PC1_top33, n=6) == 1 |
                                 dplyr::lag(PC1_top33, n=7) == 1 | 
                                 dplyr::lag(PC1_top33, n=8) == 1 |
                                 dplyr::lag(PC1_top33, n=9) == 1 |
                                 dplyr::lag(PC1_top33, n=10) == 1 |
                                 dplyr::lag(PC1_top33, n=11) == 1 |
                                 dplyr::lag(PC1_top33, n=12) == 1 |
                                 dplyr::lag(PC1_top33, n=13) == 1, 
                               1,0)) %>% 
  mutate(PC1_top25_14 = ifelse(PC1_top25 == 1 |
                                 dplyr::lag(PC1_top25, n=1) == 1 |
                                 dplyr::lag(PC1_top25, n=2) == 1 |
                                 dplyr::lag(PC1_top25, n=3) == 1 |
                                 dplyr::lag(PC1_top25, n=4) == 1 |
                                 dplyr::lag(PC1_top25, n=5) == 1 |
                                 dplyr::lag(PC1_top25, n=6) == 1 |
                                 dplyr::lag(PC1_top25, n=7) == 1 | 
                                 dplyr::lag(PC1_top25, n=8) == 1 |
                                 dplyr::lag(PC1_top25, n=9) == 1 |
                                 dplyr::lag(PC1_top25, n=10) == 1 |
                                 dplyr::lag(PC1_top25, n=11) == 1 |
                                 dplyr::lag(PC1_top25, n=12) == 1 |
                                 dplyr::lag(PC1_top25, n=13) == 1, 
                               1,0)) %>% 
  mutate(PC1_top20_14 = ifelse(PC1_top20 == 1 |
                                 dplyr::lag(PC1_top20, n=1) == 1 |
                                 dplyr::lag(PC1_top20, n=2) == 1 |
                                 dplyr::lag(PC1_top20, n=3) == 1 |
                                 dplyr::lag(PC1_top20, n=4) == 1 |
                                 dplyr::lag(PC1_top20, n=5) == 1 |
                                 dplyr::lag(PC1_top20, n=6) == 1 |
                                 dplyr::lag(PC1_top20, n=7) == 1 | 
                                 dplyr::lag(PC1_top20, n=8) == 1 |
                                 dplyr::lag(PC1_top20, n=9) == 1 |
                                 dplyr::lag(PC1_top20, n=10) == 1 |
                                 dplyr::lag(PC1_top20, n=11) == 1 |
                                 dplyr::lag(PC1_top20, n=12) == 1 |
                                 dplyr::lag(PC1_top20, n=13) == 1, 
                               1,0)) %>% 
  mutate(PC1_top10_14 = ifelse(PC1_top10 == 1 |
                                 dplyr::lag(PC1_top10, n=1) == 1 |
                                 dplyr::lag(PC1_top10, n=2) == 1 |
                                 dplyr::lag(PC1_top10, n=3) == 1 |
                                 dplyr::lag(PC1_top10, n=4) == 1 |
                                 dplyr::lag(PC1_top10, n=5) == 1 |
                                 dplyr::lag(PC1_top10, n=6) == 1 |
                                 dplyr::lag(PC1_top10, n=7) == 1 | 
                                 dplyr::lag(PC1_top10, n=8) == 1 |
                                 dplyr::lag(PC1_top10, n=9) == 1 |
                                 dplyr::lag(PC1_top10, n=10) == 1 |
                                 dplyr::lag(PC1_top10, n=11) == 1 |
                                 dplyr::lag(PC1_top10, n=12) == 1 |
                                 dplyr::lag(PC1_top10, n=13) == 1, 
                               1,0)) %>% 
  mutate(PC1_low50_7 = ifelse(PC1_low50 == 1 |
                                dplyr::lag(PC1_low50, n=1) == 1 |
                                dplyr::lag(PC1_low50, n=2) == 1 |
                                dplyr::lag(PC1_low50, n=3) == 1 |
                                dplyr::lag(PC1_low50, n=4) == 1 |
                                dplyr::lag(PC1_low50, n=5) == 1 |
                                dplyr::lag(PC1_low50, n=6) == 1, 
                              1,0)) %>% 
  mutate(PC1_low33_7 = ifelse(PC1_low33 == 1 |
                                dplyr::lag(PC1_low33, n=1) == 1 |
                                dplyr::lag(PC1_low33, n=2) == 1 |
                                dplyr::lag(PC1_low33, n=3) == 1 |
                                dplyr::lag(PC1_low33, n=4) == 1 |
                                dplyr::lag(PC1_low33, n=5) == 1 |
                                dplyr::lag(PC1_low33, n=6) == 1, 
                              1,0)) %>% 
  mutate(PC1_low25_7 = ifelse(PC1_low25 == 1 |
                                dplyr::lag(PC1_low25, n=1) == 1 |
                                dplyr::lag(PC1_low25, n=2) == 1 |
                                dplyr::lag(PC1_low25, n=3) == 1 |
                                dplyr::lag(PC1_low25, n=4) == 1 |
                                dplyr::lag(PC1_low25, n=5) == 1 |
                                dplyr::lag(PC1_low25, n=6) == 1, 
                              1,0)) %>% 
  mutate(PC1_low20_7 = ifelse(PC1_low20 == 1 |
                                dplyr::lag(PC1_low20, n=1) == 1 |
                                dplyr::lag(PC1_low20, n=2) == 1 |
                                dplyr::lag(PC1_low20, n=3) == 1 |
                                dplyr::lag(PC1_low20, n=4) == 1 |
                                dplyr::lag(PC1_low20, n=5) == 1 |
                                dplyr::lag(PC1_low20, n=6) == 1, 
                              1,0)) %>% 
  mutate(PC1_low10_7 = ifelse(PC1_low10 == 1 |
                                dplyr::lag(PC1_low10, n=1) == 1 |
                                dplyr::lag(PC1_low10, n=2) == 1 |
                                dplyr::lag(PC1_low10, n=3) == 1 |
                                dplyr::lag(PC1_low10, n=4) == 1 |
                                dplyr::lag(PC1_low10, n=5) == 1 |
                                dplyr::lag(PC1_low10, n=6) == 1, 
                              1,0)) %>% 
  
  mutate(PC1_low50_14 = ifelse(PC1_low50 == 1 |
                                 dplyr::lag(PC1_low50, n=1) == 1 |
                                 dplyr::lag(PC1_low50, n=2) == 1 |
                                 dplyr::lag(PC1_low50, n=3) == 1 |
                                 dplyr::lag(PC1_low50, n=4) == 1 |
                                 dplyr::lag(PC1_low50, n=5) == 1 |
                                 dplyr::lag(PC1_low50, n=6) == 1 |
                                 dplyr::lag(PC1_low50, n=7) == 1 | 
                                 dplyr::lag(PC1_low50, n=8) == 1 |
                                 dplyr::lag(PC1_low50, n=9) == 1 |
                                 dplyr::lag(PC1_low50, n=10) == 1 |
                                 dplyr::lag(PC1_low50, n=11) == 1 |
                                 dplyr::lag(PC1_low50, n=12) == 1 |
                                 dplyr::lag(PC1_low50, n=13) == 1, 
                               1,0)) %>% 
  mutate(PC1_low33_14 = ifelse(PC1_low33 == 1 |
                                 dplyr::lag(PC1_low33, n=1) == 1 |
                                 dplyr::lag(PC1_low33, n=2) == 1 |
                                 dplyr::lag(PC1_low33, n=3) == 1 |
                                 dplyr::lag(PC1_low33, n=4) == 1 |
                                 dplyr::lag(PC1_low33, n=5) == 1 |
                                 dplyr::lag(PC1_low33, n=6) == 1 |
                                 dplyr::lag(PC1_low33, n=7) == 1 | 
                                 dplyr::lag(PC1_low33, n=8) == 1 |
                                 dplyr::lag(PC1_low33, n=9) == 1 |
                                 dplyr::lag(PC1_low33, n=10) == 1 |
                                 dplyr::lag(PC1_low33, n=11) == 1 |
                                 dplyr::lag(PC1_low33, n=12) == 1 |
                                 dplyr::lag(PC1_low33, n=13) == 1, 
                               1,0)) %>% 
  mutate(PC1_low25_14 = ifelse(PC1_low25 == 1 |
                                 dplyr::lag(PC1_low25, n=1) == 1 |
                                 dplyr::lag(PC1_low25, n=2) == 1 |
                                 dplyr::lag(PC1_low25, n=3) == 1 |
                                 dplyr::lag(PC1_low25, n=4) == 1 |
                                 dplyr::lag(PC1_low25, n=5) == 1 |
                                 dplyr::lag(PC1_low25, n=6) == 1 |
                                 dplyr::lag(PC1_low25, n=7) == 1 | 
                                 dplyr::lag(PC1_low25, n=8) == 1 |
                                 dplyr::lag(PC1_low25, n=9) == 1 |
                                 dplyr::lag(PC1_low25, n=10) == 1 |
                                 dplyr::lag(PC1_low25, n=11) == 1 |
                                 dplyr::lag(PC1_low25, n=12) == 1 |
                                 dplyr::lag(PC1_low25, n=13) == 1, 
                               1,0)) %>% 
  mutate(PC1_low20_14 = ifelse(PC1_low20 == 1 |
                                 dplyr::lag(PC1_low20, n=1) == 1 |
                                 dplyr::lag(PC1_low20, n=2) == 1 |
                                 dplyr::lag(PC1_low20, n=3) == 1 |
                                 dplyr::lag(PC1_low20, n=4) == 1 |
                                 dplyr::lag(PC1_low20, n=5) == 1 |
                                 dplyr::lag(PC1_low20, n=6) == 1 |
                                 dplyr::lag(PC1_low20, n=7) == 1 | 
                                 dplyr::lag(PC1_low20, n=8) == 1 |
                                 dplyr::lag(PC1_low20, n=9) == 1 |
                                 dplyr::lag(PC1_low20, n=10) == 1 |
                                 dplyr::lag(PC1_low20, n=11) == 1 |
                                 dplyr::lag(PC1_low20, n=12) == 1 |
                                 dplyr::lag(PC1_low20, n=13) == 1, 
                               1,0)) %>% 
  mutate(PC1_low10_14 = ifelse(PC1_low10 == 1 |
                                 dplyr::lag(PC1_low10, n=1) == 1 |
                                 dplyr::lag(PC1_low10, n=2) == 1 |
                                 dplyr::lag(PC1_low10, n=3) == 1 |
                                 dplyr::lag(PC1_low10, n=4) == 1 |
                                 dplyr::lag(PC1_low10, n=5) == 1 |
                                 dplyr::lag(PC1_low10, n=6) == 1 |
                                 dplyr::lag(PC1_low10, n=7) == 1 | 
                                 dplyr::lag(PC1_low10, n=8) == 1 |
                                 dplyr::lag(PC1_low10, n=9) == 1 |
                                 dplyr::lag(PC1_low10, n=10) == 1 |
                                 dplyr::lag(PC1_low10, n=11) == 1 |
                                 dplyr::lag(PC1_low10, n=12) == 1 |
                                 dplyr::lag(PC1_low10, n=13) == 1, 
                               1,0)) %>% 
  ungroup()

# Generate lagged case rates variables.
covid_data_analysis_ES <- covid_data_analysis_ES %>% 
  group_by(UnitID) %>% 
  mutate(lag_log_new_cases_14 = (1/14)*(dplyr::lag(log_daily_new_cases_per100k, n=1)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=2)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=3)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=4)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=5)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=6)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=7)+
                                          dplyr::lag(log_daily_new_cases_per100k, n=8)+
                                          dplyr::lag(log_daily_new_cases_per100k, n=9)+
                                          dplyr::lag(log_daily_new_cases_per100k, n=10)+
                                          dplyr::lag(log_daily_new_cases_per100k, n=11)+
                                          dplyr::lag(log_daily_new_cases_per100k, n=12)+
                                          dplyr::lag(log_daily_new_cases_per100k, n=13)+
                                          dplyr::lag(log_daily_new_cases_per100k, n=14))) %>%
  mutate(lag_log_new_cases_7 = (1/7)*(dplyr::lag(log_daily_new_cases_per100k, n=1)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=2)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=3)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=4)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=5)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=6)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=7))) %>% 
  mutate(lag_log_new_cases_5 = (1/5)*(dplyr::lag(log_daily_new_cases_per100k, n=1)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=2)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=3)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=4)+
                                        dplyr::lag(log_daily_new_cases_per100k, n=5))) %>% 
  mutate(lag_log_new_cases_3 = (1/3)*(dplyr::lag(log_daily_new_cases_per100k, n=1)+
                                  dplyr::lag(log_daily_new_cases_per100k, n=2)+
                                  dplyr::lag(log_daily_new_cases_per100k, n=3))) %>% 
  mutate(lag_log_new_cases_1 = dplyr::lag(log_daily_new_cases_per100k, n=1)) %>% 
  ungroup() %>% 
  mutate(month = factor(month(date)))

# Save Event Study table.
saveRDS(covid_data_analysis_ES,"data/EStable.RDS")
