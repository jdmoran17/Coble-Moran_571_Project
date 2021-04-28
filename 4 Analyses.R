### ACJM Final Project
## 4 Data Cleaning
# Last Updated: 4-28-21

### Preliminaries
pacman::p_load(tidyverse,rvest,xml2,xfun,stringr,lubridate,countytimezones,
               usmap,fuzzyjoin,stringdist,readxl,plm,glmnet)

### CLASSIFICATION TEST (EXPLORATORY ANALYSIS FOR FACTORS INFLUENCING DASHBOARD ADOPTION)

# Read prepared list of schools.
school_data <- readRDS("data/School_List_Merged_w_IPEDS.RDS")

# Remove identifiers from list of schools.
school_data_sub <- school_data %>% 
  select(-College,-Dashboard_URL,-url_filename,-content_filename,-UnitID,-County,-FIPS,-State_abbr) %>% ###removing County and FIPS as we'll force these in in the actual models.
  mutate(PLAN = factor(PLAN,levels = c("Hybrid","Primarily in person","Fully in person"))) ###convert PLAN to factor for model.matrix.

# Prepare list of schools for analysis.
school_data_sub <- school_data_sub %>% 
  select(-First_time_in_state_enrollment_2019,-First_time_out_of_state_enrollment_2019,
         -First_time_foreign_enrollment_2019,-Percent_admitted_2019,-Part_time_retention_rate_2019) %>% ###remove variables with high proportion of NAs.
  filter(across(.cols = everything(),.fns = ~ !is.na(.x))) ###remove any observations with NAs. 
###Note this is an exploratory analysis and won't be used for inference; no defining characteristics of the 38 schools with NA values (mix of large & small).
  
# LASSO for classification: Prepare data set (create matrices for explanatory variables and outcome variables).
X <- model.matrix(dashboard_indicator~.,school_data_sub)[,-1]
Y <- as.matrix(school_data_sub[,ncol(school_data_sub)])

# LASSO for classification: Use glmnet to fit a sparse model of determinants of dashboard adoption.
set.seed(10)
control_model_cv <- cv.glmnet(X, Y, alpha=1, family="binomial", nfolds = 10, type.measure = "deviance")
plot(control_model_cv)

# Store coefficients for lambda.min model (this reduces the model to 10 parameters not subsumed by FIPS, which is appropriately sparse).
coef.min <- coef(control_model_cv, s="lambda.min")
coef.min <- coef.min[which(coef.min !=0),]
coef.min

# Run relaxed LASSO on coefficients identified.
control_model_relaxed <- glm(dashboard_indicator ~ PLAN + Four_year_school + Private_school + Academic_programs_offered + 
                               Secondary_programs_offered + Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019,
                             school_data,family = binomial("logit"),maxit = 100) ###exclued Carnegie classification as glm does not converge w/ it included.

##### THEREFORE, we will keep the following controls: 
#####      PLAN, Four_year_school, Private_school, Carnegie_classification, Academic_programs_offered, Secondary_programs_offered, Religious_school, 
#####      Total_enrollment_2019, Full_time_retention_rate_2019, Graduation_rate_2019

### DIFFERENCE IN DIFFERENCES

# Read prepared difference-in-differences data file.
covid_data_analysis_DiD <- readRDS("data/DiDtable.rds")

# Create indicator for FIPS-month observation.
covid_data_analysis_DiD <- covid_data_analysis_DiD %>% 
  mutate(FIPS_month = paste(FIPS,month,sep="_"))

reg3 <- plm(log_daily_new_cases_per100k ~ post + Dashboard_Indicator + post*Dashboard_Indicator + 
              PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
              Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019,
            data = covid_data_analysis_DiD,
            index = c("FIPS"),
            model = "within")

reg3a <- plm(log_daily_new_cases_per100k ~ post + Dashboard_Indicator + post*Dashboard_Indicator + 
              PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
              Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019,
            data = covid_data_analysis_DiD,
            index = c("FIPS_month"),
            model = "within")

reg4 <- plm(log_daily_new_cases_per100k ~ post + median_PC1_indicator + post*median_PC1_indicator + 
              PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
              Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019,
            data = covid_data_analysis_DiD,
            index = c("FIPS"),
            model = "within")

reg4a <- plm(log_daily_new_cases_per100k ~ post + median_PC1_indicator + post*median_PC1_indicator + 
              PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
              Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019,
            data = covid_data_analysis_DiD,
            index = c("FIPS_month"),
            model = "within")

reg4b <- plm(log_daily_new_cases_per100k ~ post + below_median_PC1_indicator + post*below_median_PC1_indicator + 
              PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
              Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019,
            data = covid_data_analysis_DiD,
            index = c("FIPS"),
            model = "within")

reg4c <- plm(log_daily_new_cases_per100k ~ post + below_median_PC1_indicator + post*below_median_PC1_indicator + 
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019,
             data = covid_data_analysis_DiD,
             index = c("FIPS_month"),
             model = "within")

reg4d <- plm(log_daily_new_cases_per100k ~ post + chart_indicator + post*chart_indicator + 
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019,
             data = covid_data_analysis_DiD,
             index = c("FIPS"),
             model = "within")

reg4e <- plm(log_daily_new_cases_per100k ~ post + chart_indicator + post*chart_indicator + 
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019,
             data = covid_data_analysis_DiD,
             index = c("FIPS_month"),
             model = "within")

parallel_trends_reg <- plm(log_daily_new_cases_per100k ~ Dashboard_Indicator + month + Dashboard_Indicator*month + 
                             PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
                             Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019,
                           data = covid_data_analysis_DiD,
                           index = c("FIPS"),
                           model = "within")

### EVENT STUDY

# Read prepared event study data file.
covid_data_analysis_ES <- readRDS("data/EStable.rds")

reg5 <- plm(log_daily_new_cases_per100k ~ PC1_top50_7 + lag_log_new_cases_3 +
              PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
              Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
            data = covid_data_analysis_ES,
            index = c("FIPS"),
            model = "within")

reg5a <- plm(log_daily_new_cases_per100k ~ PC1_top33_7 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg5b <- plm(log_daily_new_cases_per100k ~ PC1_top25_7 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg5c <- plm(log_daily_new_cases_per100k ~ PC1_top20_7 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg5d <- plm(log_daily_new_cases_per100k ~ PC1_top10_7 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg6 <- plm(log_daily_new_cases_per100k ~ PC1_top50_14 + lag_log_new_cases_3 +
              PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
              Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
            data = covid_data_analysis_ES,
            index = c("FIPS"),
            model = "within")

reg6a <- plm(log_daily_new_cases_per100k ~ PC1_top33_14 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg6b <- plm(log_daily_new_cases_per100k ~ PC1_top25_14 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg6c <- plm(log_daily_new_cases_per100k ~ PC1_top20_14 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg6d <- plm(log_daily_new_cases_per100k ~ PC1_top10_14 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg7 <- plm(log_daily_new_cases_per100k ~ PC1_low50_7 + lag_log_new_cases_3 +
              PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
              Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
            data = covid_data_analysis_ES,
            index = c("FIPS"),
            model = "within")

reg7a <- plm(log_daily_new_cases_per100k ~ PC1_low33_7 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg7b <- plm(log_daily_new_cases_per100k ~ PC1_low25_7 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg7c <- plm(log_daily_new_cases_per100k ~ PC1_low20_7 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg7d <- plm(log_daily_new_cases_per100k ~ PC1_low10_7 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg8 <- plm(log_daily_new_cases_per100k ~ PC1_low50_14 + lag_log_new_cases_3 +
              PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
              Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
            data = covid_data_analysis_ES,
            index = c("FIPS"),
            model = "within")

reg8a <- plm(log_daily_new_cases_per100k ~ PC1_low33_14 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg8b <- plm(log_daily_new_cases_per100k ~ PC1_low25_14 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg8c <- plm(log_daily_new_cases_per100k ~ PC1_low20_14 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

reg8d <- plm(log_daily_new_cases_per100k ~ PC1_low10_14 + lag_log_new_cases_3 +
               PLAN + Four_year_school + Private_school + Carnegie_classification + Academic_programs_offered + Secondary_programs_offered + 
               Religious_school + Total_enrollment_2019 + Full_time_retention_rate_2019 + Graduation_rate_2019 + month,
             data = covid_data_analysis_ES,
             index = c("FIPS"),
             model = "within")

### MODEL DIAGNOSES

## Q-Q Plots

# Reg3
qqnorm(reg3$residuals)
qqline(reg3$residuals)

# Reg3a
qqnorm(reg3a$residuals)
qqline(reg3a$residuals)

# Reg4
qqnorm(reg4$residuals)
qqline(reg4$residuals)

# Reg4a
qqnorm(reg4a$residuals)
qqline(reg4a$residuals)

# Reg4b
qqnorm(reg4b$residuals)
qqline(reg4b$residuals)

# Reg4c
qqnorm(reg4c$residuals)
qqline(reg4c$residuals)

# Reg4d
qqnorm(reg4d$residuals)
qqline(reg4d$residuals)

# Reg4e
qqnorm(reg4e$residuals)
qqline(reg4e$residuals)

# Reg5
qqnorm(reg5$residuals)
qqline(reg5$residuals)

# Reg5a
qqnorm(reg5a$residuals)
qqline(reg5a$residuals)

# Reg5b
qqnorm(reg5b$residuals)
qqline(reg5b$residuals)

# Reg5c
qqnorm(reg5c$residuals)
qqline(reg5c$residuals)

# Reg5d
qqnorm(reg5d$residuals)
qqline(reg5d$residuals)

# Reg6
qqnorm(reg6$residuals)
qqline(reg6$residuals)

# Reg6a
qqnorm(reg6a$residuals)
qqline(reg6a$residuals)

# Reg6b
qqnorm(reg6b$residuals)
qqline(reg6b$residuals)

# Reg6c
qqnorm(reg6c$residuals)
qqline(reg6c$residuals)

# Reg6d
qqnorm(reg6d$residuals)
qqline(reg6d$residuals)

# reg7
qqnorm(reg7$residuals)
qqline(reg7$residuals)

# reg7a
qqnorm(reg7a$residuals)
qqline(reg7a$residuals)

# reg7b
qqnorm(reg7b$residuals)
qqline(reg7b$residuals)

# reg7c
qqnorm(reg7c$residuals)
qqline(reg7c$residuals)

# reg7d
qqnorm(reg7d$residuals)
qqline(reg7d$residuals)

# reg8
qqnorm(reg8$residuals)
qqline(reg8$residuals)

# reg8a
qqnorm(reg8a$residuals)
qqline(reg8a$residuals)

# reg8b
qqnorm(reg8b$residuals)
qqline(reg8b$residuals)

# reg8c
qqnorm(reg8c$residuals)
qqline(reg8c$residuals)

# reg8d
qqnorm(reg8d$residuals)
qqline(reg8d$residuals)

## Residuals vs Fitted Plots

# Reg3
reg3_fitteds <- data.frame(predict(reg3),attr(residuals(reg3), "index"))
reg3_residuals <- data.frame(residuals(reg3),attr(residuals(reg3),"index"))

reg3_fitted_residual <- reg3_fitteds %>% 
  left_join(reg3_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg3_fitted_residual %>% 
  ggplot(aes(x = predict.reg3.,y = residuals.reg3.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg3")

# Reg3a
reg3a_fitteds <- data.frame(predict(reg3a),attr(residuals(reg3a), "index"))
reg3a_residuals <- data.frame(residuals(reg3a),attr(residuals(reg3a),"index"))

reg3a_fitted_residual <- reg3a_fitteds %>% 
  left_join(reg3a_residuals,by = c("FIPS_month" = "FIPS_month","time" = "time"))

reg3a_fitted_residual %>% 
  ggplot(aes(x = predict.reg3a.,y = residuals.reg3a.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg3a")

# Reg4
reg4_fitteds <- data.frame(predict(reg4),attr(residuals(reg4), "index"))
reg4_residuals <- data.frame(residuals(reg4),attr(residuals(reg4),"index"))

reg4_fitted_residual <- reg4_fitteds %>% 
  left_join(reg4_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg4_fitted_residual %>% 
  ggplot(aes(x = predict.reg4.,y = residuals.reg4.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg4")

# Reg4a
reg4a_fitteds <- data.frame(predict(reg4a),attr(residuals(reg4a), "index"))
reg4a_residuals <- data.frame(residuals(reg4a),attr(residuals(reg4a),"index"))

reg4a_fitted_residual <- reg4a_fitteds %>% 
  left_join(reg4a_residuals,by = c("FIPS_month" = "FIPS_month","time" = "time"))

reg4a_fitted_residual %>% 
  ggplot(aes(x = predict.reg4a.,y = residuals.reg4a.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg4a")

# Reg4b
reg4b_fitteds <- data.frame(predict(reg4b),attr(residuals(reg4b), "index"))
reg4b_residuals <- data.frame(residuals(reg4b),attr(residuals(reg4b),"index"))

reg4b_fitted_residual <- reg4b_fitteds %>% 
  left_join(reg4b_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg4b_fitted_residual %>% 
  ggplot(aes(x = predict.reg4b.,y = residuals.reg4b.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg4b")

# Reg4c
reg4c_fitteds <- data.frame(predict(reg4c),attr(residuals(reg4c), "index"))
reg4c_residuals <- data.frame(residuals(reg4c),attr(residuals(reg4c),"index"))

reg4c_fitted_residual <- reg4c_fitteds %>% 
  left_join(reg4c_residuals,by = c("FIPS_month" = "FIPS_month","time" = "time"))

reg4c_fitted_residual %>% 
  ggplot(aes(x = predict.reg4c.,y = residuals.reg4c.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg4c")

# Reg4d
reg4d_fitteds <- data.frame(predict(reg4d),attr(residuals(reg4d), "index"))
reg4d_residuals <- data.frame(residuals(reg4d),attr(residuals(reg4d),"index"))

reg4d_fitted_residual <- reg4d_fitteds %>% 
  left_join(reg4d_residuals,by = c("FIP" = "FIPS","time" = "time"))

reg4d_fitted_residual %>% 
  ggplot(aes(x = predict.reg4d.,y = residuals.reg4d.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg4d")

# Reg4e
reg4e_fitteds <- data.frame(predict(reg4e),attr(residuals(reg4e), "index"))
reg4e_residuals <- data.frame(residuals(reg4e),attr(residuals(reg4e),"index"))

reg4e_fitted_residual <- reg4e_fitteds %>% 
  left_join(reg4e_residuals,by = c("FIPS_month" = "FIPS_month","time" = "time"))

reg4e_fitted_residual %>% 
  ggplot(aes(x = predict.reg4e.,y = residuals.reg4e.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg4e")

# Reg5
reg5_fitteds <- data.frame(predict(reg5),attr(residuals(reg5), "index"))
reg5_residuals <- data.frame(residuals(reg5),attr(residuals(reg5),"index"))

reg5_fitted_residual <- reg5_fitteds %>% 
  left_join(reg5_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg5_fitted_residual %>% 
  ggplot(aes(x = predict.reg5.,y = residuals.reg5.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg5")

# Reg5a
reg5a_fitteds <- data.frame(predict(reg5a),attr(residuals(reg5a), "index"))
reg5a_residuals <- data.frame(residuals(reg5a),attr(residuals(reg5a),"index"))

reg5a_fitted_residual <- reg5a_fitteds %>% 
  left_join(reg5a_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg5a_fitted_residual %>% 
  ggplot(aes(x = predict.reg5a.,y = residuals.reg5a.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg5a")

# Reg5b
reg5b_fitteds <- data.frame(predict(reg5b),attr(residuals(reg5b), "index"))
reg5b_residuals <- data.frame(residuals(reg5b),attr(residuals(reg5b),"index"))

reg5b_fitted_residual <- reg5b_fitteds %>% 
  left_join(reg5b_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg5b_fitted_residual %>% 
  ggplot(aes(x = predict.reg5b.,y = residuals.reg5b.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg5b")

# Reg5c
reg5c_fitteds <- data.frame(predict(reg5c),attr(residuals(reg5c), "index"))
reg5c_residuals <- data.frame(residuals(reg5c),attr(residuals(reg5c),"index"))

reg5c_fitted_residual <- reg5c_fitteds %>% 
  left_join(reg5c_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg5c_fitted_residual %>% 
  ggplot(aes(x = predict.reg5c.,y = residuals.reg5c.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg5c")

# Reg5d
reg5d_fitteds <- data.frame(predict(reg5d),attr(residuals(reg5d), "index"))
reg5d_residuals <- data.frame(residuals(reg5d),attr(residuals(reg5d),"index"))

reg5d_fitted_residual <- reg5d_fitteds %>% 
  left_join(reg5d_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg5d_fitted_residual %>% 
  ggplot(aes(x = predict.reg5d.,y = residuals.reg5d.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg5d")

# Reg6
reg6_fitteds <- data.frame(predict(reg6),attr(residuals(reg6), "index"))
reg6_residuals <- data.frame(residuals(reg6),attr(residuals(reg6),"index"))

reg6_fitted_residual <- reg6_fitteds %>% 
  left_join(reg6_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg6_fitted_residual %>% 
  ggplot(aes(x = predict.reg6.,y = residuals.reg6.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg6")

# Reg6a
reg6a_fitteds <- data.frame(predict(reg6a),attr(residuals(reg6a), "index"))
reg6a_residuals <- data.frame(residuals(reg6a),attr(residuals(reg6a),"index"))

reg6a_fitted_residual <- reg6a_fitteds %>% 
  left_join(reg6a_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg6a_fitted_residual %>% 
  ggplot(aes(x = predict.reg6a.,y = residuals.reg6a.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg6a")

# Reg6b
reg6b_fitteds <- data.frame(predict(reg6b),attr(residuals(reg6b), "index"))
reg6b_residuals <- data.frame(residuals(reg6b),attr(residuals(reg6b),"index"))

reg6b_fitted_residual <- reg6b_fitteds %>% 
  left_join(reg6b_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg6b_fitted_residual %>% 
  ggplot(aes(x = predict.reg6b.,y = residuals.reg6b.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg6b")

# Reg6c
reg6c_fitteds <- data.frame(predict(reg6c),attr(residuals(reg6c), "index"))
reg6c_residuals <- data.frame(residuals(reg6c),attr(residuals(reg6c),"index"))

reg6c_fitted_residual <- reg6c_fitteds %>% 
  left_join(reg6c_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg6c_fitted_residual %>% 
  ggplot(aes(x = predict.reg6c.,y = residuals.reg6c.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg6c")

# Reg6d
reg6d_fitteds <- data.frame(predict(reg6d),attr(residuals(reg6d), "index"))
reg6d_residuals <- data.frame(residuals(reg6d),attr(residuals(reg6d),"index"))

reg6d_fitted_residual <- reg6d_fitteds %>% 
  left_join(reg6d_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg6d_fitted_residual %>% 
  ggplot(aes(x = predict.reg6d.,y = residuals.reg6d.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg6d")

# Reg7
reg7_fitteds <- data.frame(predict(reg7),attr(residuals(reg7), "index"))
reg7_residuals <- data.frame(residuals(reg7),attr(residuals(reg7),"index"))

reg7_fitted_residual <- reg7_fitteds %>% 
  left_join(reg7_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg7_fitted_residual %>% 
  ggplot(aes(x = predict.reg7.,y = residuals.reg7.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg7")

# Reg7a
reg7a_fitteds <- data.frame(predict(reg7a),attr(residuals(reg7a), "index"))
reg7a_residuals <- data.frame(residuals(reg7a),attr(residuals(reg7a),"index"))

reg7a_fitted_residual <- reg7a_fitteds %>% 
  left_join(reg7a_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg7a_fitted_residual %>% 
  ggplot(aes(x = predict.reg7a.,y = residuals.reg7a.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg7a")

# Reg7b
reg7b_fitteds <- data.frame(predict(reg7b),attr(residuals(reg7b), "index"))
reg7b_residuals <- data.frame(residuals(reg7b),attr(residuals(reg7b),"index"))

reg7b_fitted_residual <- reg7b_fitteds %>% 
  left_join(reg7b_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg7b_fitted_residual %>% 
  ggplot(aes(x = predict.reg7b.,y = residuals.reg7b.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg7b")

# Reg7c
reg7c_fitteds <- data.frame(predict(reg7c),attr(residuals(reg7c), "index"))
reg7c_residuals <- data.frame(residuals(reg7c),attr(residuals(reg7c),"index"))

reg7c_fitted_residual <- reg7c_fitteds %>% 
  left_join(reg7c_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg7c_fitted_residual %>% 
  ggplot(aes(x = predict.reg7c.,y = residuals.reg7c.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg7c")

# Reg7d
reg7d_fitteds <- data.frame(predict(reg7d),attr(residuals(reg7d), "index"))
reg7d_residuals <- data.frame(residuals(reg7d),attr(residuals(reg7d),"index"))

reg7d_fitted_residual <- reg7d_fitteds %>% 
  left_join(reg7d_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg7d_fitted_residual %>% 
  ggplot(aes(x = predict.reg7d.,y = residuals.reg7d.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg7d")

# Reg8
reg8_fitteds <- data.frame(predict(reg8),attr(residuals(reg8), "index"))
reg8_residuals <- data.frame(residuals(reg8),attr(residuals(reg8),"index"))

reg8_fitted_residual <- reg8_fitteds %>% 
  left_join(reg8_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg8_fitted_residual %>% 
  ggplot(aes(x = predict.reg8.,y = residuals.reg8.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg8")

# Reg8a
reg8a_fitteds <- data.frame(predict(reg8a),attr(residuals(reg8a), "index"))
reg8a_residuals <- data.frame(residuals(reg8a),attr(residuals(reg8a),"index"))

reg8a_fitted_residual <- reg8a_fitteds %>% 
  left_join(reg8a_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg8a_fitted_residual %>% 
  ggplot(aes(x = predict.reg8a.,y = residuals.reg8a.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg8a")

# Reg8b
reg8b_fitteds <- data.frame(predict(reg8b),attr(residuals(reg8b), "index"))
reg8b_residuals <- data.frame(residuals(reg8b),attr(residuals(reg8b),"index"))

reg8b_fitted_residual <- reg8b_fitteds %>% 
  left_join(reg8b_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg8b_fitted_residual %>% 
  ggplot(aes(x = predict.reg8b.,y = residuals.reg8b.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg8b")

# Reg8c
reg8c_fitteds <- data.frame(predict(reg8c),attr(residuals(reg8c), "index"))
reg8c_residuals <- data.frame(residuals(reg8c),attr(residuals(reg8c),"index"))

reg8c_fitted_residual <- reg8c_fitteds %>% 
  left_join(reg8c_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg8c_fitted_residual %>% 
  ggplot(aes(x = predict.reg8c.,y = residuals.reg8c.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg8c")

# Reg8d
reg8d_fitteds <- data.frame(predict(reg8d),attr(residuals(reg8d), "index"))
reg8d_residuals <- data.frame(residuals(reg8d),attr(residuals(reg8d),"index"))

reg8d_fitted_residual <- reg8d_fitteds %>% 
  left_join(reg8d_residuals,by = c("FIPS" = "FIPS","time" = "time"))

reg8d_fitted_residual %>% 
  ggplot(aes(x = predict.reg8d.,y = residuals.reg8d.))+
  geom_point()+
  geom_line(aes(y = 0))+
  xlab("Fitted Values")+
  ylab("Residuals")+
  ggtitle("Fitted vs. Residuals, reg8d")