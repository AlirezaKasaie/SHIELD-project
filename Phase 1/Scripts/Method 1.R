library(zoo)

ICU_UPDATED = fread(file.choose(), sep=",", header=T)

testcenters = fread(file.choose(), sep=",", header=T)

combined_dataset = full_join(ICU_UPDATED, testcenters, by = c('zipcode','Date'))

unique_zipcodes <- unique(combined_dataset$zipcode)

all_months <- seq(from = as.yearmon("2021-01", "%Y-%m"), to = as.yearmon("2022-12", "%Y-%m"), by = 1/12)

formatted_months <- format(all_months, "%Y-%m")

all_zipcodes <- unique(combined_dataset$zipcode)

full_grid <- expand.grid(zipcode = all_zipcodes, Date = formatted_months)

expanded_dataset = full_join(combined_dataset,full_grid, by = c('zipcode','Date'))

population_ADI = fread(file.choose(), sep=",", header=T)

expanded_dataset_2 = full_join(expanded_dataset,population_ADI, by = c('zipcode'))

write.csv(expanded_dataset_2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 1/ICU_UPDATED_NEW.csv", row.names = FALSE)

###############################################################################
################################# Analysis ####################################
################################ Alpha Wave ###################################
###############################################################################

ICU_UPDATED_ALPHA_WAVE = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_ALPHA_WAVE$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Alpha_Wave$`Total ICU Hospitalizations per Zipcode per Month` , type = 3)
# print(skewness_ICU_hospitalization)


ICU_UPDATED_ALPHA_WAVE$`ADI category` = as.factor(ICU_UPDATED_ALPHA_WAVE$`ADI category`)

ICU_UPDATED_ALPHA_WAVE$zipcode = as.factor(ICU_UPDATED_ALPHA_WAVE$zipcode)


ICU_UPDATED_ALPHA_WAVE$`ADI category` <- relevel(ICU_UPDATED_ALPHA_WAVE$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(`COVID_ICU_Rate` ~  `ADI category` + `testcenters`, data = ICU_UPDATED_ALPHA_WAVE)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_ALPHA_WAVE)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_ALPHA_WAVE)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_ALPHA_WAVE)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)

###############################################################################
################################ Delta Wave ###################################
###############################################################################

ICU_UPDATED_DELTA_WAVE = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_DELTA_WAVE$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Alpha_Wave$`Total ICU Hospitalizations per Zipcode per Month` , type = 3)
# print(skewness_ICU_hospitalization)


ICU_UPDATED_DELTA_WAVE$`ADI category` = as.factor(ICU_UPDATED_DELTA_WAVE$`ADI category`)

ICU_UPDATED_DELTA_WAVE$zipcode = as.factor(ICU_UPDATED_DELTA_WAVE$zipcode)


ICU_UPDATED_DELTA_WAVE$`ADI category` <- relevel(ICU_UPDATED_DELTA_WAVE$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(`COVID_ICU_Rate` ~  `ADI category` + `testcenters`, data = ICU_UPDATED_DELTA_WAVE)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_DELTA_WAVE)


tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_DELTA_WAVE)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_DELTA_WAVE)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


###############################################################################
################################ Omicron Wave ###################################
###############################################################################

ICU_UPDATED_OMICRON_WAVE = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_OMICRON_WAVE$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Alpha_Wave$`Total ICU Hospitalizations per Zipcode per Month` , type = 3)
# print(skewness_ICU_hospitalization)


ICU_UPDATED_OMICRON_WAVE$`ADI category` = as.factor(ICU_UPDATED_OMICRON_WAVE$`ADI category`)

ICU_UPDATED_OMICRON_WAVE$zipcode = as.factor(ICU_UPDATED_OMICRON_WAVE$zipcode)


ICU_UPDATED_OMICRON_WAVE$`ADI category` <- relevel(ICU_UPDATED_OMICRON_WAVE$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(`COVID_ICU_Rate` ~  `ADI category` + `testcenters`, data = ICU_UPDATED_OMICRON_WAVE)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_OMICRON_WAVE)


tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_OMICRON_WAVE)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_OMICRON_WAVE)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


###############################################################################
################################# Lag analysis #################################
################################ Alpha Wave - Lag 1 ###########################
###############################################################################


ICU_UPDATED_ALPHA_WAVE_Lag1 = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_ALPHA_WAVE_Lag1 <- ICU_UPDATED_ALPHA_WAVE_Lag1 %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Covid_ICU_admission_rate = lag(`COVID_ICU_Rate`, 1)
  ) %>%
  ungroup()

ICU_UPDATED_ALPHA_WAVE_Lag1 = na.omit(ICU_UPDATED_ALPHA_WAVE_Lag1)


#write.csv(Alpha_Wave_Lag1_WithNA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Alpha-Wave-Lag1-WithNA-Final.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_ALPHA_WAVE_Lag1$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Alpha_Wave_Lag1$Lag1_Number_of_ICU_admissions , type = 3)
# print(skewness_ICU_hospitalization)


ICU_UPDATED_ALPHA_WAVE_Lag1$`ADI category`= as.factor(ICU_UPDATED_ALPHA_WAVE_Lag1$`ADI category`)

ICU_UPDATED_ALPHA_WAVE_Lag1$zipcode = as.factor(ICU_UPDATED_ALPHA_WAVE_Lag1$zipcode)


ICU_UPDATED_ALPHA_WAVE_Lag1$`ADI category` <- relevel(ICU_UPDATED_ALPHA_WAVE_Lag1$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ `ADI category` + `testcenters`, data = ICU_UPDATED_ALPHA_WAVE_Lag1)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_ALPHA_WAVE_Lag1)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_ALPHA_WAVE_Lag1)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `testcenters`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_ALPHA_WAVE_Lag1)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)
