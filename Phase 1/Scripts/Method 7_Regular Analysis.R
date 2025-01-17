library(dplyr)
library(lubridate)
library(sf)
library(data.table)


ICU = fread(file.choose(), sep=",", header=T)

Diagnoses = fread(file.choose(), sep=",", header=T)

combined_dataset = merge(ICU, Diagnoses[, c('hsp_account_id', 'ref_bill_code', 'DX_NAME')], by = 'hsp_account_id', all.x = TRUE)


combined_dataset$first_icu_dt <- sub("^\\[|\\]$", "", combined_dataset$first_icu_dt)
combined_dataset$first_icu_dt <- as.POSIXct(combined_dataset$first_icu_dt, format="%Y/%m/%d:%I:%M:%S %p")

combined_dataset = combined_dataset %>%
  rename(Date = first_icu_dt)

combined_dataset$Date = format(combined_dataset$Date, "%Y-%m-%d")

combined_dataset$Date <- ymd(combined_dataset$Date)

combined_dataset$Date <- format(combined_dataset$Date, "%Y-%m")

combined_dataset <- combined_dataset %>%
  mutate(Date = ym(Date)) %>%
  filter(st == "IL", year(Date) %in% c(2020, 2021, 2022, 2023)) %>%
  mutate(Date = format(Date, "%Y-%m"))

combined_dataset = combined_dataset %>% 
  distinct(hsp_account_id, .keep_all = TRUE)

combined_dataset = combined_dataset %>%
  mutate(zipcode = substr(zipcode, 1, 5))

combined_dataset = combined_dataset[,-c(2:10,12:16,18:45)]

unique_zipcodes <- unique(combined_dataset$zipcode)

combined_dataset_New <- combined_dataset %>%
  group_by(zipcode,Date) %>%
  summarise(total_patients_per_zipcode_per_month = n_distinct(hsp_account_id))


all_months <- seq(from = as.yearmon("2020-01", "%Y-%m"), to = as.yearmon("2023-12", "%Y-%m"), by = 1/12)

formatted_months <- format(all_months, "%Y-%m")

all_zipcodes <- unique(combined_dataset_New$zipcode)

full_grid <- expand.grid(zipcode = all_zipcodes, Date = formatted_months)

expanded_dataset = full_join(combined_dataset_New,full_grid, by = c('zipcode','Date'))

expanded_dataset <- expanded_dataset %>%
  mutate(across(everything(), ~ replace_na(., 0)))


expanded_dataset_New <- expanded_dataset %>%
  group_by(zipcode) %>%
  mutate(total_patients_per_zipcode = sum(total_patients_per_zipcode_per_month, na.rm = TRUE)) %>%
  ungroup()

population_ADI = fread(file.choose(), sep=",", header=T)

expanded_dataset_New$zipcode = as.character(expanded_dataset_New$zipcode)

population_ADI$zipcode = as.character(population_ADI$zipcode)

expanded_dataset_New = full_join(expanded_dataset_New,population_ADI, by = c('zipcode'))

expanded_dataset_New = expanded_dataset_New[,-c(6)]

expanded_dataset_New = expanded_dataset_New %>%
  rename(ADI_score = ADI_STATERANK)

# patients_per_zipcode <- expanded_dataset_New %>%
#   na.omit() %>%  # Remove all rows with NAs
#   mutate(Rate = (total_patients_per_zipcode  / cpop) * 1000) 

patients_per_zipcode_2 <- expanded_dataset_New %>%
  na.omit() %>%  # Remove all rows with NAs
  distinct(zipcode, .keep_all = TRUE) %>%  # Keep distinct zipcodes
  mutate(Rate = (total_patients_per_zipcode  / cpop) * 1000) 

patients_per_zipcode_2 = patients_per_zipcode_2[,-c(2:6)]


# Calculate the 25th percentile of the Rate values
rate_25th_percentile <- quantile(patients_per_zipcode_2$Rate, 0.25)

# Filter out zipcodes with Rate below the 25th percentile
patients_per_zipcode_2 <- patients_per_zipcode_2 %>%
  filter(Rate >= rate_25th_percentile)

unique_zipcodes <- unique(patients_per_zipcode_2$zipcode)


write.csv(patients_per_zipcode_2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 7/ZIPCODES_UPDATED.csv", row.names = FALSE)

#########################

combined_dataset_NEW_2 = merge(combined_dataset_New,patients_per_zipcode_2, by = c('zipcode'))

combined_dataset_NEW_2 = combined_dataset_NEW_2[,-c(4)]

unique_zipcodes <- unique(combined_dataset_NEW_2$zipcode)

all_months <- seq(from = as.yearmon("2020-01", "%Y-%m"), to = as.yearmon("2023-12", "%Y-%m"), by = 1/12)

formatted_months <- format(all_months, "%Y-%m")

all_zipcodes <- unique(patients_per_zipcode_2$zipcode)

full_grid <- expand.grid(zipcode = all_zipcodes, Date = formatted_months)

combined_dataset_NEW_2 = full_join(combined_dataset_NEW_2,full_grid, by = c('zipcode','Date'))

combined_dataset_NEW_2 <- combined_dataset_NEW_2 %>%
  mutate(across(everything(), ~ replace_na(., 0)))

unique_zipcodes <- unique(combined_dataset_NEW_2$zipcode)


combined_dataset_NEW_2 <- combined_dataset_NEW_2 %>%
  group_by(zipcode) %>%
  mutate(total_patients_per_zipcode = sum(total_patients_per_zipcode_per_month, na.rm = TRUE)) %>%
  ungroup()


combined_dataset_NEW_2 = full_join(combined_dataset_NEW_2,population_ADI, by = c('zipcode'))

combined_dataset_NEW_2 = combined_dataset_NEW_2[,-c(6)]

combined_dataset_NEW_2 = combined_dataset_NEW_2 %>%
  rename(ADI_score = ADI_STATERANK)

combined_dataset_NEW_2 <- combined_dataset_NEW_2 %>%
  na.omit()

combined_dataset_NEW_2 <- combined_dataset_NEW_2 %>%
  mutate(ICU_Rate = (total_patients_per_zipcode_per_month  / cpop) * 1000)


testcenter = fread(file.choose(), sep=",", header=T)

effective_testcenters = fread(file.choose(), sep=",", header=T)

testcenter$zipcode = as.character(testcenter$zipcode)

effective_testcenters$zipcode = as.character(effective_testcenters$zipcode)


combined_dataset_NEW_3 = full_join(combined_dataset_NEW_2,testcenter, by = c('zipcode','Date'))

combined_dataset_NEW_4 = full_join(combined_dataset_NEW_3,effective_testcenters, by = c('zipcode','Date'))

unique_zipcodes <- unique(combined_dataset_NEW_3$zipcode)

write.csv(combined_dataset_NEW_4, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 7/ICU_UPDATED_Method7.csv", row.names = FALSE)

###############################################################################
################################### ANALYSIS ##################################
################################### ALPHA WAVE ################################
###############################################################################

ICU_UPDATED_Method7_WithNA_ALPHA = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithNA_ALPHA$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithNA_ALPHA$ADI_category = as.factor(ICU_UPDATED_Method7_WithNA_ALPHA$ADI_category)

ICU_UPDATED_Method7_WithNA_ALPHA$zipcode = as.factor(ICU_UPDATED_Method7_WithNA_ALPHA$zipcode)


ICU_UPDATED_Method7_WithNA_ALPHA$ADI_category <- relevel(ICU_UPDATED_Method7_WithNA_ALPHA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(`ICU_Rate` ~  `ADI_category` + `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithNA_ALPHA)
vif(vif_model)

ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_ALPHA)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_ALPHA)


tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(`ICU_Rate` + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_ALPHA)


tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_ALPHA)


tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_ALPHA)


tab_model(ICU_rate_GLMMmodel_ALPHA_6, digits = 5)

###############################################################################
################################### DELTA WAVE ################################
###############################################################################

ICU_UPDATED_Method7_WithNA_DELTA = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithNA_DELTA$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithNA_DELTA$ADI_category = as.factor(ICU_UPDATED_Method7_WithNA_DELTA$ADI_category)

ICU_UPDATED_Method7_WithNA_DELTA$zipcode = as.factor(ICU_UPDATED_Method7_WithNA_DELTA$zipcode)


ICU_UPDATED_Method7_WithNA_DELTA$ADI_category <- relevel(ICU_UPDATED_Method7_WithNA_DELTA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(`ICU_Rate` ~  `ADI_category` + `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithNA_DELTA)
vif(vif_model)

ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_DELTA)


tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_DELTA)


tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(`ICU_Rate` + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_DELTA)


tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_DELTA)


tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_6 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_DELTA)


tab_model(ICU_rate_GLMMmodel_DELTA_6, digits = 5)

###############################################################################
################################### OMICRON WAVE ##############################
###############################################################################

ICU_UPDATED_Method7_WithNA_OMICRON = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithNA_OMICRON$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithNA_OMICRON$ADI_category = as.factor(ICU_UPDATED_Method7_WithNA_OMICRON$ADI_category)

ICU_UPDATED_Method7_WithNA_OMICRON$zipcode = as.factor(ICU_UPDATED_Method7_WithNA_OMICRON$zipcode)


ICU_UPDATED_Method7_WithNA_OMICRON$ADI_category <- relevel(ICU_UPDATED_Method7_WithNA_OMICRON$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(`ICU_Rate` ~  `ADI_category` + `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithNA_OMICRON)
vif(vif_model)

ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_OMICRON)


tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_OMICRON)


tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(`ICU_Rate` + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_OMICRON)


tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_OMICRON)


tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_OMICRON)


tab_model(ICU_rate_GLMMmodel_OMICRON_6, digits = 5)


###############################################################################

ICU_UPDATED_Method7_WithOutNA_ALPHA = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithOutNA_ALPHA$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithOutNA_ALPHA$ADI_category = as.factor(ICU_UPDATED_Method7_WithOutNA_ALPHA$ADI_category)

ICU_UPDATED_Method7_WithOutNA_ALPHA$zipcode = as.factor(ICU_UPDATED_Method7_WithOutNA_ALPHA$zipcode)


ICU_UPDATED_Method7_WithOutNA_ALPHA$ADI_category <- relevel(ICU_UPDATED_Method7_WithOutNA_ALPHA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(`ICU_Rate` ~  `ADI_category` + `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithOutNA_ALPHA)
vif(vif_model)

ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_ALPHA)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_ALPHA)


tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(`ICU_Rate` + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_ALPHA)


tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_ALPHA)


tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_ALPHA)


tab_model(ICU_rate_GLMMmodel_ALPHA_6, digits = 5)

###############################################################################

ICU_UPDATED_Method7_WithOutNA_DELTA = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithOutNA_DELTA$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithOutNA_DELTA$ADI_category = as.factor(ICU_UPDATED_Method7_WithOutNA_DELTA$ADI_category)

ICU_UPDATED_Method7_WithOutNA_DELTA$zipcode = as.factor(ICU_UPDATED_Method7_WithOutNA_DELTA$zipcode)


ICU_UPDATED_Method7_WithOutNA_DELTA$ADI_category <- relevel(ICU_UPDATED_Method7_WithOutNA_DELTA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(`ICU_Rate` ~  `ADI_category` + `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithOutNA_DELTA)
vif(vif_model)

ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_DELTA)


tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_DELTA)


tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(`ICU_Rate` + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_DELTA)


tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_DELTA)


tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_6 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_DELTA)


tab_model(ICU_rate_GLMMmodel_DELTA_6, digits = 5)

###############################################################################

ICU_UPDATED_Method7_WithOutNA_OMICRON = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithOutNA_OMICRON$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithOutNA_OMICRON$ADI_category = as.factor(ICU_UPDATED_Method7_WithOutNA_OMICRON$ADI_category)

ICU_UPDATED_Method7_WithOutNA_OMICRON$zipcode = as.factor(ICU_UPDATED_Method7_WithOutNA_OMICRON$zipcode)


ICU_UPDATED_Method7_WithOutNA_OMICRON$ADI_category <- relevel(ICU_UPDATED_Method7_WithOutNA_OMICRON$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(`ICU_Rate` ~  `ADI_category` + `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithOutNA_OMICRON)
vif(vif_model)

ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_OMICRON)


tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_OMICRON)


tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(`ICU_Rate` + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_OMICRON)


tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_OMICRON)


tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_OMICRON)


tab_model(ICU_rate_GLMMmodel_OMICRON_6, digits = 5)
