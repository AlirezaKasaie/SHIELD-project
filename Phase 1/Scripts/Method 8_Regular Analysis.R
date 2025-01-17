library(dplyr)
library(lubridate)
library(data.table)
library(zoo)
library(tidyr)

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
  mutate(zipcode = substr(zipcode, 1, 5))

combined_dataset_2 = combined_dataset[,-c(2:10,12:16,18:43,45)]

unique_zipcodes <- unique(combined_dataset_2$zipcode)

unique_IDs <- unique(combined_dataset_2$hsp_account_id)

combined_dataset_New <- combined_dataset_2 %>%
  group_by(zipcode,Date) %>%
  summarise(total_patients_per_zipcode_per_month = n_distinct(hsp_account_id))

combined_dataset_New <- combined_dataset_New %>%
  group_by(zipcode) %>%
  mutate(total_patients_per_zipcode = sum(total_patients_per_zipcode_per_month, na.rm = TRUE)) %>%
  ungroup()

combined_dataset_New = combined_dataset_New[,-c(2:3)]

combined_dataset_New = combined_dataset_New %>% 
  distinct(zipcode, .keep_all = TRUE)

unique_zipcodes <- unique(combined_dataset_New$zipcode)

combined_dataset_New <- combined_dataset_New[order(-combined_dataset_New$total_patients_per_zipcode), ]

unique_zipcodes <- unique(combined_dataset_New$zipcode)


# Calculate the number of rows corresponding to the top 25%
n_top_25_percent <- ceiling(0.25 * nrow(combined_dataset_New))

# Select the top 25% rows
top_25_percent <- combined_dataset_New[1:n_top_25_percent, ]

write.csv(top_25_percent, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 8-2/Data/top_25_percent.csv", row.names = FALSE)

###############################################################################

filtered_combined_dataset <- combined_dataset_2[combined_dataset_2$zipcode %in% top_25_percent$zipcode, ]

unique_zipcodes <- unique(filtered_combined_dataset$zipcode)


covid_codes = c("Z20.822","M35.81", "J12.82", "U07.1", "B97.29", "J20.8", "J22", "J98.8", "J80", "U09.9") 

filtered_combined_dataset_COVID_CODES <- filtered_combined_dataset %>%
  filter(ref_bill_code %in% covid_codes)

unique_zipcodes <- unique(filtered_combined_dataset_COVID_CODES$zipcode)

unique_IDs <- unique(filtered_combined_dataset_COVID_CODES$hsp_account_id)


filtered_combined_dataset_COVID_CODES <- filtered_combined_dataset_COVID_CODES %>%
  group_by(zipcode,Date) %>%
  summarise(total_patients_per_zipcode_per_month = n_distinct(hsp_account_id))

filtered_combined_dataset_COVID_CODES <- filtered_combined_dataset_COVID_CODES %>%
  group_by(zipcode) %>%
  mutate(total_patients_per_zipcode = sum(total_patients_per_zipcode_per_month, na.rm = TRUE)) %>%
  ungroup()

all_months <- seq(from = as.yearmon("2021-02", "%Y-%m"), to = as.yearmon("2022-12", "%Y-%m"), by = 1/12)

formatted_months <- format(all_months, "%Y-%m")

all_zipcodes <- unique(filtered_combined_dataset_COVID_CODES$zipcode)

full_grid <- expand.grid(zipcode = all_zipcodes, Date = formatted_months)

expanded_dataset_NEW = full_join(filtered_combined_dataset_COVID_CODES,full_grid, by = c('zipcode','Date'))


#write.csv(filtered_combined_dataset_COVID_CODES, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 8-2/Data/filtered_combined_dataset_COVID_CODES.csv", row.names = FALSE)

population_ADI = fread(file.choose(), sep=",", header=T)

expanded_dataset_NEW$zipcode = as.character(expanded_dataset_NEW$zipcode)

population_ADI$zipcode = as.character(population_ADI$zipcode)

expanded_dataset_NEW = merge(expanded_dataset_NEW,population_ADI, by = c('zipcode'))

#write.csv(filtered_combined_dataset_COVID_CODES_NEW, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 8/filtered_combined_dataset_COVID_CODES_NEW.csv", row.names = FALSE)

expanded_dataset_NEW = expanded_dataset_NEW[,-c(6)]

expanded_dataset_NEW = expanded_dataset_NEW %>%
  rename(ADI_score = ADI_STATERANK)

unique_zipcodes = unique(expanded_dataset_NEW$zipcode)


# all_months <- seq(from = as.yearmon("2020-01", "%Y-%m"), to = as.yearmon("2023-12", "%Y-%m"), by = 1/12)
# 
# formatted_months <- format(all_months, "%Y-%m")
# 
# all_zipcodes <- unique(filtered_combined_dataset_COVID_CODES_NEW$zipcode)
# 
# full_grid <- expand.grid(zipcode = all_zipcodes, Date = formatted_months)
# 
# expanded_dataset_NEW = full_join(filtered_combined_dataset_COVID_CODES_NEW,full_grid, by = c('zipcode','Date'))
# 
# unique_zipcodes = unique(expanded_dataset_NEW$zipcode)
# 
# 
# expanded_dataset_NEW = expanded_dataset_NEW[,-c(5:6)]

expanded_dataset_NEW <- expanded_dataset_NEW %>%
   mutate(across(everything(), ~ replace_na(., 0)))

# expanded_dataset_NEW$zipcode = as.character(expanded_dataset_NEW$zipcode)
# 
# population_ADI$zipcode = as.character(population_ADI$zipcode)
# 
# expanded_dataset_NEW = merge(expanded_dataset_NEW,population_ADI, by = c('zipcode'))
# 
# unique_zipcodes = unique(expanded_dataset_NEW$zipcode)


testcenter = fread(file.choose(), sep=",", header=T)

effective_testcenters = fread(file.choose(), sep=",", header=T)

testcenter$zipcode = as.character(testcenter$zipcode)

effective_testcenters$zipcode = as.character(effective_testcenters$zipcode)


expanded_dataset_NEW = full_join(expanded_dataset_NEW,testcenter, by = c('zipcode','Date'))

unique_zipcodes = unique(expanded_dataset_NEW$zipcode)


expanded_dataset_NEW = full_join(expanded_dataset_NEW,effective_testcenters, by = c('zipcode','Date'))

unique_zipcodes = unique(expanded_dataset_NEW$zipcode)

write.csv(expanded_dataset_NEW, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 8-2/eICU_UPDATED_Method8.csv", row.names = FALSE)

###############################################################################








# all_months <- seq(from = as.yearmon("2020-01", "%Y-%m"), to = as.yearmon("2023-12", "%Y-%m"), by = 1/12)
# 
# formatted_months <- format(all_months, "%Y-%m")
# 
# all_zipcodes <- unique(combined_dataset_New$zipcode)
# 
# full_grid <- expand.grid(zipcode = all_zipcodes, Date = formatted_months)
# 
# expanded_dataset = full_join(combined_dataset_New,full_grid, by = c('zipcode','Date'))
# 
# expanded_dataset <- expanded_dataset %>%
#   mutate(across(everything(), ~ replace_na(., 0)))
# 
# 
# expanded_dataset_New <- expanded_dataset %>%
#   group_by(zipcode) %>%
#   mutate(total_patients_per_zipcode = sum(total_patients_per_zipcode_per_month, na.rm = TRUE)) %>%
#   ungroup()







# patients_per_zipcode <- expanded_dataset_New %>%
#   na.omit() %>%  # Remove all rows with NAs
#   mutate(Rate = (total_patients_per_zipcode  / cpop) * 1000) 

# patients_per_zipcode_2 <- expanded_dataset_New %>%
#   na.omit() %>%  # Remove all rows with NAs
#   distinct(zipcode, .keep_all = TRUE) %>%  # Keep distinct zipcodes
#   mutate(Rate = (total_patients_per_zipcode  / cpop) * 1000) 
# 
# patients_per_zipcode_2 = patients_per_zipcode_2[,-c(2:6)]






# #########################
# 
# combined_dataset_NEW_2 = merge(combined_dataset_New,patients_per_zipcode_2, by = c('zipcode'))
# 
# combined_dataset_NEW_2 = combined_dataset_NEW_2[,-c(4)]
# 
# unique_zipcodes <- unique(combined_dataset_NEW_2$zipcode)
# 
# all_months <- seq(from = as.yearmon("2020-01", "%Y-%m"), to = as.yearmon("2023-12", "%Y-%m"), by = 1/12)
# 
# formatted_months <- format(all_months, "%Y-%m")
# 
# all_zipcodes <- unique(patients_per_zipcode_2$zipcode)
# 
# full_grid <- expand.grid(zipcode = all_zipcodes, Date = formatted_months)
# 
# combined_dataset_NEW_2 = full_join(combined_dataset_NEW_2,full_grid, by = c('zipcode','Date'))
# 
# combined_dataset_NEW_2 <- combined_dataset_NEW_2 %>%
#   mutate(across(everything(), ~ replace_na(., 0)))
# 
# unique_zipcodes <- unique(combined_dataset_NEW_2$zipcode)
# 
# 
# combined_dataset_NEW_2 <- combined_dataset_NEW_2 %>%
#   group_by(zipcode) %>%
#   mutate(total_patients_per_zipcode = sum(total_patients_per_zipcode_per_month, na.rm = TRUE)) %>%
#   ungroup()
# 
# 
# combined_dataset_NEW_2 = full_join(combined_dataset_NEW_2,population_ADI, by = c('zipcode'))
# 
# combined_dataset_NEW_2 = combined_dataset_NEW_2[,-c(6)]
# 
# combined_dataset_NEW_2 = combined_dataset_NEW_2 %>%
#   rename(ADI_score = ADI_STATERANK)
# 
# combined_dataset_NEW_2 <- combined_dataset_NEW_2 %>%
#   na.omit()
# 
# combined_dataset_NEW_2 <- combined_dataset_NEW_2 %>%
#   mutate(COVID_ICU_Rate = (total_patients_per_zipcode_per_month  / cpop) * 1000)
# 
# 
# testcenter = fread(file.choose(), sep=",", header=T)
# 
# effective_testcenters = fread(file.choose(), sep=",", header=T)
# 
# testcenter$zipcode = as.character(testcenter$zipcode)
# 
# effective_testcenters$zipcode = as.character(effective_testcenters$zipcode)
# 
# 
# combined_dataset_NEW_3 = full_join(combined_dataset_NEW_2,testcenter, by = c('zipcode','Date'))
# 
# combined_dataset_NEW_4 = full_join(combined_dataset_NEW_3,effective_testcenters, by = c('zipcode','Date'))
# 
# unique_zipcodes <- unique(combined_dataset_NEW_3$zipcode)
# 
# write.csv(combined_dataset_NEW_4, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 8/ICU_UPDATED_Method8.csv", row.names = FALSE)


###############################################################################
################################### ANALYSIS ##################################
################################### WHOLE DATA SET ############################
###############################################################################

library(e1071)
library(lme4)
library(glmmTMB)
library(data.table)
library(car)
library(sjPlot)

ICU_UPDATED_Method8 = fread(file.choose(), sep=",", header=T)

#ICU_UPDATED_Method8$Date <- as.Date(paste0(ICU_UPDATED_Method8$Date, "-01"))

skewness_ICU_rate = skewness(ICU_UPDATED_Method8$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method8$ADI_category = as.factor(ICU_UPDATED_Method8$ADI_category)

ICU_UPDATED_Method8$zipcode = as.factor(ICU_UPDATED_Method8$zipcode)


ICU_UPDATED_Method8$ADI_category <- relevel(ICU_UPDATED_Method8$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(`COVID_ICU_Rate` ~  `ADI_category` + `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8)
vif(vif_model)

ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)



ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8)


tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI_category`  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8)


tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*`ADI_category`  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8)


tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category`  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8)


tab_model(ICU_rate_GLMMmodel_ALPHA_6, digits = 5)



correlation_result <- cor.test(ICU_UPDATED_Method8$COVID_ICU_Rate, 
                               ICU_UPDATED_Method8$`Effective Number of Center`, 
                               method = "pearson")

correlation_coefficient <- correlation_result$estimate
p_value <- correlation_result$p.value

cat("Correlation Coefficient: ", correlation_coefficient, "\n")
cat("P-value: ", p_value, "\n")

###############################################################################
################################### ANALYSIS ##################################
################################### ALPHA WAVE ################################
###############################################################################

library(e1071)
library(lme4)
library(data.table)
library(car)
library(sjPlot)

ICU_UPDATED_Method8_ALPHA = fread(file.choose(), sep=",", header=T)

# ICU_UPDATED_Method8_ALPHA$Date = as.factor(ICU_UPDATED_Method8_ALPHA$Date)
# 
# ICU_UPDATED_Method8_ALPHA$Date <- relevel(ICU_UPDATED_Method8_ALPHA$Date, ref = "2021-03")

#ICU_UPDATED_Method8_ALPHA$Date <- as.Date(paste0(ICU_UPDATED_Method8_ALPHA$Date, "-01"))

skewness_ICU_rate = skewness(ICU_UPDATED_Method8_ALPHA$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method8_ALPHA$ADI_category = as.factor(ICU_UPDATED_Method8_ALPHA$ADI_category)

ICU_UPDATED_Method8_ALPHA$zipcode = as.factor(ICU_UPDATED_Method8_ALPHA$zipcode)


ICU_UPDATED_Method8_ALPHA$ADI_category <- relevel(ICU_UPDATED_Method8_ALPHA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(`COVID_ICU_Rate` ~ `Effective Number of Center`*ADI_category, data = ICU_UPDATED_Method8_ALPHA)
vif(vif_model)

#ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~`testcenters` +
#                                     (1 | zipcode), 
#                                   data = ICU_UPDATED_Method8_ALPHA)

#tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)



ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_ALPHA)

#mean_dependent_variable <- mean(ICU_UPDATED_Method8_ALPHA$`COVID_ICU_Rate`, na.rm = TRUE)


summary(ICU_rate_GLMMmodel_ALPHA_2)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)

############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_ALPHA_2)$coefficients

# Raw coefficients (betas) and their standard errors
raw_betas <- coef_summary[, 1]
std_errors <- coef_summary[, 2]

# Exponentiate coefficients to get IRRs
exp_coefficients <- exp(raw_betas)

# Calculate 95% confidence intervals
lower_ci <- exp(raw_betas - 1.96 * std_errors)
upper_ci <- exp(raw_betas + 1.96 * std_errors)

# Create a data frame with all results
result_table <- data.frame(
  Variable = rownames(coef_summary),
  IRR = exp_coefficients,
  SE = std_errors,
  `Lower 95% CI` = lower_ci,
  `Upper 95% CI` = upper_ci
)

# Print the result table
print(result_table)

############################################################################


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI_category (Less Dis)`  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_ALPHA)

ICU_rate_GLMMmodel_ALPHA_3_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI_category (More Dis)`  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_ALPHA)

#mean_dependent_variable <- mean(ICU_UPDATED_Method8_ALPHA$`COVID_ICU_Rate`, na.rm = TRUE)


summary(ICU_rate_GLMMmodel_ALPHA_3)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


############################## NEW CODE #####################################

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5, transform = "exp")
tab_model(ICU_rate_GLMMmodel_ALPHA_3_1, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_ALPHA_3_1)$coefficients

# Raw coefficients (betas) and their standard errors
raw_betas <- coef_summary[, 1]
std_errors <- coef_summary[, 2]

# Exponentiate coefficients to get IRRs
exp_coefficients <- exp(raw_betas)

# Calculate 95% confidence intervals
lower_ci <- exp(raw_betas - 1.96 * std_errors)
upper_ci <- exp(raw_betas + 1.96 * std_errors)

# Create a data frame with all results
result_table <- data.frame(
  Variable = rownames(coef_summary),
  IRR = exp_coefficients,
  SE = std_errors,
  `Lower 95% CI` = lower_ci,
  `Upper 95% CI` = upper_ci
)

# Print the result table
print(result_table)

###########################################################################


#ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*`ADI_score` +
#                                     (1 | zipcode), 
#                                   data = ICU_UPDATED_Method8_ALPHA)


#tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


#ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category`  +
#                                     (1 | zipcode), 
#                                  data = ICU_UPDATED_Method8_ALPHA)

#mean_dependent_variable <- mean(ICU_UPDATED_Method8_ALPHA$`COVID_ICU_Rate`, na.rm = TRUE)

#summary(ICU_rate_GLMMmodel_ALPHA_6)

#tab_model(ICU_rate_GLMMmodel_ALPHA_6, digits = 5)

########################## NEW CODE #######################################


ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category (Less Dis)`  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_ALPHA)

tab_model(ICU_rate_GLMMmodel_ALPHA_6, digits = 5, transform = "exp")



ICU_rate_GLMMmodel_ALPHA_7 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category (More Dis)`  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_ALPHA)

tab_model(ICU_rate_GLMMmodel_ALPHA_7, digits = 5, transform = "exp")


# Fit the LMER model with interaction
ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` * `ADI_category` +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method8_ALPHA)

# Step 1: Use emtrends to calculate simple slopes and SEs
simple_slopes <- emtrends(ICU_rate_GLMMmodel_ALPHA_6, ~ ADI_category, var = "Effective Number of Center")
print(simple_slopes)

# Step 2: Extract coefficients and variance-covariance matrix for manual SE calculation
coef_summary <- summary(ICU_rate_GLMMmodel_ALPHA_6)$coefficients
vcov_matrix <- vcov(ICU_rate_GLMMmodel_ALPHA_6)

# Extract variance and covariance components
var_beta1 <- vcov_matrix["`Effective Number of Center`", "`Effective Number of Center`"]  # Variance of main effect
var_beta3 <- vcov_matrix["`Effective Number of Center`:ADI_categoryMore Disadvantaged", 
                         "`Effective Number of Center`:ADI_categoryMore Disadvantaged"]  # Variance of interaction
cov_beta1_beta3 <- vcov_matrix["`Effective Number of Center`", 
                               "`Effective Number of Center`:ADI_categoryMore Disadvantaged"]  # Covariance

# Calculate SEs for Low and High ADI
se_low_adi <- sqrt(var_beta1)  # SE for Low ADI (reference level)
se_high_adi <- sqrt(var_beta1 + var_beta3 + 2 * cov_beta1_beta3)  # SE for High ADI

# Display results
cat("SE for Low ADI:", se_low_adi, "\n")
cat("SE for High ADI:", se_high_adi, "\n")

summary(ICU_rate_GLMMmodel_ALPHA_6)$coefficients

#########################################################################

# Extract variance-covariance matrix
vcov_matrix <- vcov(ICU_rate_GLMMmodel_ALPHA_6)

# Variance and covariance components
var_beta1 <- vcov_matrix["`Effective Number of Center`", "`Effective Number of Center`"]  # Main effect variance
var_beta3 <- vcov_matrix["`Effective Number of Center`:ADI_categoryMore Disadvantaged", 
                         "`Effective Number of Center`:ADI_categoryMore Disadvantaged"]  # Interaction variance
cov_beta1_beta3 <- vcov_matrix["`Effective Number of Center`", 
                               "`Effective Number of Center`:ADI_categoryMore Disadvantaged"]  # Covariance

# SE for Low ADI (reference level)
se_low_adi <- sqrt(var_beta1)

# SE for High ADI (interaction-adjusted slope)
se_high_adi <- sqrt(var_beta1 + var_beta3 + 2 * cov_beta1_beta3)

# Display results
cat("SE for Low ADI (Reference Level):", se_low_adi, "\n")
cat("SE for High ADI:", se_high_adi, "\n")

summary(ICU_rate_GLMMmodel_ALPHA_6)$coefficients

#########################################################################

correlation_result <- cor.test(ICU_UPDATED_Method8_ALPHA$COVID_ICU_Rate, 
                               ICU_UPDATED_Method8_ALPHA$testcenters, 
                               method = "pearson")

correlation_coefficient <- correlation_result$estimate
p_value <- correlation_result$p.value

cat("Correlation Coefficient: ", correlation_coefficient, "\n")
cat("P-value: ", p_value, "\n")

###############################################################################
################################### DELTA WAVE ################################
###############################################################################

ICU_UPDATED_Method8_DELTA = fread(file.choose(), sep=",", header=T)

# ICU_UPDATED_Method8_DELTA$Date = as.factor(ICU_UPDATED_Method8_DELTA$Date)
# 
# ICU_UPDATED_Method8_DELTA$Date <- relevel(ICU_UPDATED_Method8_DELTA$Date, ref = "2021-08")

skewness_ICU_rate = skewness(ICU_UPDATED_Method8_DELTA$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method8_DELTA$ADI_category = as.factor(ICU_UPDATED_Method8_DELTA$ADI_category)

ICU_UPDATED_Method8_DELTA$zipcode = as.factor(ICU_UPDATED_Method8_DELTA$zipcode)


ICU_UPDATED_Method8_DELTA$ADI_category <- relevel(ICU_UPDATED_Method8_DELTA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(`COVID_ICU_Rate` ~  ADI_category *`Effective Number of Center`, data = ICU_UPDATED_Method8_DELTA)
vif(vif_model)

#ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` + 
#                                     (1 | zipcode), 
#                                   data = ICU_UPDATED_Method8_DELTA)


#tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_DELTA)

#mean_dependent_variable <- mean(ICU_UPDATED_Method8_DELTA$`COVID_ICU_Rate`, na.rm = TRUE)

#summary(ICU_rate_GLMMmodel_DELTA_2)

#tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)

############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_DELTA_2)$coefficients

# Raw coefficients (betas) and their standard errors
raw_betas <- coef_summary[, 1]
std_errors <- coef_summary[, 2]

# Exponentiate coefficients to get IRRs
exp_coefficients <- exp(raw_betas)

# Calculate 95% confidence intervals
lower_ci <- exp(raw_betas - 1.96 * std_errors)
upper_ci <- exp(raw_betas + 1.96 * std_errors)

# Create a data frame with all results
result_table <- data.frame(
  Variable = rownames(coef_summary),
  IRR = exp_coefficients,
  SE = std_errors,
  `Lower 95% CI` = lower_ci,
  `Upper 95% CI` = upper_ci
)

# Print the result table
print(result_table)

#########################################################################


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI_category (Less Dis)` +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_DELTA)

ICU_rate_GLMMmodel_DELTA_3_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI_category (More Dis)` +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_DELTA)

#mean_dependent_variable <- mean(ICU_UPDATED_Method8_DELTA$`COVID_ICU_Rate`, na.rm = TRUE)

#summary(ICU_rate_GLMMmodel_DELTA_3)

#tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)

############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5, transform = "exp")
tab_model(ICU_rate_GLMMmodel_DELTA_3_1, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_DELTA_3_1)$coefficients

# Raw coefficients (betas) and their standard errors
raw_betas <- coef_summary[, 1]
std_errors <- coef_summary[, 2]

# Exponentiate coefficients to get IRRs
exp_coefficients <- exp(raw_betas)

# Calculate 95% confidence intervals
lower_ci <- exp(raw_betas - 1.96 * std_errors)
upper_ci <- exp(raw_betas + 1.96 * std_errors)

# Create a data frame with all results
result_table <- data.frame(
  Variable = rownames(coef_summary),
  IRR = exp_coefficients,
  SE = std_errors,
  `Lower 95% CI` = lower_ci,
  `Upper 95% CI` = upper_ci
)

# Print the result table
print(result_table)

#########################################################################



#ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*`ADI_category` +
#                                     (1 | zipcode), 
#                                   data = ICU_UPDATED_Method8_DELTA)


#tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category (Less Dis)`  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_DELTA)

ICU_rate_GLMMmodel_DELTA_7 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category (More Dis)`  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_DELTA)

############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_DELTA_6, digits = 5, transform = "exp")
tab_model(ICU_rate_GLMMmodel_DELTA_7, digits = 5, transform = "exp")

# Fit the LMER model with interaction
ICU_rate_GLMMmodel_DELTA_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` * `ADI_category` +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_DELTA)

# Step 1: Use emtrends to calculate simple slopes and SEs
simple_slopes <- emtrends(ICU_rate_GLMMmodel_DELTA_6, ~ ADI_category, var = "Effective Number of Center")
print(simple_slopes)

# Step 2: Extract coefficients and variance-covariance matrix for manual SE calculation
coef_summary <- summary(ICU_rate_GLMMmodel_DELTA_6)$coefficients
vcov_matrix <- vcov(ICU_rate_GLMMmodel_DELTA_6)

# Extract variance and covariance components
var_beta1 <- vcov_matrix["`Effective Number of Center`", "`Effective Number of Center`"]  # Variance of main effect
var_beta3 <- vcov_matrix["`Effective Number of Center`:ADI_categoryMore Disadvantaged", 
                         "`Effective Number of Center`:ADI_categoryMore Disadvantaged"]  # Variance of interaction
cov_beta1_beta3 <- vcov_matrix["`Effective Number of Center`", 
                               "`Effective Number of Center`:ADI_categoryMore Disadvantaged"]  # Covariance

# Calculate SEs for Low and High ADI
se_low_adi <- sqrt(var_beta1)  # SE for Low ADI (reference level)
se_high_adi <- sqrt(var_beta1 + var_beta3 + 2 * cov_beta1_beta3)  # SE for High ADI

# Display results
cat("SE for Low ADI:", se_low_adi, "\n")
cat("SE for High ADI:", se_high_adi, "\n")

#########################################################################

#mean_dependent_variable <- mean(ICU_UPDATED_Method8_DELTA$`COVID_ICU_Rate`, na.rm = TRUE)

#summary(ICU_rate_GLMMmodel_DELTA_6)

#tab_model(ICU_rate_GLMMmodel_DELTA_6, digits = 5)


correlation_result <- cor.test(ICU_UPDATED_Method8_DELTA$COVID_ICU_Rate, 
                               ICU_UPDATED_Method8_DELTA$`Effective Number of Center`, 
                               method = "pearson")

correlation_coefficient <- correlation_result$estimate
p_value <- correlation_result$p.value

cat("Correlation Coefficient: ", correlation_coefficient, "\n")
cat("P-value: ", p_value, "\n")

###############################################################################
################################### OMICRON WAVE ##############################
###############################################################################

ICU_UPDATED_Method8_OMICRON = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method8_OMICRON$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method8_OMICRON$ADI_category = as.factor(ICU_UPDATED_Method8_OMICRON$ADI_category)

ICU_UPDATED_Method8_OMICRON$zipcode = as.factor(ICU_UPDATED_Method8_OMICRON$zipcode)


ICU_UPDATED_Method8_OMICRON$ADI_category <- relevel(ICU_UPDATED_Method8_OMICRON$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(`COVID_ICU_Rate` ~  ADI_category * `Effective Number of Center`, data = ICU_UPDATED_Method8_OMICRON)
vif(vif_model)

#ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` +
#                                       (1 | zipcode), 
#                                     data = ICU_UPDATED_Method8_OMICRON)


#tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`  +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method8_OMICRON)

#mean_dependent_variable <- mean(ICU_UPDATED_Method8_OMICRON$`COVID_ICU_Rate`, na.rm = TRUE)

#summary(ICU_rate_GLMMmodel_OMICRON_2)


#tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_OMICRON_2)$coefficients

# Raw coefficients (betas) and their standard errors
raw_betas <- coef_summary[, 1]
std_errors <- coef_summary[, 2]

# Exponentiate coefficients to get IRRs
exp_coefficients <- exp(raw_betas)

# Calculate 95% confidence intervals
lower_ci <- exp(raw_betas - 1.96 * std_errors)
upper_ci <- exp(raw_betas + 1.96 * std_errors)

# Create a data frame with all results
result_table <- data.frame(
  Variable = rownames(coef_summary),
  IRR = exp_coefficients,
  SE = std_errors,
  `Lower 95% CI` = lower_ci,
  `Upper 95% CI` = upper_ci
)

# Print the result table
print(result_table)

#########################################################################



ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI_category (Less Dis)`  +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method8_OMICRON)

ICU_rate_GLMMmodel_OMICRON_3_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI_category (More Dis)`  +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method8_OMICRON)

#mean_dependent_variable <- mean(ICU_UPDATED_Method8_OMICRON$`COVID_ICU_Rate`, na.rm = TRUE)

#summary(ICU_rate_GLMMmodel_OMICRON_3)

#tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)

############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5, transform = "exp")
tab_model(ICU_rate_GLMMmodel_OMICRON_3_1, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_OMICRON_3_1)$coefficients

# Raw coefficients (betas) and their standard errors
raw_betas <- coef_summary[, 1]
std_errors <- coef_summary[, 2]

# Exponentiate coefficients to get IRRs
exp_coefficients <- exp(raw_betas)

# Calculate 95% confidence intervals
lower_ci <- exp(raw_betas - 1.96 * std_errors)
upper_ci <- exp(raw_betas + 1.96 * std_errors)

# Create a data frame with all results
result_table <- data.frame(
  Variable = rownames(coef_summary),
  IRR = exp_coefficients,
  SE = std_errors,
  `Lower 95% CI` = lower_ci,
  `Upper 95% CI` = upper_ci
)

# Print the result table
print(result_table)

#########################################################################


#ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*`ADI_category`  +
#                                       (1 | zipcode), 
#                                     data = ICU_UPDATED_Method8_OMICRON)


#tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category (Less Dis)`  +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method8_OMICRON)

ICU_rate_GLMMmodel_OMICRON_7 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category (More Dis)`  +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method8_OMICRON)

#mean_dependent_variable <- mean(ICU_UPDATED_Method8_OMICRON$`COVID_ICU_Rate`, na.rm = TRUE)

#summary(ICU_rate_GLMMmodel_OMICRON_6)


#tab_model(ICU_rate_GLMMmodel_OMICRON_6, digits = 5)


############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_OMICRON_6, digits = 5, transform = "exp")
tab_model(ICU_rate_GLMMmodel_OMICRON_7, digits = 5, transform = "exp")

# Fit the LMER model with interaction
ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` * `ADI_category` +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_OMICRON)

# Step 1: Use emtrends to calculate simple slopes and SEs
simple_slopes <- emtrends(ICU_rate_GLMMmodel_OMICRON_6, ~ ADI_category, var = "Effective Number of Center")
print(simple_slopes)

# Step 2: Extract coefficients and variance-covariance matrix for manual SE calculation
coef_summary <- summary(ICU_rate_GLMMmodel_OMICRON_6)$coefficients
vcov_matrix <- vcov(ICU_rate_GLMMmodel_OMICRON_6)

# Extract variance and covariance components
var_beta1 <- vcov_matrix["`Effective Number of Center`", "`Effective Number of Center`"]  # Variance of main effect
var_beta3 <- vcov_matrix["`Effective Number of Center`:ADI_categoryMore Disadvantaged", 
                         "`Effective Number of Center`:ADI_categoryMore Disadvantaged"]  # Variance of interaction
cov_beta1_beta3 <- vcov_matrix["`Effective Number of Center`", 
                               "`Effective Number of Center`:ADI_categoryMore Disadvantaged"]  # Covariance

# Calculate SEs for Low and High ADI
se_low_adi <- sqrt(var_beta1)  # SE for Low ADI (reference level)
se_high_adi <- sqrt(var_beta1 + var_beta3 + 2 * cov_beta1_beta3)  # SE for High ADI

# Display results
cat("SE for Low ADI:", se_low_adi, "\n")
cat("SE for High ADI:", se_high_adi, "\n")

#########################################################################



correlation_result <- cor.test(ICU_UPDATED_Method8_OMICRON$COVID_ICU_Rate, 
                               ICU_UPDATED_Method8_OMICRON$`Effective Number of Center`, 
                               method = "pearson")

correlation_coefficient <- correlation_result$estimate
p_value <- correlation_result$p.value

cat("Correlation Coefficient: ", correlation_coefficient, "\n")
cat("P-value: ", p_value, "\n")

###############################################################################

# ICU_UPDATED_Method8_WithOutNA_ALPHA = fread(file.choose(), sep=",", header=T)
# 
# skewness_ICU_rate = skewness(ICU_UPDATED_Method8_WithOutNA_ALPHA$COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# ICU_UPDATED_Method8_WithOutNA_ALPHA$ADI_category = as.factor(ICU_UPDATED_Method8_WithOutNA_ALPHA$ADI_category)
# 
# ICU_UPDATED_Method8_WithOutNA_ALPHA$zipcode = as.factor(ICU_UPDATED_Method8_WithOutNA_ALPHA$zipcode)
# 
# 
# ICU_UPDATED_Method8_WithOutNA_ALPHA$ADI_category <- relevel(ICU_UPDATED_Method8_WithOutNA_ALPHA$ADI_category, ref = "Less Disadvantaged")
# 
# 
# vif_model <- lm(`COVID_ICU_Rate` ~  `ADI_category` + `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_WithOutNA_ALPHA)
# vif(vif_model)
# 
# ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_ALPHA)
# 
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_ALPHA)
# 
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_ALPHA)
# 
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_ALPHA)
# 
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_ALPHA)
# 
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_6, digits = 5)
# 
# ###############################################################################
# 
# ICU_UPDATED_Method8_WithOutNA_DELTA = fread(file.choose(), sep=",", header=T)
# 
# skewness_ICU_rate = skewness(ICU_UPDATED_Method8_WithOutNA_DELTA$COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# ICU_UPDATED_Method8_WithOutNA_DELTA$ADI_category = as.factor(ICU_UPDATED_Method8_WithOutNA_DELTA$ADI_category)
# 
# ICU_UPDATED_Method8_WithOutNA_DELTA$zipcode = as.factor(ICU_UPDATED_Method8_WithOutNA_DELTA$zipcode)
# 
# 
# ICU_UPDATED_Method8_WithOutNA_DELTA$ADI_category <- relevel(ICU_UPDATED_Method8_WithOutNA_DELTA$ADI_category, ref = "Less Disadvantaged")
# 
# 
# vif_model <- lm(`COVID_ICU_Rate` ~  `ADI_category` + `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_WithOutNA_DELTA)
# vif(vif_model)
# 
# ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_DELTA)
# 
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_DELTA)
# 
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_DELTA)
# 
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_DELTA)
# 
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_DELTA)
# 
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_6, digits = 5)
# 
# ###############################################################################
# 
# ICU_UPDATED_Method8_WithOutNA_OMICRON = fread(file.choose(), sep=",", header=T)
# 
# skewness_ICU_rate = skewness(ICU_UPDATED_Method8_WithOutNA_OMICRON$COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# ICU_UPDATED_Method8_WithOutNA_OMICRON$ADI_category = as.factor(ICU_UPDATED_Method8_WithOutNA_OMICRON$ADI_category)
# 
# ICU_UPDATED_Method8_WithOutNA_OMICRON$zipcode = as.factor(ICU_UPDATED_Method8_WithOutNA_OMICRON$zipcode)
# 
# 
# ICU_UPDATED_Method8_WithOutNA_OMICRON$ADI_category <- relevel(ICU_UPDATED_Method8_WithOutNA_OMICRON$ADI_category, ref = "Less Disadvantaged")
# 
# 
# vif_model <- lm(`COVID_ICU_Rate` ~  `ADI_category` + `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_WithOutNA_OMICRON)
# vif(vif_model)
# 
# ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = ICU_UPDATED_Method8_WithOutNA_OMICRON)
# 
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = ICU_UPDATED_Method8_WithOutNA_OMICRON)
# 
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI_category` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = ICU_UPDATED_Method8_WithOutNA_OMICRON)
# 
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = ICU_UPDATED_Method8_WithOutNA_OMICRON)
# 
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = ICU_UPDATED_Method8_WithOutNA_OMICRON)
# 
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_6, digits = 5)
