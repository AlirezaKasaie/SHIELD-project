library(e1071)
install.packages("car")
library(car)
library(lme4)
library(sjPlot)
############################################# ALPHA WAVE ######################################################

############################################# One-Month Lag ######################################################

ICU_UPDATED_Zipcode_ALPHA_OneMonthLag = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_ALPHA_OneMonthLag <- ICU_UPDATED_Zipcode_ALPHA_OneMonthLag %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 1), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 1)
  ) %>%
  ungroup()

ICU_UPDATED_Zipcode_ALPHA_OneMonthLag = na.omit(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag)


write.csv(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_ALPHA_OneMonthLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_ALPHA_OneMonthLag$ADI.category = as.factor(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag$ADI.category)

ICU_UPDATED_Zipcode_ALPHA_OneMonthLag$zipcode = as.factor(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag$zipcode)


ICU_UPDATED_Zipcode_ALPHA_OneMonthLag$ADI.category <- relevel(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)



ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)


ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_ALPHA_6, digits = 5)


vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Alpha_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_1, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_2, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_3, digits = 5)


ICU_UPDATED_Zipcode_ALPHA_OneMonthLag$SamplesCollected <- scale(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag$SamplesCollected)


ICU_hospitalization_GLMMmodel_Alpha_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_4, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_5, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_6, digits = 5)



############################################# Two-Month Lag ######################################################

library(dplyr)

ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag <- ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(total_patients_per_zipcode_per_month, 2), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(COVID_ICU_Rate, 2)
  ) %>%
  ungroup()


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag <- na.omit(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag)

write.csv(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag$ADI.category = as.factor(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag$ADI.category)

ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag$zipcode = as.factor(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag$zipcode)


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag$ADI.category <- relevel(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag)
vif(vif_model)


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag = read.csv(file.choose(), sep=",", header=T)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective.Number.of.Center` +
                                     (1 | zipcode), 
                                  data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag)

############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_ALPHA_1)$coefficients

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

#tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI_category..Less.Dis.  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag)

ICU_rate_GLMMmodel_ALPHA_2_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI_category..More.Dis.  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag)

#tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)

############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_ALPHA_2_1, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_ALPHA_2_1)$coefficients

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


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective.Number.of.Center`*ADI_category..Less.Dis. +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag)

ICU_rate_GLMMmodel_ALPHA_3_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective.Number.of.Center`*ADI_category..More.Dis. +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag)

#tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_ALPHA_3_1, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_ALPHA_2_1)$coefficients

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


############################################# DELTA WAVE ######################################################

############################################# One-Month Lag ######################################################


ICU_UPDATED_Zipcode_DELTA_OneMonthLag = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_DELTA_OneMonthLag <- ICU_UPDATED_Zipcode_DELTA_OneMonthLag %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 1), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 1)
  ) %>%
  ungroup()



ICU_UPDATED_Zipcode_DELTA_OneMonthLag <- na.omit(ICU_UPDATED_Zipcode_DELTA_OneMonthLag)

write.csv(ICU_UPDATED_Zipcode_DELTA_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_DELTA_OneMonthLag.csv", row.names = FALSE)



skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_DELTA_OneMonthLag$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_DELTA_OneMonthLag$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_DELTA_OneMonthLag$ADI.category = as.factor(ICU_UPDATED_Zipcode_DELTA_OneMonthLag$ADI.category)

ICU_UPDATED_Zipcode_DELTA_OneMonthLag$zipcode = as.factor(ICU_UPDATED_Zipcode_DELTA_OneMonthLag$zipcode)


ICU_UPDATED_Zipcode_DELTA_OneMonthLag$ADI.category <- relevel(ICU_UPDATED_Zipcode_DELTA_OneMonthLag$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)



ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


ICU_UPDATED_Zipcode_DELTA_OneMonthLag$SamplesCollected <- scale(ICU_UPDATED_Zipcode_DELTA_OneMonthLag$SamplesCollected)


ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)


ICU_rate_GLMMmodel_DELTA_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_DELTA_6, digits = 5)



vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Delta_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_1, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_2, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_3, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_4, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_5, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_6, digits = 5)


############################################# Two-Month Lag ######################################################


ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag <- ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(total_patients_per_zipcode_per_month, 2), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(COVID_ICU_Rate, 2)
  ) %>%
  ungroup()


ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag <- na.omit(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag)

write.csv(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag$ADI.category = as.factor(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag$ADI.category)

ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag$zipcode = as.factor(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag$zipcode)


ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag$ADI.category <- relevel(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective.Number.of.Center` +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag)


#tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)

############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_DELTA_1)$coefficients

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


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI_category..Less.Dis. +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag)

ICU_rate_GLMMmodel_DELTA_2_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI_category..More.Dis. +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag)

#tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)

############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_DELTA_2_1, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_DELTA_2_1)$coefficients

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


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective.Number.of.Center`*ADI_category..Less.Dis. +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag)

ICU_rate_GLMMmodel_DELTA_3_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective.Number.of.Center`*ADI_category..More.Dis. +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag)

#tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)

############################# NEW CODE #####################################


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



############################################# OMICRON WAVE ######################################################

############################################# One-Month Lag ######################################################


ICU_UPDATED_Zipcode_OMICRON_OneMonthLag = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_OMICRON_OneMonthLag <- ICU_UPDATED_Zipcode_OMICRON_OneMonthLag %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 1), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 1)
  ) %>%
  ungroup()



ICU_UPDATED_Zipcode_OMICRON_OneMonthLag <- na.omit(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag)

write.csv(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_OMICRON_OneMonthLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_OMICRON_OneMonthLag$ADI.category = as.factor(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag$ADI.category)

ICU_UPDATED_Zipcode_OMICRON_OneMonthLag$zipcode = as.factor(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag$zipcode)


ICU_UPDATED_Zipcode_OMICRON_OneMonthLag$ADI.category <- relevel(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


ICU_UPDATED_Zipcode_OMICRON_OneMonthLag$SamplesCollected <- scale(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag$SamplesCollected)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)


ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag)

tab_model(ICU_rate_GLMMmodel_OMICRON_6, digits = 5)


vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Omicron_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_1, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_2, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_3, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_4, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_5, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_6, digits = 5)


############################################# Two-Month Lag ######################################################


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag <- ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(total_patients_per_zipcode_per_month, 2), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(COVID_ICU_Rate, 2)
  ) %>%
  ungroup()


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag <- na.omit(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag)

write.csv(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag$ADI.category = as.factor(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag$ADI.category)

ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag$zipcode = as.factor(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag$zipcode)


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag$ADI.category <- relevel(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective.Number.of.Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag)

#tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


############################# NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_OMICRON_1)$coefficients

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

ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI_category..Less.Dis. + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag)

ICU_rate_GLMMmodel_OMICRON_2_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI_category..More.Dis. + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag)


#tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)

############################# NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_OMICRON_2_1, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_OMICRON_2_1)$coefficients

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


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective.Number.of.Center` * ADI_category..Less.Dis. + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag)

ICU_rate_GLMMmodel_OMICRON_3_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective.Number.of.Center` * ADI_category..More.Dis. + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag)


#tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


############################# NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_OMICRON_3_2, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_OMICRON_3)$coefficients

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


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag$SamplesCollected <- scale(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag$SamplesCollected)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag)

tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag)

tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)


ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag)

tab_model(ICU_rate_GLMMmodel_OMICRON_6, digits = 5)


vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Omicron_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_1, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_2, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_3, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_4, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_5, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_6, digits = 5)

