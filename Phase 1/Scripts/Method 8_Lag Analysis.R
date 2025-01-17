###############################################################################
################################### LAG ANALYSIS ##############################
################################## ONE MONTH ##################################
################################### ALPHA WAVE ################################
###############################################################################

library(data.table)
library(dplyr)
library(lubridate)
library(e1071)
library(car)
library(lme4)
library(sjPlot)

Lag_Analysis_Mehtod8_ALPHA_OneMonth = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod8_ALPHA_OneMonth <- Lag_Analysis_Mehtod8_ALPHA_OneMonth %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_COVID_ICU_Rate = lag(COVID_ICU_Rate, 1)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod8_ALPHA_OneMonth = na.omit(Lag_Analysis_Mehtod8_ALPHA_OneMonth)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_ALPHA_OneMonth$Lag1_COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod8_ALPHA_OneMonth$ADI_category = as.factor(Lag_Analysis_Mehtod8_ALPHA_OneMonth$ADI_category)

Lag_Analysis_Mehtod8_ALPHA_OneMonth$zipcode = as.factor(Lag_Analysis_Mehtod8_ALPHA_OneMonth$zipcode)


Lag_Analysis_Mehtod8_ALPHA_OneMonth$ADI_category <- relevel(Lag_Analysis_Mehtod8_ALPHA_OneMonth$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_COVID_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod8_ALPHA_OneMonth)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `testcenters`  +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_ALPHA_OneMonth)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `Effective Number of Center`  +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_ALPHA_OneMonth)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `ADI_category` +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_ALPHA_OneMonth)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `testcenters`*`ADI_category`  +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_ALPHA_OneMonth)

tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category`  +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_ALPHA_OneMonth)

tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)


###############################################################################
################################### DELTA WAVE ################################
###############################################################################

Lag_Analysis_Mehtod8_DELTA_OneMonth = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod8_DELTA_OneMonth <- Lag_Analysis_Mehtod8_DELTA_OneMonth %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_COVID_ICU_Rate = lag(COVID_ICU_Rate, 1)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod8_DELTA_OneMonth = na.omit(Lag_Analysis_Mehtod8_DELTA_OneMonth)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_DELTA_OneMonth$Lag1_COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod8_DELTA_OneMonth$ADI_category = as.factor(Lag_Analysis_Mehtod8_DELTA_OneMonth$ADI_category)

Lag_Analysis_Mehtod8_DELTA_OneMonth$zipcode = as.factor(Lag_Analysis_Mehtod8_DELTA_OneMonth$zipcode)


Lag_Analysis_Mehtod8_DELTA_OneMonth$ADI_category <- relevel(Lag_Analysis_Mehtod8_DELTA_OneMonth$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_COVID_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod8_DELTA_OneMonth)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `testcenters`  +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_DELTA_OneMonth)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `Effective Number of Center` +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_DELTA_OneMonth)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `ADI_category`  +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_DELTA_OneMonth)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `testcenters`*`ADI_category`  +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_DELTA_OneMonth)

tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category`  +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_DELTA_OneMonth)

tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)


###############################################################################
################################### OMICRON WAVE ##############################
###############################################################################

Lag_Analysis_Mehtod8_OMICRON_OneMonth = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod8_OMICRON_OneMonth <- Lag_Analysis_Mehtod8_OMICRON_OneMonth %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_COVID_ICU_Rate = lag(COVID_ICU_Rate, 1)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod8_OMICRON_OneMonth = na.omit(Lag_Analysis_Mehtod8_OMICRON_OneMonth)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_OMICRON_OneMonth$Lag1_COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod8_OMICRON_OneMonth$ADI_category = as.factor(Lag_Analysis_Mehtod8_OMICRON_OneMonth$ADI_category)

Lag_Analysis_Mehtod8_OMICRON_OneMonth$zipcode = as.factor(Lag_Analysis_Mehtod8_OMICRON_OneMonth$zipcode)


Lag_Analysis_Mehtod8_OMICRON_OneMonth$ADI_category <- relevel(Lag_Analysis_Mehtod8_OMICRON_OneMonth$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_COVID_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod8_OMICRON_OneMonth)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `testcenters`  +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod8_OMICRON_OneMonth)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `Effective Number of Center`  +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod8_OMICRON_OneMonth)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `ADI_category`  +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod8_OMICRON_OneMonth)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `testcenters`*`ADI_category`  +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod8_OMICRON_OneMonth)

tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category`  +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod8_OMICRON_OneMonth)

tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)


# ###############################################################################
# ################################### LAG ANALYSIS ##############################
# ################################## WITHOUT NA #################################
# ################################## ONE MONTH ##################################
# ################################### ALPHA WAVE ################################
# ###############################################################################
# 
# Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA = fread(file.choose(), sep=",", header=T)
# 
# Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA <- Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA %>%
#   arrange(Date) %>%
#   group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
#   mutate(
#     Lag1_COVID_ICU_Rate = lag(COVID_ICU_Rate, 1)
#   ) %>%
#   ungroup()
# 
# Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA = na.omit(Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA)
# 
# 
# skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA$Lag1_COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# 
# Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA$ADI_category = as.factor(Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA$ADI_category)
# 
# Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA$zipcode = as.factor(Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA$zipcode)
# 
# 
# Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA$ADI_category <- relevel(Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA$ADI_category, ref = "Less Disadvantaged")
# 
# 
# vif_model <- lm(Lag1_COVID_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA)
# vif(vif_model)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_ALPHA_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)
# 
# 
# ###############################################################################
# ################################### DELTA WAVE ################################
# ###############################################################################
# 
# Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA = fread(file.choose(), sep=",", header=T)
# 
# Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA <- Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA %>%
#   arrange(Date) %>%
#   group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
#   mutate(
#     Lag1_COVID_ICU_Rate = lag(COVID_ICU_Rate, 1)
#   ) %>%
#   ungroup()
# 
# Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA = na.omit(Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA)
# 
# 
# skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA$Lag1_COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# 
# Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA$ADI_category = as.factor(Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA$ADI_category)
# 
# Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA$zipcode = as.factor(Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA$zipcode)
# 
# 
# Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA$ADI_category <- relevel(Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA$ADI_category, ref = "Less Disadvantaged")
# 
# 
# vif_model <- lm(Lag1_COVID_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA)
# vif(vif_model)
# 
# 
# ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_DELTA_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)
# 
# 
# ###############################################################################
# ################################### OMICRON WAVE ##############################
# ###############################################################################
# 
# Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA = fread(file.choose(), sep=",", header=T)
# 
# Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA <- Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA %>%
#   arrange(Date) %>%
#   group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
#   mutate(
#     Lag1_COVID_ICU_Rate = lag(COVID_ICU_Rate, 1)
#   ) %>%
#   ungroup()
# 
# Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA = na.omit(Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA)
# 
# 
# skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA$Lag1_COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# 
# Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA$ADI_category = as.factor(Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA$ADI_category)
# 
# Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA$zipcode = as.factor(Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA$zipcode)
# 
# 
# Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA$ADI_category <- relevel(Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA$ADI_category, ref = "Less Disadvantaged")
# 
# 
# vif_model <- lm(Lag1_COVID_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA)
# vif(vif_model)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag1_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = Lag_Analysis_Mehtod8_OMICRON_OneMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)
# 


###############################################################################
################################### LAG ANALYSIS ##############################
################################## TWO MONTH ##################################
################################### ALPHA WAVE ################################
###############################################################################

Lag_Analysis_Mehtod8_ALPHA_TwoMonth = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod8_ALPHA_TwoMonth <- Lag_Analysis_Mehtod8_ALPHA_TwoMonth %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_COVID_ICU_Rate = lag(COVID_ICU_Rate, 2)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod8_ALPHA_TwoMonth = na.omit(Lag_Analysis_Mehtod8_ALPHA_TwoMonth)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_ALPHA_TwoMonth$Lag2_COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod8_ALPHA_TwoMonth$ADI_category = as.factor(Lag_Analysis_Mehtod8_ALPHA_TwoMonth$ADI_category)

Lag_Analysis_Mehtod8_ALPHA_TwoMonth$zipcode = as.factor(Lag_Analysis_Mehtod8_ALPHA_TwoMonth$zipcode)


Lag_Analysis_Mehtod8_ALPHA_TwoMonth$ADI_category <- relevel(Lag_Analysis_Mehtod8_ALPHA_TwoMonth$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_COVID_ICU_Rate ~ ADI_category * `Effective Number of Center`, data = Lag_Analysis_Mehtod8_ALPHA_TwoMonth)
vif(vif_model)


#ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `testcenters` +
#                                     (1 | zipcode), 
 #                                  data = Lag_Analysis_Mehtod8_ALPHA_TwoMonth)

#tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center` +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_ALPHA_TwoMonth)

#mean_dependent_variable <- mean(Lag_Analysis_Mehtod8_ALPHA_TwoMonth$`COVID_ICU_Rate`, na.rm = TRUE)

#summary(ICU_rate_GLMMmodel_ALPHA_2)

#tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)

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

#########################################################################


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `ADI_category (Less Dis)` +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_ALPHA_TwoMonth)

ICU_rate_GLMMmodel_ALPHA_3_1 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `ADI_category (More Dis)` +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_ALPHA_TwoMonth)

#mean_dependent_variable <- mean(Lag_Analysis_Mehtod8_ALPHA_TwoMonth$`COVID_ICU_Rate`, na.rm = TRUE)

#summary(ICU_rate_GLMMmodel_ALPHA_3)

#tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


############################## NEW CODE #####################################


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

#########################################################################


ICU_rate_GLMMmodel_ALPHA_4_1 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category (More Dis)` + 
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_ALPHA_TwoMonth)

#tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)



#mean_dependent_variable <- mean(Lag_Analysis_Mehtod8_ALPHA_TwoMonth$`COVID_ICU_Rate`, na.rm = TRUE)

#summary(ICU_rate_GLMMmodel_ALPHA_5)

#tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)


############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_ALPHA_4_1, digits = 5, transform = "exp")

# Load required packages
install.packages("emmeans")  # If not already installed
library(emmeans)

# Fit the LMER model with interaction
ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` * `ADI_category` +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_ALPHA_TwoMonth)

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


#########################################################################

###############################################################################
################################### DELTA WAVE ################################
###############################################################################


Lag_Analysis_Mehtod8_DELTA_TwoMonth = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod8_DELTA_TwoMonth <- Lag_Analysis_Mehtod8_DELTA_TwoMonth %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_COVID_ICU_Rate = lag(COVID_ICU_Rate, 2)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod8_DELTA_TwoMonth = na.omit(Lag_Analysis_Mehtod8_DELTA_TwoMonth)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_DELTA_TwoMonth$Lag2_COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod8_DELTA_TwoMonth$ADI_category = as.factor(Lag_Analysis_Mehtod8_DELTA_TwoMonth$ADI_category)

Lag_Analysis_Mehtod8_DELTA_TwoMonth$zipcode = as.factor(Lag_Analysis_Mehtod8_DELTA_TwoMonth$zipcode)


Lag_Analysis_Mehtod8_DELTA_TwoMonth$ADI_category <- relevel(Lag_Analysis_Mehtod8_DELTA_TwoMonth$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_COVID_ICU_Rate ~ ADI_category * `Effective Number of Center`, data = Lag_Analysis_Mehtod8_DELTA_TwoMonth)
vif(vif_model)


#ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `testcenters`  +
#                                     (1 | zipcode), 
#                                   data = Lag_Analysis_Mehtod8_DELTA_TwoMonth)

#tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center`  +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_DELTA_TwoMonth)

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

#mean_dependent_variable <- mean(Lag_Analysis_Mehtod8_DELTA_TwoMonth$`COVID_ICU_Rate`, na.rm = TRUE)

#summary(ICU_rate_GLMMmodel_DELTA_2)

#tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `ADI_category (Less Dis)`  +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_DELTA_TwoMonth)

ICU_rate_GLMMmodel_DELTA_3_1 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `ADI_category (More Dis)`  +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_DELTA_TwoMonth)

#mean_dependent_variable <- mean(Lag_Analysis_Mehtod8_DELTA_TwoMonth$`COVID_ICU_Rate`, na.rm = TRUE)

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


#ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `testcenters`*`ADI_category`  +
#                                     (1 | zipcode), 
#                                   data = Lag_Analysis_Mehtod8_DELTA_TwoMonth)

#tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category (Less Dis)`  +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_DELTA_TwoMonth)

ICU_rate_GLMMmodel_DELTA_5_1 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category (More Dis)`  +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_DELTA_TwoMonth)

#mean_dependent_variable <- mean(Lag_Analysis_Mehtod8_DELTA_TwoMonth$`COVID_ICU_Rate`, na.rm = TRUE)

#summary(ICU_rate_GLMMmodel_DELTA_5)


#tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)


############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5, transform = "exp")
tab_model(ICU_rate_GLMMmodel_DELTA_5_1, digits = 5, transform = "exp")

# Extract coefficients and standard errors
coef_summary <- summary(ICU_rate_GLMMmodel_DELTA_5_1)$coefficients

# Fit the LMER model with interaction
ICU_rate_GLMMmodel_DELTA_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` * `ADI_category` +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_DELTA_TwoMonth)

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




###############################################################################
################################### OMICRON WAVE ##############################
###############################################################################

Lag_Analysis_Mehtod8_OMICRON_TwoMonth = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod8_OMICRON_TwoMonth <- Lag_Analysis_Mehtod8_OMICRON_TwoMonth %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_COVID_ICU_Rate = lag(COVID_ICU_Rate, 2)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod8_OMICRON_TwoMonth = na.omit(Lag_Analysis_Mehtod8_OMICRON_TwoMonth)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_OMICRON_TwoMonth$Lag2_COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod8_OMICRON_TwoMonth$ADI_category = as.factor(Lag_Analysis_Mehtod8_OMICRON_TwoMonth$ADI_category)

Lag_Analysis_Mehtod8_OMICRON_TwoMonth$zipcode = as.factor(Lag_Analysis_Mehtod8_OMICRON_TwoMonth$zipcode)


Lag_Analysis_Mehtod8_OMICRON_TwoMonth$ADI_category <- relevel(Lag_Analysis_Mehtod8_OMICRON_TwoMonth$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_COVID_ICU_Rate ~ ADI_category * `Effective Number of Center`, data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth)
vif(vif_model)


#ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `testcenters`  +
#                                       (1 | zipcode), 
#                                     data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth)

#tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center` +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth)


#mean_dependent_variable <- mean(Lag_Analysis_Mehtod8_OMICRON_TwoMonth$`COVID_ICU_Rate`, na.rm = TRUE)

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


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `ADI_category (Less Dis)`  +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth)

ICU_rate_GLMMmodel_OMICRON_3_1 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `ADI_category (More Dis)`  +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth)

#mean_dependent_variable <- mean(Lag_Analysis_Mehtod8_OMICRON_TwoMonth$`COVID_ICU_Rate`, na.rm = TRUE)

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


#ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `testcenters`*`ADI_category`  +
#                                       (1 | zipcode), 
#                                     data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth)

#tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category (Less Dis)`  +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth)

ICU_rate_GLMMmodel_OMICRON_5_1 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category (More Dis)`  +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth)

#mean_dependent_variable <- mean(Lag_Analysis_Mehtod8_OMICRON_TwoMonth$`COVID_ICU_Rate`, na.rm = TRUE)

#summary(ICU_rate_GLMMmodel_OMICRON_5)

#tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)

############################## NEW CODE #####################################


tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5, transform = "exp")
tab_model(ICU_rate_GLMMmodel_OMICRON_5_1, digits = 5, transform = "exp")


# Fit the LMER model with interaction
ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` * `ADI_category` +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth)

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


# ###############################################################################
# ################################### LAG ANALYSIS ##############################
# ################################## WITHOUT NA #################################
# ################################## TWO MONTH ##################################
# ################################### ALPHA WAVE ################################
# ###############################################################################
# 
# Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA = fread(file.choose(), sep=",", header=T)
# 
# Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA <- Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA %>%
#   arrange(Date) %>%
#   group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
#   mutate(
#     Lag2_COVID_ICU_Rate = lag(COVID_ICU_Rate, 2)
#   ) %>%
#   ungroup()
# 
# Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA = na.omit(Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA)
# 
# 
# skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA$Lag2_COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# 
# Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA$ADI_category = as.factor(Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA$ADI_category)
# 
# Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA$zipcode = as.factor(Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA$zipcode)
# 
# 
# Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA$ADI_category <- relevel(Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA$ADI_category, ref = "Less Disadvantaged")
# 
# 
# vif_model <- lm(Lag2_COVID_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA)
# vif(vif_model)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_ALPHA_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)
# 
# 
# ###############################################################################
# ################################### DELTA WAVE ################################
# ###############################################################################
# 
# Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA = fread(file.choose(), sep=",", header=T)
# 
# Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA <- Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA %>%
#   arrange(Date) %>%
#   group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
#   mutate(
#     Lag2_COVID_ICU_Rate = lag(COVID_ICU_Rate, 2)
#   ) %>%
#   ungroup()
# 
# Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA = na.omit(Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA)
# 
# 
# skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA$Lag2_COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# 
# Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA$ADI_category = as.factor(Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA$ADI_category)
# 
# Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA$zipcode = as.factor(Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA$zipcode)
# 
# 
# Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA$ADI_category <- relevel(Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA$ADI_category, ref = "Less Disadvantaged")
# 
# 
# vif_model <- lm(Lag2_COVID_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA)
# vif(vif_model)
# 
# 
# ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = Lag_Analysis_Mehtod8_DELTA_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)
# 
# 
# ###############################################################################
# ################################### OMICRON WAVE ##############################
# ###############################################################################
# 
# Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA = fread(file.choose(), sep=",", header=T)
# 
# Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA <- Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA %>%
#   arrange(Date) %>%
#   group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
#   mutate(
#     Lag2_COVID_ICU_Rate = lag(COVID_ICU_Rate, 2)
#   ) %>%
#   ungroup()
# 
# Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA = na.omit(Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA)
# 
# 
# skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA$Lag2_COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# 
# Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA$ADI_category = as.factor(Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA$ADI_category)
# 
# Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA$zipcode = as.factor(Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA$zipcode)
# 
# 
# Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA$ADI_category <- relevel(Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA$ADI_category, ref = "Less Disadvantaged")
# 
# 
# vif_model <- lm(Lag2_COVID_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA)
# vif(vif_model)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag2_COVID_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = Lag_Analysis_Mehtod8_OMICRON_TwoMonth_WithOutNA)
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)
