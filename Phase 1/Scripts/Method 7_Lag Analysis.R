###############################################################################
################################### LAG ANALYSIS ##############################
################################## WITH NA ####################################
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

Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA <- Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_ICU_Rate = lag(ICU_Rate, 1)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA = na.omit(Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA)


#write.csv(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_ALPHA_OneMonthLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA$Lag1_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA$ADI_category = as.factor(Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA$ADI_category)

Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA$zipcode = as.factor(Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA$zipcode)


Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA$ADI_category <- relevel(Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag1_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag1_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag1_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Lag1_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Lag1_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)


###############################################################################
################################### DELTA WAVE ################################
###############################################################################

Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA <- Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_ICU_Rate = lag(ICU_Rate, 1)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA = na.omit(Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA$Lag1_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA$ADI_category = as.factor(Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA$ADI_category)

Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA$zipcode = as.factor(Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA$zipcode)


Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA$ADI_category <- relevel(Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag1_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag1_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag1_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Lag1_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Lag1_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)


###############################################################################
################################### OMICRON WAVE ##############################
###############################################################################

Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA <- Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_ICU_Rate = lag(ICU_Rate, 1)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA = na.omit(Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA$Lag1_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA$ADI_category = as.factor(Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA$ADI_category)

Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA$zipcode = as.factor(Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA$zipcode)


Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA$ADI_category <- relevel(Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag1_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag1_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag1_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag1_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag1_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)


###############################################################################
################################### LAG ANALYSIS ##############################
################################## WITHOUT NA #################################
################################## ONE MONTH ##################################
################################### ALPHA WAVE ################################
###############################################################################

Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA <- Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_ICU_Rate = lag(ICU_Rate, 1)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA = na.omit(Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA$Lag1_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA$ADI_category = as.factor(Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA$ADI_category)

Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA$zipcode = as.factor(Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA$zipcode)


Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA$ADI_category <- relevel(Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag1_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag1_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag1_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Lag1_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Lag1_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)


###############################################################################
################################### DELTA WAVE ################################
###############################################################################

Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA <- Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_ICU_Rate = lag(ICU_Rate, 1)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA = na.omit(Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA$Lag1_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA$ADI_category = as.factor(Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA$ADI_category)

Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA$zipcode = as.factor(Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA$zipcode)


Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA$ADI_category <- relevel(Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag1_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag1_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag1_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Lag1_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Lag1_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)


###############################################################################
################################### OMICRON WAVE ##############################
###############################################################################

Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA <- Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_ICU_Rate = lag(ICU_Rate, 1)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA = na.omit(Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA$Lag1_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA$ADI_category = as.factor(Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA$ADI_category)

Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA$zipcode = as.factor(Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA$zipcode)


Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA$ADI_category <- relevel(Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag1_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag1_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag1_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag1_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag1_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod7_OMICRON_OneMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)



###############################################################################
################################### LAG ANALYSIS ##############################
################################## WITH NA ####################################
################################## TWO MONTH ##################################
################################### ALPHA WAVE ################################
###############################################################################

Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA <- Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_ICU_Rate = lag(ICU_Rate, 2)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA = na.omit(Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA$Lag2_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA$ADI_category = as.factor(Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA$ADI_category)

Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA$zipcode = as.factor(Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA$zipcode)


Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA$ADI_category <- relevel(Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag2_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag2_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag2_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Lag2_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Lag2_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)


###############################################################################
################################### DELTA WAVE ################################
###############################################################################


Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA <- Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_ICU_Rate = lag(ICU_Rate, 2)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA = na.omit(Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA$Lag2_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA$ADI_category = as.factor(Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA$ADI_category)

Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA$zipcode = as.factor(Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA$zipcode)


Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA$ADI_category <- relevel(Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag2_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag2_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag2_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Lag2_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Lag2_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)



###############################################################################
################################### OMICRON WAVE ##############################
###############################################################################

Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA <- Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_ICU_Rate = lag(ICU_Rate, 2)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA = na.omit(Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA$Lag2_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA$ADI_category = as.factor(Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA$ADI_category)

Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA$zipcode = as.factor(Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA$zipcode)


Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA$ADI_category <- relevel(Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag2_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag2_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag2_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag2_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag2_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)


###############################################################################
################################### LAG ANALYSIS ##############################
################################## WITHOUT NA #################################
################################## TWO MONTH ##################################
################################### ALPHA WAVE ################################
###############################################################################

Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA <- Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_ICU_Rate = lag(ICU_Rate, 2)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA = na.omit(Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA$Lag2_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA$ADI_category = as.factor(Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA$ADI_category)

Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA$zipcode = as.factor(Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA$zipcode)


Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA$ADI_category <- relevel(Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag2_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag2_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag2_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Lag2_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Lag2_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_ALPHA_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)


###############################################################################
################################### DELTA WAVE ################################
###############################################################################

Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA <- Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_ICU_Rate = lag(ICU_Rate, 2)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA = na.omit(Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA$Lag2_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA$ADI_category = as.factor(Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA$ADI_category)

Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA$zipcode = as.factor(Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA$zipcode)


Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA$ADI_category <- relevel(Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag2_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag2_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag2_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Lag2_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Lag2_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Lag_Analysis_Mehtod7_DELTA_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)


###############################################################################
################################### OMICRON WAVE ##############################
###############################################################################

Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA <- Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_ICU_Rate = lag(ICU_Rate, 2)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA = na.omit(Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA$Lag2_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA$ADI_category = as.factor(Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA$ADI_category)

Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA$zipcode = as.factor(Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA$zipcode)


Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA$ADI_category <- relevel(Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag2_ICU_Rate + 1) ~ `testcenters` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag2_ICU_Rate + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag2_ICU_Rate + 1) ~ `ADI_category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag2_ICU_Rate + 1) ~ `testcenters`*`ADI_category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag2_ICU_Rate + 1) ~ `Effective Number of Center`*`ADI_category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Lag_Analysis_Mehtod7_OMICRON_TwoMonth_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)
