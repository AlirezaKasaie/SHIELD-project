############################################# ALPHA WAVE ######################################################

############################################# One-Month Lag (WITH NA) ######################################################

ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA <- ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 1), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 1)
  ) %>%
  ungroup()

ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA = na.omit(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA)


write.csv(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_ALPHA_OneMonthLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA$ADI.category = as.factor(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA$ADI.category)

ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA$zipcode = as.factor(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA$zipcode)


ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA$ADI.category <- relevel(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)



ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)


ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_6, digits = 5)


vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Alpha_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_1, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_2, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_3, digits = 5)


ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA$SamplesCollected)


ICU_hospitalization_GLMMmodel_Alpha_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_4, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_5, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_6, digits = 5)


############################################# One-Month Lag (WITHOUT NA) ######################################################

ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA <- ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 1), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 1)
  ) %>%
  ungroup()

ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA = na.omit(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA)


write.csv(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_ALPHA_OneMonthLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA$ADI.category = as.factor(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA$ADI.category)

ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA$zipcode = as.factor(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA$zipcode)


ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA$ADI.category <- relevel(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)



ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA$SamplesCollected)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)


ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_6, digits = 5)


vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Alpha_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_1, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_2, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_3, digits = 5)


ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_with_NA$SamplesCollected)


ICU_hospitalization_GLMMmodel_Alpha_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_4, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_5, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_OneMonthLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_6, digits = 5)


############################################# Two-Month Lag (WITH NA) ######################################################


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA <- ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 2), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 2)
  ) %>%
  ungroup()


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA <- na.omit(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA)

write.csv(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA$ADI.category = as.factor(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA$ADI.category)

ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA$zipcode = as.factor(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA$zipcode)


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA$ADI.category <- relevel(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)


ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_6, digits = 5)


vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Alpha_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_1, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_2, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_3, digits = 5)


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA$SamplesCollected)


ICU_hospitalization_GLMMmodel_Alpha_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_4, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_5, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_6, digits = 5)

############################################# Two-Month Lag (WITHOUT NA) ######################################################


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA <- ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 2), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 2)
  ) %>%
  ungroup()


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA <- na.omit(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA)

write.csv(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA$ADI.category = as.factor(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA$ADI.category)

ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA$zipcode = as.factor(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA$zipcode)


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA$ADI.category <- relevel(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_4, digits = 5)


ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)


ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_ALPHA_6, digits = 5)


vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Alpha_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_1, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_2, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_3, digits = 5)


ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA$SamplesCollected)


ICU_hospitalization_GLMMmodel_Alpha_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_4, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_5, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA_TwoMonthsLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_6, digits = 5)


############################################# DELTA WAVE ######################################################

############################################# One-Month Lag (WITH NA) ######################################################


ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA <- ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 1), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 1)
  ) %>%
  ungroup()



ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA <- na.omit(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA)

write.csv(ICU_UPDATED_Zipcode_DELTA_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_DELTA_OneMonthLag.csv", row.names = FALSE)



skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA$ADI.category = as.factor(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA$ADI.category)

ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA$zipcode = as.factor(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA$zipcode)


ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA$ADI.category <- relevel(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)



ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA$SamplesCollected)


ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)


ICU_rate_GLMMmodel_DELTA_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_6, digits = 5)



vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Delta_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_1, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_2, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_3, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_4, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_5, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_6, digits = 5)



############################################# One-Month Lag (WITHOUT NA) ######################################################


ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA <- ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 1), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 1)
  ) %>%
  ungroup()



ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA <- na.omit(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA)

write.csv(ICU_UPDATED_Zipcode_DELTA_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_DELTA_OneMonthLag.csv", row.names = FALSE)



skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA$ADI.category = as.factor(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA$ADI.category)

ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA$zipcode = as.factor(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA$zipcode)


ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA$ADI.category <- relevel(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)



ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA$SamplesCollected)


ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)


ICU_rate_GLMMmodel_DELTA_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_6, digits = 5)



vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Delta_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_1, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_2, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_3, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_4, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_5, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_OneMonthLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_6, digits = 5)


############################################# Two-Month Lag (WITH NA) ######################################################


ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA <- ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 2), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 2)
  ) %>%
  ungroup()



ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA <- na.omit(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA)

write.csv(ICU_UPDATED_Zipcode_DELTA_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_DELTA_OneMonthLag.csv", row.names = FALSE)



skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA$ADI.category = as.factor(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA$ADI.category)

ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA$zipcode = as.factor(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA$zipcode)


ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA$ADI.category <- relevel(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)



ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA$SamplesCollected)


ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)


ICU_rate_GLMMmodel_DELTA_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_6, digits = 5)



vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Delta_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_1, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_2, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_3, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_4, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_5, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_with_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_6, digits = 5)


############################################# Two-Month Lag (WITHOUT NA) ######################################################


ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA <- ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 2), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 2)
  ) %>%
  ungroup()



ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA <- na.omit(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA)

write.csv(ICU_UPDATED_Zipcode_DELTA_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_DELTA_OneMonthLag.csv", row.names = FALSE)



skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA$ADI.category = as.factor(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA$ADI.category)

ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA$zipcode = as.factor(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA$zipcode)


ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA$ADI.category <- relevel(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)



ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA$SamplesCollected)


ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_4, digits = 5)


ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)


ICU_rate_GLMMmodel_DELTA_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_DELTA_6, digits = 5)



vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Delta_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_1, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_2, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_3, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_4, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_5, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA_TwoMonthsLag_without_NA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_6, digits = 5)


############################################# OMICRON WAVE ######################################################

############################################# One-Month Lag (WITH NA) ######################################################


ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA <- ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 1), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 1)
  ) %>%
  ungroup()



ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA <- na.omit(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA)

write.csv(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_OMICRON_OneMonthLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA$ADI.category = as.factor(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA$ADI.category)

ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA$zipcode = as.factor(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA$zipcode)


ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA$ADI.category <- relevel(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA$SamplesCollected)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)


ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_6, digits = 5)


vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Omicron_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_1, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_2, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_3, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_4, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_5, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_with_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_6, digits = 5)


############################################# One-Month Lag (WITHOUT NA) ######################################################


ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA <- ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 1), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 1)
  ) %>%
  ungroup()



ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA <- na.omit(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA)

write.csv(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_OMICRON_OneMonthLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA$ADI.category = as.factor(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA$ADI.category)

ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA$zipcode = as.factor(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA$zipcode)


ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA$ADI.category <- relevel(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA$SamplesCollected)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)


ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_6, digits = 5)


vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Omicron_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_1, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_2, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_3, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_4, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_5, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_OneMonthLag_without_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_6, digits = 5)



############################################# Two-Month Lag (WITH NA) ######################################################


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA <- ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 2), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 2)
  ) %>%
  ungroup()


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA <- na.omit(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA)

write.csv(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA$ADI.category = as.factor(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA$ADI.category)

ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA$zipcode = as.factor(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA$zipcode)


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA$ADI.category <- relevel(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA$SamplesCollected)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)


ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_6, digits = 5)


vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Omicron_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_1, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_2, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_3, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_4, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_5, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_with_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_6, digits = 5)


################################## Two-Month Lag (WITHOUT NA) ######################################################


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA <- ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Number_of_ICU_admissions = lag(Total.ICU.Hospitalizations.per.Zipcode.per.Month, 2), # Lag of 1 month
    Lag1_Covid_ICU_admission_rate = lag(Covid.ICU.Rate, 2)
  ) %>%
  ungroup()


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA <- na.omit(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA)

write.csv(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA$Lag1_Number_of_ICU_admissions , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA$ADI.category = as.factor(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA$ADI.category)

ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA$zipcode = as.factor(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA$zipcode)


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA$ADI.category <- relevel(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA$SamplesCollected)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_4, digits = 5)


ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)


ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ SamplesCollected*ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA)

tab_model(ICU_rate_GLMMmodel_OMICRON_6, digits = 5)


vif_model <- lm(Lag1_Number_of_ICU_admissions ~ SamplesCollected + ADI.category + TestCenters, data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA)
vif(vif_model)


ICU_hospitalization_GLMMmodel_Omicron_1 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_1, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_2 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_2, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_3 <- glmer(Lag1_Number_of_ICU_admissions ~ TestCenters*ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_3, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_4 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_4, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_5 <- glmer(Lag1_Number_of_ICU_admissions ~ ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_5, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_6 <- glmer(Lag1_Number_of_ICU_admissions ~ SamplesCollected*ADI.category + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON_TwoMonthsLag_without_NA, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_6, digits = 5)
