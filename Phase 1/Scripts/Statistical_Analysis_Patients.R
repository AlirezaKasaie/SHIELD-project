library(tidyr)
library(dplyr)
library(e1071)

#ICU_UPDATED_Patients = combined_dataset[,-c(1:2,4:5,9:10,12,14:16,18,21:45)]

#TCOVID_NEW = TCOVID_NEW %>%
  #filter(zipcode %in% ICU_UPDATED_Patients$zipcode)

#TCOVID_NEW = TCOVID_NEW %>%
  #group_by(zipcode, `DATE - COLLECTED`) %>%
  #mutate(Total_SHIELD_centers_per_zipcode_per_month = n_distinct(`SHIELD ID`))

#total_unique_zipcodes = n_distinct(TCOVID_NEW$zipcode)
# total_unique_zipcodes_1 = n_distinct(ICU_UPDATED_FINAL$zipcode)

#TCOVID_NEW$`DATE - COLLECTED` = as.Date(paste0(TCOVID_NEW$`DATE - COLLECTED`, "-01"), format = "%Y-%m-%d")

#TCOVID_NEW$`DATE - COLLECTED` = format(TCOVID_NEW$`DATE - COLLECTED`, "%Y-%m")

#write.csv(ICU_UPDATED_FINAL, file = "C:/Users/skasaiesharifi/Documents/ICU_UPDATED_FINAL.csv", row.names = FALSE)


#TCOVID_NEW_aggregated = TCOVID_NEW %>%
  #group_by(zipcode, `DATE - COLLECTED`) %>%
  #summarize(`Total_SHIELD_centers_per_zipcode_per_month` = first(`Total_SHIELD_centers_per_zipcode_per_month`), .groups = 'drop')

#rm(TCOVID_NEW_aggregated)

#ICU_UPDATED_Patients = merge(ICU_UPDATED_Patients, TCOVID_NEW_aggregated, by.x = c("zipcode", "Date"), by.y = c("zipcode", "DATE - COLLECTED"), all.x = TRUE)

#ICU_UPDATED_Patients <- ICU_UPDATED_Patients %>%
  #mutate(`Total_SHIELD_centers_per_zipcode_per_month` = replace_na(`Total_SHIELD_centers_per_zipcode_per_month`, 0))


#ICU_UPDATED_Patients <- merge(ICU_UPDATED_Patients, Dataset2, by.x = "zipcode", by.y = "zipcode", all.x = TRUE)

#write.csv(ICU_UPDATED_Patients, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/ICU_UPDATED_Patients.csv", row.names = FALSE)


########################################################################################################################

#*******************************Multivariable modeling - ALPHA *************************

ICU_UPDATED_ALPHA_Patients = read.csv(file.choose(), sep=",", header=T)

Data_Monthly_Alpha = read.csv(file.choose(), sep=",", header=T)

#Data_Monthly_Alpha = Data_Monthly_Alpha[,-c(5:12)]

Data_Monthly_Alpha = Data_Monthly_Alpha %>%
  rename(zipcode = zip)

Data_Monthly_Alpha = Data_Monthly_Alpha %>%
  rename(Date = Month)

Data_Monthly_Alpha = Data_Monthly_Alpha %>%
  mutate(Zip = substr(zipcode, 1, 5))

ICU_UPDATED_ALPHA_Patients$zipcode = as.character(ICU_UPDATED_ALPHA_Patients$zipcode)

Data_Monthly_Alpha$zipcode = as.character(Data_Monthly_Alpha$zipcode)

ICU_UPDATED_ALPHA_Patients = merge(ICU_UPDATED_ALPHA_Patients, Data_Monthly_Alpha, by = c("zipcode","Date"))

zipcodes_to_remove <- c("60141", "60479","60549","60519","60602","60910","60945","61317","61337","61338","61367","62022") 

ICU_UPDATED_ALPHA_Patients <- ICU_UPDATED_ALPHA_Patients %>%
  filter(!zipcode %in% zipcodes_to_remove)

#ICU_UPDATED_ALPHA_Patients <- full_join(ICU_UPDATED_ALPHA_Patients, Data_Monthly_Alpha, by = c("zipcode", "Date"))

ICU_UPDATED_ALPHA_Patients = ICU_UPDATED_ALPHA_Patients[,-c(16)]

write.csv(ICU_UPDATED_ALPHA_Patients, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Alpha/ICU_UPDATED_ALPHA_Patients_Fatir.csv", row.names = FALSE)

skewness_ICU_los = skewness(ICU_UPDATED_ALPHA_Patients$icu_los , type = 3)
print(skewness_ICU_los)

skewness_ICU_los = skewness(ICU_UPDATED_ALPHA_Patients$Death , type = 3)
print(skewness_ICU_los)

ICU_UPDATED_ALPHA_Patients$gender = as.factor(ICU_UPDATED_ALPHA_Patients$gender)
ICU_UPDATED_ALPHA_Patients$race = as.factor(ICU_UPDATED_ALPHA_Patients$race)
ICU_UPDATED_ALPHA_Patients$ethnic = as.factor(ICU_UPDATED_ALPHA_Patients$ethnic)
ICU_UPDATED_ALPHA_Patients$financial_class = as.factor(ICU_UPDATED_ALPHA_Patients$financial_class)
ICU_UPDATED_ALPHA_Patients$ADI.category = as.factor(ICU_UPDATED_ALPHA_Patients$ADI.category)
#ICU_UPDATED_ALPHA_Patients$Death = as.numeric(ICU_UPDATED_ALPHA_Patients$Death)
ICU_UPDATED_ALPHA_Patients$mrn = as.factor(ICU_UPDATED_ALPHA_Patients$mrn)
ICU_UPDATED_ALPHA_Patients$zipcode = as.factor(ICU_UPDATED_ALPHA_Patients$zipcode)


ICU_UPDATED_ALPHA_Patients$gender <- relevel(ICU_UPDATED_ALPHA_Patients$gender, ref = "Female")
ICU_UPDATED_ALPHA_Patients$race <- relevel(ICU_UPDATED_ALPHA_Patients$race, ref = "White")
ICU_UPDATED_ALPHA_Patients$ethnic <- relevel(ICU_UPDATED_ALPHA_Patients$ethnic, ref = "Non-Hispanic")
ICU_UPDATED_ALPHA_Patients$financial_class <- relevel(ICU_UPDATED_ALPHA_Patients$financial_class, ref = "Insured")
ICU_UPDATED_ALPHA_Patients$ADI.category <- relevel(ICU_UPDATED_ALPHA_Patients$ADI.category, ref = "Less Disadvantaged")
#ICU_UPDATED_ALPHA_Patients$Death <- relevel(ICU_UPDATED_ALPHA_Patients$Death, ref = "1")


vif_model <- lm(icu_los ~ SamplesCollected + PositiveCases + ADI.category + TestCenters +
                  gender + race + ethnic + age + Death + financial_class, data = ICU_UPDATED_ALPHA_Patients)
vif(vif_model)



ICU_UPDATED_ALPHA_Patients$SamplesCollected <- scale(ICU_UPDATED_ALPHA_Patients$SamplesCollected)

ICU_UPDATED_ALPHA_Patients$age <- scale(ICU_UPDATED_ALPHA_Patients$age)


ICU_los_GLMMmodel_Alpha_1 <- glmer(icu_los ~ TestCenters + (1 | mrn), 
                                          data = ICU_UPDATED_ALPHA_Patients, 
                                          family = poisson)

tab_model(ICU_los_GLMMmodel_Alpha_1)


ICU_los_GLMMmodel_Alpha_2 <- glmer(icu_los ~ ADI.category + (1 | mrn), 
                                   data = ICU_UPDATED_ALPHA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Alpha_2)


ICU_los_GLMMmodel_Alpha_3 <- glmer(icu_los ~ SamplesCollected + (1 | mrn), 
                                   data = ICU_UPDATED_ALPHA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Alpha_3)


# ICU_los_GLMMmodel_Alpha_4 <- glmer(icu_los ~ PositiveCases + (1 | mrn), 
#                                    data = ICU_UPDATED_ALPHA_Patients, 
#                                    family = poisson)
# 
# tab_model(ICU_los_GLMMmodel_Alpha_4)


ICU_los_GLMMmodel_Alpha_5 <- glmer(icu_los ~ TestCenters*ADI.category + (1 | mrn), 
                                   data = ICU_UPDATED_ALPHA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Alpha_5)


ICU_los_GLMMmodel_Alpha_6 <- glmer(icu_los ~ ADI.category*SamplesCollected + (1 | mrn), 
                                   data = ICU_UPDATED_ALPHA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Alpha_6)


# ICU_los_GLMMmodel_Alpha_7 <- glmer(icu_los ~ ADI.category*PositiveCases + (1 | mrn), 
#                                    data = ICU_UPDATED_ALPHA_Patients, 
#                                    family = poisson)
# 
# tab_model(ICU_los_GLMMmodel_Alpha_7)


ICU_los_GLMMmodel_Alpha_8 <- glmer(icu_los ~ gender + (1 | mrn), 
                                   data = ICU_UPDATED_ALPHA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Alpha_8)


ICU_los_GLMMmodel_Alpha_9 <- glmer(icu_los ~ race + (1 | mrn), 
                                   data = ICU_UPDATED_ALPHA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Alpha_9)


ICU_los_GLMMmodel_Alpha_10 <- glmer(icu_los ~ ethnic + (1 | mrn), 
                                   data = ICU_UPDATED_ALPHA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Alpha_10)


ICU_los_GLMMmodel_Alpha_11 <- glmer(icu_los ~ age + (1 | mrn), 
                                    data = ICU_UPDATED_ALPHA_Patients, 
                                    family = poisson)

tab_model(ICU_los_GLMMmodel_Alpha_11)


ICU_los_GLMMmodel_Alpha_12 <- glmer(icu_los ~ financial_class + (1 | mrn), 
                                    data = ICU_UPDATED_ALPHA_Patients, 
                                    family = poisson)

tab_model(ICU_los_GLMMmodel_Alpha_12)


correlation_test <- cor.test(ICU_UPDATED_ALPHA_Patients$Death, ICU_UPDATED_ALPHA_Patients$TestCenters, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")

correlation_test <- cor.test(ICU_UPDATED_ALPHA_Patients$Death, ICU_UPDATED_ALPHA_Patients$SamplesCollected, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


# correlation_test <- cor.test(ICU_UPDATED_ALPHA_Patients$icu_los, ICU_UPDATED_ALPHA_Patients$PositiveCases, use = "complete.obs")
# 
# # Extract the correlation coefficient and p-value
# correlation <- correlation_test$estimate
# p_value <- correlation_test$p.value
# 
# cat("Correlation coefficient:", correlation, "\n")
# cat("P-value:", p_value, "\n")



ICU_death_GLMMmodel_Alpha_1 <- glmer(Death ~ TestCenters + (1 | mrn), 
                                    data = ICU_UPDATED_ALPHA_Patients, 
                                    family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Alpha_1)


ICU_death_GLMMmodel_Alpha_2 <- glmer(Death ~ ADI.category + (1 | mrn), 
                                     data = ICU_UPDATED_ALPHA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Alpha_2)


ICU_death_GLMMmodel_Alpha_3 <- glmer(Death ~ SamplesCollected + (1 | mrn), 
                                     data = ICU_UPDATED_ALPHA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Alpha_3)


# ICU_death_GLMMmodel_Alpha_4 <- glmer(Death ~ PositiveCases + (1 | mrn), 
#                                      data = ICU_UPDATED_ALPHA_Patients, 
#                                      family = binomial(link = "logit"))
# 
# tab_model(ICU_los_GLMMmodel_Alpha_4)


ICU_death_GLMMmodel_Alpha_5 <- glmer(Death ~ TestCenters*ADI.category + (1 | mrn), 
                                     data = ICU_UPDATED_ALPHA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Alpha_5)


ICU_death_GLMMmodel_Alpha_6 <- glmer(Death ~ SamplesCollected*ADI.category + (1 | mrn), 
                                     data = ICU_UPDATED_ALPHA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Alpha_6)


ICU_death_GLMMmodel_Alpha_7 <- glmer(Death ~ gender + (1 | mrn), 
                                     data = ICU_UPDATED_ALPHA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Alpha_7)


ICU_death_GLMMmodel_Alpha_8 <- glmer(Death ~ race + (1 | mrn), 
                                     data = ICU_UPDATED_ALPHA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Alpha_8)


ICU_death_GLMMmodel_Alpha_9 <- glmer(Death ~ ethnic + (1 | mrn), 
                                     data = ICU_UPDATED_ALPHA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Alpha_9)


ICU_death_GLMMmodel_Alpha_10 <- glmer(Death ~ age + (1 | mrn), 
                                     data = ICU_UPDATED_ALPHA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Alpha_10)


ICU_death_GLMMmodel_Alpha_11 <- glmer(Death ~ financial_class + (1 | mrn), 
                                      data = ICU_UPDATED_ALPHA_Patients, 
                                      family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Alpha_11)

#ICU_los_GLMMmodel_ALPHA_1 <- lmer(log(icu_los + 1) ~ gender +
                                    #(1 | mrn), 
                                  #data = ICU_UPDATED_ALPHA_Patients)


#tab_model(ICU_los_GLMMmodel_ALPHA_1)


#ICU_los_GLMMmodel_ALPHA_2 <- lmer(log(icu_los + 1) ~ race +
                                    #(1 | mrn), 
                                  #ata = ICU_UPDATED_ALPHA_Patients)


#tab_model(ICU_los_GLMMmodel_ALPHA_2)


#ICU_los_GLMMmodel_ALPHA_3 <- lmer(log(icu_los + 1) ~ ethnic +
                                   # (1 | mrn), 
                                 # data = ICU_UPDATED_ALPHA_Patients)


#tab_model(ICU_los_GLMMmodel_ALPHA_3)


#ICU_los_GLMMmodel_ALPHA_4 <- lmer(log(icu_los + 1) ~ age +
                                    #(1 | mrn), 
                                  #data = ICU_UPDATED_ALPHA_Patients)


#tab_model(ICU_los_GLMMmodel_ALPHA_4)


#ICU_los_GLMMmodel_ALPHA_5 <- lmer(log(icu_los + 1) ~ financial_class +
                                    #(1 | mrn), 
                                  #data = ICU_UPDATED_ALPHA_Patients)


#tab_model(ICU_los_GLMMmodel_ALPHA_5)


#ICU_los_GLMMmodel_ALPHA_6 <- lmer(log(icu_los + 1) ~ Death +
                                    #(1 | mrn), 
                                  #data = ICU_UPDATED_ALPHA_Patients)


#tab_model(ICU_los_GLMMmodel_ALPHA_6)



# ICU_los_GLMMmodel_ALPHA_7 <- lmer(log(icu_los + 1) ~ TestCenters +
#                                    (1 | mrn), 
#                                  data = ICU_UPDATED_ALPHA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_ALPHA_7)





# Fit the Generalized Linear Mixed Model (GLMM)
#ICU_los_GLMMmodel_ALPHA <- glmer(log(icu_los + 1) ~ gender + race + ethnic + age + financial_class + 
                                   #Total_SHIELD_centers_per_zipcode_per_month + ADI + 
                                   #(1 | zipcode), 
                                 #data = ICU_UPDATED_ALPHA_Patients, 
                                 #family = gaussian(link = "identity"))

# Summary of the GLMM model


#ICU_los_GLMMmodel_ALPHA_8 <- lmer(log(icu_los + 1) ~ ADI.category +
                                   # (1 | mrn), 
                                  #data = ICU_UPDATED_ALPHA_Patients)


#tab_model(ICU_los_GLMMmodel_ALPHA_8)


#ICU_los_GLMMmodel_ALPHA_9 <- lmer(log(icu_los + 1) ~ SamplesCollected +
                                    #(1 | mrn), 
                                  #data = ICU_UPDATED_ALPHA_Patients)


#tab_model(ICU_los_GLMMmodel_ALPHA_9)


#ICU_los_GLMMmodel_ALPHA_10 <- lmer(log(icu_los + 1) ~ PositiveCases +
                                   # (1 | mrn), 
                                  #data = ICU_UPDATED_ALPHA_Patients)


# tab_model(ICU_los_GLMMmodel_ALPHA_10)
# 
# 
# ICU_los_GLMMmodel_ALPHA_11 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_ALPHA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_ALPHA_11)
# 
# 
# ICU_los_GLMMmodel_ALPHA_12 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*SamplesCollected +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_ALPHA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_ALPHA_12)
# 
# 
# ICU_los_GLMMmodel_ALPHA_13 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*PositiveCases +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_ALPHA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_ALPHA_13)
# 
# 
# ICU_los_GLMMmodel_ALPHA_14 <- lmer(log(icu_los + 1) ~ ADI.category*SamplesCollected +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_ALPHA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_ALPHA_14)
# 
# 
# ICU_los_GLMMmodel_ALPHA_15 <- lmer(log(icu_los + 1) ~ ADI.category*PositiveCases +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_ALPHA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_ALPHA_15)
# 
# 
# ICU_los_GLMMmodel_ALPHA_16 <- lmer(log(icu_los + 1) ~ SamplesCollected*PositiveCases +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_ALPHA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_ALPHA_16)
# 
# 
# ICU_los_GLMMmodel_ALPHA_17 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*SamplesCollected +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_ALPHA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_ALPHA_17)
# 
# 
# ICU_los_GLMMmodel_ALPHA_18 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_ALPHA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_ALPHA_18)
# 
# 
# ICU_los_GLMMmodel_ALPHA_19 <- lmer(log(icu_los + 1) ~ ADI.category*SamplesCollected*PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_ALPHA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_ALPHA_19)
# 
# 
# ICU_los_GLMMmodel_ALPHA_20 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*SamplesCollected*PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_ALPHA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_ALPHA_20)
# 
# 
# ICU_los_GLMMmodel_ALPHA_21 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*SamplesCollected*PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_ALPHA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_ALPHA_21)
# 
# 
# ICU_los_GLMMmodel_ALPHA_22 <- lmer(log(icu_los + 1) ~ gender + race + ethnic + age + financial_class + Death + Total_SHIELD_centers_per_zipcode_per_month + 
#                                      ADI.category + SamplesCollected + PositiveCases + Total_SHIELD_centers_per_zipcode_per_month*ADI.category*SamplesCollected*PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_ALPHA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_ALPHA_22)


########################################################################################################################

#*******************************Multivariable modeling - DELTA *************************

ICU_UPDATED_DELTA_Patients = read.csv(file.choose(), sep=",", header=T)

Data_Monthly_Delta = read.csv(file.choose(), sep=",", header=T)

#Data_Monthly_Delta = Data_Monthly_Delta[,-c(5:12)]

Data_Monthly_Delta = Data_Monthly_Delta %>%
  rename(zipcode = zip)

Data_Monthly_Delta = Data_Monthly_Delta %>%
  rename(Date = Month)

Data_Monthly_Delta = Data_Monthly_Delta %>%
  mutate(zipcode = substr(zipcode, 1, 5))

ICU_UPDATED_DELTA_Patients$zipcode = as.character(ICU_UPDATED_DELTA_Patients$zipcode)

Data_Monthly_Delta$zipcode = as.character(Data_Monthly_Delta$zipcode)

ICU_UPDATED_DELTA_Patients = merge(ICU_UPDATED_DELTA_Patients, Data_Monthly_Delta, by = c("zipcode","Date"))

zipcodes_to_remove <- c("60141", "60479","60549","60519","60602","60910","60945","61317","61337","61338","61367","62022") 

ICU_UPDATED_DELTA_Patients <- ICU_UPDATED_DELTA_Patients %>%
  filter(!zipcode %in% zipcodes_to_remove)

#ICU_UPDATED_ALPHA_Patients <- full_join(ICU_UPDATED_ALPHA_Patients, Data_Monthly_Alpha, by = c("zipcode", "Date"))

#ICU_UPDATED_ALPHA_Patients = ICU_UPDATED_ALPHA_Patients[,-c(16)]

write.csv(ICU_UPDATED_DELTA_Patients, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Delta/ICU_UPDATED_DELTA_Patients_Fatir.csv", row.names = FALSE)


skewness_ICU_los = skewness(ICU_UPDATED_DELTA_Patients$icu_los , type = 3)
print(skewness_ICU_los)

skewness_ICU_los = skewness(ICU_UPDATED_DELTA_Patients$Death , type = 3)
print(skewness_ICU_los)

ICU_UPDATED_DELTA_Patients$gender = as.factor(ICU_UPDATED_DELTA_Patients$gender)
ICU_UPDATED_DELTA_Patients$race = as.factor(ICU_UPDATED_DELTA_Patients$race)
ICU_UPDATED_DELTA_Patients$ethnic = as.factor(ICU_UPDATED_DELTA_Patients$ethnic)
ICU_UPDATED_DELTA_Patients$financial_class = as.factor(ICU_UPDATED_DELTA_Patients$financial_class)
ICU_UPDATED_DELTA_Patients$ADI.category = as.factor(ICU_UPDATED_DELTA_Patients$ADI.category)
#ICU_UPDATED_DELTA_Patients$Death = as.factor(ICU_UPDATED_DELTA_Patients$Death)
ICU_UPDATED_DELTA_Patients$mrn = as.factor(ICU_UPDATED_DELTA_Patients$mrn)


ICU_UPDATED_DELTA_Patients$gender <- relevel(ICU_UPDATED_DELTA_Patients$gender, ref = "Female")
ICU_UPDATED_DELTA_Patients$race <- relevel(ICU_UPDATED_DELTA_Patients$race, ref = "White")
ICU_UPDATED_DELTA_Patients$ethnic <- relevel(ICU_UPDATED_DELTA_Patients$ethnic, ref = "Non-Hispanic")
ICU_UPDATED_DELTA_Patients$financial_class <- relevel(ICU_UPDATED_DELTA_Patients$financial_class, ref = "Insured")
ICU_UPDATED_DELTA_Patients$ADI.category <- relevel(ICU_UPDATED_DELTA_Patients$ADI.category, ref = "Less Disadvantaged")
#ICU_UPDATED_DELTA_Patients$Death <- relevel(ICU_UPDATED_DELTA_Patients$Death, ref = "1")


vif_model <- lm(icu_los ~ SamplesCollected + PositiveCases + ADI.category + TestCenters +
                  gender + race + ethnic + age + Death + financial_class, data = ICU_UPDATED_DELTA_Patients)
vif(vif_model)


ICU_UPDATED_DELTA_Patients$SamplesCollected <- scale(ICU_UPDATED_DELTA_Patients$SamplesCollected)

ICU_UPDATED_DELTA_Patients$age <- scale(ICU_UPDATED_DELTA_Patients$age)


ICU_los_GLMMmodel_Delta_1 <- glmer(icu_los ~ TestCenters + (1 | mrn), 
                                    data = ICU_UPDATED_DELTA_Patients, 
                                    family = poisson)

tab_model(ICU_los_GLMMmodel_Delta_1)


ICU_los_GLMMmodel_Delta_2 <- glmer(icu_los ~ ADI.category + (1 | mrn), 
                                   data = ICU_UPDATED_DELTA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Delta_2)


ICU_los_GLMMmodel_Delta_3 <- glmer(icu_los ~ SamplesCollected + (1 | mrn), 
                                   data = ICU_UPDATED_DELTA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Delta_3)


ICU_los_GLMMmodel_Delta_4 <- glmer(icu_los ~ TestCenters*ADI.category + (1 | mrn), 
                                   data = ICU_UPDATED_DELTA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Delta_4)


ICU_los_GLMMmodel_Delta_5 <- glmer(icu_los ~ SamplesCollected*ADI.category + (1 | mrn), 
                                   data = ICU_UPDATED_DELTA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Delta_5)


ICU_los_GLMMmodel_Delta_6 <- glmer(icu_los ~ gender + (1 | mrn), 
                                   data = ICU_UPDATED_DELTA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Delta_6)


ICU_los_GLMMmodel_Delta_7 <- glmer(icu_los ~ race + (1 | mrn), 
                                   data = ICU_UPDATED_DELTA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Delta_7)


ICU_los_GLMMmodel_Delta_8 <- glmer(icu_los ~ ethnic + (1 | mrn), 
                                   data = ICU_UPDATED_DELTA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Delta_8)


ICU_los_GLMMmodel_Delta_9 <- glmer(icu_los ~ age + (1 | mrn), 
                                   data = ICU_UPDATED_DELTA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Delta_9)


ICU_los_GLMMmodel_Delta_10 <- glmer(icu_los ~ financial_class + (1 | mrn), 
                                   data = ICU_UPDATED_DELTA_Patients, 
                                   family = poisson)

tab_model(ICU_los_GLMMmodel_Delta_10)


ICU_death_GLMMmodel_Delta_1 <- glmer(Death ~ TestCenters + (1 | mrn), 
                                      data = ICU_UPDATED_DELTA_Patients, 
                                      family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Delta_1)


ICU_death_GLMMmodel_Delta_2 <- glmer(Death ~ ADI.category + (1 | mrn), 
                                     data = ICU_UPDATED_DELTA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Delta_2)


ICU_death_GLMMmodel_Delta_3 <- glmer(Death ~ SamplesCollected + (1 | mrn), 
                                     data = ICU_UPDATED_DELTA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Delta_3)


ICU_death_GLMMmodel_Delta_4 <- glmer(Death ~ TestCenters*ADI.category + (1 | mrn), 
                                     data = ICU_UPDATED_DELTA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Delta_4)


ICU_death_GLMMmodel_Delta_5 <- glmer(Death ~ SamplesCollected*ADI.category + (1 | mrn), 
                                     data = ICU_UPDATED_DELTA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Delta_5)


ICU_death_GLMMmodel_Delta_6 <- glmer(Death ~ gender + (1 | mrn), 
                                     data = ICU_UPDATED_DELTA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Delta_6)


ICU_death_GLMMmodel_Delta_7 <- glmer(Death ~ race + (1 | mrn), 
                                     data = ICU_UPDATED_DELTA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Delta_7)


ICU_death_GLMMmodel_Delta_8 <- glmer(Death ~ ethnic + (1 | mrn), 
                                     data = ICU_UPDATED_DELTA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Delta_8)



ICU_death_GLMMmodel_Delta_9 <- glmer(Death ~ age + (1 | mrn), 
                                     data = ICU_UPDATED_DELTA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Delta_9)



ICU_death_GLMMmodel_Delta_10 <- glmer(Death ~ financial_class + (1 | mrn), 
                                     data = ICU_UPDATED_DELTA_Patients, 
                                     family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Delta_10)

# ICU_los_GLMMmodel_DELTA_1 <- lmer(log(icu_los + 1) ~ gender +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_1)
# 
# 
# ICU_los_GLMMmodel_DELTA_2 <- lmer(log(icu_los + 1) ~ race +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_2)
# 
# 
# ICU_los_GLMMmodel_DELTA_3 <- lmer(log(icu_los + 1) ~ ethnic +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_3)
# 
# 
# ICU_los_GLMMmodel_DELTA_4 <- lmer(log(icu_los + 1) ~ age +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_4)
# 
# 
# ICU_los_GLMMmodel_DELTA_5 <- lmer(log(icu_los + 1) ~ financial_class +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_5)
# 
# 
# ICU_los_GLMMmodel_DELTA_6 <- lmer(log(icu_los + 1) ~ Death +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_6)
# 
# 
# 
# ICU_los_GLMMmodel_DELTA_7 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_7)


correlation_test <- cor.test(ICU_UPDATED_DELTA_Patients$Death, ICU_UPDATED_DELTA_Patients$TestCenters, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_DELTA_Patients$Death, ICU_UPDATED_DELTA_Patients$SamplesCollected, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


# correlation_test <- cor.test(Delta_patients$icu_los, Delta_patients$SamplesCollected, use = "complete.obs")
# 
# # Extract the correlation coefficient and p-value
# correlation <- correlation_test$estimate
# p_value <- correlation_test$p.value
# 
# cat("Correlation coefficient:", correlation, "\n")
# cat("P-value:", p_value, "\n")
# 
# 
# correlation_test <- cor.test(Delta_patients$icu_los, Delta_patients$PositiveCases, use = "complete.obs")
# 
# # Extract the correlation coefficient and p-value
# correlation <- correlation_test$estimate
# p_value <- correlation_test$p.value
# 
# cat("Correlation coefficient:", correlation, "\n")
# cat("P-value:", p_value, "\n")

# Fit the Generalized Linear Mixed Model (GLMM)
#ICU_los_GLMMmodel_ALPHA <- glmer(log(icu_los + 1) ~ gender + race + ethnic + age + financial_class + 
#Total_SHIELD_centers_per_zipcode_per_month + ADI + 
#(1 | zipcode), 
#data = ICU_UPDATED_ALPHA_Patients, 
#family = gaussian(link = "identity"))

# Summary of the GLMM model


# ICU_los_GLMMmodel_DELTA_8 <- lmer(log(icu_los + 1) ~ ADI.category +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_8)
# 
# 
# ICU_los_GLMMmodel_DELTA_9 <- lmer(log(icu_los + 1) ~ SamplesCollected +
#                                     (1 | mrn), 
#                                   data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_9)
# 
# 
# ICU_los_GLMMmodel_DELTA_10 <- lmer(log(icu_los + 1) ~ PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_10)
# 
# 
# ICU_los_GLMMmodel_DELTA_11 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_11)
# 
# 
# ICU_los_GLMMmodel_DELTA_12 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*SamplesCollected +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_12)
# 
# 
# ICU_los_GLMMmodel_DELTA_13 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_13)
# 
# 
# ICU_los_GLMMmodel_DELTA_14 <- lmer(log(icu_los + 1) ~ ADI.category*SamplesCollected +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_14)
# 
# 
# ICU_los_GLMMmodel_DELTA_15 <- lmer(log(icu_los + 1) ~ ADI.category*PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_15)
# 
# 
# ICU_los_GLMMmodel_DELTA_16 <- lmer(log(icu_los + 1) ~ SamplesCollected*PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_16)
# 
# 
# ICU_los_GLMMmodel_DELTA_17 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*SamplesCollected +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_17)
# 
# 
# ICU_los_GLMMmodel_DELTA_18 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_18)
# 
# 
# ICU_los_GLMMmodel_DELTA_19 <- lmer(log(icu_los + 1) ~ ADI.category*SamplesCollected*PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_19)
# 
# 
# ICU_los_GLMMmodel_DELTA_20 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*SamplesCollected*PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_20)
# 
# 
# ICU_los_GLMMmodel_DELTA_21 <- lmer(log(icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*SamplesCollected*PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_21)
# 
# 
# ICU_los_GLMMmodel_DELTA_22 <- lmer(log(icu_los + 1) ~ gender + race + ethnic + age + financial_class + Death + Total_SHIELD_centers_per_zipcode_per_month + 
#                                      ADI.category + SamplesCollected + PositiveCases + Total_SHIELD_centers_per_zipcode_per_month*ADI.category*SamplesCollected*PositiveCases +
#                                      (1 | mrn), 
#                                    data = ICU_UPDATED_DELTA_Patients)
# 
# 
# tab_model(ICU_los_GLMMmodel_DELTA_22)



# # Calculate the number of observations per zipcode
# zipcode_summary <- ICU_UPDATED_DELTA_Patients %>%
#   group_by(zipcode) %>%
#   summarise(
#     count = n(),
#     mean_icu_los = mean(icu_los, na.rm = TRUE),
#     sd_icu_los = sd(icu_los, na.rm = TRUE)
#   )
# 
# # Print summary
# print(zipcode_summary , n = 215)
# 
# 
# delta_summary <- ICU_UPDATED_DELTA_Patients %>%
#   group_by(zipcode) %>%
#   summarise(
#     count = n(),
#     mean_icu_los = mean(icu_los, na.rm = TRUE),
#     sd_icu_los = sd(icu_los, na.rm = TRUE)
#   )
# 
# # Identify problematic zipcodes
# problematic_zipcodes_delta <- delta_summary %>%
#   filter(count < 5 | is.na(sd_icu_los) | sd_icu_los == 0)
# 
# # Print problematic zipcodes
# print(problematic_zipcodes_delta, n = nrow(problematic_zipcodes_delta))
# 
# 
# ICU_UPDATED_DELTA_Patients_cleaned <- ICU_UPDATED_DELTA_Patients %>%
#   filter(!zipcode %in% problematic_zipcodes_delta$zipcode)
# 
# # Check the dimensions of the cleaned dataset
# print(dim(ICU_UPDATED_DELTA_Patients_cleaned))
# 
# # Optionally, view the first few rows of the cleaned dataset
# head(ICU_UPDATED_DELTA_Patients_cleaned)

#*******************************Multivariable modeling - OMICRON *************************

ICU_UPDATED_OMICRON_Patients = read.csv(file.choose(), sep=",", header=T)

Data_Monthly_Omicron = read.csv(file.choose(), sep=",", header=T)

#Data_Monthly_Delta = Data_Monthly_Delta[,-c(5:12)]

Data_Monthly_Omicron = Data_Monthly_Omicron %>%
  rename(zipcode = zip)

Data_Monthly_Omicron = Data_Monthly_Omicron %>%
  rename(Date = Month)

Data_Monthly_Omicron = Data_Monthly_Omicron %>%
  mutate(zipcode = substr(zipcode, 1, 5))

ICU_UPDATED_OMICRON_Patients$zipcode = as.character(ICU_UPDATED_OMICRON_Patients$zipcode)

Data_Monthly_Omicron$zipcode = as.character(Data_Monthly_Omicron$zipcode)

ICU_UPDATED_OMICRON_Patients = merge(ICU_UPDATED_OMICRON_Patients, Data_Monthly_Omicron, by = c("zipcode","Date"))

zipcodes_to_remove <- c("60141", "60479","60549","60519","60602","60910","60945","61317","61337","61338","61367","62022") 

ICU_UPDATED_OMICRON_Patients <- ICU_UPDATED_OMICRON_Patients %>%
  filter(!zipcode %in% zipcodes_to_remove)

#ICU_UPDATED_ALPHA_Patients <- full_join(ICU_UPDATED_ALPHA_Patients, Data_Monthly_Alpha, by = c("zipcode", "Date"))

#ICU_UPDATED_ALPHA_Patients = ICU_UPDATED_ALPHA_Patients[,-c(16)]

write.csv(ICU_UPDATED_OMICRON_Patients, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Omicron/ICU_UPDATED_OMICRON_Patients_Fatir.csv", row.names = FALSE)


library(dplyr)
library(e1071)

skewness_ICU_los = skewness(ICU_UPDATED_OMICRON_Patients$icu_los , type = 3)
print(skewness_ICU_los)

skewness_ICU_los = skewness(ICU_UPDATED_OMICRON_Patients$Death , type = 3)
print(skewness_ICU_los)

ICU_UPDATED_OMICRON_Patients$gender = as.factor(ICU_UPDATED_OMICRON_Patients$gender)
ICU_UPDATED_OMICRON_Patients$race = as.factor(ICU_UPDATED_OMICRON_Patients$race)
ICU_UPDATED_OMICRON_Patients$ethnic = as.factor(ICU_UPDATED_OMICRON_Patients$ethnic)
ICU_UPDATED_OMICRON_Patients$financial_class = as.factor(ICU_UPDATED_OMICRON_Patients$financial_class)
ICU_UPDATED_OMICRON_Patients$ADI.category = as.factor(ICU_UPDATED_OMICRON_Patients$ADI.category)
#ICU_UPDATED_DELTA_Patients$Death = as.factor(ICU_UPDATED_DELTA_Patients$Death)
ICU_UPDATED_OMICRON_Patients$mrn = as.factor(ICU_UPDATED_OMICRON_Patients$mrn)


ICU_UPDATED_OMICRON_Patients$gender <- relevel(ICU_UPDATED_OMICRON_Patients$gender, ref = "Female")
ICU_UPDATED_OMICRON_Patients$race <- relevel(ICU_UPDATED_OMICRON_Patients$race, ref = "White")
ICU_UPDATED_OMICRON_Patients$ethnic <- relevel(ICU_UPDATED_OMICRON_Patients$ethnic, ref = "Non-Hispanic")
ICU_UPDATED_OMICRON_Patients$financial_class <- relevel(ICU_UPDATED_OMICRON_Patients$financial_class, ref = "Insured")
ICU_UPDATED_OMICRON_Patients$ADI.category <- relevel(ICU_UPDATED_OMICRON_Patients$ADI.category, ref = "Less Disadvantaged")
#ICU_UPDATED_DELTA_Patients$Death <- relevel(ICU_UPDATED_DELTA_Patients$Death, ref = "1")


ICU_UPDATED_OMICRON_Patients$SamplesCollected <- scale(ICU_UPDATED_OMICRON_Patients$SamplesCollected)

ICU_UPDATED_OMICRON_Patients$age <- scale(ICU_UPDATED_OMICRON_Patients$age)


vif_model <- lm(icu_los ~ SamplesCollected + PositiveCases + ADI.category + TestCenters +
                  gender + race + ethnic + age + Death + financial_class, data = ICU_UPDATED_OMICRON_Patients)
vif(vif_model)


ICU_los_GLMMmodel_Omicron_1 <- glmer(icu_los ~ TestCenters + (1 | mrn), 
                                   data = ICU_UPDATED_OMICRON_Patients, 
                                   family = poisson)


tab_model(ICU_los_GLMMmodel_Omicron_1)



ICU_los_GLMMmodel_Omicron_2 <- glmer(icu_los ~ ADI.category + (1 | mrn), 
                                     data = ICU_UPDATED_OMICRON_Patients, 
                                     family = poisson)


tab_model(ICU_los_GLMMmodel_Omicron_2)



ICU_los_GLMMmodel_Omicron_3 <- glmer(icu_los ~ SamplesCollected + (1 | mrn), 
                                     data = ICU_UPDATED_OMICRON_Patients, 
                                     family = poisson)


tab_model(ICU_los_GLMMmodel_Omicron_3)



ICU_los_GLMMmodel_Omicron_4 <- glmer(icu_los ~ TestCenters*ADI.category + (1 | mrn), 
                                     data = ICU_UPDATED_OMICRON_Patients, 
                                     family = poisson)


tab_model(ICU_los_GLMMmodel_Omicron_4)



ICU_los_GLMMmodel_Omicron_5 <- glmer(icu_los ~ SamplesCollected*ADI.category + (1 | mrn), 
                                     data = ICU_UPDATED_OMICRON_Patients, 
                                     family = poisson)


tab_model(ICU_los_GLMMmodel_Omicron_5)



ICU_los_GLMMmodel_Omicron_6 <- glmer(icu_los ~ gender + (1 | mrn), 
                                     data = ICU_UPDATED_OMICRON_Patients, 
                                     family = poisson)


tab_model(ICU_los_GLMMmodel_Omicron_6)



ICU_los_GLMMmodel_Omicron_7 <- glmer(icu_los ~ race + (1 | mrn), 
                                     data = ICU_UPDATED_OMICRON_Patients, 
                                     family = poisson)


tab_model(ICU_los_GLMMmodel_Omicron_7)



ICU_los_GLMMmodel_Omicron_8 <- glmer(icu_los ~ ethnic + (1 | mrn), 
                                     data = ICU_UPDATED_OMICRON_Patients, 
                                     family = poisson)


tab_model(ICU_los_GLMMmodel_Omicron_8)



ICU_los_GLMMmodel_Omicron_9 <- glmer(icu_los ~ age + (1 | mrn), 
                                     data = ICU_UPDATED_OMICRON_Patients, 
                                     family = poisson)


tab_model(ICU_los_GLMMmodel_Omicron_9)



ICU_los_GLMMmodel_Omicron_10 <- glmer(icu_los ~ financial_class + (1 | mrn), 
                                     data = ICU_UPDATED_OMICRON_Patients, 
                                     family = poisson)


tab_model(ICU_los_GLMMmodel_Omicron_10)


correlation_test <- cor.test(ICU_UPDATED_OMICRON_Patients$Death, ICU_UPDATED_OMICRON_Patients$TestCenters, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_OMICRON_Patients$Death, ICU_UPDATED_OMICRON_Patients$SamplesCollected, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


# correlation_test <- cor.test(ICU_UPDATED_OMICRON_Patients$icu_los, ICU_UPDATED_OMICRON_Patients$PositiveCases, use = "complete.obs")
# 
# # Extract the correlation coefficient and p-value
# correlation <- correlation_test$estimate
# p_value <- correlation_test$p.value
# 
# cat("Correlation coefficient:", correlation, "\n")
# cat("P-value:", p_value, "\n")



ICU_death_GLMMmodel_Omicron_1 <- glmer(Death ~ TestCenters + (1 | mrn), 
                                      data = ICU_UPDATED_OMICRON_Patients, 
                                      family = binomial(link = "logit"))

tab_model(ICU_death_GLMMmodel_Omicron_1)
