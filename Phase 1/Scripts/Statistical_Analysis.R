library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)
library(sjPlot)
library(car)
library(scales)
library(broom.mixed)
library(sjmisc)
library(sjstats)
install.packages("corrplot")
library(corrplot)
library(e1071)
library(readxl)
install.packages("glmmTMB")
library(glmmTMB)
library(MASS)




#ICU_UPDATED = read.csv(file.choose(), sep=",", header=T)

#count_per_zip_ICU = ICU_UPDATED%>%
  #group_by(zipcode) %>%
  #summarise(total.ICU.admission.per.zipcode = n())

#count_per_zip_ICU = count_per_zip_ICU[order(count_per_zip_ICU$total.ICU.admission.per.zipcode, decreasing = TRUE), ]

#count_per_zip_ICU = count_per_zip_ICU %>% filter(total.ICU.admission.per.zipcode > 10)

#ICU_UPDATED_2_NEW = ICU_UPDATED %>%
  #filter(zipcode %in% count_per_zip_ICU$zipcode)

#ICU_UPDATED_2_NEW = ICU_UPDATED_2_NEW %>%
  #select(-c("dob","st","city","death_date","adm_date_time","disch_date_time","los","last_icu_dt","discharge_disposition":"DX_NAME"))

#ICU_UPDATED_2_NEW$first_icu_dt = as.Date(ICU_UPDATED_2_NEW$first_icu_dt, format = "%Y-%m-%d")
#ICU_UPDATED_2_NEW$Date = format(ICU_UPDATED_2_NEW$first_icu_dt, "%Y-%m")

#ICU_UPDATED_2_NEW$first_icu_dt = NULL


#ICU_UPDATED_2_NEW = ICU_UPDATED_2_NEW %>%
  #group_by(zipcode,Date) %>%
  #mutate(Total_ICU_admission_per_zipcode_per_month = n()) %>%
  #ungroup()

#ICU_UPDATED_2_NEW = ICU_UPDATED_2_NEW %>%
  #group_by(Date) %>%
  #mutate(Total_ICU_admission_per_month = n()) %>%
  #ungroup()

#write.csv(ICU_UPDATED_2_NEW, file = "C:/Users/skasaiesharifi/Documents/ICU_UPDATED_2_NEW.csv", row.names = FALSE)

#ICU_UPDATED_2_NEW = read.csv(file.choose(), sep=",", header=T)

#ICU_UPDATED_2_NEW$race = factor(ICU_UPDATED_2_NEW$race)
#ICU_UPDATED_2_NEW$gender = factor(ICU_UPDATED_2_NEW$gender)
#ICU_UPDATED_2_NEW$ethnic = factor(ICU_UPDATED_2_NEW$ethnic)
#ICU_UPDATED_2_NEW$financial_class = factor(ICU_UPDATED_2_NEW$financial_class)

# predictor_variables = ICU_UPDATED_2[, c("Number_of_SHIELD_centers", "Number_of_Other_centers", "avg_Age", "avg_icu_los", "Number.of.Hispanic", "Number.of.Non.Hispanic",
                                        # "Number.of.Insured", "Number.of.Uninsured")]

# Compute the correlation matrix
# correlation_matrix = cor(predictor_variables)

# correlation_matrix = as.data.frame(correlation_matrix)

# print(correlation_matrix)

# icu_admission_model = lm(total.ICU.admission.per.zipcode ~ Number_of_SHIELD_centers+Number_of_Other_centers+avg_Age+avg_icu_los+
                             # Number.of.Hispanic+Number.of.Non.Hispanic+Number.of.Insured+Number.of.Uninsured,data=ICU_UPDATED_2)

# death_model = lm(total_death ~ total.tests.per.month+Cumulative.Tests+Education..9th.grade.+Education..at.least.HS.diploma.+Education..at.least.Bachelor.s.degree.+
                          #Population.Uninsured+Population..Insured+SHIELDS+Other+ADI+Age..0.17.+Age..18.64.+Age..65..+Sex..Male.+Sex..Female.+Race..Other.+Race..Black.African.American.+
                          #Race..White.+Ethnicity..Hispanic.+Ethnicity..Non.Hispanic.,data = Death_Data)

#alias(death_model)

#vif_values = vif(icu_admission_model)


ICU_UPDATED_ALPHA = read.csv(file.choose(), sep=",", header=T)

POPULATION = read.csv(file.choose(), sep=",", header=T)

POPULATION = POPULATION %>%
  rename(zipcode = ZIP)

ICU_UPDATED_2 = merge(ICU_UPDATED_2, POPULATION, by = "zipcode")

write.csv(ICU_UPDATED_2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/ICU_UPDATED_Zipcode.csv", row.names = FALSE)

#*******************************Multivariable modeling - ALPHA *************************

remove.packages("lme4")
remove.packages("Matrix")

# Reinstall Matrix first, then lme4
install.packages("Matrix")
install.packages("lme4")

#count_per_zip_ICU = ICU_UPDATED_2%>%
  #group_by(zipcode) %>%
  #summarise(total.ICU.admission.per.zipcode = sum(total.ICU.admission.per.zipcode))

#count_per_zip_ICU = count_per_zip_ICU[order(count_per_zip_ICU$total.ICU.admission.per.zipcode, decreasing = TRUE), ]

#count_per_zip_ICU = count_per_zip_ICU %>% filter(total.ICU.admission.per.zipcode >= 10)

#count_per_zip_SHIELDS = ICU_UPDATED_2%>%
  #group_by(zipcode) %>%
  #summarise(total.SHIELDS.per.zipcode = max(Number_of_SHIELD_centers))

#count_per_zip_SHIELDS = count_per_zip_SHIELDS[order(count_per_zip_SHIELDS$total.SHIELDS.per.zipcode, decreasing = TRUE), ]

#filtered_ICU_UPDATED_2 = ICU_UPDATED_2[ICU_UPDATED_2$zipcode %in% count_per_zip_ICU$zipcode, ]


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#tab_model(icu_admission_GLMMmodel)

#ICU_UPDATED_FINAL_2 = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_ALPHA_Zipcode = read.csv(file.choose(), sep=",", header=T)

#ICU_UPDATED_ALPHA$ADI.category=NULL

#ICU_UPDATED_ALPHA$Date = as.factor(ICU_UPDATED_ALPHA$Date)


#ICU_UPDATED_ALPHA$Covid.ICU.Rate <- ICU_UPDATED_ALPHA$Covid.ICU.Rate + 1e-10

#Checking the skewness of the ICU rate. Type specifies the algorithm used to calculate skewness. 
#Type=3 is the default and corresponds to the adjusted Fisher-Pearson standardized moment coefficient, which is commonly used.

skewness_ICU_rate = skewness(ICU_UPDATED_ALPHA_Zipcode$Covid.ICU.Rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_ALPHA_Zipcode$Total.ICU.Hospitalizations.per.Zipcode.per.Month , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_ALPHA_Zipcode$ADI.category = as.factor(ICU_UPDATED_ALPHA_Zipcode$ADI.category)

ICU_UPDATED_ALPHA_Zipcode$zipcode = as.factor(ICU_UPDATED_ALPHA_Zipcode$zipcode)


ICU_UPDATED_ALPHA_Zipcode$ADI.category <- relevel(ICU_UPDATED_ALPHA_Zipcode$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Covid.ICU.Rate ~ SamplesCollected + PositiveCases + ADI.category + TestCenters, data = ICU_UPDATED_ALPHA_Zipcode)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Covid.ICU.Rate + 1) ~ TestCenters + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_ALPHA_Zipcode)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)



ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_ALPHA_Zipcode)


tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)



ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Covid.ICU.Rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_ALPHA_Zipcode)


tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


# ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Covid.ICU.Rate + 1) ~ PositiveCases + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_ALPHA_Zipcode)
#
#
#tab_model(ICU_rate_GLMMmodel_ALPHA_4)



ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Covid.ICU.Rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_ALPHA_Zipcode)


tab_model(ICU_rate_GLMMmodel_ALPHA_5, digits = 5)



ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_ALPHA_Zipcode)


tab_model(ICU_rate_GLMMmodel_ALPHA_6, digits = 5)



# ICU_rate_GLMMmodel_ALPHA_7 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*PositiveCases + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_ALPHA_Zipcode)
# 
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_7)



correlation_test <- cor.test(ICU_UPDATED_ALPHA_Zipcode$Total.ICU.Hospitalizations.per.Zipcode.per.Month, ICU_UPDATED_ALPHA_Zipcode$TestCenters, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_ALPHA_Zipcode$Total.ICU.Hospitalizations.per.Zipcode.per.Month, ICU_UPDATED_ALPHA_Zipcode$SamplesCollected, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_ALPHA_Zipcode$Total.ICU.Hospitalizations.per.Zipcode.per.Month, ICU_UPDATED_ALPHA_Zipcode$PositiveCases, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


vif_model <- lm(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + PositiveCases + ADI.category + TestCenters, data = ICU_UPDATED_ALPHA_Zipcode)
vif(vif_model)


ICU_UPDATED_ALPHA_Zipcode$SamplesCollected <- scale(ICU_UPDATED_ALPHA_Zipcode$SamplesCollected)



ICU_hospitalization_GLMMmodel_Alpha_1 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ TestCenters + (1 | zipcode), 
                                   data = ICU_UPDATED_ALPHA_Zipcode, 
                                   family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_1, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_2 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_ALPHA_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_2, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_3 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_ALPHA_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_3, digits = 5)


# ICU_hospitalization_GLMMmodel_Alpha_4 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ PositiveCases + (1 | zipcode), 
#                                                data = ICU_UPDATED_ALPHA_Zipcode, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Alpha_4)


ICU_hospitalization_GLMMmodel_Alpha_5 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ TestCenters*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_ALPHA_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_5, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_6 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category*SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_ALPHA_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_6, digits = 5)


# ICU_hospitalization_GLMMmodel_Alpha_7 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category*PositiveCases + (1 | zipcode), 
#                                                data = ICU_UPDATED_ALPHA_Zipcode, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Alpha_7)



#*******************************Multivariable modeling - DELTA *************************

ICU_UPDATED_DELTA_Zipcode = read.csv(file.choose(), sep=",", header=T)


skewness_ICU_rate = skewness(ICU_UPDATED_DELTA_Zipcode$Covid.ICU.Rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_DELTA_Zipcode$Total.ICU.Hospitalizations.per.Zipcode.per.Month , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_DELTA_Zipcode$ADI.category = as.factor(ICU_UPDATED_DELTA_Zipcode$ADI.category)

ICU_UPDATED_DELTA_Zipcode$zipcode = as.factor(ICU_UPDATED_DELTA_Zipcode$zipcode)


ICU_UPDATED_DELTA_Zipcode$ADI.category <- relevel(ICU_UPDATED_DELTA_Zipcode$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Covid.ICU.Rate ~ SamplesCollected + PositiveCases + ADI.category + TestCenters, data = ICU_UPDATED_DELTA_Zipcode)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Covid.ICU.Rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_DELTA_Zipcode)


tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)



ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_DELTA_Zipcode)


tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_UPDATED_DELTA_Zipcode$SamplesCollected <- scale(ICU_UPDATED_DELTA_Zipcode$SamplesCollected)



ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Covid.ICU.Rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_DELTA_Zipcode)


tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


# ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Covid.ICU.Rate + 1) ~ PositiveCases + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_DELTA_Zipcode)
# 
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_4)



ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Covid.ICU.Rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_DELTA_Zipcode)


tab_model(ICU_rate_GLMMmodel_DELTA_5, digits = 5)



ICU_rate_GLMMmodel_DELTA_6 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_DELTA_Zipcode)


tab_model(ICU_rate_GLMMmodel_DELTA_6, digits = 5)



# ICU_rate_GLMMmodel_DELTA_7 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*PositiveCases + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_DELTA_Zipcode)
# 
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_7)



correlation_test <- cor.test(ICU_UPDATED_DELTA_Zipcode$Covid.ICU.Rate, ICU_UPDATED_DELTA_Zipcode$TestCenters, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_DELTA_Zipcode$Covid.ICU.Rate, ICU_UPDATED_DELTA_Zipcode$SamplesCollected, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_DELTA_Zipcode$Covid.ICU.Rate, ICU_UPDATED_DELTA_Zipcode$PositiveCases, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


vif_model <- lm(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + PositiveCases + ADI.category + TestCenters, data = ICU_UPDATED_DELTA_Zipcode)
vif(vif_model)


ICU_hospitalization_GLMMmodel_DELTA_1 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_DELTA_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_DELTA_1, digits = 5)


ICU_hospitalization_GLMMmodel_DELTA_2 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_DELTA_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_DELTA_2, digits = 5)


ICU_hospitalization_GLMMmodel_DELTA_3 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_DELTA_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_DELTA_3, digits = 5)


# ICU_hospitalization_GLMMmodel_DELTA_4 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ PositiveCases + (1 | zipcode), 
#                                                data = ICU_UPDATED_DELTA_Zipcode, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_DELTA_4)


ICU_hospitalization_GLMMmodel_DELTA_5 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ TestCenters*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_DELTA_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_DELTA_5, digits = 5)


ICU_hospitalization_GLMMmodel_DELTA_6 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category*SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_DELTA_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_DELTA_6, digits = 5)


# ICU_hospitalization_GLMMmodel_DELTA_7 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category*PositiveCases + (1 | zipcode), 
#                                                data = ICU_UPDATED_DELTA_Zipcode, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_DELTA_7)


#*******************************Multivariable modeling - OMICRON *************************

ICU_UPDATED_OMICRON_Zipcode = read.csv(file.choose(), sep=",", header=T)


skewness_ICU_rate = skewness(ICU_UPDATED_OMICRON_Zipcode$Covid.ICU.Rate , type = 3)
print(skewness_ICU_rate)

skewness_ICU_hospitalization = skewness(ICU_UPDATED_OMICRON_Zipcode$Total.ICU.Hospitalizations.per.Zipcode.per.Month , type = 3)
print(skewness_ICU_hospitalization)


ICU_UPDATED_OMICRON_Zipcode$ADI.category = as.factor(ICU_UPDATED_OMICRON_Zipcode$ADI.category)

ICU_UPDATED_OMICRON_Zipcode$zipcode = as.factor(ICU_UPDATED_OMICRON_Zipcode$zipcode)


ICU_UPDATED_OMICRON_Zipcode$ADI.category <- relevel(ICU_UPDATED_OMICRON_Zipcode$ADI.category, ref = "Less Disadvantaged")


vif_model <- lm(Covid.ICU.Rate ~ SamplesCollected + PositiveCases + ADI.category + TestCenters, data = ICU_UPDATED_OMICRON_Zipcode)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Covid.ICU.Rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_OMICRON_Zipcode)


tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)



ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_OMICRON_Zipcode)


tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_UPDATED_OMICRON_Zipcode$SamplesCollected <- scale(ICU_UPDATED_OMICRON_Zipcode$SamplesCollected)



ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Covid.ICU.Rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_OMICRON_Zipcode)


tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


# ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Covid.ICU.Rate + 1) ~ PositiveCases + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_OMICRON_Zipcode)
# 
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_4)



ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Covid.ICU.Rate + 1) ~ TestCenters*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_OMICRON_Zipcode)


tab_model(ICU_rate_GLMMmodel_OMICRON_5, digits = 5)



ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_OMICRON_Zipcode)


tab_model(ICU_rate_GLMMmodel_OMICRON_6, digits = 5)



ICU_rate_GLMMmodel_OMICRON_7 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*PositiveCases + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_OMICRON_Zipcode)


tab_model(ICU_rate_GLMMmodel_OMICRON_7)



correlation_test <- cor.test(ICU_UPDATED_OMICRON_Zipcode$Total.ICU.Hospitalizations.per.Zipcode.per.Month, ICU_UPDATED_OMICRON_Zipcode$TestCenters, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_OMICRON_Zipcode$Total.ICU.Hospitalizations.per.Zipcode.per.Month, ICU_UPDATED_OMICRON_Zipcode$SamplesCollected, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_OMICRON_Zipcode$Total.ICU.Hospitalizations.per.Zipcode.per.Month, ICU_UPDATED_OMICRON_Zipcode$PositiveCases, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


vif_model <- lm(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + PositiveCases + ADI.category + TestCenters, data = ICU_UPDATED_OMICRON_Zipcode)
vif(vif_model)


ICU_hospitalization_GLMMmodel_OMICRON_1 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_OMICRON_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_OMICRON_1, digits = 5)


ICU_hospitalization_GLMMmodel_OMICRON_2 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_OMICRON_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_OMICRON_2, digits = 5)


ICU_hospitalization_GLMMmodel_OMICRON_3 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_OMICRON_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_OMICRON_3, digits = 5)


#ICU_UPDATED_OMICRON_Zipcode$PositiveCases <- scale(ICU_UPDATED_OMICRON_Zipcode$PositiveCases)


# ICU_hospitalization_GLMMmodel_OMICRON_4 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ PositiveCases + (1 | zipcode), 
#                                                data = ICU_UPDATED_OMICRON_Zipcode, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_OMICRON_4)


ICU_hospitalization_GLMMmodel_OMICRON_5 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ TestCenters*ADI.category + (1 | zipcode), 
                                               data = ICU_UPDATED_OMICRON_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_OMICRON_5, digits = 5)


ICU_hospitalization_GLMMmodel_OMICRON_6 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category*SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_OMICRON_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_OMICRON_6, digits = 5)


ICU_hospitalization_GLMMmodel_OMICRON_7 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category*PositiveCases + (1 | zipcode), 
                                               data = ICU_UPDATED_OMICRON_Zipcode, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_OMICRON_7)



