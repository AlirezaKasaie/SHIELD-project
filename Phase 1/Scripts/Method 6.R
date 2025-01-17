library(dplyr)
library(lubridate)
library(data.table)

###############################################################################
############################## ALPHA WAVE #####################################
###############################################################################

ICU_UPDATED_Method6_ALPHA = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_Method6_ALPHA <- ICU_UPDATED_Method6_ALPHA %>%
  distinct(zipcode, Date, .keep_all = TRUE)

New_Dataset <- unique(ICU_UPDATED_Method6_ALPHA$zipcode)
months <- c('2021-03', '2021-04', '2021-05', '2021-06', '2021-07', '2021-08')
all_combinations <- expand.grid(zipcode = New_Dataset, Date = months)

ICU_UPDATED_Method6_ALPHA <- all_combinations %>%
  left_join(ICU_UPDATED_Method6_ALPHA, by = c("zipcode", "Date")) %>%
  replace_na(list(Total_ICU_admissions_per_zipcode_per_month = 0))

alpha = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_Method6_ALPHA <- merge(ICU_UPDATED_Method6_ALPHA, alpha, by = c("zipcode"))

unique_zipcodes <- unique(ICU_UPDATED_Method6_ALPHA$zipcode)

write.csv(ICU_UPDATED_Method6_ALPHA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 6/ICU_UPDATED_Method6_ALPHA.csv", row.names = FALSE)

###############################################################################
############################## DELTA WAVE #####################################
###############################################################################

ICU_UPDATED_Method6_DELTA = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_Method6_DELTA <- ICU_UPDATED_Method6_DELTA %>%
  distinct(zipcode, Date, .keep_all = TRUE)

New_Dataset <- unique(ICU_UPDATED_Method6_DELTA$zipcode)
months <- c('2021-08', '2021-09', '2021-10', '2021-11', '2021-12', '2022-01')
all_combinations <- expand.grid(zipcode = New_Dataset, Date = months)

ICU_UPDATED_Method6_DELTA <- all_combinations %>%
  left_join(ICU_UPDATED_Method6_DELTA, by = c("zipcode", "Date")) %>%
  replace_na(list(Total_ICU_admissions_per_zipcode_per_month = 0))

delta = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_Method6_DELTA <- merge(ICU_UPDATED_Method6_DELTA, delta, by = c("zipcode"))

unique_zipcodes <- unique(ICU_UPDATED_Method6_DELTA$zipcode)

write.csv(ICU_UPDATED_Method6_DELTA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 6/ICU_UPDATED_Method6_DELTA.csv", row.names = FALSE)

###############################################################################
############################## OMICRON WAVE ###################################
###############################################################################

ICU_UPDATED_Method6_OMICRON = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_Method6_OMICRON <- ICU_UPDATED_Method6_OMICRON %>%
  distinct(zipcode, Date, .keep_all = TRUE)

New_Dataset <- unique(ICU_UPDATED_Method6_OMICRON$zipcode)
months <- c('2021-12', '2022-01', '2022-02', '2022-03', '2022-04', '2022-05')
all_combinations <- expand.grid(zipcode = New_Dataset, Date = months)

ICU_UPDATED_Method6_OMICRON <- all_combinations %>%
  left_join(ICU_UPDATED_Method6_OMICRON, by = c("zipcode", "Date")) %>%
  replace_na(list(Total_ICU_admissions_per_zipcode_per_month = 0))

omicron = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_Method6_OMICRON <- merge(ICU_UPDATED_Method6_OMICRON, omicron, by = c("zipcode"))

unique_zipcodes <- unique(ICU_UPDATED_Method6_OMICRON$zipcode)

write.csv(ICU_UPDATED_Method6_OMICRON, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 6/ICU_UPDATED_Method6_OMICRON.csv", row.names = FALSE)

###############################################################################
################################## ANALYSES ###################################
############################## ALPHA WAVE ###################################@@
###############################################################################

ICU_UPDATED_ALPHA_Method6 = fread(file.choose(), sep=",", header=T)

Alpha_Method6 = fread(file.choose(), sep=",", header=T)


ICU_UPDATED_ALPHA_Method6$zipcode <- as.character(ICU_UPDATED_ALPHA_Method6$zipcode)
Alpha_Method6$zipcode <- as.character(Alpha_Method6$zipcode)

Alpha_Wave_Method6 <- full_join(ICU_UPDATED_ALPHA_Method6, Alpha_Method6, by = c("zipcode", "Date"))

write.csv(Alpha_Wave_Method6, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 6/Analysis/New folder/Alpha_Wave_Method 6_Final.csv", row.names = FALSE)

Alpha_Wave_Method6_Final_WithNA = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(Alpha_Wave_Method6_Final_WithNA$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Alpha_Wave$`Total ICU Hospitalizations per Zipcode per Month` , type = 3)
# print(skewness_ICU_hospitalization)


Alpha_Wave_Method6_Final_WithNA$`ADI category` = as.factor(Alpha_Wave_Method6_Final_WithNA$`ADI category`)

Alpha_Wave_Method6_Final_WithNA$zipcode = as.factor(Alpha_Wave_Method6_Final_WithNA$zipcode)


Alpha_Wave_Method6_Final_WithNA$`ADI category` <- relevel(Alpha_Wave_Method6_Final_WithNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(`COVID_ICU_Rate` ~  `ADI category` + `Effective Number of Centers`, data = Alpha_Wave_Method6_Final_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Method6_Final_WithNA)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Method6_Final_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Method6_Final_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


Alpha_Wave_Method6_Final_WithOutNA = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(Alpha_Wave_Method6_Final_WithOutNA$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Alpha_Wave$`Total ICU Hospitalizations per Zipcode per Month` , type = 3)
# print(skewness_ICU_hospitalization)


Alpha_Wave_Method6_Final_WithOutNA$`ADI category` = as.factor(Alpha_Wave_Method6_Final_WithOutNA$`ADI category`)

Alpha_Wave_Method6_Final_WithOutNA$zipcode = as.factor(Alpha_Wave_Method6_Final_WithOutNA$zipcode)


Alpha_Wave_Method6_Final_WithOutNA$`ADI category` <- relevel(Alpha_Wave_Method6_Final_WithOutNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(`COVID_ICU_Rate` ~  `ADI category` + `Effective Number of Centers`, data = Alpha_Wave_Method6_Final_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Method6_Final_WithOutNA)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Method6_Final_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Method6_Final_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)

###############################################################################
################################ DELTA Wave ###################################
###############################################################################

ICU_UPDATED_DELTA_Method6 = fread(file.choose(), sep=",", header=T)

Delta_Method6 = fread(file.choose(), sep=",", header=T)


ICU_UPDATED_DELTA_Method6$zipcode <- as.character(ICU_UPDATED_DELTA_Method6$zipcode)
Delta_Method6$zipcode <- as.character(Delta_Method6$zipcode)

Delta_Wave_Method6 <- full_join(ICU_UPDATED_DELTA_Method6, Delta_Method6, by = c("zipcode", "Date"))

write.csv(Delta_Wave_Method6, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 6/Analysis/New folder/Delta_Wave_Method6_Final.csv", row.names = FALSE)

Delta_Wave_Final = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(Delta_Wave_Final$`COVID_ICU_Rate` , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Delta_Wave$`Total ICU Hospitalizations per Zipcode per Month` , type = 3)
# print(skewness_ICU_hospitalization)


Delta_Wave_Final$`ADI category` = as.factor(Delta_Wave_Final$`ADI category`)

Delta_Wave_Final$zipcode = as.factor(Delta_Wave_Final$zipcode)


Delta_Wave_Final$`ADI category` <- relevel(Delta_Wave_Final$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(`COVID_ICU_Rate` ~  `ADI category` + `Effective Number of Centers`, data = Delta_Wave_Final)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Final)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Final)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Final)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)

###############################################################################
################################ OMICRON Wave #################################
###############################################################################


ICU_UPDATED_Omicron_Method6 = fread(file.choose(), sep=",", header=T)

Omicron_Method6 = fread(file.choose(), sep=",", header=T)


ICU_UPDATED_Omicron_Method6$zipcode <- as.character(ICU_UPDATED_Omicron_Method6$zipcode)
Omicron_Method6$zipcode <- as.character(Omicron_Method6$zipcode)

Omicron_Wave_Method6 <- full_join(ICU_UPDATED_Omicron_Method6, Omicron_Method6, by = c("zipcode", "Date"))

write.csv(Omicron_Wave_Method6, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 6/Analysis/New folder/Omicron_Wave_Method 6_Final.csv", row.names = FALSE)

Omicron_Wave_Final = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(Omicron_Wave_Final$`COVID_ICU_Rate` , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Omicron_Wave$`Total ICU Hospitalizations per Zipcode per Month` , type = 3)
# print(skewness_ICU_hospitalization)


Omicron_Wave_Final$`ADI category` = as.factor(Omicron_Wave_Final$`ADI category`)

Omicron_Wave_Final$zipcode = as.factor(Omicron_Wave_Final$zipcode)


Omicron_Wave_Final$`ADI category` <- relevel(Omicron_Wave_Final$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(`COVID_ICU_Rate` ~  `ADI category` + `Effective Number of Centers`, data = Omicron_Wave_Final)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Final)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `ADI category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Final)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Final)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)
