library('data.table')
library(e1071)
library(car)
library(lme4)
library(sjPlot)
library(dplyr)
library(tidyr)

#covid_specimen <- fread(file.choose(), sep=",", header=T)


covid_specimen <- fread(file.choose(), sep=",", header=T)

covid_specimen$'DATE - COLLECTED' <- as.POSIXct(covid_specimen$'DATE - COLLECTED', format = "%m/%d/%Y %I:%M:%S %p")
covid_specimen$SpecimenBarcode <- as.factor(covid_specimen$'SPECIMEN BARCODE')
covid_specimen$location <- as.factor(covid_specimen$'LOCATION')
covid_specimen$Result <- as.factor(covid_specimen$'RESULT (P/Other)')
covid_specimen$Date <- as.Date(covid_specimen$'DATE - COLLECTED')
covid_specimen$Site.SHIELD.Id <- as.factor(covid_specimen$'Site.SHIELD.Id')


sample <- covid_specimen[, c("[NAI]PATIENT ZIP", "location", "SPECIMEN BARCODE", "RESULT", "Date", "Site.SHIELD.Id")]
sample <- sample %>% rename(zipcode = '[NAI]PATIENT ZIP')

sample[sample == ""] <- NA
missing_values <- is.na(sample)
missing_count <- colSums(missing_values)
missing_count

sample <- sample[complete.cases(sample), ]

filtered_sample <- sample %>% filter(grepl("^[0-9]+$", zipcode))
filtered_sample <- filtered_sample %>% filter(zipcode >= 60002 & zipcode <= 62959)
filtered_sample <- filtered_sample %>% mutate(zipcode = substr(zipcode, 1, 5))

filtered_sample$zipcode <- as.factor(filtered_sample$zipcode)

############################## Alpha Wave #####################################


alpha <- filtered_sample %>% filter(as.Date(Date, format = "%m/%d/%Y") >= as.Date("2021-08-01") &
                                      as.Date(Date, format = "%m/%d/%Y") <= as.Date("2021-08-31"))

b <- alpha %>% mutate(Month = format(Date, "%Y-%m"))
b$Date <- NULL

# monthly_alpha <- b %>% group_by(zipcode, Month) %>%
#   summarise(SamplesCollected = n_distinct(`SPECIMEN BARCODE`),
#             TestCenters = n_distinct(location))

#write.csv(alpha, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Alpha/Alpha.csv", row.names = FALSE)

sum(is.na(alpha))

#Add a column for counting purposes
alpha <- alpha %>%
  mutate(Count = 1)

# Create a pivot table
alpha_new <- alpha %>%
  group_by(Site.SHIELD.Id, zipcode) %>%
  summarise(Count = sum(Count), .groups = 'drop') %>%
  spread(zipcode, Count, fill = 0)

# Calculate total for each column and add as a new row
total_row <- alpha_new %>%
  summarise(across(-Site.SHIELD.Id, sum)) %>%
  mutate(Site.SHIELD.Id = "Total")

alpha_new <- bind_rows(alpha_new, total_row)

# Calculate Total for each row and add it as a new column
alpha_new <- alpha_new %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()

# Create a new matrix by dividing each element by the total of each row
alpha_new2 <- alpha_new %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~ . / Total)) %>%
  ungroup()

# Remove the Total column for the final proportion matrix
alpha_new2 <- alpha_new2 %>%
  select(-Total)

# Calculate the proportion of test centers serving each Zipcode and add as a new row
proportion_row <- alpha_new2 %>%
  filter(Site.SHIELD.Id != "Total") %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(Site.SHIELD.Id = "Proportion of test centers serve the Zipcode")

# Bind the proportion row to the matrix
alpha_new2 <- bind_rows(alpha_new2, proportion_row)

write.csv(alpha_new2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Alpha/Alpha-August.csv", row.names = FALSE)


############################## Delta Wave #####################################


delta <- filtered_sample %>% filter(as.Date(Date, format = "%m/%d/%Y") >= as.Date("2022-01-01") &
                                      as.Date(Date, format = "%m/%d/%Y") <= as.Date("2022-01-31"))

b <- delta %>% mutate(Month = format(Date, "%Y-%m"))
b$Date <- NULL

# monthly_alpha <- b %>% group_by(zipcode, Month) %>%
#   summarise(SamplesCollected = n_distinct(`SPECIMEN BARCODE`),
#             TestCenters = n_distinct(location))

#write.csv(delta, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Delta/Delta.csv", row.names = FALSE)

sum(is.na(delta))

#Add a column for counting purposes
delta <- delta %>%
  mutate(Count = 1)

# Create a pivot table
delta_new <- delta %>%
  group_by(Site.SHIELD.Id, zipcode) %>%
  summarise(Count = sum(Count), .groups = 'drop') %>%
  spread(zipcode, Count, fill = 0)

# Calculate total for each column and add as a new row
total_row <- delta_new %>%
  summarise(across(-Site.SHIELD.Id, sum)) %>%
  mutate(Site.SHIELD.Id = "Total")

delta_new <- bind_rows(delta_new, total_row)

# Calculate Total for each row and add it as a new column
delta_new <- delta_new %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()

# Create a new matrix by dividing each element by the total of each row
delta_new2 <- delta_new %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~ . / Total)) %>%
  ungroup()

# Remove the Total column for the final proportion matrix
delta_new2 <- delta_new2 %>%
  select(-Total)

# Calculate the proportion of test centers serving each Zipcode and add as a new row
proportion_row <- delta_new2 %>%
  filter(Site.SHIELD.Id != "Total") %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(Site.SHIELD.Id = "Proportion of test centers serve the Zipcode")

# Bind the proportion row to the matrix
delta_new2 <- bind_rows(delta_new2, proportion_row)

write.csv(delta_new2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Delta/Delta-January.csv", row.names = FALSE)


############################## Omicron Wave #####################################


omicron <- filtered_sample %>% filter(as.Date(Date, format = "%m/%d/%Y") >= as.Date("2022-05-01") &
                                      as.Date(Date, format = "%m/%d/%Y") <= as.Date("2022-05-31"))

b <- omicron %>% mutate(Month = format(Date, "%Y-%m"))
b$Date <- NULL

# monthly_alpha <- b %>% group_by(zipcode, Month) %>%
#   summarise(SamplesCollected = n_distinct(`SPECIMEN BARCODE`),
#             TestCenters = n_distinct(location))

#write.csv(omicron, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Omicron/Omicron.csv", row.names = FALSE)

sum(is.na(omicron))

#Add a column for counting purposes
omicron <- omicron %>%
  mutate(Count = 1)

# Create a pivot table
omicron_new <- omicron %>%
  group_by(Site.SHIELD.Id, zipcode) %>%
  summarise(Count = sum(Count), .groups = 'drop') %>%
  spread(zipcode, Count, fill = 0)

# Calculate total for each column and add as a new row
total_row <- omicron_new %>%
  summarise(across(-Site.SHIELD.Id, sum)) %>%
  mutate(Site.SHIELD.Id = "Total")

omicron_new <- bind_rows(omicron_new, total_row)

# Calculate Total for each row and add it as a new column
omicron_new <- omicron_new %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()

# Create a new matrix by dividing each element by the total of each row
omicron_new2 <- omicron_new %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~ . / Total)) %>%
  ungroup()

# Remove the Total column for the final proportion matrix
omicron_new2 <- omicron_new2 %>%
  select(-Total)

# Calculate the proportion of test centers serving each Zipcode and add as a new row
proportion_row <- omicron_new2 %>%
  filter(Site.SHIELD.Id != "Total") %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(Site.SHIELD.Id = "Proportion of test centers serve the Zipcode")

# Bind the proportion row to the matrix
omicron_new2 <- bind_rows(omicron_new2, proportion_row)

write.csv(omicron_new2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Omicron/Omicron-May.csv", row.names = FALSE)


###############################################################################

################################# Analysis ####################################

################################ Alpha Wave ###################################

###############################################################################

ICU_UPDATED_Alpha = fread(file.choose(), sep=",", header=T)

Alpha_Method3 = fread(file.choose(), sep=",", header=T)


ICU_UPDATED_Alpha$zipcode <- as.character(ICU_UPDATED_Alpha$zipcode)
Alpha_Method3$zipcode <- as.character(Alpha_Method3$zipcode)

Alpha_Wave <- full_join(ICU_UPDATED_Alpha, Alpha_Method3, by = c("zipcode", "Date"))

write.csv(Alpha_Wave, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/New folder/Alpha_Wave_Final.csv", row.names = FALSE)

Alpha_Wave_Final_WithNA = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(Alpha_Wave_Final_WithNA$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Alpha_Wave$`Total ICU Hospitalizations per Zipcode per Month` , type = 3)
# print(skewness_ICU_hospitalization)


Alpha_Wave_Final_WithNA$`ADI category` = as.factor(Alpha_Wave_Final_WithNA$`ADI category`)

Alpha_Wave_Final_WithNA$zipcode = as.factor(Alpha_Wave_Final_WithNA$zipcode)


Alpha_Wave_Final_WithNA$`ADI category` <- relevel(Alpha_Wave_Final_WithNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(`Covid ICU Rate` ~  `ADI category` + `Effective Number of Centers`, data = Alpha_Wave_Final_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Final_WithNA)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Final_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Final_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


# vif_model <- lm(`Total ICU Hospitalizations per Zipcode per Month` ~  `ADI category` + `testcenters`, data = Alpha_Wave)
# vif(vif_model)
# 
# 
# ICU_hospitalization_GLMMmodel_Alpha_1 <- glmer(`Total ICU Hospitalizations per Zipcode per Month` ~ `testcenters` + 
#                                                  (1 | zipcode), 
#                                                data = Alpha_Wave, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Alpha_1, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Alpha_2 <- glmer(`Total ICU Hospitalizations per Zipcode per Month` ~ `ADI category` + 
#                                                  (1 | zipcode), 
#                                                data = Alpha_Wave, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Alpha_2, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Alpha_3 <- glmer(`Total ICU Hospitalizations per Zipcode per Month` ~ `testcenters`*`ADI category` + 
#                                                  (1 | zipcode), 
#                                                data = Alpha_Wave, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Alpha_3, digits = 5)


# write.csv(Alpha_Wave, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Alpha-Wave-Final-Dataset.csv", row.names = FALSE)



Alpha_Wave_Final_WithOutNA = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(Alpha_Wave_Final_WithOutNA$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


Alpha_Wave_Final_WithOutNA$`ADI category` = as.factor(Alpha_Wave_Final_WithOutNA$`ADI category`)

Alpha_Wave_Final_WithOutNA$zipcode = as.factor(Alpha_Wave_Final_WithOutNA$zipcode)


Alpha_Wave_Final_WithOutNA$`ADI category` <- relevel(Alpha_Wave_Final_WithOutNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(`Covid ICU Rate` ~  `ADI category` + `Effective Number of Centers`, data = Alpha_Wave_Final_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Final_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Final_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Final_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


###############################################################################

################################ Delta Wave ###################################

###############################################################################


ICU_UPDATED_Delta = fread(file.choose(), sep=",", header=T)

Delta_Method3 = fread(file.choose(), sep=",", header=T)


ICU_UPDATED_Delta$zipcode <- as.character(ICU_UPDATED_Delta$zipcode)
Delta_Method3$zipcode <- as.character(Delta_Method3$zipcode)

Delta_Wave <- full_join(ICU_UPDATED_Delta, Delta_Method3, by = c("zipcode", "Date"))

write.csv(Delta_Wave, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/New folder/Delta_Wave_Final.csv", row.names = FALSE)

Delta_Wave_Final_WithNA = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(Delta_Wave_Final_WithNA$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Delta_Wave$`Total ICU Hospitalizations per Zipcode per Month` , type = 3)
# print(skewness_ICU_hospitalization)


Delta_Wave_Final_WithNA$`ADI category` = as.factor(Delta_Wave_Final_WithNA$`ADI category`)

Delta_Wave_Final_WithNA$zipcode = as.factor(Delta_Wave_Final_WithNA$zipcode)


Delta_Wave_Final_WithNA$`ADI category` <- relevel(Delta_Wave_Final_WithNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(`Covid ICU Rate` ~  `ADI category` + `Effective Number of Centers`, data = Delta_Wave_Final_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Final_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Final_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Final_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


# vif_model <- lm(`Total ICU Hospitalizations per Zipcode per Month` ~  `ADI category` + `testcenters`, data = Delta_Wave)
# vif(vif_model)
# 
# ICU_hospitalization_GLMMmodel_Delta_1 <- glmer(`Total ICU Hospitalizations per Zipcode per Month` ~ `testcenters` + 
#                                                  (1 | zipcode), 
#                                                data = Delta_Wave, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Delta_1, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Delta_2 <- glmer(`Total ICU Hospitalizations per Zipcode per Month` ~ `ADI category` + 
#                                                  (1 | zipcode), 
#                                                data = Delta_Wave, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Delta_2, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Delta_3 <- glmer(`Total ICU Hospitalizations per Zipcode per Month` ~ `testcenters`*`ADI category` + 
#                                                  (1 | zipcode), 
#                                                data = Delta_Wave, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Delta_3, digits = 5)
# 
# 
# write.csv(Delta_Wave, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Delta-Wave-Final-Dataset.csv", row.names = FALSE)


Delta_Wave_Final_WithOutNA = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(Delta_Wave_Final_WithOutNA$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Delta_Wave$`Total ICU Hospitalizations per Zipcode per Month` , type = 3)
# print(skewness_ICU_hospitalization)


Delta_Wave_Final_WithOutNA$`ADI category` = as.factor(Delta_Wave_Final_WithOutNA$`ADI category`)

Delta_Wave_Final_WithOutNA$zipcode = as.factor(Delta_Wave_Final_WithOutNA$zipcode)


Delta_Wave_Final_WithOutNA$`ADI category` <- relevel(Delta_Wave_Final_WithOutNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(`Covid ICU Rate` ~  `ADI category` + `Effective Number of Centers`, data = Delta_Wave_Final_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Final_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Final_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Final_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


###############################################################################

################################ Omicron Wave ###################################

###############################################################################


ICU_UPDATED_Omicron = fread(file.choose(), sep=",", header=T)

Omicron_Method3 = fread(file.choose(), sep=",", header=T)


ICU_UPDATED_Omicron$zipcode <- as.character(ICU_UPDATED_Omicron$zipcode)
Omicron_Method3$zipcode <- as.character(Omicron_Method3$zipcode)

Omicron_Wave <- full_join(ICU_UPDATED_Omicron, Omicron_Method3, by = c("zipcode", "Date"))

write.csv(Omicron_Wave, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/New folder/Omicron_Wave_Final.csv", row.names = FALSE)

Omicron_Wave_Final_WithNA = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(Omicron_Wave_Final_WithNA$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Omicron_Wave$`Total ICU Hospitalizations per Zipcode per Month` , type = 3)
# print(skewness_ICU_hospitalization)


Omicron_Wave_Final_WithNA$`ADI category` = as.factor(Omicron_Wave_Final_WithNA$`ADI category`)

Omicron_Wave_Final_WithNA$zipcode = as.factor(Omicron_Wave_Final_WithNA$zipcode)


Omicron_Wave_Final_WithNA$`ADI category` <- relevel(Omicron_Wave_Final_WithNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(`Covid ICU Rate` ~  `ADI category` + `Effective Number of Centers`, data = Omicron_Wave_Final_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Omicron_Wave_Final_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Omicron_Wave_Final_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Omicron_Wave_Final_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


# vif_model <- lm(`Total ICU Hospitalizations per Zipcode per Month` ~  `ADI category` + `testcenters`, data = Omicron_Wave)
# vif(vif_model)
# 
# 
# ICU_hospitalization_GLMMmodel_Omicron_1 <- glmer(`Total ICU Hospitalizations per Zipcode per Month` ~ `testcenters` + 
#                                                  (1 | zipcode), 
#                                                data = Omicron_Wave, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Omicron_1, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Omicron_2 <- glmer(`Total ICU Hospitalizations per Zipcode per Month` ~ `ADI category` + 
#                                                    (1 | zipcode), 
#                                                  data = Omicron_Wave, 
#                                                  family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Omicron_2, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Omicron_3 <- glmer(`Total ICU Hospitalizations per Zipcode per Month` ~ `testcenters`*`ADI category` + 
#                                                    (1 | zipcode), 
#                                                  data = Omicron_Wave, 
#                                                  family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Omicron_3, digits = 5)


Omicron_Wave_Final_WithOutNA = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(Omicron_Wave_Final_WithOutNA$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


Omicron_Wave_Final_WithOutNA$`ADI category` = as.factor(Omicron_Wave_Final_WithOutNA$`ADI category`)

Omicron_Wave_Final_WithOutNA$zipcode = as.factor(Omicron_Wave_Final_WithOutNA$zipcode)


Omicron_Wave_Final_WithOutNA$`ADI category` <- relevel(Omicron_Wave_Final_WithOutNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(`Covid ICU Rate` ~  `ADI category` + `Effective Number of Centers`, data = Omicron_Wave_Final_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Final_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `ADI category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Final_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Final_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)




#write.csv(Omicron_Wave, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Omicron-Wave-Final-Dataset.csv", row.names = FALSE)


###############################################################################

################################# Lag Analysis ################################

################################ One-Month Lag ################################

################################ Alpha Wave ###################################

###############################################################################

Alpha_Method3_Lag1 = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_ALPHA_Lag1 = fread(file.choose(), sep=",", header=T)

Alpha_Method3_Lag1$zipcode <- as.character(Alpha_Method3_Lag1$zipcode)

ICU_UPDATED_ALPHA_Lag1$zipcode <- as.character(ICU_UPDATED_ALPHA_Lag1$zipcode)

Alpha_Wave_Lag1 <- full_join(ICU_UPDATED_ALPHA_Lag1, Alpha_Method3_Lag1, by = c("zipcode", "Date"))

Alpha_Wave_Lag1_WithNA = fread(file.choose(), sep=",", header=T)

Alpha_Wave_Lag1_WithNA <- Alpha_Wave_Lag1_WithNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Covid_ICU_admission_rate = lag(`Covid ICU Rate`, 1)
  ) %>%
  ungroup()

Alpha_Wave_Lag1_WithNA = na.omit(Alpha_Wave_Lag1_WithNA)


write.csv(Alpha_Wave_Lag1_WithNA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Alpha-Wave-Lag1-WithNA-Final.csv", row.names = FALSE)


skewness_ICU_rate = skewness(Alpha_Wave_Lag1_WithNA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Alpha_Wave_Lag1$Lag1_Number_of_ICU_admissions , type = 3)
# print(skewness_ICU_hospitalization)


Alpha_Wave_Lag1_WithNA$`ADI category`= as.factor(Alpha_Wave_Lag1_WithNA$`ADI category`)

Alpha_Wave_Lag1_WithNA$zipcode = as.factor(Alpha_Wave_Lag1_WithNA$zipcode)


Alpha_Wave_Lag1_WithNA$`ADI category` <- relevel(Alpha_Wave_Lag1_WithNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ `ADI category` + `Effective Number of Centers`, data = Alpha_Wave_Lag1_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Lag1_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Lag1_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Lag1_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)



# vif_model <- lm(Lag1_Number_of_ICU_admissions ~  `testcenters` + `ADI category`, data = Alpha_Wave_Lag1)
# vif(vif_model)
# 
# 
# ICU_hospitalization_GLMMmodel_Alpha_1 <- glmer(Lag1_Number_of_ICU_admissions ~ `testcenters` + (1 | zipcode), 
#                                                data = Alpha_Wave_Lag1, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Alpha_1, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Alpha_2 <- glmer(Lag1_Number_of_ICU_admissions ~ `ADI category` + (1 | zipcode), 
#                                                data = Alpha_Wave_Lag1, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Alpha_2, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Alpha_3 <- glmer(Lag1_Number_of_ICU_admissions ~ `testcenters`*`ADI category` + (1 | zipcode), 
#                                                data = Alpha_Wave_Lag1, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Alpha_3, digits = 5)


Alpha_Wave_Lag1_WithOutNA = fread(file.choose(), sep=",", header=T)

Alpha_Wave_Lag1_WithOutNA <- Alpha_Wave_Lag1_WithOutNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Covid_ICU_admission_rate = lag(`Covid ICU Rate`, 1)
  ) %>%
  ungroup()

Alpha_Wave_Lag1_WithOutNA = na.omit(Alpha_Wave_Lag1_WithOutNA)


write.csv(Alpha_Wave_Lag1_WithOutNA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Alpha-Wave-Lag1-WithOutNA-Final.csv", row.names = FALSE)


skewness_ICU_rate = skewness(Alpha_Wave_Lag1_WithOutNA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Alpha_Wave_Lag1$Lag1_Number_of_ICU_admissions , type = 3)
# print(skewness_ICU_hospitalization)


Alpha_Wave_Lag1_WithOutNA$`ADI category`= as.factor(Alpha_Wave_Lag1_WithOutNA$`ADI category`)

Alpha_Wave_Lag1_WithOutNA$zipcode = as.factor(Alpha_Wave_Lag1_WithOutNA$zipcode)


Alpha_Wave_Lag1_WithOutNA$`ADI category` <- relevel(Alpha_Wave_Lag1_WithOutNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ `ADI category` + `Effective Number of Centers`, data = Alpha_Wave_Lag1_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Lag1_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Lag1_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Lag1_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


################################ Delta Wave ###################################

###############################################################################

Delta_Method3_Lag1 = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_DELTA_Lag1 = fread(file.choose(), sep=",", header=T)

Delta_Method3_Lag1$zipcode <- as.character(Delta_Method3_Lag1$zipcode)

ICU_UPDATED_DELTA_Lag1$zipcode <- as.character(ICU_UPDATED_DELTA_Lag1$zipcode)

Delta_Wave_Lag1 <- full_join(ICU_UPDATED_DELTA_Lag1, Delta_Method3_Lag1, by = c("zipcode", "Date"))

write.csv(Delta_Wave_Lag1, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Delta-Wave-Lag1.csv", row.names = FALSE)

Delta_Wave_Lag1_WithNA = fread(file.choose(), sep=",", header=T)

Delta_Wave_Lag1_WithNA <- Delta_Wave_Lag1_WithNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Covid_ICU_admission_rate = lag(`Covid ICU Rate`, 1)
  ) %>%
  ungroup()

Delta_Wave_Lag1_WithNA = na.omit(Delta_Wave_Lag1_WithNA)

write.csv(Delta_Wave_Lag1_WithNA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Delta-Wave-Lag1-WithNA-Final.csv", row.names = FALSE)

skewness_ICU_rate = skewness(Delta_Wave_Lag1_WithNA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Delta_Wave_Lag1$Lag1_Number_of_ICU_admissions , type = 3)
# print(skewness_ICU_hospitalization)


Delta_Wave_Lag1_WithNA$`ADI category`= as.factor(Delta_Wave_Lag1_WithNA$`ADI category`)

Delta_Wave_Lag1_WithNA$zipcode = as.factor(Delta_Wave_Lag1_WithNA$zipcode)


Delta_Wave_Lag1_WithNA$`ADI category` <- relevel(Delta_Wave_Lag1_WithNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ `ADI category` + `Effective Number of Centers`, data = Delta_Wave_Lag1_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Lag1_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Lag1_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Lag1_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)



# vif_model <- lm(Lag1_Number_of_ICU_admissions ~  `testcenters` + `ADI category`, data = Delta_Wave_Lag1)
# vif(vif_model)
# 
# 
# ICU_hospitalization_GLMMmodel_Delta_1 <- glmer(Lag1_Number_of_ICU_admissions ~ `testcenters` + (1 | zipcode), 
#                                                data = Delta_Wave_Lag1, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Delta_1, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Delta_2 <- glmer(Lag1_Number_of_ICU_admissions ~ `ADI category` + (1 | zipcode), 
#                                                data = Delta_Wave_Lag1, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Delta_2, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Delta_3 <- glmer(Lag1_Number_of_ICU_admissions ~ `testcenters`*`ADI category` + (1 | zipcode), 
#                                                data = Delta_Wave_Lag1, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Delta_3, digits = 5)



Delta_Wave_Lag1_WithOutNA = fread(file.choose(), sep=",", header=T)

Delta_Wave_Lag1_WithOutNA <- Delta_Wave_Lag1_WithOutNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Covid_ICU_admission_rate = lag(`Covid ICU Rate`, 1)
  ) %>%
  ungroup()

Delta_Wave_Lag1_WithOutNA = na.omit(Delta_Wave_Lag1_WithOutNA)

write.csv(Delta_Wave_Lag1_WithOutNA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Delta-Wave-Lag1-WithOutNA-Final.csv", row.names = FALSE)

skewness_ICU_rate = skewness(Delta_Wave_Lag1_WithOutNA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Delta_Wave_Lag1$Lag1_Number_of_ICU_admissions , type = 3)
# print(skewness_ICU_hospitalization)


Delta_Wave_Lag1_WithOutNA$`ADI category`= as.factor(Delta_Wave_Lag1_WithOutNA$`ADI category`)

Delta_Wave_Lag1_WithOutNA$zipcode = as.factor(Delta_Wave_Lag1_WithOutNA$zipcode)


Delta_Wave_Lag1_WithOutNA$`ADI category` <- relevel(Delta_Wave_Lag1_WithOutNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ `ADI category` + `Effective Number of Centers`, data = Delta_Wave_Lag1_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Lag1_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Lag1_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Lag1_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


################################ Omicron Wave #################################

###############################################################################

Omicron_Method3_Lag1 = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_OMICRON_Lag1 = fread(file.choose(), sep=",", header=T)

Omicron_Method3_Lag1$zipcode <- as.character(Omicron_Method3_Lag1$zipcode)

ICU_UPDATED_OMICRON_Lag1$zipcode <- as.character(ICU_UPDATED_OMICRON_Lag1$zipcode)

Omicron_Wave_Lag1 <- full_join(ICU_UPDATED_OMICRON_Lag1, Omicron_Method3_Lag1, by = c("zipcode", "Date"))

write.csv(Omicron_Wave_Lag1, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Omicron-Wave-Lag1.csv", row.names = FALSE)

Omicron_Wave_Lag1_WithNA = fread(file.choose(), sep=",", header=T)

Omicron_Wave_Lag1_WithNA <- Omicron_Wave_Lag1_WithNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Covid_ICU_admission_rate = lag(`Covid ICU Rate`, 1)
  ) %>%
  ungroup()

Omicron_Wave_Lag1_WithNA = na.omit(Omicron_Wave_Lag1_WithNA)


write.csv(Omicron_Wave_Lag1_WithNA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Omicron-Wave-Lag1-WithNA-Final.csv", row.names = FALSE)


skewness_ICU_rate = skewness(Omicron_Wave_Lag1_WithNA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Omicron_Wave_Lag1$Lag1_Number_of_ICU_admissions , type = 3)
# print(skewness_ICU_hospitalization)


Omicron_Wave_Lag1_WithNA$`ADI category`= as.factor(Omicron_Wave_Lag1_WithNA$`ADI category`)

Omicron_Wave_Lag1_WithNA$zipcode = as.factor(Omicron_Wave_Lag1_WithNA$zipcode)


Omicron_Wave_Lag1_WithNA$`ADI category` <- relevel(Omicron_Wave_Lag1_WithNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ `ADI category` + `Effective Number of Centers`, data = Omicron_Wave_Lag1_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Omicron_Wave_Lag1_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Omicron_Wave_Lag1_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Omicron_Wave_Lag1_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)



# vif_model <- lm(Lag1_Number_of_ICU_admissions ~  `testcenters` + `ADI category`, data = Omicron_Wave_Lag1)
# vif(vif_model)
# 
# 
# ICU_hospitalization_GLMMmodel_Omicron_1 <- glmer(Lag1_Number_of_ICU_admissions ~ `testcenters` + (1 | zipcode), 
#                                                data = Omicron_Wave_Lag1, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Omicron_1, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Omicron_2 <- glmer(Lag1_Number_of_ICU_admissions ~ `ADI category` + (1 | zipcode), 
#                                                data = Omicron_Wave_Lag1, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Omicron_2, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Omicron_3 <- glmer(Lag1_Number_of_ICU_admissions ~ `testcenters`*`ADI category` + (1 | zipcode), 
#                                                data = Omicron_Wave_Lag1, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Omicron_3, digits = 5)


Omicron_Wave_Lag1_WithOutNA = fread(file.choose(), sep=",", header=T)

Omicron_Wave_Lag1_WithOutNA <- Omicron_Wave_Lag1_WithOutNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag1_Covid_ICU_admission_rate = lag(`Covid ICU Rate`, 1)
  ) %>%
  ungroup()

Omicron_Wave_Lag1_WithOutNA = na.omit(Omicron_Wave_Lag1_WithOutNA)


write.csv(Omicron_Wave_Lag1_WithOutNA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Omicron-Wave-Lag1-WithOutNA-Final.csv", row.names = FALSE)


skewness_ICU_rate = skewness(Omicron_Wave_Lag1_WithOutNA$Lag1_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Omicron_Wave_Lag1$Lag1_Number_of_ICU_admissions , type = 3)
# print(skewness_ICU_hospitalization)


Omicron_Wave_Lag1_WithOutNA$`ADI category`= as.factor(Omicron_Wave_Lag1_WithOutNA$`ADI category`)

Omicron_Wave_Lag1_WithOutNA$zipcode = as.factor(Omicron_Wave_Lag1_WithOutNA$zipcode)


Omicron_Wave_Lag1_WithOutNA$`ADI category` <- relevel(Omicron_Wave_Lag1_WithOutNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(Lag1_Covid_ICU_admission_rate ~ `ADI category` + `Effective Number of Centers`, data = Omicron_Wave_Lag1_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Lag1_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `ADI category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Lag1_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag1_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Lag1_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)


################################ Two-Month Lag ################################

################################ Alpha Wave ###################################

###############################################################################

Alpha_Method3_Lag2 = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_ALPHA_Lag2 = fread(file.choose(), sep=",", header=T)

Alpha_Method3_Lag2$zipcode <- as.character(Alpha_Method3_Lag2$zipcode)

ICU_UPDATED_ALPHA_Lag2$zipcode <- as.character(ICU_UPDATED_ALPHA_Lag2$zipcode)

Alpha_Wave_Lag2 <- full_join(ICU_UPDATED_ALPHA_Lag2, Alpha_Method3_Lag2, by = c("zipcode", "Date"))

write.csv(Alpha_Wave_Lag2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Alpha-Wave-Lag2.csv", row.names = FALSE)

Alpha_Wave_Lag2_WithNA = fread(file.choose(), sep=",", header=T)

Alpha_Wave_Lag2_WithNA <- Alpha_Wave_Lag2_WithNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_Covid_ICU_admission_rate = lag(`Covid ICU Rate`, 2)
  ) %>%
  ungroup()

Alpha_Wave_Lag2_WithNA = na.omit(Alpha_Wave_Lag2_WithNA)


write.csv(Alpha_Wave_Lag2_WithNA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Alpha-Wave-Lag2-WithNA-Final.csv", row.names = FALSE)


skewness_ICU_rate = skewness(Alpha_Wave_Lag2_WithNA$Lag2_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Alpha_Wave_Lag1$Lag1_Number_of_ICU_admissions , type = 3)
# print(skewness_ICU_hospitalization)


Alpha_Wave_Lag2_WithNA$`ADI category`= as.factor(Alpha_Wave_Lag2_WithNA$`ADI category`)

Alpha_Wave_Lag2_WithNA$zipcode = as.factor(Alpha_Wave_Lag2_WithNA$zipcode)


Alpha_Wave_Lag2_WithNA$`ADI category` <- relevel(Alpha_Wave_Lag2_WithNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_Covid_ICU_admission_rate ~ `ADI category` + `Effective Number of Centers`, data = Alpha_Wave_Lag2_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Lag2_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Lag2_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Lag2_WithNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)



# vif_model <- lm(Lag1_Number_of_ICU_admissions ~  `testcenters` + `ADI category`, data = Alpha_Wave_Lag2)
# vif(vif_model)
# 
# 
# ICU_hospitalization_GLMMmodel_Alpha_1 <- glmer(Lag1_Number_of_ICU_admissions ~ `testcenters` + (1 | zipcode), 
#                                                data = Alpha_Wave_Lag2, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Alpha_1, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Alpha_2 <- glmer(Lag1_Number_of_ICU_admissions ~ `ADI category` + (1 | zipcode), 
#                                                data = Alpha_Wave_Lag2, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Alpha_2, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Alpha_3 <- glmer(Lag1_Number_of_ICU_admissions ~ `testcenters`*`ADI category` + (1 | zipcode), 
#                                                data = Alpha_Wave_Lag2, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Alpha_3, digits = 5)


Alpha_Wave_Lag2_WithOutNA = fread(file.choose(), sep=",", header=T)

Alpha_Wave_Lag2_WithOutNA <- Alpha_Wave_Lag2_WithOutNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_Covid_ICU_admission_rate = lag(`Covid ICU Rate`, 2)
  ) %>%
  ungroup()

Alpha_Wave_Lag2_WithOutNA = na.omit(Alpha_Wave_Lag2_WithOutNA)


write.csv(Alpha_Wave_Lag2_WithOutNA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Alpha-Wave-Lag2-WithOutNA-Final.csv", row.names = FALSE)


skewness_ICU_rate = skewness(Alpha_Wave_Lag2_WithOutNA$Lag2_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Alpha_Wave_Lag1$Lag1_Number_of_ICU_admissions , type = 3)
# print(skewness_ICU_hospitalization)


Alpha_Wave_Lag2_WithOutNA$`ADI category`= as.factor(Alpha_Wave_Lag2_WithOutNA$`ADI category`)

Alpha_Wave_Lag2_WithOutNA$zipcode = as.factor(Alpha_Wave_Lag2_WithOutNA$zipcode)


Alpha_Wave_Lag2_WithOutNA$`ADI category` <- relevel(Alpha_Wave_Lag2_WithOutNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_Covid_ICU_admission_rate ~ `ADI category` + `Effective Number of Centers`, data = Alpha_Wave_Lag2_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Lag2_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Lag2_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Alpha_Wave_Lag2_WithOutNA)

tab_model(ICU_rate_GLMMmodel_ALPHA_3, digits = 5)


################################ Delta Wave ###################################

###############################################################################

Delta_Method3_Lag2 = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_DELTA_Lag2 = fread(file.choose(), sep=",", header=T)

Delta_Method3_Lag2$zipcode <- as.character(Delta_Method3_Lag2$zipcode)

ICU_UPDATED_DELTA_Lag2$zipcode <- as.character(ICU_UPDATED_DELTA_Lag2$zipcode)

Delta_Wave_Lag2 <- full_join(ICU_UPDATED_DELTA_Lag2, Delta_Method3_Lag2, by = c("zipcode", "Date"))

write.csv(Delta_Wave_Lag2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Delta-Wave-Lag2.csv", row.names = FALSE)

Delta_Wave_Lag2_WithNA = fread(file.choose(), sep=",", header=T)

Delta_Wave_Lag2_WithNA <- Delta_Wave_Lag2_WithNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_Covid_ICU_admission_rate = lag(`Covid ICU Rate`, 2)
  ) %>%
  ungroup()

Delta_Wave_Lag2_WithNA = na.omit(Delta_Wave_Lag2_WithNA)


write.csv(Delta_Wave_Lag2_WithNA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Delta-Wave-Lag2-WithNA-Final.csv", row.names = FALSE)


skewness_ICU_rate = skewness(Delta_Wave_Lag2_WithNA$Lag2_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Delta_Wave_Lag2$Lag1_Number_of_ICU_admissions , type = 3)
# print(skewness_ICU_hospitalization)


Delta_Wave_Lag2_WithNA$`ADI category`= as.factor(Delta_Wave_Lag2_WithNA$`ADI category`)

Delta_Wave_Lag2_WithNA$zipcode = as.factor(Delta_Wave_Lag2_WithNA$zipcode)


Delta_Wave_Lag2_WithNA$`ADI category` <- relevel(Delta_Wave_Lag2_WithNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_Covid_ICU_admission_rate ~ `ADI category` + `Effective Number of Centers`, data = Delta_Wave_Lag2_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Lag2_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Lag2_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Lag2_WithNA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)



# vif_model <- lm(Lag1_Number_of_ICU_admissions ~  `testcenters` + `ADI category`, data = Delta_Wave_Lag2)
# vif(vif_model)
# 
# 
# ICU_hospitalization_GLMMmodel_Delta_1 <- glmer(Lag1_Number_of_ICU_admissions ~ `testcenters` + (1 | zipcode), 
#                                                data = Delta_Wave_Lag2, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Delta_1, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Delta_2 <- glmer(Lag1_Number_of_ICU_admissions ~ `ADI category` + (1 | zipcode), 
#                                                data = Delta_Wave_Lag2, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Delta_2, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Delta_3 <- glmer(Lag1_Number_of_ICU_admissions ~ `testcenters`*`ADI category` + (1 | zipcode), 
#                                                data = Delta_Wave_Lag2, 
#                                                family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Delta_3, digits = 5)


Delta_Wave_Lag2_WithOutNA = fread(file.choose(), sep=",", header=T)

Delta_Wave_Lag2_WithOutNA <- Delta_Wave_Lag2_WithOutNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_Covid_ICU_admission_rate = lag(`Covid ICU Rate`, 2)
  ) %>%
  ungroup()

Delta_Wave_Lag2_WithOutNA = na.omit(Delta_Wave_Lag2_WithOutNA)


write.csv(Delta_Wave_Lag2_WithOutNA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Delta-Wave-Lag2-WithOutNA-Final.csv", row.names = FALSE)


skewness_ICU_rate = skewness(Delta_Wave_Lag2_WithOutNA$Lag2_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Delta_Wave_Lag2$Lag1_Number_of_ICU_admissions , type = 3)
# print(skewness_ICU_hospitalization)


Delta_Wave_Lag2_WithOutNA$`ADI category`= as.factor(Delta_Wave_Lag2_WithOutNA$`ADI category`)

Delta_Wave_Lag2_WithOutNA$zipcode = as.factor(Delta_Wave_Lag2_WithOutNA$zipcode)


Delta_Wave_Lag2_WithOutNA$`ADI category` <- relevel(Delta_Wave_Lag2_WithOutNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_Covid_ICU_admission_rate ~ `ADI category` + `Effective Number of Centers`, data = Delta_Wave_Lag2_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Lag2_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Lag2_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = Delta_Wave_Lag2_WithOutNA)

tab_model(ICU_rate_GLMMmodel_DELTA_3, digits = 5)


################################ Omicron Wave #################################

###############################################################################

Omicron_Method3_Lag2 = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_OMICRON_Lag2 = fread(file.choose(), sep=",", header=T)

Omicron_Method3_Lag2$zipcode <- as.character(Omicron_Method3_Lag2$zipcode)

ICU_UPDATED_OMICRON_Lag2$zipcode <- as.character(ICU_UPDATED_OMICRON_Lag2$zipcode)

Omicron_Wave_Lag2 <- full_join(ICU_UPDATED_OMICRON_Lag2, Omicron_Method3_Lag2, by = c("zipcode", "Date"))

write.csv(Omicron_Wave_Lag2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Omicron-Wave-Lag2.csv", row.names = FALSE)

Omicron_Wave_Lag2_WithNA = fread(file.choose(), sep=",", header=T)

Omicron_Wave_Lag2_WithNA <- Omicron_Wave_Lag2_WithNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_Covid_ICU_admission_rate = lag(`Covid ICU Rate`, 2)
  ) %>%
  ungroup()

Omicron_Wave_Lag2_WithNA = na.omit(Omicron_Wave_Lag2_WithNA)


write.csv(Omicron_Wave_Lag2_WithNA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Omicron-Wave-Lag2-WithNA-Final.csv", row.names = FALSE)


skewness_ICU_rate = skewness(Omicron_Wave_Lag2_WithNA$Lag2_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Omicron_Wave_Lag2$Lag1_Number_of_ICU_admissions , type = 3)
# print(skewness_ICU_hospitalization)


Omicron_Wave_Lag2_WithNA$`ADI category`= as.factor(Omicron_Wave_Lag2_WithNA$`ADI category`)

Omicron_Wave_Lag2_WithNA$zipcode = as.factor(Omicron_Wave_Lag2_WithNA$zipcode)


Omicron_Wave_Lag2_WithNA$`ADI category` <- relevel(Omicron_Wave_Lag2_WithNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_Covid_ICU_admission_rate ~ `ADI category` + `Effective Number of Centers`, data = Omicron_Wave_Lag2_WithNA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Lag2_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `ADI category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Lag2_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Lag2_WithNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)



# vif_model <- lm(Lag1_Number_of_ICU_admissions ~  `testcenters` + `ADI category`, data = Omicron_Wave_Lag2)
# vif(vif_model)
# 
# 
# ICU_hospitalization_GLMMmodel_Omicron_1 <- glmer(Lag1_Number_of_ICU_admissions ~ `testcenters` + (1 | zipcode), 
#                                                  data = Omicron_Wave_Lag2, 
#                                                  family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Omicron_1, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Omicron_2 <- glmer(Lag1_Number_of_ICU_admissions ~ `ADI category` + (1 | zipcode), 
#                                                  data = Omicron_Wave_Lag2, 
#                                                  family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Omicron_2, digits = 5)
# 
# 
# ICU_hospitalization_GLMMmodel_Omicron_3 <- glmer(Lag1_Number_of_ICU_admissions ~ `testcenters`*`ADI category` + (1 | zipcode), 
#                                                  data = Omicron_Wave_Lag2, 
#                                                  family = poisson)
# 
# tab_model(ICU_hospitalization_GLMMmodel_Omicron_3, digits = 5)


Omicron_Wave_Lag2_WithOutNA = fread(file.choose(), sep=",", header=T)

Omicron_Wave_Lag2_WithOutNA <- Omicron_Wave_Lag2_WithOutNA %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_Covid_ICU_admission_rate = lag(`Covid ICU Rate`, 2)
  ) %>%
  ungroup()

Omicron_Wave_Lag2_WithOutNA = na.omit(Omicron_Wave_Lag2_WithOutNA)


write.csv(Omicron_Wave_Lag2_WithOutNA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Analysis/Lag Analysis/Scenario 1/Omicron-Wave-Lag2-WithOutNA-Final.csv", row.names = FALSE)


skewness_ICU_rate = skewness(Omicron_Wave_Lag2_WithOutNA$Lag2_Covid_ICU_admission_rate , type = 3)
print(skewness_ICU_rate)

# skewness_ICU_hospitalization = skewness(Omicron_Wave_Lag2$Lag1_Number_of_ICU_admissions , type = 3)
# print(skewness_ICU_hospitalization)


Omicron_Wave_Lag2_WithOutNA$`ADI category`= as.factor(Omicron_Wave_Lag2_WithOutNA$`ADI category`)

Omicron_Wave_Lag2_WithOutNA$zipcode = as.factor(Omicron_Wave_Lag2_WithOutNA$zipcode)


Omicron_Wave_Lag2_WithOutNA$`ADI category` <- relevel(Omicron_Wave_Lag2_WithOutNA$`ADI category`, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_Covid_ICU_admission_rate ~ `ADI category` + `Effective Number of Centers`, data = Omicron_Wave_Lag2_WithOutNA)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Lag2_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `ADI category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Lag2_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Lag2_Covid_ICU_admission_rate + 1) ~ `Effective Number of Centers`*`ADI category` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = Omicron_Wave_Lag2_WithOutNA)

tab_model(ICU_rate_GLMMmodel_OMICRON_3, digits = 5)

