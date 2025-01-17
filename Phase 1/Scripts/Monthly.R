#*******************************Zipcode Level **********************************

#*******************************Multivariable modeling - ALPHA *************************

Data_Monthly_Alpha = read.csv(file.choose(), sep=",", header=T)

Data_Monthly_Alpha = Data_Monthly_Alpha %>%
  rename(zipcode = zip)

Data_Monthly_Alpha = Data_Monthly_Alpha %>%
  rename(Date = Month)

Data_Monthly_Alpha = Data_Monthly_Alpha %>%
  mutate(Zip = substr(zipcode, 1, 5))

all_combinations <- expand.grid(
  zipcode = unique(Data_Monthly_Alpha$zipcode),
  Date = format(seq(as.Date("2021-03-01"), as.Date("2021-06-01"), by = "1 month"), "%Y-%m")
)

all_combinations$zipcode <- as.character(all_combinations$zipcode)
Data_Monthly_Alpha$zipcode <- as.character(Data_Monthly_Alpha$zipcode)

Data_Monthly_Alpha <- full_join(all_combinations, Data_Monthly_Alpha, by = c("zipcode", "Date"))

#Data_Monthly_Alpha[is.na(Data_Monthly_Alpha)] = 0

ICU_UPDATED_Alpha = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Alpha$zipcode <- as.character(ICU_UPDATED_Alpha$zipcode)

ICU_UPDATED_Alpha <- merge(ICU_UPDATED_Alpha, Data_Monthly_Alpha, by = c("zipcode", "Date"))

#ICU_UPDATED_Alpha = merge(ICU_UPDATED_Alpha, Data_Monthly_Alpha, by = c("zipcode","Date"))

ICU_UPDATED_Alpha = ICU_UPDATED_Alpha[,-c(18,21:22)]

write.csv(ICU_UPDATED_Alpha, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU//Alpha/ICU_UPDATED_ALPHA_Zipcode_filtered_by_population.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Alpha$Covid.ICU.Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Alpha$ADI.category = as.factor(ICU_UPDATED_Alpha$ADI.category)

ICU_UPDATED_Alpha$ADI.category <- relevel(ICU_UPDATED_Alpha$ADI.category, ref = "Less Disadvantaged")

ICU_UPDATED_Alpha$zipcode <- as.factor(ICU_UPDATED_Alpha$zipcode)


ICU_UPDATED_Alpha$Number.of.Samples.Collected <- scale(ICU_UPDATED_Alpha$Number.of.Samples.Collected)


ICU_UPDATED_Alpha$Number.of.Positive.Cases <- scale(ICU_UPDATED_Alpha$Number.of.Positive.Cases)





vif_model <- lm(Covid.ICU.Rate ~ SamplesCollected + PositiveCases + ADI.category + Total_SHIELD_centers_per_zipcode_per_month , data = ICU_UPDATED_Alpha)
vif(vif_model)



library(sjPlot)

ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month  + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Alpha)

tab_model(ICU_rate_GLMMmodel_ALPHA_1)



ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Alpha)

tab_model(ICU_rate_GLMMmodel_ALPHA_2)


ICU_rate_GLMMmodel_ALPHA_3 <- lmer(log(Covid.ICU.Rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Alpha)

tab_model(ICU_rate_GLMMmodel_ALPHA_3)


ICU_rate_GLMMmodel_ALPHA_4 <- lmer(log(Covid.ICU.Rate + 1) ~ PositiveCases + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Alpha)

tab_model(ICU_rate_GLMMmodel_ALPHA_4)


ICU_rate_GLMMmodel_ALPHA_5 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Alpha)

tab_model(ICU_rate_GLMMmodel_ALPHA_5)


#ICU_rate_GLMMmodel_ALPHA_6 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*Number.of.Samples.Collected + offset(log(cpop)) +
                                    # (1 | zipcode), 
                                   #data = ICU_UPDATED_Alpha)

#tab_model(ICU_rate_GLMMmodel_ALPHA_6)


#ICU_rate_GLMMmodel_ALPHA_7 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*Number.of.Positive.Cases + offset(log(cpop)) +
                                     #(1 | zipcode), 
                                   #data = ICU_UPDATED_Alpha)

#tab_model(ICU_rate_GLMMmodel_ALPHA_7)


ICU_rate_GLMMmodel_ALPHA_8 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Alpha)

tab_model(ICU_rate_GLMMmodel_ALPHA_8)


ICU_rate_GLMMmodel_ALPHA_9 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*PositiveCases + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Alpha)

tab_model(ICU_rate_GLMMmodel_ALPHA_9)


ICU_rate_GLMMmodel_ALPHA_10 <- lmer(log(Covid.ICU.Rate + 1) ~ Number.of.Samples.Collected*Number.of.Positive.Cases + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Alpha)

tab_model(ICU_rate_GLMMmodel_ALPHA_10)



#ICU_rate_GLMMmodel_ALPHA_11 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*Number.of.Samples.Collected + offset(log(cpop)) +
                                      #(1 | zipcode), 
                                    #data = ICU_UPDATED_Alpha)

#tab_model(ICU_rate_GLMMmodel_ALPHA_11)


#ICU_rate_GLMMmodel_ALPHA_12 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*Number.of.Positive.Cases + offset(log(cpop)) +
                                      #(1 | zipcode), 
                                    #data = ICU_UPDATED_Alpha)

#tab_model(ICU_rate_GLMMmodel_ALPHA_12)


#ICU_rate_GLMMmodel_ALPHA_13 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*Number.of.Samples.Collected*Number.of.Positive.Cases + offset(log(cpop)) +
                                      #(1 | zipcode), 
                                    #data = ICU_UPDATED_Alpha)

#tab_model(ICU_rate_GLMMmodel_ALPHA_13)


#ICU_rate_GLMMmodel_ALPHA_14 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*Number.of.Samples.Collected*Number.of.Positive.Cases + offset(log(cpop)) +
                                      #(1 | zipcode), 
                                    #data = ICU_UPDATED_Alpha)

#tab_model(ICU_rate_GLMMmodel_ALPHA_14)


#ICU_rate_GLMMmodel_ALPHA_15 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*Number.of.Samples.Collected*Number.of.Positive.Cases + offset(log(cpop)) +
                                      #(1 | zipcode), 
                                   # data = ICU_UPDATED_Alpha)

#tab_model(ICU_rate_GLMMmodel_ALPHA_15)


#*******************************Multivariable modeling - DELTA *************************


Data_Monthly_Delta = read.csv(file.choose(), sep=",", header=T)

Data_Monthly_Delta = Data_Monthly_Delta %>%
  rename(zipcode = Zip)

Data_Monthly_Delta = Data_Monthly_Delta %>%
  rename(Date = Month)

Data_Monthly_Delta = Data_Monthly_Delta %>%
  mutate(Zip = substr(zipcode, 1, 5))

all_combinations <- expand.grid(
  zipcode = unique(Data_Monthly_Delta$zipcode),
  Date = format(seq(as.Date("2021-08-01"), as.Date("2021-11-01"), by = "1 month"), "%Y-%m")
)

all_combinations$zipcode <- as.character(all_combinations$zipcode)
Data_Monthly_Delta$zipcode <- as.character(Data_Monthly_Delta$zipcode)

Data_Monthly_Delta <- full_join(all_combinations, Data_Monthly_Delta, by = c("zipcode", "Date"))

Data_Monthly_Delta[is.na(Data_Monthly_Delta)] = 0

ICU_UPDATED_Delta = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Delta$zipcode <- as.character(ICU_UPDATED_Delta$zipcode)

ICU_UPDATED_Delta <- full_join(ICU_UPDATED_Delta, Data_Monthly_Delta, by = c("zipcode", "Date"))

#ICU_UPDATED_Alpha = merge(ICU_UPDATED_Alpha, Data_Monthly_Alpha, by = c("zipcode","Date"))

ICU_UPDATED_Delta = ICU_UPDATED_Delta[,-c(18,21:22)]

write.csv(ICU_UPDATED_Delta, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Delta/ICU_UPDATED_DELTA_Zipcode_filtered_by_population.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Delta$Covid.ICU.Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Delta$ADI.category = as.factor(ICU_UPDATED_Delta$ADI.category)

ICU_UPDATED_Delta$ADI.category <- relevel(ICU_UPDATED_Delta$ADI.category, ref = "Less Disadvantaged")

ICU_UPDATED_Delta$zipcode <- as.factor(ICU_UPDATED_Delta$zipcode)


ICU_UPDATED_Delta$Number.of.Samples.Collected <- scale(ICU_UPDATED_Delta$Number.of.Samples.Collected)


ICU_UPDATED_Delta$Number.of.Positive.Cases <- scale(ICU_UPDATED_Delta$Number.of.Positive.Cases)


vif_model <- lm(Covid.ICU.Rate ~ SamplesCollected + PositiveCases + ADI.category + Total_SHIELD_centers_per_zipcode_per_month , data = ICU_UPDATED_Delta)
vif(vif_model)



ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month  + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Delta)

tab_model(ICU_rate_GLMMmodel_DELTA_1)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Delta)

tab_model(ICU_rate_GLMMmodel_DELTA_2)


ICU_rate_GLMMmodel_DELTA_3 <- lmer(log(Covid.ICU.Rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Delta)

tab_model(ICU_rate_GLMMmodel_DELTA_3)


ICU_rate_GLMMmodel_DELTA_4 <- lmer(log(Covid.ICU.Rate + 1) ~ PositiveCases + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Delta)

tab_model(ICU_rate_GLMMmodel_DELTA_4)


ICU_rate_GLMMmodel_DELTA_5 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Delta)

tab_model(ICU_rate_GLMMmodel_DELTA_5)


#ICU_rate_GLMMmodel_DELTA_6 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*Number.of.Samples.Collected + offset(log(cpop)) +
                                       #(1 | zipcode), 
                                    # data = ICU_UPDATED_Delta)

#tab_model(ICU_rate_GLMMmodel_DELTA_6)


#ICU_rate_GLMMmodel_DELTA_7 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*Number.of.Positive.Cases + offset(log(cpop)) +
                                       #(1 | zipcode), 
                                     #data = ICU_UPDATED_Delta)

#tab_model(ICU_rate_GLMMmodel_DELTA_7)


ICU_rate_GLMMmodel_DELTA_8 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*SamplesCollected + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Delta)

tab_model(ICU_rate_GLMMmodel_DELTA_8)


ICU_rate_GLMMmodel_DELTA_9 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*PositiveCases + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Delta)

tab_model(ICU_rate_GLMMmodel_DELTA_9)


#ICU_rate_GLMMmodel_DELTA_10 <- lmer(log(Covid.ICU.Rate + 1) ~ Number.of.Samples.Collected*Number.of.Positive.Cases + offset(log(cpop)) +
                                     #(1 | zipcode), 
                                  # data = ICU_UPDATED_Delta)

#tab_model(ICU_rate_GLMMmodel_DELTA_10)


#ICU_rate_GLMMmodel_DELTA_11 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*Number.of.Samples.Collected + offset(log(cpop)) +
                                        #(1 | zipcode), 
                                      #data = ICU_UPDATED_Delta)

#tab_model(ICU_rate_GLMMmodel_DELTA_11)


#ICU_rate_GLMMmodel_DELTA_12 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*Number.of.Positive.Cases + offset(log(cpop)) +
                                        #(1 | zipcode), 
                                     # data = ICU_UPDATED_Delta)

#tab_model(ICU_rate_GLMMmodel_DELTA_12)


#ICU_rate_GLMMmodel_DELTA_13 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*Number.of.Samples.Collected*Number.of.Positive.Cases + offset(log(cpop)) +
                                        #(1 | zipcode), 
                                      #data = ICU_UPDATED_Delta)

#tab_model(ICU_rate_GLMMmodel_DELTA_13)


#ICU_rate_GLMMmodel_DELTA_14 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*Number.of.Samples.Collected*Number.of.Positive.Cases + offset(log(cpop)) +
                                      #(1 | zipcode), 
                                    #data = ICU_UPDATED_Delta)

#tab_model(ICU_rate_GLMMmodel_DELTA_14)


#ICU_rate_GLMMmodel_DELTA_15 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*Number.of.Samples.Collected*Number.of.Positive.Cases + offset(log(cpop)) +
                                        #(1 | zipcode), 
                                     # data = ICU_UPDATED_Delta)

#tab_model(ICU_rate_GLMMmodel_DELTA_15)


#*******************************Multivariable modeling - OMICRON *************************

Data_Monthly_Omicron = read.csv(file.choose(), sep=",", header=T)

Data_Monthly_Omicron = Data_Monthly_Omicron %>%
  rename(zipcode = Zip)

Data_Monthly_Omicron = Data_Monthly_Omicron %>%
  rename(Date = Month)

Data_Monthly_Omicron = Data_Monthly_Omicron %>%
  mutate(Zip = substr(zipcode, 1, 5))

all_combinations <- expand.grid(
  zipcode = unique(Data_Monthly_Omicron$zipcode),
  Date = format(seq(as.Date("2021-12-01"), as.Date("2022-03-01"), by = "1 month"), "%Y-%m")
)

all_combinations$zipcode <- as.character(all_combinations$zipcode)
Data_Monthly_Omicron$zipcode <- as.character(Data_Monthly_Omicron$zipcode)

Data_Monthly_Omicron <- full_join(all_combinations, Data_Monthly_Omicron, by = c("zipcode", "Date"))

Data_Monthly_Omicron[is.na(Data_Monthly_Omicron)] = 0

ICU_UPDATED_Omicron = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Omicron$zipcode <- as.character(ICU_UPDATED_Omicron$zipcode)

ICU_UPDATED_Omicron <- full_join(ICU_UPDATED_Omicron, Data_Monthly_Omicron, by = c("zipcode", "Date"))

#ICU_UPDATED_Alpha = merge(ICU_UPDATED_Alpha, Data_Monthly_Alpha, by = c("zipcode","Date"))

ICU_UPDATED_Omicron = ICU_UPDATED_Omicron[,-c(18,21:22)]

write.csv(ICU_UPDATED_Omicron, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Omicron/ICU_UPDATED_OMICRON_Zipcode_filtered_by_population.csv", row.names = FALSE)


skewness_ICU_rate = skewness(ICU_UPDATED_Omicron$Covid.ICU.Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Omicron$ADI.category = as.factor(ICU_UPDATED_Omicron$ADI.category)

ICU_UPDATED_Omicron$ADI.category <- relevel(ICU_UPDATED_Omicron$ADI.category, ref = "Less Disadvantaged")

ICU_UPDATED_Omicron$zipcode <- as.factor(ICU_UPDATED_Omicron$zipcode)


ICU_UPDATED_Omicron$Number.of.Samples.Collected <- scale(ICU_UPDATED_Omicron$Number.of.Samples.Collected)


ICU_UPDATED_Omicron$Number.of.Positive.Cases <- scale(ICU_UPDATED_Omicron$Number.of.Positive.Cases)


vif_model <- lm(Covid.ICU.Rate ~ SamplesCollected + PositiveCases + ADI.category + Total_SHIELD_centers_per_zipcode_per_month , data = ICU_UPDATED_Omicron)
vif(vif_model)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month  + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Omicron)

tab_model(ICU_rate_GLMMmodel_OMICRON_1)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Omicron)

tab_model(ICU_rate_GLMMmodel_OMICRON_2)


ICU_rate_GLMMmodel_OMICRON_3 <- lmer(log(Covid.ICU.Rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Omicron)

tab_model(ICU_rate_GLMMmodel_OMICRON_3)


ICU_rate_GLMMmodel_OMICRON_4 <- lmer(log(Covid.ICU.Rate + 1) ~ PositiveCases + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Omicron)

tab_model(ICU_rate_GLMMmodel_OMICRON_4)


ICU_rate_GLMMmodel_OMICRON_5 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Omicron)

tab_model(ICU_rate_GLMMmodel_OMICRON_5)


#ICU_rate_GLMMmodel_OMICRON_6 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*Number.of.Samples.Collected + offset(log(cpop)) +
                                    #(1 | zipcode), 
                                  # data = ICU_UPDATED_Omicron)

#tab_model(ICU_rate_GLMMmodel_OMICRON_6)


#ICU_rate_GLMMmodel_OMICRON_7 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*Number.of.Positive.Cases + offset(log(cpop)) +
                                       #(1 | zipcode), 
                                     #data = ICU_UPDATED_Omicron)

#tab_model(ICU_rate_GLMMmodel_OMICRON_7)


ICU_rate_GLMMmodel_OMICRON_8 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*SamplesCollected + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Omicron)

tab_model(ICU_rate_GLMMmodel_OMICRON_8)


ICU_rate_GLMMmodel_OMICRON_9 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*PositiveCases + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Omicron)

tab_model(ICU_rate_GLMMmodel_OMICRON_9)


#ICU_rate_GLMMmodel_OMICRON_10 <- lmer(log(Covid.ICU.Rate + 1) ~ Number.of.Samples.Collected*Number.of.Positive.Cases + offset(log(cpop)) +
                                       #(1 | zipcode), 
                                     #data = ICU_UPDATED_Omicron)

#tab_model(ICU_rate_GLMMmodel_OMICRON_10)


#ICU_rate_GLMMmodel_OMICRON_11 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*Number.of.Samples.Collected + offset(log(cpop)) +
                                       #(1 | zipcode), 
                                     #data = ICU_UPDATED_Omicron)

#tab_model(ICU_rate_GLMMmodel_OMICRON_11)


#ICU_rate_GLMMmodel_OMICRON_12 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*Number.of.Positive.Cases + offset(log(cpop)) +
                                        #(1 | zipcode), 
                                     # data = ICU_UPDATED_Omicron)

#tab_model(ICU_rate_GLMMmodel_OMICRON_12)


#ICU_rate_GLMMmodel_OMICRON_13 <- lmer(log(Covid.ICU.Rate + 1) ~ ADI.category*Number.of.Samples.Collected*Number.of.Positive.Cases + offset(log(cpop)) +
                                        #(1 | zipcode), 
                                      #data = ICU_UPDATED_Omicron)

#tab_model(ICU_rate_GLMMmodel_OMICRON_13)


#ICU_rate_GLMMmodel_OMICRON_14 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*Number.of.Samples.Collected*Number.of.Positive.Cases + offset(log(cpop)) +
                                       # (1 | zipcode), 
                                      #data = ICU_UPDATED_Omicron)

#tab_model(ICU_rate_GLMMmodel_OMICRON_14)


#ICU_rate_GLMMmodel_OMICRON_15 <- lmer(log(Covid.ICU.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category*Number.of.Samples.Collected*Number.of.Positive.Cases + offset(log(cpop)) +
                                       # (1 | zipcode), 
                                      #data = ICU_UPDATED_Omicron)

#tab_model(ICU_rate_GLMMmodel_OMICRON_15)



#*******************************************************************


library(car)

vif_model <- lm(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + PositiveCases + ADI.category + Total_SHIELD_centers_per_zipcode_per_month , data = ICU_UPDATED_Alpha)
vif(vif_model)



correlation_test <- cor.test(ICU_UPDATED_Alpha$Covid.ICU.Rate, ICU_UPDATED_Alpha$Total_SHIELD_centers_per_zipcode_per_month, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_Alpha$Covid.ICU.Rate, ICU_UPDATED_Alpha$SamplesCollected, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_Alpha$Covid.ICU.Rate, ICU_UPDATED_Alpha$PositiveCases, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


library(lme4)

COVID_Patients_GLMMmodel_Alpha_1 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ Total_SHIELD_centers_per_zipcode_per_month + (1 | zipcode), 
               data = ICU_UPDATED_Alpha_1, 
               family = poisson)

tab_model(COVID_Patients_GLMMmodel_Alpha_1)


COVID_Patients_GLMMmodel_Alpha_2 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category + (1 | zipcode), 
                                          data = ICU_UPDATED_Alpha, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Alpha_2)


ICU_UPDATED_Alpha$SamplesCollected <- scale(ICU_UPDATED_Alpha$SamplesCollected)

COVID_Patients_GLMMmodel_Alpha_3 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + (1 | zipcode), 
                                          data = ICU_UPDATED_Alpha, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Alpha_3)


COVID_Patients_GLMMmodel_Alpha_4 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ PositiveCases + (1 | zipcode), 
                                          data = ICU_UPDATED_Alpha, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Alpha_4)


COVID_Patients_GLMMmodel_Alpha_5 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category + (1 | zipcode), 
                                          data = ICU_UPDATED_Alpha, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Alpha_5)


COVID_Patients_GLMMmodel_Alpha_6 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category*SamplesCollected + (1 | zipcode), 
                                          data = ICU_UPDATED_Alpha, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Alpha_6)


COVID_Patients_GLMMmodel_Alpha_7 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category*PositiveCases + (1 | zipcode), 
                                          data = ICU_UPDATED_Alpha, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Alpha_7)




correlation_test <- cor.test(ICU_UPDATED_Delta$Total.ICU.Hospitalizations.per.Zipcode.per.Month, ICU_UPDATED_Delta$Total_SHIELD_centers_per_zipcode_per_month, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_Delta$Total.ICU.Hospitalizations.per.Zipcode.per.Month, ICU_UPDATED_Delta$SamplesCollected, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_Delta$Total.ICU.Hospitalizations.per.Zipcode.per.Month, ICU_UPDATED_Delta$PositiveCases, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


vif_model <- lm(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + PositiveCases + ADI.category + Total_SHIELD_centers_per_zipcode_per_month , data = ICU_UPDATED_Delta)
vif(vif_model) 


COVID_Patients_GLMMmodel_Delta_1 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ Total_SHIELD_centers_per_zipcode_per_month + (1 | zipcode), 
                                          data = ICU_UPDATED_Delta, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Delta_1)


COVID_Patients_GLMMmodel_Delta_2 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category + (1 | zipcode), 
                                          data = ICU_UPDATED_Delta, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Delta_2)


ICU_UPDATED_Delta$SamplesCollected <- scale(ICU_UPDATED_Delta$SamplesCollected)

COVID_Patients_GLMMmodel_Delta_3 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + (1 | zipcode), 
                                          data = ICU_UPDATED_Delta, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Delta_3)


COVID_Patients_GLMMmodel_Delta_4 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ PositiveCases + (1 | zipcode), 
                                          data = ICU_UPDATED_Delta, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Delta_4)


COVID_Patients_GLMMmodel_Delta_5 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category + (1 | zipcode), 
                                          data = ICU_UPDATED_Delta, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Delta_5)


COVID_Patients_GLMMmodel_Delta_6 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category*SamplesCollected + (1 | zipcode), 
                                          data = ICU_UPDATED_Delta, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Delta_6)


COVID_Patients_GLMMmodel_Delta_7 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category*PositiveCases + (1 | zipcode), 
                                          data = ICU_UPDATED_Delta, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Delta_7)



correlation_test <- cor.test(ICU_UPDATED_Omicron$Total.ICU.Hospitalizations.per.Zipcode.per.Month, ICU_UPDATED_Omicron$Total_SHIELD_centers_per_zipcode_per_month, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_Omicron$Total.ICU.Hospitalizations.per.Zipcode.per.Month, ICU_UPDATED_Omicron$SamplesCollected, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


correlation_test <- cor.test(ICU_UPDATED_Omicron$Total.ICU.Hospitalizations.per.Zipcode.per.Month, ICU_UPDATED_Omicron$PositiveCases, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- correlation_test$estimate
p_value <- correlation_test$p.value

cat("Correlation coefficient:", correlation, "\n")
cat("P-value:", p_value, "\n")


vif_model <- lm(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + PositiveCases + ADI.category + Total_SHIELD_centers_per_zipcode_per_month , data = ICU_UPDATED_Omicron)
vif(vif_model) 


COVID_Patients_GLMMmodel_Omicron_1 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ Total_SHIELD_centers_per_zipcode_per_month + (1 | zipcode), 
                                          data = ICU_UPDATED_Omicron, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Omicron_1)


COVID_Patients_GLMMmodel_Omicron_2 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category + (1 | zipcode), 
                                          data = ICU_UPDATED_Omicron, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Omicron_2)


ICU_UPDATED_Omicron$SamplesCollected <- scale(ICU_UPDATED_Omicron$SamplesCollected)

COVID_Patients_GLMMmodel_Omicron_3 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + (1 | zipcode), 
                                          data = ICU_UPDATED_Omicron, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Omicron_3)


ICU_UPDATED_Omicron$PositiveCases <- scale(ICU_UPDATED_Omicron$PositiveCases)

COVID_Patients_GLMMmodel_Omicron_4 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ PositiveCases + (1 | zipcode), 
                                          data = ICU_UPDATED_Omicron, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Omicron_4)


COVID_Patients_GLMMmodel_Omicron_5 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ Total_SHIELD_centers_per_zipcode_per_month*ADI.category + (1 | zipcode), 
                                          data = ICU_UPDATED_Omicron, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Omicron_5)


COVID_Patients_GLMMmodel_Omicron_6 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category*SamplesCollected + (1 | zipcode), 
                                          data = ICU_UPDATED_Omicron, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Omicron_6)


COVID_Patients_GLMMmodel_Omicron_7 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ ADI.category*PositiveCases + (1 | zipcode), 
                                          data = ICU_UPDATED_Omicron, 
                                          family = poisson)

tab_model(COVID_Patients_GLMMmodel_Omicron_7)
