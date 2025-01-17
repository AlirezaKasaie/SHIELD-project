library(tidyverse)
library(sjPlot)

install.packages("plm")
library(plm)

install.packages("stargazer")
library(stargazer)

ICU_UPDATED = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED$Date <- as.Date(paste0(ICU_UPDATED$Date, "-01"))

ICU_UPDATED$ADI_category = as.factor(ICU_UPDATED$ADI_category)

ICU_UPDATED$ADI_category <- relevel(ICU_UPDATED$ADI_category, ref = "Less Disadvantaged")


#Declare the data to be a panel dataset

ICU_UPDATED_Panel = pdata.frame(ICU_UPDATED, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1 = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
             data = ICU_UPDATED_Panel, model = "within")

stargazer(fixeff_1, type = "text")

tab_model(fixeff_1, digits = 5)


#Run a random effect

randeff_1 = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                data = ICU_UPDATED_Panel, model = "random")

stargazer(randeff_1, type = "text")

tab_model(randeff_1, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(randeff_1,fixeff_1)


##############################################################################
############################### ALPHA WAVE ###################################
##############################################################################


ICU_UPDATED_ALPHA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_ALPHA$Date <- as.Date(paste0(ICU_UPDATED_ALPHA$Date, "-01"))

ICU_UPDATED_ALPHA$ADI_category = as.factor(ICU_UPDATED_ALPHA$ADI_category)

ICU_UPDATED_ALPHA$ADI_category <- relevel(ICU_UPDATED_ALPHA$ADI_category, ref = "Less Disadvantaged")


#Declare the data to be a panel dataset

ICU_UPDATED_ALPHA_Panel = pdata.frame(ICU_UPDATED_ALPHA, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_ALPHA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                     data = ICU_UPDATED_ALPHA_Panel, index = c("zipcode","Date"), model = "within")


tab_model(fixeff_1_ALPHA, digits = 5)



#Run a random effect

randeff_1_ALPHA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                data = ICU_UPDATED_ALPHA_Panel, index = c("zipcode","Date"), model = "random")


tab_model(randeff_1_ALPHA, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_ALPHA,randeff_1_ALPHA)


##############################################################################
############################### DELTA WAVE ###################################
##############################################################################


ICU_UPDATED_DELTA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_DELTA$Date <- as.Date(paste0(ICU_UPDATED_DELTA$Date, "-01"))

ICU_UPDATED_DELTA$ADI_category = as.factor(ICU_UPDATED_DELTA$ADI_category)

ICU_UPDATED_DELTA$ADI_category <- relevel(ICU_UPDATED_DELTA$ADI_category, ref = "Less Disadvantaged")


#Declare the data to be a panel dataset

ICU_UPDATED_DELTA_Panel = pdata.frame(ICU_UPDATED_DELTA, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_DELTA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                     data = ICU_UPDATED_DELTA_Panel, index = c("zipcode","Date"), model = "within")


tab_model(fixeff_1_DELTA, digits = 5)


#Run a random effect

randeff_1_DELTA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                      data = ICU_UPDATED_DELTA_Panel, index = c("zipcode","Date"), model = "random")


tab_model(randeff_1_DELTA, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_DELTA,randeff_1_DELTA)


##############################################################################
############################### OMICRON WAVE #################################
##############################################################################


ICU_UPDATED_OMICRON = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_OMICRON$Date <- as.Date(paste0(ICU_UPDATED_OMICRON$Date, "-01"))

ICU_UPDATED_OMICRON$ADI_category = as.factor(ICU_UPDATED_OMICRON$ADI_category)

ICU_UPDATED_OMICRON$ADI_category <- relevel(ICU_UPDATED_OMICRON$ADI_category, ref = "Less Disadvantaged")


#Declare the data to be a panel dataset

ICU_UPDATED_OMICRON_Panel = pdata.frame(ICU_UPDATED_OMICRON, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_OMICRON = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                       data = ICU_UPDATED_OMICRON_Panel, index = c("zipcode","Date"), model = "within")


tab_model(fixeff_1_OMICRON, digits = 5)


#Run a random effect

randeff_1_OMICRON = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                      data = ICU_UPDATED_OMICRON_Panel, index = c("zipcode","Date"), model = "random")


tab_model(randeff_1_OMICRON, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_OMICRON,randeff_1_OMICRON)


##############################################################################
############################### LESS DISADVANTAGED ###########################
##############################################################################


ICU_UPDATED_Less_Disadvantaged = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Less_Disadvantaged$Date <- as.Date(paste0(ICU_UPDATED_Less_Disadvantaged$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_Less_Disadvantaged_Panel = pdata.frame(ICU_UPDATED_Less_Disadvantaged, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_Less_Disadvantaged = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                       data = ICU_UPDATED_Less_Disadvantaged_Panel, model = "within")

stargazer(fixeff_1_Less_Disadvantaged, type = "text")

tab_model(fixeff_1_Less_Disadvantaged, digits = 5)


#Run a random effect

randeff_1_Less_Disadvantaged = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                        data = ICU_UPDATED_Less_Disadvantaged_Panel, model = "random")

stargazer(randeff_1_Less_Disadvantaged, type = "text")

tab_model(randeff_1_Less_Disadvantaged, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_Less_Disadvantaged,randeff_1_Less_Disadvantaged)

##############################################################################
############################### LESS DISADVANTAGED ###########################
#################################### ALPHA WAVE ##############################
##############################################################################


ICU_UPDATED_Less_Disadvantaged_ALPHA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Less_Disadvantaged_ALPHA$Date <- as.Date(paste0(ICU_UPDATED_Less_Disadvantaged_ALPHA$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_Less_Disadvantaged_ALPHA_Panel = pdata.frame(ICU_UPDATED_Less_Disadvantaged_ALPHA, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_Less_Disadvantaged_ALPHA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                  data = ICU_UPDATED_Less_Disadvantaged_ALPHA_Panel, 
                                  index = c("zipcode","Date"), model = "within")


tab_model(fixeff_1_Less_Disadvantaged_ALPHA, digits = 5)


#Run a random effect

randeff_1_Less_Disadvantaged_ALPHA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                   data = ICU_UPDATED_Less_Disadvantaged_ALPHA_Panel, 
                                   index = c("zipcode","Date"), model = "random")

tab_model(randeff_1_Less_Disadvantaged_ALPHA, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_Less_Disadvantaged_ALPHA,randeff_1_Less_Disadvantaged_ALPHA)


##############################################################################
############################### LESS DISADVANTAGED ###########################
#################################### DELTA WAVE ##############################
##############################################################################


ICU_UPDATED_Less_Disadvantaged_DELTA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Less_Disadvantaged_DELTA$Date <- as.Date(paste0(ICU_UPDATED_Less_Disadvantaged_DELTA$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_Less_Disadvantaged_DELTA_Panel = pdata.frame(ICU_UPDATED_Less_Disadvantaged_DELTA, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_Less_Disadvantaged_DELTA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                        data = ICU_UPDATED_Less_Disadvantaged_DELTA_Panel,
                                        index = c("zipcode","Date"), model = "within")

tab_model(fixeff_1_Less_Disadvantaged_DELTA, digits = 5)


#Run a random effect

randeff_1_Less_Disadvantaged_DELTA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                         data = ICU_UPDATED_Less_Disadvantaged_DELTA_Panel,
                                         index = c("zipcode","Date"), model = "random")

tab_model(randeff_1_Less_Disadvantaged_DELTA, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_Less_Disadvantaged_DELTA,randeff_1_Less_Disadvantaged_DELTA)

##############################################################################
############################### LESS DISADVANTAGED ###########################
################################## OMICRON WAVE ##############################
##############################################################################


ICU_UPDATED_Less_Disadvantaged_OMICRON = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_Less_Disadvantaged_OMICRON$Date <- as.Date(paste0(ICU_UPDATED_Less_Disadvantaged_OMICRON$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_Less_Disadvantaged_OMICRON_Panel = pdata.frame(ICU_UPDATED_Less_Disadvantaged_OMICRON, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_Less_Disadvantaged_OMICRON = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                        data = ICU_UPDATED_Less_Disadvantaged_OMICRON_Panel,
                                        index = c("zipcode","Date"), model = "within")

tab_model(fixeff_1_Less_Disadvantaged_OMICRON, digits = 5)


#Run a random effect

randeff_1_Less_Disadvantaged_OMICRON = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                         data = ICU_UPDATED_Less_Disadvantaged_OMICRON_Panel,
                                         index = c("zipcode","Date"), model = "random")


tab_model(randeff_1_Less_Disadvantaged_OMICRON, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_Less_Disadvantaged_OMICRON,randeff_1_Less_Disadvantaged_OMICRON)

##############################################################################
############################### MORE DISADVANTAGED ###########################
##############################################################################


ICU_UPDATED_More_Disadvantaged = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_More_Disadvantaged$Date <- as.Date(paste0(ICU_UPDATED_More_Disadvantaged$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_More_Disadvantaged_Panel = pdata.frame(ICU_UPDATED_More_Disadvantaged, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_More_Disadvantaged = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                  data = ICU_UPDATED_More_Disadvantaged_Panel, model = "within")

stargazer(fixeff_1_More_Disadvantaged, type = "text")

tab_model(fixeff_1_More_Disadvantaged, digits = 5)


#Run a random effect

randeff_1_More_Disadvantaged = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                   data = ICU_UPDATED_More_Disadvantaged_Panel, model = "random")

stargazer(randeff_1_More_Disadvantaged, type = "text")

tab_model(randeff_1_More_Disadvantaged, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_More_Disadvantaged,randeff_1_More_Disadvantaged)


##############################################################################
############################### More DISADVANTAGED ###########################
################################## ALPHA WAVE ################################
##############################################################################

ICU_UPDATED_More_Disadvantaged_ALPHA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_More_Disadvantaged_ALPHA$Date <- as.Date(paste0(ICU_UPDATED_More_Disadvantaged_ALPHA$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_More_Disadvantaged_ALPHA_Panel = pdata.frame(ICU_UPDATED_More_Disadvantaged_ALPHA, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_More_Disadvantaged_ALPHA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                  data = ICU_UPDATED_More_Disadvantaged_ALPHA_Panel,
                                  index = c("zipcode","Date"), model = "within")


tab_model(fixeff_1_More_Disadvantaged_ALPHA, digits = 5)


#Run a random effect

randeff_1_More_Disadvantaged_ALPHA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                   data = ICU_UPDATED_More_Disadvantaged_ALPHA_Panel, 
                                   index = c("zipcode","Date"), model = "random")


tab_model(randeff_1_More_Disadvantaged_ALPHA, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_More_Disadvantaged_ALPHA,randeff_1_More_Disadvantaged_ALPHA)

##############################################################################
############################### More DISADVANTAGED ###########################
################################## DELTA WAVE ################################
##############################################################################

ICU_UPDATED_More_Disadvantaged_DELTA = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_More_Disadvantaged_DELTA$Date <- as.Date(paste0(ICU_UPDATED_More_Disadvantaged_DELTA$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_More_Disadvantaged_DELTA_Panel = pdata.frame(ICU_UPDATED_More_Disadvantaged_DELTA, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_More_Disadvantaged_DELTA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                        data = ICU_UPDATED_More_Disadvantaged_DELTA_Panel,
                                        index = c("zipcode","Date"), model = "within")


tab_model(fixeff_1_More_Disadvantaged_DELTA, digits = 5)


#Run a random effect

randeff_1_More_Disadvantaged_DELTA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                         data = ICU_UPDATED_More_Disadvantaged_DELTA_Panel,
                                         index = c("zipcode","Date"), model = "random")


tab_model(randeff_1_More_Disadvantaged_DELTA, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_More_Disadvantaged_DELTA,randeff_1_More_Disadvantaged_DELTA)

##############################################################################
############################### More DISADVANTAGED ###########################
################################## OMICRON WAVE ##############################
##############################################################################

ICU_UPDATED_More_Disadvantaged_OMICRON = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_More_Disadvantaged_OMICRON$Date <- as.Date(paste0(ICU_UPDATED_More_Disadvantaged_OMICRON$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_More_Disadvantaged_OMICRON_Panel = pdata.frame(ICU_UPDATED_More_Disadvantaged_OMICRON, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_More_Disadvantaged_OMICRON = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                        data = ICU_UPDATED_More_Disadvantaged_OMICRON_Panel,
                                        index = c("zipcode","Date"), model = "within")


tab_model(fixeff_1_More_Disadvantaged_OMICRON, digits = 5)


#Run a random effect

randeff_1_More_Disadvantaged_OMICRON = plm(log(COVID_ICU_Rate+1) ~ `testcenters`+`Effective.Number.of.Center`,
                                         data = ICU_UPDATED_More_Disadvantaged_OMICRON_Panel,
                                         index = c("zipcode","Date"), model = "random")


tab_model(randeff_1_More_Disadvantaged_OMICRON, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_More_Disadvantaged_OMICRON,randeff_1_More_Disadvantaged_OMICRON)


##############################################################################
##################################### LAG ANALYSIS ###########################
################################# ALPHA WAVE #################################
################################## ONE MONTH #################################
##############################################################################


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


Lag_Analysis_Mehtod8_ALPHA_OneMonth$Date <- as.Date(paste0(Lag_Analysis_Mehtod8_ALPHA_OneMonth$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_Lag1_ALPHA_Panel = pdata.frame(Lag_Analysis_Mehtod8_ALPHA_OneMonth, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_Lag1_ALPHA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                                        data = ICU_UPDATED_Lag1_ALPHA_Panel, 
                          index = c("zipcode","Date"), model = "within")

tab_model(fixeff_1_Lag1_ALPHA, digits = 5)


#Run a random effect

randeff_1_Lag1_ALPHA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                                         data = ICU_UPDATED_Lag1_ALPHA_Panel, 
                           index = c("zipcode","Date"), model = "random")


tab_model(randeff_1_Lag1_ALPHA, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_Lag1_ALPHA,randeff_1_Lag1_ALPHA)


##############################################################################
################################# DELTA WAVE #################################
################################## ONE MONTH #################################
##############################################################################


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


Lag_Analysis_Mehtod8_DELTA_OneMonth$Date <- as.Date(paste0(Lag_Analysis_Mehtod8_DELTA_OneMonth$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_Lag1_DELTA_Panel = pdata.frame(Lag_Analysis_Mehtod8_DELTA_OneMonth, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_Lag1_DELTA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                          data = ICU_UPDATED_Lag1_DELTA_Panel,
                          index = c('zipcode','Date'), model = "within")


tab_model(fixeff_1_Lag1_DELTA, digits = 5)


#Run a random effect

randeff_1_Lag1_DELTA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                           data = ICU_UPDATED_Lag1_DELTA_Panel,
                           index = c('zipcode','Date'), model = "random")


tab_model(randeff_1_Lag1_DELTA, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_Lag1_DELTA,randeff_1_Lag1_DELTA)


##############################################################################
################################# OMICRON WAVE ###############################
################################## ONE MONTH #################################
##############################################################################


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


Lag_Analysis_Mehtod8_OMICRON_OneMonth$Date <- as.Date(paste0(Lag_Analysis_Mehtod8_OMICRON_OneMonth$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_Lag1_OMICRON_Panel = pdata.frame(Lag_Analysis_Mehtod8_OMICRON_OneMonth, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_Lag1_OMICRON = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                          data = ICU_UPDATED_Lag1_OMICRON_Panel, 
                          index = c('zipcode','Date'), model = "within")


tab_model(fixeff_1_Lag1_OMICRON, digits = 5)


#Run a random effect

randeff_1_Lag1_OMICRON = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                           data = ICU_UPDATED_Lag1_OMICRON_Panel,
                           index = c('zipcode','Date'), model = "random")


tab_model(randeff_1_Lag1_OMICRON, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_Lag1_OMICRON,randeff_1_Lag1_OMICRON)


##############################################################################
##################################### LAG ANALYSIS ###########################
################################# ALPHA WAVE #################################
################################## TWO MONTH #################################
##############################################################################


Lag_Analysis_Mehtod8_ALPHA_TWOMonth = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod8_ALPHA_TWOMonth <- Lag_Analysis_Mehtod8_ALPHA_TWOMonth %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_COVID_ICU_Rate = lag(COVID_ICU_Rate, 2)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod8_ALPHA_TWOMonth = na.omit(Lag_Analysis_Mehtod8_ALPHA_TWOMonth)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_ALPHA_TWOMonth$Lag2_COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod8_ALPHA_TWOMonth$ADI_category = as.factor(Lag_Analysis_Mehtod8_ALPHA_TWOMonth$ADI_category)

Lag_Analysis_Mehtod8_ALPHA_TWOMonth$zipcode = as.factor(Lag_Analysis_Mehtod8_ALPHA_TWOMonth$zipcode)


Lag_Analysis_Mehtod8_ALPHA_TWOMonth$ADI_category <- relevel(Lag_Analysis_Mehtod8_ALPHA_TWOMonth$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_COVID_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod8_ALPHA_TWOMonth)
vif(vif_model)


Lag_Analysis_Mehtod8_ALPHA_TWOMonth$Date <- as.Date(paste0(Lag_Analysis_Mehtod8_ALPHA_TWOMonth$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_Lag2_ALPHA_Panel = pdata.frame(Lag_Analysis_Mehtod8_ALPHA_TWOMonth, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_Lag2_ALPHA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                          data = ICU_UPDATED_Lag2_ALPHA_Panel,
                          index = c('zipcode','Date'), model = "within")


tab_model(fixeff_1_Lag2_ALPHA, digits = 5)


#Run a random effect

randeff_1_Lag2_ALPHA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                           data = ICU_UPDATED_Lag2_ALPHA_Panel,
                           index = c('zipcode','Date'), model = "random")


tab_model(randeff_1_Lag2_ALPHA, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_Lag2_ALPHA,randeff_1_Lag2_ALPHA)


##############################################################################
################################# DELTA WAVE #################################
################################## TWO MONTH #################################
##############################################################################


Lag_Analysis_Mehtod8_DELTA_TWOMonth = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod8_DELTA_TWOMonth <- Lag_Analysis_Mehtod8_DELTA_TWOMonth %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_COVID_ICU_Rate = lag(COVID_ICU_Rate, 2)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod8_DELTA_TWOMonth = na.omit(Lag_Analysis_Mehtod8_DELTA_TWOMonth)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_DELTA_TWOMonth$Lag2_COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod8_DELTA_TWOMonth$ADI_category = as.factor(Lag_Analysis_Mehtod8_DELTA_TWOMonth$ADI_category)

Lag_Analysis_Mehtod8_DELTA_TWOMonth$zipcode = as.factor(Lag_Analysis_Mehtod8_DELTA_TWOMonth$zipcode)


Lag_Analysis_Mehtod8_DELTA_TWOMonth$ADI_category <- relevel(Lag_Analysis_Mehtod8_DELTA_TWOMonth$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_COVID_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod8_DELTA_TWOMonth)
vif(vif_model)


Lag_Analysis_Mehtod8_DELTA_TWOMonth$Date <- as.Date(paste0(Lag_Analysis_Mehtod8_DELTA_TWOMonth$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_Lag2_DELTA_Panel = pdata.frame(Lag_Analysis_Mehtod8_DELTA_TWOMonth, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_Lag2_DELTA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                          data = ICU_UPDATED_Lag2_DELTA_Panel,
                          index = c('zipcode','Date'), model = "within")


tab_model(fixeff_1_Lag2_DELTA, digits = 5)


#Run a random effect

randeff_1_Lag2_DELTA = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                           data = ICU_UPDATED_Lag2_DELTA_Panel,
                           index = c('zipcode','Date'), model = "random")


tab_model(randeff_1_Lag2_DELTA, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_Lag2_DELTA,randeff_1_Lag2_DELTA)


##############################################################################
################################# OMICRON WAVE ###############################
################################## TWO MONTH #################################
##############################################################################


Lag_Analysis_Mehtod8_OMICRON_TWOMonth = fread(file.choose(), sep=",", header=T)

Lag_Analysis_Mehtod8_OMICRON_TWOMonth <- Lag_Analysis_Mehtod8_OMICRON_TWOMonth %>%
  arrange(Date) %>%
  group_by(zipcode) %>% # Ensure lagging is done within each Zipcode group
  mutate(
    Lag2_COVID_ICU_Rate = lag(COVID_ICU_Rate, 2)
  ) %>%
  ungroup()

Lag_Analysis_Mehtod8_OMICRON_TWOMonth = na.omit(Lag_Analysis_Mehtod8_OMICRON_TWOMonth)


skewness_ICU_rate = skewness(Lag_Analysis_Mehtod8_OMICRON_TWOMonth$Lag2_COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)


Lag_Analysis_Mehtod8_OMICRON_TWOMonth$ADI_category = as.factor(Lag_Analysis_Mehtod8_OMICRON_TWOMonth$ADI_category)

Lag_Analysis_Mehtod8_OMICRON_TWOMonth$zipcode = as.factor(Lag_Analysis_Mehtod8_OMICRON_TWOMonth$zipcode)


Lag_Analysis_Mehtod8_OMICRON_TWOMonth$ADI_category <- relevel(Lag_Analysis_Mehtod8_OMICRON_TWOMonth$ADI_category, ref = "Less Disadvantaged")


vif_model <- lm(Lag2_COVID_ICU_Rate ~ `ADI_category` + `testcenters` + `Effective Number of Center`, data = Lag_Analysis_Mehtod8_OMICRON_TWOMonth)
vif(vif_model)


Lag_Analysis_Mehtod8_OMICRON_TWOMonth$Date <- as.Date(paste0(Lag_Analysis_Mehtod8_OMICRON_TWOMonth$Date, "-01"))


#Declare the data to be a panel dataset

ICU_UPDATED_Lag2_OMICRON_Panel = pdata.frame(Lag_Analysis_Mehtod8_OMICRON_TWOMonth, index = c('zipcode','Date') )

#Run a panel model

#Run a fixed effect model first. It is also called "within" model because it is 
#only focuses on variations within the individual observations

fixeff_1_Lag2_OMICRON = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                          data = ICU_UPDATED_Lag2_OMICRON_Panel,
                          index = c('zipcode','Date'), model = "within")

tab_model(fixeff_1_Lag2_OMICRON, digits = 5)


#Run a random effect

randeff_1_Lag2_OMICRON = plm(log(COVID_ICU_Rate+1) ~ `testcenters`*`ADI_category`+`Effective.Number.of.Center`*`ADI_category`,
                           data = ICU_UPDATED_Lag2_OMICRON_Panel,
                           index = c('zipcode','Date'), model = "random")


tab_model(randeff_1_Lag2_OMICRON, digits = 5)

#To check whether to use the fixed-effect or random-effect models. If the p-value is significant,
#Then use the fixed-effect model

phtest(fixeff_1_Lag2_OMICRON,randeff_1_Lag2_OMICRON)
