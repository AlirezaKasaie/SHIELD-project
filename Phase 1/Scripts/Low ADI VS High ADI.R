
############################################# ALPHA WAVE ######################################################

############################################# Less Disadvantaged ADI ######################################################

ICU_UPDATED_Zipcode_ALPHA = read.csv(file.choose(), sep=",", header=T)

ICU_rate_GLMMmodel_Alpha_1 <- lmer(log(Covid.ICU.Rate + 1) ~ TestCenters + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_ALPHA)

tab_model(ICU_rate_GLMMmodel_Alpha_1, digits = 5)


ICU_rate_GLMMmodel_Alpha_2 <- lmer(log(Covid.ICU.Rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA)

tab_model(ICU_rate_GLMMmodel_Alpha_2, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_1 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ TestCenters + (1 | zipcode), 
                                                data = ICU_UPDATED_Zipcode_ALPHA, 
                                                family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_1, digits = 5)


ICU_UPDATED_Zipcode_ALPHA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_ALPHA$SamplesCollected)


ICU_hospitalization_GLMMmodel_Alpha_2 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_2, digits = 5)


############################################# More Disadvantaged ADI ######################################################


ICU_UPDATED_Zipcode_ALPHA = read.csv(file.choose(), sep=",", header=T)

ICU_rate_GLMMmodel_Alpha_1 <- lmer(log(Covid.ICU.Rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA)

tab_model(ICU_rate_GLMMmodel_Alpha_1, digits = 5)


ICU_rate_GLMMmodel_Alpha_2 <- lmer(log(Covid.ICU.Rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_ALPHA)

tab_model(ICU_rate_GLMMmodel_Alpha_2, digits = 5)


ICU_hospitalization_GLMMmodel_Alpha_1 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_1, digits = 5)


ICU_UPDATED_Zipcode_ALPHA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_ALPHA$SamplesCollected)


ICU_hospitalization_GLMMmodel_Alpha_2 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_ALPHA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Alpha_2, digits = 5)


############################################# DELTA WAVE ######################################################

############################################# Less Disadvantaged ADI ######################################################


ICU_UPDATED_Zipcode_DELTA = read.csv(file.choose(), sep=",", header=T)

ICU_rate_GLMMmodel_Delta_1 <- lmer(log(Covid.ICU.Rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA)

tab_model(ICU_rate_GLMMmodel_Delta_1, digits = 5)


ICU_UPDATED_Zipcode_DELTA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_DELTA$SamplesCollected)


ICU_rate_GLMMmodel_Delta_2 <- lmer(log(Covid.ICU.Rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA)

tab_model(ICU_rate_GLMMmodel_Delta_2, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_1 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_1, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_2 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_2, digits = 5)


############################################# More Disadvantaged ADI ######################################################

ICU_UPDATED_Zipcode_DELTA = read.csv(file.choose(), sep=",", header=T)

ICU_rate_GLMMmodel_Delta_1 <- lmer(log(Covid.ICU.Rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA)

tab_model(ICU_rate_GLMMmodel_Delta_1, digits = 5)


ICU_UPDATED_Zipcode_DELTA$SamplesCollected <- scale(ICU_UPDATED_Zipcode_DELTA$SamplesCollected)


ICU_rate_GLMMmodel_Delta_2 <- lmer(log(Covid.ICU.Rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_DELTA)

tab_model(ICU_rate_GLMMmodel_Delta_2, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_1 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_1, digits = 5)


ICU_hospitalization_GLMMmodel_Delta_2 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_DELTA, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Delta_2, digits = 5)


############################################# OMICRON WAVE ######################################################

############################################# Less Disadvantaged ADI ######################################################


ICU_UPDATED_Zipcode_OMICRON = read.csv(file.choose(), sep=",", header=T)

ICU_rate_GLMMmodel_Omicron_1 <- lmer(log(Covid.ICU.Rate + 1) ~ TestCenters + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Zipcode_OMICRON)

tab_model(ICU_rate_GLMMmodel_Omicron_1, digits = 5)


ICU_UPDATED_Zipcode_OMICRON$SamplesCollected <- scale(ICU_UPDATED_Zipcode_OMICRON$SamplesCollected)


ICU_rate_GLMMmodel_Omicron_2 <- lmer(log(Covid.ICU.Rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON)

tab_model(ICU_rate_GLMMmodel_Omicron_2, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_1 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ TestCenters + (1 | zipcode), 
                                               data = ICU_UPDATED_Zipcode_OMICRON, 
                                               family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_1, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_2 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_2, digits = 5)


############################################# More Disadvantaged ADI ######################################################


ICU_UPDATED_Zipcode_OMICRON = read.csv(file.choose(), sep=",", header=T)

ICU_rate_GLMMmodel_Omicron_1 <- lmer(log(Covid.ICU.Rate + 1) ~ TestCenters + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON)

tab_model(ICU_rate_GLMMmodel_Omicron_1, digits = 5)


ICU_UPDATED_Zipcode_OMICRON$SamplesCollected <- scale(ICU_UPDATED_Zipcode_OMICRON$SamplesCollected)


ICU_rate_GLMMmodel_Omicron_2 <- lmer(log(Covid.ICU.Rate + 1) ~ SamplesCollected + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Zipcode_OMICRON)

tab_model(ICU_rate_GLMMmodel_Omicron_2, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_1 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ TestCenters + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_1, digits = 5)


ICU_hospitalization_GLMMmodel_Omicron_2 <- glmer(Total.ICU.Hospitalizations.per.Zipcode.per.Month ~ SamplesCollected + (1 | zipcode), 
                                                 data = ICU_UPDATED_Zipcode_OMICRON, 
                                                 family = poisson)

tab_model(ICU_hospitalization_GLMMmodel_Omicron_2, digits = 5)
