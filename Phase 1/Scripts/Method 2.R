###############################################################################
###############################################################################
###############################################################################

ICU_UPDATED_Method2 <- fread(file.choose(), sep=",", header=T)

Effective_Test_Centers <- fread(file.choose(), sep=",", header=T)

TestCenters <- fread(file.choose(), sep=",", header=T)

ICU_UPDATED_Method2 <- full_join(ICU_UPDATED_Method2, TestCenters, by = c("zipcode","Date"))

write.csv(ICU_UPDATED_Method2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 2/ICU_UPDATED_Method2.csv", row.names = FALSE)

###############################################################################
############################# ALPHA WAVE (WITH NA) ############################
###############################################################################

ALPHA_LessDisadvantaged <- fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ALPHA_LessDisadvantaged$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


ALPHA_LessDisadvantaged$zipcode = as.factor(ALPHA_LessDisadvantaged$zipcode)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ALPHA_LessDisadvantaged)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ALPHA_LessDisadvantaged)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)



ALPHA_MoreDisadvantaged <- fread(file.choose(), sep=",", header=T)


skewness_ICU_rate = skewness(ALPHA_MoreDisadvantaged$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


ALPHA_MoreDisadvantaged$zipcode = as.factor(ALPHA_MoreDisadvantaged$zipcode)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ALPHA_MoreDisadvantaged)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ALPHA_MoreDisadvantaged)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)

###############################################################################
############################### DELTA WAVE (WITH NA) ##########################
###############################################################################

DELTA_LessDisadvantaged <- fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(DELTA_LessDisadvantaged$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


DELTA_LessDisadvantaged$zipcode = as.factor(DELTA_LessDisadvantaged$zipcode)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = DELTA_LessDisadvantaged)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = DELTA_LessDisadvantaged)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)



DELTA_MoreDisadvantaged <- fread(file.choose(), sep=",", header=T)


skewness_ICU_rate = skewness(DELTA_MoreDisadvantaged$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


DELTA_MoreDisadvantaged$zipcode = as.factor(DELTA_MoreDisadvantaged$zipcode)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = DELTA_MoreDisadvantaged)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = DELTA_MoreDisadvantaged)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)

###############################################################################
########################### OMICRON WAVE (WITH NA) ############################
###############################################################################

OMICRON_LessDisadvantaged <- fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(OMICRON_LessDisadvantaged$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


OMICRON_LessDisadvantaged$zipcode = as.factor(OMICRON_LessDisadvantaged$zipcode)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = OMICRON_LessDisadvantaged)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = OMICRON_LessDisadvantaged)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


OMICRON_MoreDisadvantaged <- fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(OMICRON_MoreDisadvantaged$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


OMICRON_MoreDisadvantaged$zipcode = as.factor(OMICRON_MoreDisadvantaged$zipcode)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = OMICRON_MoreDisadvantaged)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = OMICRON_MoreDisadvantaged)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


###############################################################################
############################# ALPHA WAVE (WITHOUT NA) #########################
###############################################################################

ALPHA_LessDisadvantaged <- fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ALPHA_LessDisadvantaged$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


ALPHA_LessDisadvantaged$zipcode = as.factor(ALPHA_LessDisadvantaged$zipcode)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ALPHA_LessDisadvantaged)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ALPHA_LessDisadvantaged)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)



ALPHA_MoreDisadvantaged <- fread(file.choose(), sep=",", header=T)


skewness_ICU_rate = skewness(ALPHA_MoreDisadvantaged$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


ALPHA_MoreDisadvantaged$zipcode = as.factor(ALPHA_MoreDisadvantaged$zipcode)


ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ALPHA_MoreDisadvantaged)

tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ALPHA_MoreDisadvantaged)

tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


###############################################################################
############################### DELTA WAVE (WITHOUT NA) #######################
###############################################################################

DELTA_LessDisadvantaged <- fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(DELTA_LessDisadvantaged$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


DELTA_LessDisadvantaged$zipcode = as.factor(DELTA_LessDisadvantaged$zipcode)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = DELTA_LessDisadvantaged)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = DELTA_LessDisadvantaged)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)



DELTA_MoreDisadvantaged <- fread(file.choose(), sep=",", header=T)


skewness_ICU_rate = skewness(DELTA_MoreDisadvantaged$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


DELTA_MoreDisadvantaged$zipcode = as.factor(DELTA_MoreDisadvantaged$zipcode)


ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = DELTA_MoreDisadvantaged)

tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = DELTA_MoreDisadvantaged)

tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


###############################################################################
########################### OMICRON WAVE (WITHOUT NA) #########################
###############################################################################

OMICRON_LessDisadvantaged <- fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(OMICRON_LessDisadvantaged$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


OMICRON_LessDisadvantaged$zipcode = as.factor(OMICRON_LessDisadvantaged$zipcode)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = OMICRON_LessDisadvantaged)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = OMICRON_LessDisadvantaged)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


OMICRON_MoreDisadvantaged <- fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(OMICRON_MoreDisadvantaged$`Covid ICU Rate` , type = 3)
print(skewness_ICU_rate)


OMICRON_MoreDisadvantaged$zipcode = as.factor(OMICRON_MoreDisadvantaged$zipcode)


ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`Covid ICU Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = OMICRON_MoreDisadvantaged)

tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`Covid ICU Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = OMICRON_MoreDisadvantaged)

tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)
