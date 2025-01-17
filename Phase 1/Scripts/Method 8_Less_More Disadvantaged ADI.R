###############################################################################
################################### LESS DISADVANTAGED ########################
################################### WHOLE DATA SET ############################
###############################################################################

ICU_UPDATED_Method8_LESS_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method8_LESS_DISADVANTAGED$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method8_LESS_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method8_LESS_DISADVANTAGED$zipcode)


vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_LESS_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)

###############################################################################
################################### LESS DISADVANTAGED ########################
################################### ALPHA WAVE ################################
###############################################################################

ICU_UPDATED_Method8_ALPHA_LESS_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_Method8_ALPHA_LESS_DISADVANTAGED$Date = as.factor(ICU_UPDATED_Method8_ALPHA_LESS_DISADVANTAGED$Date)

ICU_UPDATED_Method8_ALPHA_LESS_DISADVANTAGED$Date <- relevel(ICU_UPDATED_Method8_ALPHA_LESS_DISADVANTAGED$Date, ref = "2021-03")

skewness_ICU_rate = skewness(ICU_UPDATED_Method8_ALPHA_LESS_DISADVANTAGED$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method8_ALPHA_LESS_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method8_ALPHA_LESS_DISADVANTAGED$zipcode)


vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_ALPHA_LESS_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*Date + 
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_ALPHA_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*Date  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_ALPHA_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)

###############################################################################
################################### DELTA WAVE  ###############################
###############################################################################

ICU_UPDATED_Method8_DELTA_LESS_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_Method8_DELTA_LESS_DISADVANTAGED$Date = as.factor(ICU_UPDATED_Method8_DELTA_LESS_DISADVANTAGED$Date)

ICU_UPDATED_Method8_DELTA_LESS_DISADVANTAGED$Date <- relevel(ICU_UPDATED_Method8_DELTA_LESS_DISADVANTAGED$Date, ref = "2021-08")

skewness_ICU_rate = skewness(ICU_UPDATED_Method8_DELTA_LESS_DISADVANTAGED$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method8_DELTA_LESS_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method8_DELTA_LESS_DISADVANTAGED$zipcode)


vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_DELTA_LESS_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*Date  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_DELTA_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*Date +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_DELTA_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


###############################################################################
################################### OMICRON WAVE  #############################
###############################################################################

ICU_UPDATED_Method8_OMICRON_LESS_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_Method8_OMICRON_LESS_DISADVANTAGED$Date = as.factor(ICU_UPDATED_Method8_OMICRON_LESS_DISADVANTAGED$Date)

ICU_UPDATED_Method8_OMICRON_LESS_DISADVANTAGED$Date <- relevel(ICU_UPDATED_Method8_OMICRON_LESS_DISADVANTAGED$Date, ref = "2021-12")

skewness_ICU_rate = skewness(ICU_UPDATED_Method8_OMICRON_LESS_DISADVANTAGED$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method8_OMICRON_LESS_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method8_OMICRON_LESS_DISADVANTAGED$zipcode)


vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_OMICRON_LESS_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*Date +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method8_OMICRON_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*Date  +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method8_OMICRON_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


################################################################################
################################ More DISADVANTAGED  ###########################
################################### WHOLE DATA SET #############################
################################################################################

ICU_UPDATED_Method8_MORE_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method8_MORE_DISADVANTAGED$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method8_MORE_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method8_MORE_DISADVANTAGED$zipcode)


vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_MORE_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)




################################################################################
################################ More DISADVANTAGED  ###########################
################################### ALPHA WAVE #################################
################################################################################

ICU_UPDATED_Method8_ALPHA_MORE_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_Method8_ALPHA_MORE_DISADVANTAGED$Date = as.factor(ICU_UPDATED_Method8_ALPHA_MORE_DISADVANTAGED$Date)

ICU_UPDATED_Method8_ALPHA_MORE_DISADVANTAGED$Date <- relevel(ICU_UPDATED_Method8_ALPHA_MORE_DISADVANTAGED$Date, ref = "2021-03")

skewness_ICU_rate = skewness(ICU_UPDATED_Method8_ALPHA_MORE_DISADVANTAGED$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method8_ALPHA_MORE_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method8_ALPHA_MORE_DISADVANTAGED$zipcode)


vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_ALPHA_MORE_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*Date  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_ALPHA_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*Date +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_ALPHA_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)

###############################################################################
################################### DELTA WAVE ################################
###############################################################################

ICU_UPDATED_Method8_DELTA_MORE_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_Method8_DELTA_MORE_DISADVANTAGED$Date = as.factor(ICU_UPDATED_Method8_DELTA_MORE_DISADVANTAGED$Date)

ICU_UPDATED_Method8_DELTA_MORE_DISADVANTAGED$Date <- relevel(ICU_UPDATED_Method8_DELTA_MORE_DISADVANTAGED$Date, ref = "2021-08")

skewness_ICU_rate = skewness(ICU_UPDATED_Method8_DELTA_MORE_DISADVANTAGED$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method8_DELTA_MORE_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method8_DELTA_MORE_DISADVANTAGED$zipcode)


vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_DELTA_MORE_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*Date  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_DELTA_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*Date  +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method8_DELTA_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


###############################################################################
################################### OMICRON WAVE ##############################
###############################################################################

ICU_UPDATED_Method8_OMICRON_MORE_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_Method8_OMICRON_MORE_DISADVANTAGED$Date = as.factor(ICU_UPDATED_Method8_OMICRON_MORE_DISADVANTAGED$Date)

ICU_UPDATED_Method8_OMICRON_MORE_DISADVANTAGED$Date <- relevel(ICU_UPDATED_Method8_OMICRON_MORE_DISADVANTAGED$Date, ref = "2021-12")

skewness_ICU_rate = skewness(ICU_UPDATED_Method8_OMICRON_MORE_DISADVANTAGED$COVID_ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method8_OMICRON_MORE_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method8_OMICRON_MORE_DISADVANTAGED$zipcode)


vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_OMICRON_MORE_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters`*Date +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method8_OMICRON_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center`*Date +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method8_OMICRON_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


# ###############################################################################
# ####################################### WITHOUT NA ############################
# ################################### LESS DISADVANTAGED ########################
# ################################### ALPHA WAVE ################################
# ###############################################################################
# 
# ICU_UPDATED_Method8_WithOutNA_ALPHA_LESS_DISADVANTAGED = fread(file.choose(), sep=",", header=T)
# 
# skewness_ICU_rate = skewness(ICU_UPDATED_Method8_WithOutNA_ALPHA_LESS_DISADVANTAGED$COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# ICU_UPDATED_Method7_WithOutNA_ALPHA_LESS_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method7_WithOutNA_ALPHA_LESS_DISADVANTAGED$zipcode)
# 
# 
# vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_WithOutNA_ALPHA_LESS_DISADVANTAGED)
# vif(vif_model)
# 
# ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_ALPHA_LESS_DISADVANTAGED)
# 
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_ALPHA_LESS_DISADVANTAGED)
# 
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)
# 
# ###############################################################################
# ################################ DELTA WAVE ###################################
# ###############################################################################
# 
# ICU_UPDATED_Method8_WithOutNA_DELTA_Less_DISADVANTAGED = fread(file.choose(), sep=",", header=T)
# 
# skewness_ICU_rate = skewness(ICU_UPDATED_Method8_WithOutNA_DELTA_Less_DISADVANTAGED$COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# ICU_UPDATED_Method8_WithOutNA_DELTA_Less_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method8_WithOutNA_DELTA_Less_DISADVANTAGED$zipcode)
# 
# 
# vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_WithOutNA_DELTA_Less_DISADVANTAGED)
# vif(vif_model)
# 
# ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_DELTA_Less_DISADVANTAGED)
# 
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_DELTA_Less_DISADVANTAGED)
# 
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)
# 
# 
# ###############################################################################
# ################################ OMICRON WAVE #################################
# ###############################################################################
# 
# ICU_UPDATED_Method8_WithOutNA_OMICRON_Less_DISADVANTAGED = fread(file.choose(), sep=",", header=T)
# 
# skewness_ICU_rate = skewness(ICU_UPDATED_Method8_WithOutNA_OMICRON_Less_DISADVANTAGED$COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# ICU_UPDATED_Method8_WithOutNA_OMICRON_Less_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method8_WithOutNA_OMICRON_Less_DISADVANTAGED$zipcode)
# 
# 
# vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_WithOutNA_OMICRON_Less_DISADVANTAGED)
# vif(vif_model)
# 
# ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = ICU_UPDATED_Method8_WithOutNA_OMICRON_Less_DISADVANTAGED)
# 
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = ICU_UPDATED_Method8_WithOutNA_OMICRON_Less_DISADVANTAGED)
# 
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)
# 
# ###############################################################################
# ####################################### WITHOUT NA ############################
# ################################### MORE DISADVANTAGED ########################
# ################################### ALPHA WAVE ################################
# ###############################################################################
# 
# ICU_UPDATED_Method8_WithOutNA_ALPHA_MORE_DISADVANTAGED = fread(file.choose(), sep=",", header=T)
# 
# skewness_ICU_rate = skewness(ICU_UPDATED_Method8_WithOutNA_ALPHA_MORE_DISADVANTAGED$COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# ICU_UPDATED_Method8_WithOutNA_ALPHA_MORE_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method8_WithOutNA_ALPHA_MORE_DISADVANTAGED$zipcode)
# 
# 
# vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_WithOutNA_ALPHA_MORE_DISADVANTAGED)
# vif(vif_model)
# 
# ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_ALPHA_MORE_DISADVANTAGED)
# 
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_ALPHA_MORE_DISADVANTAGED)
# 
# 
# tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)
# 
# 
# ###############################################################################
# ################################ DELTA WAVE  ##################################
# ###############################################################################
# 
# ICU_UPDATED_Method8_WithOutNA_DELTA_MORE_DISADVANTAGED = fread(file.choose(), sep=",", header=T)
# 
# skewness_ICU_rate = skewness(ICU_UPDATED_Method8_WithOutNA_DELTA_MORE_DISADVANTAGED$COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# ICU_UPDATED_Method8_WithOutNA_DELTA_MORE_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method8_WithOutNA_DELTA_MORE_DISADVANTAGED$zipcode)
# 
# 
# vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_WithOutNA_DELTA_MORE_DISADVANTAGED)
# vif(vif_model)
# 
# ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_DELTA_MORE_DISADVANTAGED)
# 
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                      (1 | zipcode), 
#                                    data = ICU_UPDATED_Method8_WithOutNA_DELTA_MORE_DISADVANTAGED)
# 
# 
# tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)
# 
# 
# ###############################################################################
# ################################ OMICRON WAVE  ################################
# ###############################################################################
# 
# 
# ICU_UPDATED_Method8_WithOutNA_OMICRON_More_DISADVANTAGED = fread(file.choose(), sep=",", header=T)
# 
# skewness_ICU_rate = skewness(ICU_UPDATED_Method8_WithOutNA_OMICRON_More_DISADVANTAGED$COVID_ICU_Rate , type = 3)
# print(skewness_ICU_rate)
# 
# ICU_UPDATED_Method8_WithOutNA_OMICRON_More_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method8_WithOutNA_OMICRON_More_DISADVANTAGED$zipcode)
# 
# 
# vif_model <- lm(`COVID_ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method8_WithOutNA_OMICRON_More_DISADVANTAGED)
# vif(vif_model)
# 
# ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = ICU_UPDATED_Method8_WithOutNA_OMICRON_More_DISADVANTAGED)
# 
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)
# 
# 
# ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`COVID_ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
#                                        (1 | zipcode), 
#                                      data = ICU_UPDATED_Method8_WithOutNA_OMICRON_More_DISADVANTAGED)
# 
# 
# tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)
