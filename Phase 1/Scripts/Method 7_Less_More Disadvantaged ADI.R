###############################################################################
####################################### WITH NA ###############################
################################### LESS DISADVANTAGED ########################
################################### ALPHA WAVE ################################
###############################################################################

ICU_UPDATED_Method7_WithNA_ALPHA_LESS_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithNA_ALPHA_LESS_DISADVANTAGED$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithNA_ALPHA_LESS_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method7_WithNA_ALPHA_LESS_DISADVANTAGED$zipcode)


vif_model <- lm(`ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithNA_ALPHA_LESS_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_ALPHA_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_ALPHA_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)

###############################################################################
################################### DELTA WAVE  ###############################
###############################################################################

ICU_UPDATED_Method7_WithNA_DELTA_LESS_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithNA_DELTA_LESS_DISADVANTAGED$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithNA_DELTA_LESS_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method7_WithNA_DELTA_LESS_DISADVANTAGED$zipcode)


vif_model <- lm(`ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithNA_DELTA_LESS_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_DELTA_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_DELTA_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


###############################################################################
################################### OMICRON WAVE  #############################
###############################################################################

ICU_UPDATED_Method7_WithNA_OMICRON_LESS_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithNA_OMICRON_LESS_DISADVANTAGED$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithNA_OMICRON_LESS_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method7_WithNA_OMICRON_LESS_DISADVANTAGED$zipcode)


vif_model <- lm(`ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithNA_OMICRON_LESS_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_OMICRON_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_OMICRON_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


################################################################################
##################################### WITH NA ##################################
################################ More DISADVANTAGED  ###########################
################################### ALPHA WAVE #################################
################################################################################

ICU_UPDATED_Method7_WithNA_ALPHA_MORE_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithNA_ALPHA_MORE_DISADVANTAGED$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithNA_ALPHA_MORE_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method7_WithNA_ALPHA_MORE_DISADVANTAGED$zipcode)


vif_model <- lm(`ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithNA_ALPHA_MORE_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_ALPHA_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_ALPHA_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)

###############################################################################
################################### DELTA WAVE ################################
###############################################################################

ICU_UPDATED_Method7_WithNA_DELTA_MORE_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithNA_DELTA_MORE_DISADVANTAGED$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithNA_DELTA_MORE_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method7_WithNA_DELTA_MORE_DISADVANTAGED$zipcode)


vif_model <- lm(`ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithNA_DELTA_MORE_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_DELTA_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithNA_DELTA_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


###############################################################################
################################### OMICRON WAVE ##############################
###############################################################################

ICU_UPDATED_Method7_WithNA_OMICRON_MORE_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithNA_OMICRON_MORE_DISADVANTAGED$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithNA_OMICRON_MORE_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method7_WithNA_OMICRON_MORE_DISADVANTAGED$zipcode)


vif_model <- lm(`ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithNA_OMICRON_MORE_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method7_WithNA_OMICRON_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method7_WithNA_OMICRON_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)


###############################################################################
####################################### WITHOUT NA ############################
################################### LESS DISADVANTAGED ########################
################################### ALPHA WAVE ################################
###############################################################################

ICU_UPDATED_Method7_WithOutNA_ALPHA_LESS_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithOutNA_ALPHA_LESS_DISADVANTAGED$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithOutNA_ALPHA_LESS_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method7_WithOutNA_ALPHA_LESS_DISADVANTAGED$zipcode)


vif_model <- lm(`ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithOutNA_ALPHA_LESS_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_ALPHA_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_ALPHA_LESS_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)

###############################################################################
################################ DELTA WAVE ###################################
###############################################################################

ICU_UPDATED_Method7_WithOutNA_DELTA_Less_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithOutNA_DELTA_Less_DISADVANTAGED$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithOutNA_DELTA_Less_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method7_WithOutNA_DELTA_Less_DISADVANTAGED$zipcode)


vif_model <- lm(`ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithOutNA_DELTA_Less_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_DELTA_Less_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_DELTA_Less_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


###############################################################################
################################ OMICRON WAVE #################################
###############################################################################

ICU_UPDATED_Method7_WithOutNA_OMICRON_Less_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithOutNA_OMICRON_Less_DISADVANTAGED$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithOutNA_OMICRON_Less_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method7_WithOutNA_OMICRON_Less_DISADVANTAGED$zipcode)


vif_model <- lm(`ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithOutNA_OMICRON_Less_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_OMICRON_Less_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_OMICRON_Less_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)

###############################################################################
####################################### WITHOUT NA ############################
################################### MORE DISADVANTAGED ########################
################################### ALPHA WAVE ################################
###############################################################################

ICU_UPDATED_Method7_WithOutNA_ALPHA_MORE_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithOutNA_ALPHA_MORE_DISADVANTAGED$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithOutNA_ALPHA_MORE_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method7_WithOutNA_ALPHA_MORE_DISADVANTAGED$zipcode)


vif_model <- lm(`ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithOutNA_ALPHA_MORE_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_ALPHA_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_ALPHA_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_1, digits = 5)


ICU_rate_GLMMmodel_ALPHA_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_ALPHA_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_ALPHA_2, digits = 5)


###############################################################################
################################ DELTA WAVE  ##################################
###############################################################################

ICU_UPDATED_Method7_WithOutNA_DELTA_MORE_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithOutNA_DELTA_MORE_DISADVANTAGED$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithOutNA_DELTA_MORE_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method7_WithOutNA_DELTA_MORE_DISADVANTAGED$zipcode)


vif_model <- lm(`ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithOutNA_DELTA_MORE_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_DELTA_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_DELTA_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_DELTA_1, digits = 5)


ICU_rate_GLMMmodel_DELTA_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                     (1 | zipcode), 
                                   data = ICU_UPDATED_Method7_WithOutNA_DELTA_MORE_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_DELTA_2, digits = 5)


###############################################################################
################################ OMICRON WAVE  ################################
###############################################################################


ICU_UPDATED_Method7_WithOutNA_OMICRON_More_DISADVANTAGED = fread(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_UPDATED_Method7_WithOutNA_OMICRON_More_DISADVANTAGED$ICU_Rate , type = 3)
print(skewness_ICU_rate)

ICU_UPDATED_Method7_WithOutNA_OMICRON_More_DISADVANTAGED$zipcode = as.factor(ICU_UPDATED_Method7_WithOutNA_OMICRON_More_DISADVANTAGED$zipcode)


vif_model <- lm(`ICU_Rate` ~ `testcenters` + `Effective Number of Center`, data = ICU_UPDATED_Method7_WithOutNA_OMICRON_More_DISADVANTAGED)
vif(vif_model)

ICU_rate_GLMMmodel_OMICRON_1 <- lmer(log(`ICU_Rate` + 1) ~ `testcenters` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method7_WithOutNA_OMICRON_More_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_OMICRON_1, digits = 5)


ICU_rate_GLMMmodel_OMICRON_2 <- lmer(log(`ICU_Rate` + 1) ~ `Effective Number of Center` + offset(log(cpop)) +
                                       (1 | zipcode), 
                                     data = ICU_UPDATED_Method7_WithOutNA_OMICRON_More_DISADVANTAGED)


tab_model(ICU_rate_GLMMmodel_OMICRON_2, digits = 5)
