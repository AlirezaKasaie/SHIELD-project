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


#********************************Checking for Multicollinearity ****************

ICU_UPDATED = read.csv(file.choose(), sep=",", header=T)

count_per_zip_ICU = ICU_UPDATED%>%
  group_by(zipcode) %>%
  summarise(total.ICU.admission.per.zipcode = n())

count_per_zip_ICU = count_per_zip_ICU[order(count_per_zip_ICU$total.ICU.admission.per.zipcode, decreasing = TRUE), ]

count_per_zip_ICU = count_per_zip_ICU %>% filter(total.ICU.admission.per.zipcode > 10)

ICU_UPDATED_2_NEW = ICU_UPDATED %>%
  filter(zipcode %in% count_per_zip_ICU$zipcode)

ICU_UPDATED_2_NEW = ICU_UPDATED_2_NEW %>%
  select(-c("dob","st","city","death_date","adm_date_time","disch_date_time","los","last_icu_dt","discharge_disposition":"DX_NAME"))

ICU_UPDATED_2_NEW$first_icu_dt = as.Date(ICU_UPDATED_2_NEW$first_icu_dt, format = "%Y-%m-%d")
ICU_UPDATED_2_NEW$Date = format(ICU_UPDATED_2_NEW$first_icu_dt, "%Y-%m")

ICU_UPDATED_2_NEW$first_icu_dt = NULL


ICU_UPDATED_2_NEW = ICU_UPDATED_2_NEW %>%
  group_by(zipcode,Date) %>%
  mutate(Total_ICU_admission_per_zipcode_per_month = n()) %>%
  ungroup()

ICU_UPDATED_2_NEW = ICU_UPDATED_2_NEW %>%
  group_by(Date) %>%
  mutate(Total_ICU_admission_per_month = n()) %>%
  ungroup()

write.csv(ICU_UPDATED_2_NEW, file = "C:/Users/skasaiesharifi/Documents/ICU_UPDATED_2_NEW.csv", row.names = FALSE)

ICU_UPDATED_2_NEW = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_2_NEW$race = factor(ICU_UPDATED_2_NEW$race)
ICU_UPDATED_2_NEW$gender = factor(ICU_UPDATED_2_NEW$gender)
ICU_UPDATED_2_NEW$ethnic = factor(ICU_UPDATED_2_NEW$ethnic)
ICU_UPDATED_2_NEW$financial_class = factor(ICU_UPDATED_2_NEW$financial_class)

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



#*******************************Multivariable modeling *************************

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

ICU_UPDATED_FINAL_2 = read.csv(file.choose(), sep=",", header=T)

ICU_UPDATED_FINAL_2$Date = as.factor(ICU_UPDATED_FINAL_2$Date)

ICU_UPDATED_FINAL_2$Date = relevel(ICU_UPDATED_FINAL_2$Date, ref = "Jan-21")


#Checking the skewness of the ICU rate. Type specifies the algorithm used to calculate skewness. 
#Type=3 is the default and corresponds to the adjusted Fisher-Pearson standardized moment coefficient, which is commonly used.
skewness_ICU_rate = skewness(ICU_UPDATED_FINAL_2$`ICU Hospitalization Rate`, type = 3)
print(skewness_ICU_rate)


ICU_rate_GLMMmodel_1 = lmer(log(ICU.Hospitalization.Rate  + 1) ~ ADI_STATERNK + Total_SHIELD_centers_per_zipcode_per_month + 
                          + ADI_STATERNK*Total_SHIELD_centers_per_zipcode_per_month + (1|zipcode) + (1|Date) , data = ICU_UPDATED_FINAL_2)

tab_model(ICU_rate_GLMMmodel_1)

ICU_rate_GLMMmodel_2 = lmer(log(`ICU Hospitalization Rate` + 1) ~ Total_SHIELD_centers_per_zipcode_per_month + (1|zipcode) , data = ICU_UPDATED_FINAL_2)

tab_model(ICU_rate_GLMMmodel_2)

ICU_rate_GLMMmodel_3 = lmer(log(`ICU Hospitalization Rate` + 1) ~ ADI_STATERNK + (1|zipcode) , data = ICU_UPDATED_FINAL_2)

tab_model(ICU_rate_GLMMmodel_3)


ICU_Data_Alpha = read.csv(file.choose(), sep=",", header=T)

skewness_ICU_rate = skewness(ICU_Data_Alpha$ICU.Hospitalization.Rate, type = 3)
print(skewness_ICU_rate)

ICU_rate_Alpha_1 = lmer(log(ICU.Hospitalization.Rate + 1) ~  ADI_STATERNK + Total_SHIELD_centers_per_zipcode_per_month + 
                              + ADI_STATERNK*Total_SHIELD_centers_per_zipcode_per_month + (1|zipcode) + (1|Date) , data = ICU_Data_Alpha)

tab_model(ICU_rate_Alpha_1)

ICU_rate_Alpha_2 = lmer(log(ICU.Hospitalization.Rate + 1) ~ Total_SHIELD_centers_per_zipcode_per_month + (1|zipcode) , data = ICU_Data_Alpha)

tab_model(ICU_rate_Alpha_2)

ICU_rate_Alpha_3 = lmer(log(ICU.Hospitalization.Rate + 1) ~ ADI_STATERNK + (1|zipcode) , data = ICU_Data_Alpha)

tab_model(ICU_rate_Alpha_3)


skewness_ICU_los = skewness(ICU_UPDATED_FINAL_2$avg_icu_los, type = 3)
print(skewness_ICU_los)

ICU_los_GLMMmodel = lmer(log(avg_icu_los + 1) ~ ADI_STATERNK + Total_SHIELD_centers_per_zipcode_per_month + 
                            + ADI_STATERNK*Total_SHIELD_centers_per_zipcode_per_month + (1|zipcode) + (1|Date) , data = ICU_Data_Alpha)


tab_model(ICU_los_GLMMmodel)

ICU_los_Alpha_1 = lmer(log(avg_icu_los + 1) ~  ADI_STATERNK + Total_SHIELD_centers_per_zipcode_per_month + 
                          + ADI_STATERNK*Total_SHIELD_centers_per_zipcode_per_month + (1|zipcode) + (1|Date) , data = ICU_Data_Alpha)

tab_model(ICU_los_Alpha_1)


ICU_los_Alpha_2 = lmer(log(avg_icu_los + 1) ~ Total_SHIELD_centers_per_zipcode_per_month + (1|zipcode) , data = ICU_Data_Alpha)

tab_model(ICU_los_Alpha_2)


ICU_los_Alpha_3 = lmer(log(avg_icu_los + 1) ~ ADI_STATERNK + (1|zipcode) , data = ICU_Data_Alpha)

tab_model(ICU_los_Alpha_3)


ICU_admission_GLMMmodel = glmer(Total_ICU_admission_per_zipcode_per_month ~ Date + gender + race + ethnic + age + financial_class
                          + ADI + Total_SHIELD_centers_per_zipcode_per_month + ADI*race + ADI*ethnic + 
                            ADI*Total_SHIELD_centers_per_zipcode_per_month 
                          + Date*Total_SHIELD_centers_per_zipcode_per_month + (1|zipcode) + (1|mrn), 
                          data = ICU_UPDATED_FINAL, family = poisson, nAGQ=0)

tab_model(ICU_admission_GLMMmodel)



Correlation_Result = cor(ICU_UPDATED_FINAL$Total_ICU_admission_per_zipcode_per_month , ICU_UPDATED_FINAL$Total_SHIELD_centers_per_zipcode_per_month, method = "pearson", use = "complete.obs")
Correlation_Result

Correlation_Result = cor(ICU_UPDATED_FINAL$ICU_admission_Rate , ICU_UPDATED_FINAL$Total_SHIELD_centers_per_zipcode_per_month, method = "pearson", use = "complete.obs")
Correlation_Result

Correlation_Result = cor(ICU_UPDATED_FINAL$icu_los , ICU_UPDATED_FINAL$Total_SHIELD_centers_per_zipcode_per_month, method = "pearson", use = "complete.obs")
Correlation_Result


ggplot(filtered_ICU_UPDATED_2, aes(x = Number_of_SHIELD_centers , y = total.ICU.admission.per.month )) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Correlation between Monthly Number of Loyola ICU admission and Number of SHIELD centers",
       x = "Number of SHIELD centers",
       y = "Number of Loyola ICU admission") +
  theme_minimal()+
  annotate("text", x = Inf, y = Inf, label = sprintf("r = %.2f", Correlation_Result), 
           hjust = 1.1, vjust = 1.1, size = 8, color = "red")






#*************************************************************************************************************************

death_GLMMmodel = glmer(total_death ~ total.tests.per.month+Cumulative.Tests+Education..9th.grade.+Education..at.least.HS.diploma.+Education..at.least.Bachelor.s.degree.+
                          Population.Uninsured+Population..Insured+SHIELDS+Other+ADI+Age..0.17.+Age..18.64.+Age..65..+Sex..Male.+Race..Other.+Race..Black.African.American.+
                          Race..White.+Ethnicity..Hispanic.+Ethnicity..Non.Hispanic.+(1 | zipcode),
                        data = Death_Data,
                        family = poisson,
                        nAGQ=0)
tab_model(death_GLMMmodel)

Test_Centers = glmer(total_death ~ SHIELDS+Other+(1|zipcode),
             data=Death_Data,
             family=poisson,
             nAGQ=0)
tab_model(Test_Centers)

tests_rate = glmer(total_death ~ Vaccination.Rate +(1|zipcode),
                   data=Death_Data,
                   family=poisson,
                   nAGQ=0)
tab_model(tests_rate)

ADI = glmer(total_death ~ ADI +(1|zipcode),
                   data=Death_Data,
                   family=poisson,
                   nAGQ=0)
tab_model(ADI)


Death_Data$Cumulative.Tests = as.numeric(Death_Data$Cumulative.Tests)
Death_Data$Cumulative.Tests.Scaled = scale(Death_Data$Cumulative.Tests, center = TRUE, scale = TRUE)

cumulative_tests = glmer(total_death ~ Cumulative.Tests.Scaled  +(1|zipcode),
            data=Death_Data,
            family=poisson,
            nAGQ=0)
tab_model(cumulative_tests)

Death_Data$total.tests.per.month = as.numeric(Death_Data$total.tests.per.month)
Death_Data$total.tests.per.month.Scaled = scale(Death_Data$total.tests.per.month, center = TRUE, scale = TRUE)

total_tests_per_month = glmer(total_death ~ total.tests.per.month.Scaled  +(1|zipcode),
                         data=Death_Data,
                         family=poisson,
                         nAGQ=0)
tab_model(total_tests_per_month)


Death_Data$Education..9th.grade. = as.numeric(Death_Data$Education..9th.grade.)
Death_Data$Education..9th.grade.Scaled = scale(Death_Data$Education..9th.grade., center = TRUE, scale = TRUE)

Death_Data$Education..at.least.HS.diploma. = as.numeric(Death_Data$Education..at.least.HS.diploma.)
Death_Data$Education..at.least.HS.diploma.Scaled = scale(Death_Data$Education..at.least.HS.diploma., center = TRUE, scale = TRUE)

Death_Data$Education..at.least.Bachelor.s.degree. = as.numeric(Death_Data$Education..at.least.Bachelor.s.degree.)
Death_Data$Education..at.least.Bachelor.s.degree.Scaled = scale(Death_Data$Education..at.least.Bachelor.s.degree., center = TRUE, scale = TRUE)

Education = glmer(total_death ~ Education..9th.grade.Scaled+ Education..at.least.HS.diploma.Scaled+Education..at.least.Bachelor.s.degree.Scaled  +(1|zipcode),
                         data=Death_Data,
                         family=poisson,
                         nAGQ=0)
tab_model(Education)


Death_Data$Population.Uninsured = as.numeric(Death_Data$Population.Uninsured)
Death_Data$Population.Uninsured.Scaled = scale(Death_Data$Population.Uninsured, center = TRUE, scale = TRUE)

Death_Data$Population..Insured  = as.numeric(Death_Data$Population..Insured)
Death_Data$Population..Insured.Scaled = scale(Death_Data$Population..Insured, center = TRUE, scale = TRUE)

Insurance = glmer(total_death ~ Population..Insured.Scaled + Population.Uninsured.Scaled +(1|zipcode),
                  data=Death_Data,
                  family=poisson,
                  nAGQ=0)
tab_model(Insurance)

Death_Data$Age..0.17.  = as.numeric(Death_Data$Age..0.17. )
Death_Data$Age..0.17..Scaled = scale(Death_Data$Age..0.17. , center = TRUE, scale = TRUE)

Death_Data$ Age..18.64.  = as.numeric(Death_Data$ Age..18.64. )
Death_Data$ Age..18.64..Scaled = scale(Death_Data$ Age..18.64. , center = TRUE, scale = TRUE)

Death_Data$ Age..65..  = as.numeric(Death_Data$ Age..65.. )
Death_Data$ Age..65...Scaled = scale(Death_Data$ Age..65.. , center = TRUE, scale = TRUE)

Age = glmer(total_death ~ Age..0.17..Scaled  + Age..18.64..Scaled + Age..65...Scaled +(1|zipcode),
                  data=Death_Data,
                  family=poisson,
                  nAGQ=0)
tab_model(Age)


Death_Data$ Sex..Male.  = as.numeric(Death_Data$ Sex..Male. )
Death_Data$ Sex..Male.Scaled = scale(Death_Data$ Sex..Male. , center = TRUE, scale = TRUE)

Death_Data$ Sex..Female.  = as.numeric(Death_Data$ Sex..Female. )
Death_Data$ Sex..Female.Scaled = scale(Death_Data$ Sex..Female. , center = TRUE, scale = TRUE)

Gender = glmer(total_death ~ Sex..Female.Scaled  + Sex..Male.Scaled +(1|zipcode),
            data=Death_Data,
            family=poisson,
            nAGQ=0)
tab_model(Gender)


Death_Data$ Race..Black.African.American. = as.numeric(Death_Data$ Race..Black.African.American. )
Death_Data$ Race..Black.African.American..Scaled = scale(Death_Data$ Race..Black.African.American. , center = TRUE, scale = TRUE)

Death_Data$ Race..White.  = as.numeric(Death_Data$ Race..White.  )
Death_Data$ Race..White..Scaled = scale(Death_Data$ Race..White. , center = TRUE, scale = TRUE)

Death_Data$ Race..Other.  = as.numeric(Death_Data$ Race..Other.  )
Death_Data$ Race..Other..Scaled = scale(Death_Data$ Race..Other. , center = TRUE, scale = TRUE)

Race = glmer(total_death ~ Race..Black.African.American..Scaled  + Race..White..Scaled + Race..Other..Scaled +(1|zipcode),
               data=Death_Data,
               family=poisson,
               nAGQ=0)
tab_model(Race)

Death_Data$ Ethnicity..Hispanic.  = as.numeric(Death_Data$ Ethnicity..Hispanic.  )
Death_Data$ Ethnicity..Hispanic..Scaled = scale(Death_Data$ Ethnicity..Hispanic. , center = TRUE, scale = TRUE)

Death_Data$ Ethnicity..Non.Hispanic.  = as.numeric(Death_Data$ Ethnicity..Non.Hispanic.  )
Death_Data$ Ethnicity..Non.Hispanic..Scaled = scale(Death_Data$ Ethnicity..Non.Hispanic. , center = TRUE, scale = TRUE)

Ethnicity = glmer(total_death ~ Ethnicity..Non.Hispanic..Scaled  + Ethnicity..Hispanic..Scaled +(1|zipcode),
             data=Death_Data,
             family=poisson,
             nAGQ=0)
tab_model(Ethnicity)


SHIELD = glmer(Death.Rate ~ SHIELDS +(1|zipcode),
            data=Death_Data,
            family=poisson,
            nAGQ=0)
tab_model(SHIELD)

tests_rate = glmer(Death.Rate ~ Vaccination.Rate  +(1|zipcode),
               data=Death_Data,
               family=poisson,
               nAGQ=0)
tab_model(tests_rate)

cumulative_tests = glmer(Death.Rate ~ Cumulative.Tests  +(1|zipcode),
                   data=Death_Data,
                   family=poisson,
                   nAGQ=0)
tab_model(cumulative_tests)

ADI = glmer(Death.Rate ~ ADI  +(1|zipcode),
                         data=Death_Data,
                         family=poisson,
                         nAGQ=0)
tab_model(ADI)

Education = glmer(Death.Rate ~ Education..9th.grade.Scaled+ Education..at.least.HS.diploma.Scaled+Education..at.least.Bachelor.s.degree.Scaled   +(1|zipcode),
            data=Death_Data,
            family=poisson,
            nAGQ=0)
tab_model(Education)

Insurance = glmer(Death.Rate ~ Population..Insured.Scaled + Population.Uninsured.Scaled +(1|zipcode),
                  data=Death_Data,
                  family=poisson,
                  nAGQ=0)
tab_model(Insurance)

Age = glmer(Death.Rate ~ Age..0.17..Scaled  + Age..18.64..Scaled + Age..65...Scaled +(1|zipcode),
            data=Death_Data,
            family=poisson,
            nAGQ=0)
tab_model(Age)

Gender = glmer(Death.Rate ~ Sex..Female.Scaled  + Sex..Male.Scaled +(1|zipcode),
               data=Death_Data,
               family=poisson,
               nAGQ=0)
tab_model(Gender)

Race = glmer(Death.Rate ~ Race..Black.African.American..Scaled  + Race..White..Scaled + Race..Other..Scaled +(1|zipcode),
             data=Death_Data,
             family=poisson,
             nAGQ=0)
tab_model(Race)

Ethnicity = glmer(Death.Rate ~ Ethnicity..Non.Hispanic..Scaled  + Ethnicity..Hispanic..Scaled +(1|zipcode),
                  data=Death_Data,
                  family=poisson,
                  nAGQ=0)
tab_model(Ethnicity)


Correlation_Result = cor(Death_Data$total_death , Death_Data$SHIELDS, method = "pearson", use = "complete.obs")
Correlation_Result

ggplot(Death_Data, aes(x = SHIELDS , y = total_death )) +
  geom_point(alpha = 0.5) +  # Adjust point transparency with alpha
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Add a linear regression line
  labs(title = "Correlation between Total Number of Deaths per Month and Number of SHIELD centers",
       x = "Number of SHIELD centers",
       y = "Total Number of Deaths") +
  theme_minimal()+
  annotate("text", x = Inf, y = Inf, label = sprintf("r = %.2f", Correlation_Result), 
           hjust = 1.1, vjust = 1.1, size = 8, color = "red")


Correlation_Result = cor(Death_Data$Death.Rate , Death_Data$SHIELDS, method = "pearson", use = "complete.obs")
Correlation_Result

ggplot(Death_Data, aes(x = SHIELDS , y = Death.Rate )) +
  geom_point(alpha = 0.5) +  # Adjust point transparency with alpha
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Add a linear regression line
  labs(title = "Correlation between Monthly Death Rate and Number of SHIELD centers",
       x = "Number of SHIELD centers",
       y = "Death Rate per 1000 Population") +
  theme_minimal()+
  annotate("text", x = Inf, y = Inf, label = sprintf("r = %.2f", Correlation_Result), 
           hjust = 1.1, vjust = 1.1, size = 8, color = "red")


