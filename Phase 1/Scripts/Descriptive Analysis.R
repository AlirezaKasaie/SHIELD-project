###############################################################################
############################# Descriptive Analysis ############################
################################### ALPHA WAVE ################################
###############################################################################

library(dplyr)
library(ggplot2)
library(summarytools)
library(data.table)

ICU_UPDATED_ALPHA <- fread(file.choose(), sep = ",", header = TRUE)

descriptive_stats_ALPHA <- ICU_UPDATED_ALPHA %>%
  summarize(
    total_patients_per_zipcode_per_month_mean = mean(total_patients_per_zipcode_per_month, na.rm = TRUE),
    total_patients_per_zipcode_per_month_sd = sd(total_patients_per_zipcode_per_month, na.rm = TRUE),
    total_patients_per_zipcode_mean = mean(total_patients_per_zipcode, na.rm = TRUE),
    total_patients_per_zipcode_sd = sd(total_patients_per_zipcode, na.rm = TRUE),
    COVID_ICU_Rate_mean = mean(COVID_ICU_Rate, na.rm = TRUE),
    COVID_ICU_Rate_sd = sd(COVID_ICU_Rate, na.rm = TRUE),
    Number_of_Test_Centers_mean = mean(testcenters, na.rm = TRUE),
    Number_of_Test_Centers_sd = sd(testcenters, na.rm = TRUE),
    Effective_Number_of_Test_Centers_mean = mean(`Effective Number of Center`, na.rm = TRUE),
    Effective_Number_of_Test_Centers_sd = sd(`Effective Number of Center`, na.rm = TRUE)
  )

print(descriptive_stats_ALPHA)
