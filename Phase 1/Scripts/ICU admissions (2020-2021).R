if(!require(mice)) install.packages("mice")

library('dplyr')
library('ggplot2')
library('tidyverse')
library('lubridate')
library("mice")

ICU = read.csv(file.choose(), sep=",", header=T)


str(ICU)
colnames(ICU)
sum(is.na(ICU))

ICU = ICU %>%
  mutate(Date = as.Date(mdy_hms(last_icu_dt)),
         Time = format(mdy_hms(last_icu_dt), "%H:%M:%S"))

ICU = ICU[ , -43]

ICU = ICU %>%
  relocate(42, .after = 16)

names(ICU)[names(ICU) == "Date"] = "Last_ICU_Date"


imputed_data = mice(ICU, m=5, method='pmm', seed=500)


