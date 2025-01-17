library('dplyr')
library('ggplot2')
library('tidyverse')
library('lubridate')
install.packages("here")
library('here')
library('data.table')

covid_specimen <- fread(file.choose(), sep=",", header=T)

covid_specimen$'DATE - COLLECTED' <- as.POSIXct(covid_specimen$'DATE - COLLECTED', format = "%m/%d/%Y %I:%M:%S %p")
covid_specimen$SpecimenBarcode <- as.factor(covid_specimen$'SPECIMEN BARCODE')
covid_specimen$location <- as.factor(covid_specimen$'LOCATION')
covid_specimen$Result <- as.factor(covid_specimen$'RESULT (P/Other)')
covid_specimen$Date <- as.Date(covid_specimen$'DATE - COLLECTED')
covid_specimen$SiteSHIELDID <- as.factor(covid_specimen$'SiteSHIELDID')


sample <- covid_specimen[, c("Zip", "location", "SPECIMEN BARCODE", "RESULT", "Date", "SiteSHIELDID")]
sample <- sample %>% rename(zipcode = 'Zip')

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


# One Month Lag

alpha <- filtered_sample %>% filter(as.Date(Date, format = "%m/%d/%Y") >= as.Date("2021-03-01") &
                                      as.Date(Date, format = "%m/%d/%Y") <= as.Date("2021-07-31"))

b <- alpha %>% mutate(Month = format(Date, "%Y-%m"))
b$Date <- NULL

monthly_alpha <- b %>% group_by(zipcode, Month) %>%
  summarise(SamplesCollected = n_distinct(SpecimenBarcode),
            TestCenters = n_distinct(location))


write.csv(monthly_alpha, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/Alpha-OneMonthLag.csv", row.names = FALSE)


# Two Month Lag

alpha_TwoMonthsLag <- filtered_sample %>% filter(as.Date(Date, format = "%m/%d/%Y") >= as.Date("2021-03-01") &
                                                   as.Date(Date, format = "%m/%d/%Y") <= as.Date("2021-08-31"))

b <- alpha_TwoMonthsLag %>% mutate(Month = format(Date, "%Y-%m"))
b$Date <- NULL

monthly_alpha_TwoMonthsLag <- b %>% group_by(zipcode, Month) %>%
  summarise(SamplesCollected = n_distinct(SpecimenBarcode),
            TestCenters = n_distinct(location))


write.csv(monthly_alpha_TwoMonthsLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/Alpha-TwoMonthsLag.csv", row.names = FALSE)


############################## Delta Wave #####################################


# One Month Lag

delta <- filtered_sample %>% filter(as.Date(Date, format = "%m/%d/%Y") >= as.Date("2021-08-01") &
                                      as.Date(Date, format = "%m/%d/%Y") <= as.Date("2022-12-31"))

d <- delta %>% mutate(Month = format(Date, "%Y-%m"))
d$Date <- NULL

monthly_delta_OneMonthLag <- d %>% group_by(zipcode, Month) %>%
  summarise(SamplesCollected = n_distinct(SpecimenBarcode),
            TestCenters = n_distinct(location))

write.csv(monthly_delta_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/Delta-OneMonthLag.csv", row.names = FALSE)

# Two Month Lag

delta <- filtered_sample %>% filter(as.Date(Date, format = "%m/%d/%Y") >= as.Date("2021-08-01") &
                                      as.Date(Date, format = "%m/%d/%Y") <= as.Date("2022-01-31"))

b <- delta %>% mutate(Month = format(Date, "%Y-%m"))
b$Date <- NULL

monthly_delta_TwoMonthsLag <- b %>% group_by(zipcode, Month) %>%
  summarise(SamplesCollected = n_distinct(SpecimenBarcode),
            TestCenters = n_distinct(location))

write.csv(monthly_delta_TwoMonthsLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/Delta-TwoMonthsLag.csv", row.names = FALSE)


############################# Omicron Wave #####################################


# One Month Lag

omicron <- filtered_sample %>% filter(as.Date(Date, format = "%m/%d/%Y") >= as.Date("2021-12-01") &
                                        as.Date(Date, format = "%m/%d/%Y") <= as.Date("2022-04-30"))

d <- omicron %>% mutate(Month = format(Date, "%Y-%m"))
d$Date <- NULL

monthly_omicron_OneMonthLag <- d %>% group_by(zipcode, Month) %>%
  summarise(SamplesCollected = n_distinct(SpecimenBarcode),
            TestCenters = n_distinct(location))

write.csv(monthly_omicron_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/Omicron-OneMonthLag.csv", row.names = FALSE)

# Two Month Lag

omicron <- filtered_sample %>% filter(as.Date(Date, format = "%m/%d/%Y") >= as.Date("2021-12-01") &
                                        as.Date(Date, format = "%m/%d/%Y") <= as.Date("2022-05-31"))

b <- omicron %>% mutate(Month = format(Date, "%Y-%m"))
b$Date <- NULL

monthly_omicron_TwoMonthsLag <- b %>% group_by(zipcode, Month) %>%
  summarise(SamplesCollected = n_distinct(SpecimenBarcode),
            TestCenters = n_distinct(location))

write.csv(monthly_omicron_TwoMonthsLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/Omicron-TwoMonthsLag.csv", row.names = FALSE)

################################################################################


zipcodes_to_remove <- c("60141", "60479","60549","60519","60602","60910","60945","61317","61337","61338","61367","62022") 

monthly_alpha <- monthly_alpha %>%
  filter(!zipcode %in% zipcodes_to_remove)

monthly_alpha_TwoMonthsLag <- monthly_alpha_TwoMonthsLag %>%
  filter(!zipcode %in% zipcodes_to_remove)

monthly_delta_OneMonthLag <- monthly_delta_OneMonthLag %>%
  filter(!zipcode %in% zipcodes_to_remove)

monthly_delta_TwoMonthsLag <- monthly_delta_TwoMonthsLag %>%
  filter(!zipcode %in% zipcodes_to_remove)

monthly_omicron_OneMonthLag <- monthly_omicron_OneMonthLag %>%
  filter(!zipcode %in% zipcodes_to_remove)

monthly_omicron_TwoMonthsLag <- monthly_omicron_TwoMonthsLag %>%
  filter(!zipcode %in% zipcodes_to_remove)

###############################################################################

# Alpha One Month Lag (with NA)

Alpha_Zipcode <- fread(file.choose(), sep=",", header=T)

monthly_alpha_OneMonthLag <- fread(file.choose(), sep=",", header=T)

Alpha_Zipcode$zipcode = as.character(Alpha_Zipcode$zipcode)

monthly_alpha_OneMonthLag$zipcode = as.character(monthly_alpha_OneMonthLag$zipcode)

monthly_alpha_OneMonthLag <- monthly_alpha_OneMonthLag %>% rename(Date = 'Month')

ICU_Alpha_OneMonthLag = full_join(Alpha_Zipcode, monthly_alpha_OneMonthLag, by = c("zipcode","Date"))

write.csv(ICU_Alpha_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/ICU_UPDATED_ALPHA_OneMonthLag.csv", row.names = FALSE)


# Alpha One Month Lag (without NA)

ICU_Alpha_OneMonthLag_without_NA = merge(Alpha_Zipcode, monthly_alpha_OneMonthLag, by = c("zipcode","Date"))

write.csv(ICU_Alpha_OneMonthLag_without_NA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/ICU_UPDATED_ALPHA_OneMonthLag_without_NA.csv", row.names = FALSE)


# Alpha Two Months Lag (with NA)

Alpha_Zipcode_TwoMonths <- fread(file.choose(), sep=",", header=T)

monthly_alpha_TwoMonthsLag <- fread(file.choose(), sep=",", header=T)

Alpha_Zipcode_TwoMonths$zipcode = as.character(Alpha_Zipcode_TwoMonths$zipcode)

monthly_alpha_TwoMonthsLag$zipcode = as.character(monthly_alpha_TwoMonthsLag$zipcode)

monthly_alpha_TwoMonthsLag <- monthly_alpha_TwoMonthsLag %>% rename(Date = 'Month')

ICU_Alpha_TwoMonthsLag = full_join(Alpha_Zipcode_TwoMonths, monthly_alpha_TwoMonthsLag, by = c("zipcode","Date"))

write.csv(ICU_Alpha_TwoMonthsLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/ICU_UPDATED_ALPHA_TwoMonthLag_with_NA.csv", row.names = FALSE)


# Alpha Two Month Lag (without NA)

ICU_Alpha_TwoMonthsLag_without_NA = merge(Alpha_Zipcode_TwoMonths, monthly_alpha_TwoMonthsLag, by = c("zipcode","Date"))

write.csv(ICU_Alpha_TwoMonthsLag_without_NA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/ICU_UPDATED_ALPHA_TwoMonthsLag_without_NA.csv", row.names = FALSE)


###############################################################################

# Delta One Month Lag (with NA)

Delta_Zipcode_OneMonth <- fread(file.choose(), sep=",", header=T)

monthly_delta_OneMonthLag <- fread(file.choose(), sep=",", header=T)

Delta_Zipcode_OneMonth$zipcode = as.character(Delta_Zipcode_OneMonth$zipcode)

monthly_delta_OneMonthLag$zipcode = as.character(monthly_delta_OneMonthLag$zipcode)

monthly_delta_OneMonthLag <- monthly_delta_OneMonthLag %>% rename(Date = 'Month')

ICU_Delta_OneMonthLag = full_join(Delta_Zipcode_OneMonth, monthly_delta_OneMonthLag, by = c("zipcode","Date"))

write.csv(ICU_Delta_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/ICU_UPDATED_DELTA_OneMonthLag_with_NA.csv", row.names = FALSE)


# Delta One Month Lag (without NA)

ICU_Delta_OneMonthLag_without_NA = merge(Delta_Zipcode_OneMonth, monthly_delta_OneMonthLag, by = c("zipcode","Date"))

write.csv(ICU_Delta_OneMonthLag_without_NA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/ICU_UPDATED_DELTA_OneMonthLag_without_NA.csv", row.names = FALSE)


# Delta Two Months Lag (with NA)

Delta_Zipcode_TwoMonths <- fread(file.choose(), sep=",", header=T)

monthly_delta_TwoMonthsLag <- fread(file.choose(), sep=",", header=T)

Delta_Zipcode_TwoMonths$zipcode = as.character(Delta_Zipcode_TwoMonths$zipcode)

monthly_delta_TwoMonthsLag$zipcode = as.character(monthly_delta_TwoMonthsLag$zipcode)

monthly_delta_TwoMonthsLag <- monthly_delta_TwoMonthsLag %>% rename(Date = 'Month')

ICU_Delta_TwoMonthsLag = full_join(Delta_Zipcode_TwoMonths, monthly_delta_TwoMonthsLag, by = c("zipcode","Date"))

write.csv(ICU_Delta_TwoMonthsLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/ICU_UPDATED_Delta_TwoMonthsLag_with_NA.csv", row.names = FALSE)


# Delta Two Months Lag (without NA)

ICU_Delta_TwoMonthsLag_without_NA = merge(Delta_Zipcode_TwoMonths, monthly_delta_TwoMonthsLag, by = c("zipcode","Date"))

write.csv(ICU_Delta_TwoMonthsLag_without_NA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/ICU_UPDATED_DELTA_TwoMonthsLag_without_NA.csv", row.names = FALSE)

###############################################################################

# Omicron One Month Lag (with NA)

Omicron_Zipcode_OneMonth <- fread(file.choose(), sep=",", header=T)

monthly_omicron_OneMonthLag <- fread(file.choose(), sep=",", header=T)

Omicron_Zipcode_OneMonth$zipcode = as.character(Omicron_Zipcode_OneMonth$zipcode)

monthly_omicron_OneMonthLag$zipcode = as.character(monthly_omicron_OneMonthLag$zipcode)

monthly_omicron_OneMonthLag <- monthly_omicron_OneMonthLag %>% rename(Date = 'Month')

ICU_Omicron_OneMonthLag = full_join(Omicron_Zipcode_OneMonth, monthly_omicron_OneMonthLag, by = c("zipcode","Date"))

write.csv(ICU_Omicron_OneMonthLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/ICU_UPDATED_OMICRON_OneMonthLag_with_NA.csv", row.names = FALSE)


# Omicron One Month Lag (without NA)

ICU_Omicron_OneMonthLag_without_NA = merge(Omicron_Zipcode_OneMonth, monthly_omicron_OneMonthLag, by = c("zipcode","Date"))

write.csv(ICU_Omicron_OneMonthLag_without_NA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/ICU_UPDATED_OMICRON_OneMonthLag_without_NA.csv", row.names = FALSE)


# Omicron Two Months Lag (with NA)

Omicron_Zipcode_TwoMonths <- fread(file.choose(), sep=",", header=T)

monthly_omicron_TwoMonthsLag <- fread(file.choose(), sep=",", header=T)

Omicron_Zipcode_TwoMonths$zipcode = as.character(Omicron_Zipcode_TwoMonths$zipcode)

monthly_omicron_TwoMonthsLag$zipcode = as.character(monthly_omicron_TwoMonthsLag$zipcode)

monthly_omicron_TwoMonthsLag <- monthly_omicron_TwoMonthsLag %>% rename(Date = 'Month')

ICU_Omicron_TwoMonthsLag = full_join(Omicron_Zipcode_TwoMonths, monthly_omicron_TwoMonthsLag, by = c("zipcode","Date"))

write.csv(ICU_Omicron_TwoMonthsLag, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/ICU_UPDATED_Omicron_TwoMonthsLag_with_NA.csv", row.names = FALSE)


# Omicron Two Months Lag (without NA)

ICU_Omicron_TwoMonthsLag_without_NA = merge(Omicron_Zipcode_TwoMonths, monthly_omicron_TwoMonthsLag, by = c("zipcode","Date"))

write.csv(ICU_Omicron_TwoMonthsLag_without_NA, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Lag/New/ICU_UPDATED_OMICRON_TwoMonthsLag_without_NA.csv", row.names = FALSE)
