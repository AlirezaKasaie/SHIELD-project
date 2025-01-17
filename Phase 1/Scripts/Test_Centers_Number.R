library('data.table')
library(e1071)
library(car)
library(lme4)
library(sjPlot)
library(dplyr)
library(tidyr)

#covid_specimen <- fread(file.choose(), sep=",", header=T)


covid_specimen <- fread(file.choose(), sep=",", header=T)

covid_specimen$'DATE - COLLECTED' <- as.POSIXct(covid_specimen$'DATE - COLLECTED', format = "%m/%d/%Y %I:%M:%S %p")
covid_specimen$SpecimenBarcode <- as.factor(covid_specimen$'SPECIMEN BARCODE')
covid_specimen$location <- as.factor(covid_specimen$'LOCATION')
covid_specimen$Result <- as.factor(covid_specimen$'RESULT (P/Other)')
covid_specimen$Date <- as.Date(covid_specimen$'DATE - COLLECTED')
covid_specimen$Site.SHIELD.Id <- as.factor(covid_specimen$'Site.SHIELD.Id')


sample <- covid_specimen[, c("[NAI]PATIENT ZIP", "LOCATION", "SPECIMEN BARCODE", "RESULT", "Date", "Site.SHIELD.Id")]
sample <- sample %>% rename(zipcode = '[NAI]PATIENT ZIP')

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


alpha <- filtered_sample %>% filter(as.Date(Date, format = "%m/%d/%Y") >= as.Date("2023-07-01") &
                                      as.Date(Date, format = "%m/%d/%Y") <= as.Date("2023-07-31"))

b <- alpha %>% mutate(Month = format(Date, "%Y-%m"))
b$Date <- NULL

# monthly_alpha <- b %>% group_by(zipcode, Month) %>%
#   summarise(SamplesCollected = n_distinct(`SPECIMEN BARCODE`),
#             TestCenters = n_distinct(location))

#write.csv(alpha, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Alpha/Alpha.csv", row.names = FALSE)

sum(is.na(alpha))

#Add a column for counting purposes
alpha <- alpha %>%
  mutate(Count = 1)

# Create a pivot table
alpha_new <- alpha %>%
  group_by(Site.SHIELD.Id, zipcode) %>%
  summarise(Count = sum(Count), .groups = 'drop') %>%
  spread(zipcode, Count, fill = 0)

# Calculate total for each column and add as a new row
total_row <- alpha_new %>%
  summarise(across(-Site.SHIELD.Id, sum)) %>%
  mutate(Site.SHIELD.Id = "Total")

alpha_new <- bind_rows(alpha_new, total_row)

# Calculate Total for each row and add it as a new column
alpha_new <- alpha_new %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()

# Create a new matrix by dividing each element by the total of each row
alpha_new2 <- alpha_new %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~ . / Total)) %>%
  ungroup()

# Remove the Total column for the final proportion matrix
alpha_new2 <- alpha_new2 %>%
  select(-Total)

# Calculate the proportion of test centers serving each Zipcode and add as a new row
proportion_row <- alpha_new2 %>%
  filter(Site.SHIELD.Id != "Total") %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(Site.SHIELD.Id = "Proportion of test centers serve the Zipcode")

# Bind the proportion row to the matrix
alpha_new2 <- bind_rows(alpha_new2, proportion_row)

write.csv(alpha_new2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Phase 2/Phase2/Data/Number of Effective Test Centers/2023/Jul 2023.csv", row.names = FALSE)


############################## Delta Wave #####################################


delta <- filtered_sample %>% filter(as.Date(Date, format = "%m/%d/%Y") >= as.Date("2023-01-01") &
                                      as.Date(Date, format = "%m/%d/%Y") <= as.Date("2023-01-31"))

b <- delta %>% mutate(Month = format(Date, "%Y-%m"))
b$Date <- NULL

# monthly_alpha <- b %>% group_by(zipcode, Month) %>%
#   summarise(SamplesCollected = n_distinct(`SPECIMEN BARCODE`),
#             TestCenters = n_distinct(location))

#write.csv(delta, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Delta/Delta.csv", row.names = FALSE)

sum(is.na(delta))

#Add a column for counting purposes
delta <- delta %>%
  mutate(Count = 1)

# Create a pivot table
delta_new <- delta %>%
  group_by(Site.SHIELD.Id, zipcode) %>%
  summarise(Count = sum(Count), .groups = 'drop') %>%
  spread(zipcode, Count, fill = 0)

# Calculate total for each column and add as a new row
total_row <- delta_new %>%
  summarise(across(-Site.SHIELD.Id, sum)) %>%
  mutate(Site.SHIELD.Id = "Total")

delta_new <- bind_rows(delta_new, total_row)

# Calculate Total for each row and add it as a new column
delta_new <- delta_new %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()

# Create a new matrix by dividing each element by the total of each row
delta_new2 <- delta_new %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~ . / Total)) %>%
  ungroup()

# Remove the Total column for the final proportion matrix
delta_new2 <- delta_new2 %>%
  select(-Total)

# Calculate the proportion of test centers serving each Zipcode and add as a new row
proportion_row <- delta_new2 %>%
  filter(Site.SHIELD.Id != "Total") %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(Site.SHIELD.Id = "Proportion of test centers serve the Zipcode")

# Bind the proportion row to the matrix
delta_new2 <- bind_rows(delta_new2, proportion_row)

write.csv(delta_new2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Test Centers Count/Delta/Delta-January.csv", row.names = FALSE)


############################## Omicron Wave #####################################


omicron <- filtered_sample %>% filter(as.Date(Date, format = "%m/%d/%Y") >= as.Date("2022-05-01") &
                                        as.Date(Date, format = "%m/%d/%Y") <= as.Date("2022-05-31"))

b <- omicron %>% mutate(Month = format(Date, "%Y-%m"))
b$Date <- NULL

# monthly_alpha <- b %>% group_by(zipcode, Month) %>%
#   summarise(SamplesCollected = n_distinct(`SPECIMEN BARCODE`),
#             TestCenters = n_distinct(location))

#write.csv(omicron, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 3/Omicron/Omicron.csv", row.names = FALSE)

sum(is.na(omicron))

#Add a column for counting purposes
omicron <- omicron %>%
  mutate(Count = 1)

# Create a pivot table
omicron_new <- omicron %>%
  group_by(Site.SHIELD.Id, zipcode) %>%
  summarise(Count = sum(Count), .groups = 'drop') %>%
  spread(zipcode, Count, fill = 0)

# Calculate total for each column and add as a new row
total_row <- omicron_new %>%
  summarise(across(-Site.SHIELD.Id, sum)) %>%
  mutate(Site.SHIELD.Id = "Total")

omicron_new <- bind_rows(omicron_new, total_row)

# Calculate Total for each row and add it as a new column
omicron_new <- omicron_new %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()

# Create a new matrix by dividing each element by the total of each row
omicron_new2 <- omicron_new %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~ . / Total)) %>%
  ungroup()

# Remove the Total column for the final proportion matrix
omicron_new2 <- omicron_new2 %>%
  select(-Total)

# Calculate the proportion of test centers serving each Zipcode and add as a new row
proportion_row <- omicron_new2 %>%
  filter(Site.SHIELD.Id != "Total") %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(Site.SHIELD.Id = "Proportion of test centers serve the Zipcode")

# Bind the proportion row to the matrix
omicron_new2 <- bind_rows(omicron_new2, proportion_row)

write.csv(omicron_new2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Test Centers Count/Omicron/Omicron-May.csv", row.names = FALSE)
