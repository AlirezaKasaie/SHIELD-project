ICU <- fread(file.choose(), sep=",", header=T)

Diagnosis <- fread(file.choose(), sep=",", header=T)

combined_dataset = merge(ICU, Diagnosis[, c('hsp_account_id', 'ref_bill_code', 'DX_NAME')], by = 'hsp_account_id', all.x = TRUE)

combined_dataset = combined_dataset[,-c(2:9,12:16,18:43)]

#ICU = ICU[,-c(1,3:9,12:16,18:43)]


combined_dataset$first_icu_dt <- sub("^\\[|\\]$", "", combined_dataset$first_icu_dt)
combined_dataset$first_icu_dt <- as.POSIXct(combined_dataset$first_icu_dt, format="%Y/%m/%d:%I:%M:%S %p")

combined_dataset = combined_dataset %>%
  rename(Date = first_icu_dt)

combined_dataset$Date = format(combined_dataset$Date, "%Y-%m-%d")


combined_dataset <- combined_dataset %>%
  mutate(Date = ymd(Date)) %>%  # Convert to Date format
  filter(st == "IL", year(Date) %in% c(2020, 2021, 2022, 2023)) %>% 
  mutate(Date = format(Date, "%Y-%m-%d"))

combined_dataset = combined_dataset %>% 
  distinct(hsp_account_id, .keep_all = TRUE)

combined_dataset = combined_dataset %>%
  mutate(zipcode = substr(zipcode, 1, 5))

unique_zipcodes <- unique(combined_dataset$zipcode)

patients_per_zipcode <- combined_dataset %>%
  group_by(zipcode) %>%
  summarise(total_patients = n_distinct(hsp_account_id))

write.csv(patients_per_zipcode, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 7/ZIPCODES.csv", row.names = FALSE)


population_ADI <- fread(file.choose(), sep=",", header=T)

patients_per_zipcode$zipcode = as.character(patients_per_zipcode$zipcode)

population_ADI$zipcode = as.character(population_ADI$zipcode)

patients_per_zipcode <- full_join(patients_per_zipcode, population_ADI, by = c("zipcode"))

patients_per_zipcode <- patients_per_zipcode %>%
  na.omit() %>%  # Remove all rows with NAs
  distinct(zipcode, .keep_all = TRUE) %>%  # Keep distinct zipcodes
  mutate(Rate = (total_patients / cpop) * 1000) 

# Calculate the 25th percentile of the Rate values
rate_25th_percentile <- quantile(patients_per_zipcode$Rate, 0.25)

# Filter out zipcodes with Rate below the 25th percentile
patients_per_zipcode <- patients_per_zipcode %>%
  filter(Rate >= rate_25th_percentile)

write.csv(patients_per_zipcode, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 7/Patient per zipcode.csv", row.names = FALSE)

###############################################################################

combined_dataset_NEW <- combined_dataset %>%
  group_by(zipcode, Date) %>%
  summarise(Total_ICU_admissions_per_zipcode_per_month = n())

combined_dataset$Date <- ymd(combined_dataset$Date)

combined_dataset$Date <- format(combined_dataset$Date, "%Y-%m")

combined_dataset_NEW = merge(combined_dataset_NEW,combined_dataset, by = c('zipcode','Date'))

combined_dataset_NEW = combined_dataset_NEW[,-c(4:7)]

combined_dataset_NEW = merge(combined_dataset_NEW,population_ADI, by = c('zipcode'))

all_months <- seq(from = as.yearmon("2020-01", "%Y-%m"), to = as.yearmon("2023-12", "%Y-%m"), by = 1/12)

formatted_months <- format(all_months, "%Y-%m")

all_zipcodes <- unique(combined_dataset_NEW$zipcode)

full_grid <- expand.grid(zipcode = all_zipcodes, Date = formatted_months)

expanded_dataset = full_join(combined_dataset_NEW,full_grid, by = c('zipcode','Date'))

write.csv(expanded_dataset, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Method 7/ICU_UPDATED.csv", row.names = FALSE)


###############################################################################

ICU_UPDATED <- fread(file.choose(), sep=",", header=T)

ICU_UPDATED <- ICU_UPDATED %>%
  group_by(zipcode, Date) %>%
  summarise(Total_ICU_admissions_per_zipcode_per_month = sum(Total_ICU_admissions_per_zipcode_per_month)) %>%
  ungroup()