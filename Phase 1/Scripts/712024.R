############################## Alpha Wave ######################################

ICU_UPDATED_ALPHA_Zipcode = read.csv(file.choose(), sep=",", header=T)

Data_Monthly_Alpha = read.csv(file.choose(), sep=",", header=T)

#Data_Monthly_Alpha = Data_Monthly_Alpha[,-c(5:12)]

Data_Monthly_Alpha = Data_Monthly_Alpha %>%
  rename(zipcode = zip)

Data_Monthly_Alpha = Data_Monthly_Alpha %>%
  rename(Date = Month)

Data_Monthly_Alpha = Data_Monthly_Alpha %>%
  mutate(Zip = substr(zipcode, 1, 5))

ICU_UPDATED_ALPHA_Zipcode$zipcode = as.character(ICU_UPDATED_ALPHA_Zipcode$zipcode)

Data_Monthly_Alpha$zipcode = as.character(Data_Monthly_Alpha$zipcode)

ICU_UPDATED_ALPHA_Zipcode = merge(ICU_UPDATED_ALPHA_Zipcode, Data_Monthly_Alpha, by = c("zipcode","Date"))

zipcodes_to_remove <- c("60141", "60479","60549","60519","60602","60910","60945","61317","61337","61338","61367","62022") 

ICU_UPDATED_ALPHA_Zipcode <- ICU_UPDATED_ALPHA_Zipcode %>%
  filter(!zipcode %in% zipcodes_to_remove)

#ICU_UPDATED_ALPHA_Patients <- full_join(ICU_UPDATED_ALPHA_Patients, Data_Monthly_Alpha, by = c("zipcode", "Date"))

ICU_UPDATED_ALPHA_Zipcode = ICU_UPDATED_ALPHA_Zipcode[,-c(21)]



write.csv(ICU_UPDATED_ALPHA_Zipcode, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Alpha/ICU_UPDATED_ALPHA_Zipcode_FATIR.csv", row.names = FALSE)


############################## Delta Wave ######################################

ICU_UPDATED_DELTA_Zipcode = read.csv(file.choose(), sep=",", header=T)

Data_Monthly_Delta = read.csv(file.choose(), sep=",", header=T)

#Data_Monthly_Delta = Data_Monthly_Delta[,-c(5:12)]

Data_Monthly_Delta = Data_Monthly_Delta %>%
  rename(zipcode = zip)

Data_Monthly_Delta = Data_Monthly_Delta %>%
  rename(Date = Month)

Data_Monthly_Delta = Data_Monthly_Delta %>%
  mutate(Zip = substr(zipcode, 1, 5))

ICU_UPDATED_DELTA_Zipcode$zipcode = as.character(ICU_UPDATED_DELTA_Zipcode$zipcode)

Data_Monthly_Delta$zipcode = as.character(Data_Monthly_Delta$zipcode)

ICU_UPDATED_DELTA_Zipcode = merge(ICU_UPDATED_DELTA_Zipcode, Data_Monthly_Delta, by = c("zipcode","Date"))

zipcodes_to_remove <- c("60141", "60479","60549","60519","60602","60910","60945","61317","61337","61338","61367","62022") 

ICU_UPDATED_DELTA_Zipcode <- ICU_UPDATED_DELTA_Zipcode %>%
  filter(!zipcode %in% zipcodes_to_remove)

#ICU_UPDATED_ALPHA_Patients <- full_join(ICU_UPDATED_ALPHA_Patients, Data_Monthly_Alpha, by = c("zipcode", "Date"))

ICU_UPDATED_ALPHA_Zipcode = ICU_UPDATED_ALPHA_Zipcode[,-c(21)]


write.csv(ICU_UPDATED_DELTA_Zipcode, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Delta/ICU_UPDATED_DELTA_Zipcode_Fatir.csv", row.names = FALSE)


############################## Omicron Wave ######################################

ICU_UPDATED_OMICRON_Zipcode = read.csv(file.choose(), sep=",", header=T)

Data_Monthly_Omicron = read.csv(file.choose(), sep=",", header=T)

#Data_Monthly_Delta = Data_Monthly_Delta[,-c(5:12)]

Data_Monthly_Omicron = Data_Monthly_Omicron %>%
  rename(zipcode = L)

Data_Monthly_Omicron = Data_Monthly_Omicron %>%
  rename(Date = Month)

Data_Monthly_Omicron = Data_Monthly_Omicron %>%
  mutate(Zip = substr(zipcode, 1, 5))

ICU_UPDATED_OMICRON_Zipcode$zipcode = as.character(ICU_UPDATED_OMICRON_Zipcode$zipcode)

Data_Monthly_Omicron$zipcode = as.character(Data_Monthly_Omicron$zipcode)

ICU_UPDATED_OMICRON_Zipcode = merge(ICU_UPDATED_OMICRON_Zipcode, Data_Monthly_Omicron, by = c("zipcode","Date"))

zipcodes_to_remove <- c("60141", "60479","60549","60519","60602","60910","60945","61317","61337","61338","61367","62022") 

ICU_UPDATED_OMICRON_Zipcode <- ICU_UPDATED_OMICRON_Zipcode %>%
  filter(!zipcode %in% zipcodes_to_remove)

#ICU_UPDATED_ALPHA_Patients <- full_join(ICU_UPDATED_ALPHA_Patients, Data_Monthly_Alpha, by = c("zipcode", "Date"))

ICU_UPDATED_OMICRON_Zipcode = ICU_UPDATED_OMICRON_Zipcode[,-c(21)]

write.csv(ICU_UPDATED_OMICRON_Zipcode, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/Omicron/ICU_UPDATED_OMICRON_Zipcode_Fatir.csv", row.names = FALSE)
