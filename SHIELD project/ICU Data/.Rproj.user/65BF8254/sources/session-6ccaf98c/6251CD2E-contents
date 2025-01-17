CDPH = read.csv(file.choose(), sep=",", header=T)

CDPH$Week.Start = as.Date(CDPH$Week.Start, format = "%m/%d/%Y")

CDPH_filtered = CDPH %>%
  filter(year(as.Date(Week.Start)) %in% c(2020, 2021, 2022))


CDPH_filtered = CDPH_filtered %>%
  group_by(ZIP.Code) %>%
  mutate(Cumulative_Death_Total = cumsum(Deaths...Cumulative)) %>%
  ungroup()

CDPH_filtered = CDPH_filtered %>%
  group_by(ZIP.Code) %>%
  summarise(Cumulative_Death_Total = max(Cumulative_Death_Total)) %>%
  ungroup() 

CDPH_filtered = CDPH_filtered[-60, ]

CDPH_filtered = CDPH_filtered %>%
  rename(zipcode = ZIP.Code)
#*******************************************************************************

Test_Centers = read.csv(file.choose(), sep=",", header=T)


Test_Centers = Test_Centers %>% 
  filter(covidSHIELD.Requested == "TRUE")

Test_Centers = Test_Centers %>% 
  filter(State == "IL")

Test_Centers = Test_Centers %>%
  mutate(Zip = substr(Zip, 1, 5))

# Calculate the total number of unique "Account.Name" for each zipcode
total_test_centers_per_zipcode = Test_Centers %>%
  group_by(Zip) %>%
  summarise(Total_Account_Names = n_distinct(Account.Name))

total_test_centers_per_zipcode = total_test_centers_per_zipcode %>%
  rename(zipcode = Zip)


total_test_centers_per_zipcode = total_test_centers_per_zipcode[-1, ]


dim(CDPH_filtered)
dim(total_test_centers_per_zipcode)

# Assuming "zipcode" is the common column and both datasets have the columns you mentioned
# Perform a full join
CDPH_Merged = merge(CDPH_filtered, total_test_centers_per_zipcode, by = "zipcode", all = TRUE)

CDPH_Merged = CDPH_Merged %>%
  rename(Total_Number_of_Test_Centers = Total_Account_Names)

CDPH_Merged = na.omit(CDPH_Merged)

dim(CDPH_Merged)

#*********************************Question 1*******************************************

Correlation_Result = cor(CDPH_Merged$Cumulative_Death_Total, CDPH_Merged$Total_Number_of_Test_Centers, method = "pearson", use = "complete.obs")
print(Correlation_Result)

ggplot(CDPH_Merged, aes(x = Total_Number_of_Test_Centers, y = Cumulative_Death_Total)) +
  geom_point(alpha = 0.5) +  # Adjust point transparency with alpha
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Add a linear regression line
  labs(title = "Correlation between Total Deaths and Total Test Centers",
       x = "Total Number of Test Centers",
       y = "Total Number of Deaths") +
  theme_minimal()

death_model = glm(Cumulative_Death_Total ~ Total_Number_of_Test_Centers, family = poisson, data = CDPH_Merged)

summary(death_model)

#******************************************************************************

CDPH = na.omit(CDPH)

CDPH_filtered = CDPH %>%
  filter(year(as.Date(Week.Start)) %in% c(2020, 2021, 2022))

CDPH_filtered = CDPH_filtered %>%
  group_by(ZIP.Code) %>%
  mutate(Cumulative_Case_Total = cumsum(Cases...Cumulative)) %>%
  ungroup()

CDPH_filtered = CDPH_filtered %>%
  group_by(ZIP.Code) %>%
  summarise(Cumulative_Case_Total = max(Cumulative_Case_Total, na.rm = TRUE))


CDPH_filtered = CDPH_filtered[-60, ]

CDPH_filtered = CDPH_filtered %>%
  rename(zipcode = ZIP.Code)

dim(CDPH_filtered)
dim(total_test_centers_per_zipcode)

CDPH_Merged = merge(CDPH_filtered, total_test_centers_per_zipcode, by = "zipcode", all = TRUE)

CDPH_Merged = CDPH_Merged %>%
  rename(Total_Number_of_Test_Centers = Total_Account_Names)

CDPH_Merged = na.omit(CDPH_Merged)

write.csv(CDPH_Merged, file = "C:/Users/skasaiesharifi/Documents/CDPH_Merged.csv", row.names = FALSE)

#******************************************************************************

Correlation_Result = cor(CDPH_Merged$Cumulative_Case_Total, CDPH_Merged$Total_Number_of_Test_Centers, method = "pearson", use = "complete.obs")
print(Correlation_Result)

ggplot(CDPH_Merged, aes(x = Total_Number_of_Test_Centers, y = Cumulative_Case_Total)) +
  geom_point(alpha = 0.5) +  # Adjust point transparency with alpha
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Add a linear regression line
  labs(title = "Correlation between Total Cases and Total Test Centers",
       x = "Total Number of Test Centers",
       y = "Total Number of Cases") +
  theme_minimal()

Case_model = glm(Cumulative_Case_Total ~ Total_Number_of_Test_Centers, family = poisson, data = CDPH_Merged)

summary(Case_model)

#*********************************Question 2*********************************************

CDPH_Mergeed_1 = read.csv(file.choose(), sep=",", header=T)

Correlation_Result = cor(CDPH_Mergeed_1$Cumulative_Death_Total, CDPH_Mergeed_1$ADI, method = "pearson", use = "complete.obs")
print(Correlation_Result)

ggplot(CDPH_Mergeed_1, aes(x = ADI, y = Cumulative_Death_Total)) +
  geom_point(alpha = 0.5) +  # Adjust point transparency with alpha
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Add a linear regression line
  labs(title = "Correlation between Total Deaths and ADI",
       x = "ADI",
       y = "Total Number of Deaths") +
  theme_minimal()

death_model_1 = glm(Cumulative_Death_Total ~ ADI, family = poisson, data = CDPH_Mergeed_1)

summary(death_model_1)

#*****************************************************************************

Correlation_Result = cor(CDPH_Mergeed_1$Cumulative_Case_Total, CDPH_Mergeed_1$ADI, method = "pearson", use = "complete.obs")
print(Correlation_Result)

ggplot(CDPH_Mergeed_1, aes(x = ADI, y = Cumulative_Case_Total)) +
  geom_point(alpha = 0.5) +  # Adjust point transparency with alpha
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Add a linear regression line
  labs(title = "Correlation between Total Cases and ADI",
       x = "ADI",
       y = "Total Number of Cases") +
  theme_minimal()

Case_model_1 = glm(Cumulative_Case_Total ~ ADI, family = poisson, data = CDPH_Mergeed_1)

summary(Case_model_1)

#*********************************Question 3*********************************************
