

#*************************************SHIELD Centers******************************************

rm(X)

X = unique(Dataset_Dynamic$zipcode)
X = as.data.frame(X)

Test_Centers = read.csv(file.choose(), sep=",", header=T)

library(dplyr)
library(tidyverse)

result = Test_Centers %>%
  filter(covidSHIELD.Requested == TRUE) %>% 
  group_by(Zip) %>% 
  summarise(Total.Accounts = n()) 

result = result %>%
  rename(Numebr_of_SHIELD_Centers = Total.Accounts)

result = as.data.frame(result)

write.csv(X, file = "C:/Users/skasaiesharifi/Documents/X.csv", row.names = FALSE)

#******************************************************************************

Dataset_Dynamic = read.csv(file.choose(), sep=",", header=T)

Dataset_Dynamic = Dataset_Dynamic %>%
  rename(zipcode = Zipcode)

#rm(Dataset_Dynamic)

result = rename(result, zipcode = Zip)

result <- result |> 
  mutate(zipcode = as.character(zipcode))

Dataset_Dynamic <- Dataset_Dynamic |> 
  mutate(zipcode = as.character(zipcode))

Dataset_Dynamic = left_join(Dataset_Dynamic, result, by = join_by(zipcode == zipcode))

#write.csv(Dataset_Dynamic, file = "C:/Users/skasaiesharifi/Documents/Dataset_Dynamic.csv", row.names = FALSE)

#*************************************Other Test Centers******************************************

result_1= Test_Centers %>%
  filter(covidSHIELD.Requested == FALSE) %>% 
  group_by(Zip) %>% 
  summarise(Total.Accounts = n()) 

result_1 = result_1 %>%
  rename(zipcode = Zip)

result_1 = as.data.frame(result_1)

result_1 <- result_1 |> 
  mutate(zipcode = as.character(zipcode))

Dataset_Dynamic = left_join(Dataset_Dynamic, result_1, by = join_by(zipcode == zipcode))

write.csv(merged_data, file = "C:/Users/skasaiesharifi/Documents/merged_dataset.csv", row.names = FALSE)

#*************************************************************************************************
Dataset_Cumulative = read.csv(file.choose(), sep=",", header=T)

Dataset_Cumulative = Dataset_Cumulative |> 
  mutate(month = as_factor(Month),
         year = as_factor(Year),
         zipcode = as.character(zipcode)) |> 
  group_by(zipcode, year, month) |> 
  summarise(total_death = sum(Deaths_Cumulative),
            Population = first(Population),
            SHIELDS = first(Numebr_of_SHIELD_Centers),
            Other = first(Number_of_Other_Centers)
  )

#*************************************************************************************************

common_columns = c("zipcode","Year","Month")

merged_data = left_join(Dataset_Dynamic, Dataset_Cumulative, by = common_columns)

#***********************************Trend of Death Rate for all zipcodes*******************************************************

Dataset_Dynamic = read.csv(file.choose(), sep=",", header=T)

library(ggplot2)
library(dplyr)
library(lubridate)

# Assuming your dataset is loaded into a dataframe called Dataset_Dynamic
# And assuming the date information is in columns 'Year' and 'Month', with 'Month' being the full month name

# convert the year and month 
Dataset_Dynamic$Date = as.Date(paste(Dataset_Dynamic$Year, Dataset_Dynamic$Month, "01", sep="-"), format="%Y-%B-%d")

ggplot(Dataset_Dynamic, aes(x=Date, y=Death.Rate, group=zipcode, color=factor(zipcode))) +
  geom_line() +
  labs(title="Monthly Trend of Death Rate per 1000 Population for all Zipcodes",
       x="Date",
       y="Death Rate per 1000 Population",
       color="Zipcode") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#***********************************Trend of Death Rate for two zipcodes*******************************************************

zipcodes_of_interest = c("60622", "60633","60634")

library(tidyverse)
# Filter the dataset for only the zipcodes of interest
Dataset_Dynamic_filtered = Dataset_Dynamic %>% 
  filter(zipcode %in% zipcodes_of_interest)

Dataset_Dynamic_filtered$Date = as.Date(paste(Dataset_Dynamic_filtered$Year, Dataset_Dynamic_filtered$Month, "01", sep="-"), format="%Y-%B-%d")

ggplot(Dataset_Dynamic_filtered, aes(x=Date, y=Death.Rate, group=zipcode, color=factor(zipcode))) +
  geom_line() +
  labs(title="Monthly Trend of Death Rate per 1000 Population for Zipcodes 60610, 60643, and 60628",
       x="Date",
       y="Death Rate per 1000 Population",
       color="Zipcode") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(face = "bold"), # Bold plot title
        axis.title.x = element_text(face = "bold"), # Bold x axis title
        axis.title.y = element_text(face = "bold"), # Bold y axis title
        legend.title = element_text(face = "bold"), # Bold legend title
        text = element_text(size = 12))

Correlation_Result = cor(Dataset_Dynamic$TotalPopulation, Dataset_Dynamic$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Trend of Vaccination Rate for all zipcodes*******************************************************

Dataset_Dynamic$Date = as.Date(paste(Dataset_Dynamic$Year, Dataset_Dynamic$Month, "01", sep="-"), format="%Y-%B-%d")

ggplot(Dataset_Dynamic, aes(x=Date, y=Vaccination.Rate, group=zipcode, color=factor(zipcode))) +
  geom_line() +
  labs(title="Trend of Total Vaccination over time for all zipcodes",
       x="Date",
       y="Vaccination Rate",
       color="Zipcode") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#***********************************Trend of Vaccination Rate for two zipcodes*******************************************************

Dataset_Dynamic_filtered$Date = as.Date(paste(Dataset_Dynamic_filtered$Year, Dataset_Dynamic_filtered$Month, "01", sep="-"), format="%Y-%B-%d")

ggplot(Dataset_Dynamic_filtered, aes(x=Date, y=Vaccination.Rate, group=zipcode, color=factor(zipcode))) +
  geom_line() +
  labs(title="Trend of Vaccination Rate over time for Zipcodes 60607 and 60621",
       x="Date",
       y="Vaccination Rate",
       color="Zipcode") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

Correlation_Result = cor(Dataset_Dynamic$Date_numeric, Dataset_Dynamic$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Total Vaccination on Death rate*******************************************************

ggplot(Dataset_Dynamic_filtered, aes(x=total_vaccination, y=Death.Rate, color=factor(zipcode), label=Date)) +
  geom_point() + # Plot points
  geom_smooth(method=lm, aes(group=zipcode)) + # Add a linear trend line for each zipcode
  labs(title="Impact of Total Vaccination on Death Rate for Zipcodes 60607 and 60621",
       x="Total Vaccination",
       y="Death Rate",
       color="Zipcode") +
  theme_minimal() +
  geom_text(aes(label=Date), check_overlap = TRUE, vjust = 1.5, hjust = 1.5) # Add labels for year and month

Correlation_Result = cor(Dataset_Dynamic$total_vaccination, Dataset_Dynamic$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Vaccination Rate on Death rate*******************************************************

ggplot(Dataset_Dynamic_filtered, aes(x=Vaccination.Rate, y=Death.Rate, color=factor(zipcode), label=Date)) +
  geom_point() + # Plot points
  geom_smooth(method=lm, aes(group=zipcode)) + # Add a linear trend line for each zipcode
  labs(title="Impact of Vaccination Rate on Death Rate for Zipcodes 60621 and 60633",
       x="Vaccination Rate",
       y="Death Rate",
       color="Zipcode") +
  theme_minimal() +
  geom_text(aes(label=Date), check_overlap = TRUE, vjust = 1.5, hjust = 1.5) # Add labels for year and month

Correlation_Result = cor(Dataset_Dynamic$total_vaccination, Dataset_Dynamic$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Vaccination Rate VS Death Rate*******************************************************

ggplot(Dataset_Dynamic, aes(x=Vaccination.Rate, y=Death.Rate, color=factor(zipcode))) +
  geom_point() + # Add points
  geom_smooth(method=lm) + # Add a linear regression line
  labs(title="Vaccination Rate vs Death Rate for All Zipcodes",
       x="Vaccination Rate",
       y="Death Rate",
       color="Zipcode") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) # Rotate x-axis labels for clarity

Correlation_Result = cor(Dataset_Dynamic$Vaccination.Rate, Dataset_Dynamic$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Vaccination Rate VS Population*******************************************************

ggplot(Dataset_Dynamic_filtered, aes(x=TotalPopulation, y=Vaccination.Rate, color=factor(zipcode))) +
  geom_point() + # Add points
  geom_smooth(method=lm) + # Add a linear regression line
  labs(title="Population vs Vaccination Rate for zipcode 60607 and 60621",
       x="Population",
       y="Vaccination Rate",
       color="Zipcode") +
  theme_minimal() +
  theme(legend.position="bottom") # Move the legend to the bottom to avoid overlap

ggplot(Dataset_Dynamic_filtered, aes(x=Population, y=Vaccination.Rate, color=factor(zipcode))) +
  geom_point() + # Add points
  geom_smooth(method=lm) + # Add a linear regression line
  labs(title="Population vs Vaccination Rate for zipcode 60613 and 60637",
       x="Population",
       y="Vaccination Rate",
       color="Zipcode") +
  theme_minimal() +
  theme(legend.position="bottom") # Move the legend to the bottom to avoid overlap

Correlation_Result = cor(Dataset_Dynamic_filtered$TotalPopulation, Dataset_Dynamic_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of SHIELD centers on the vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Dynamic_filtered, aes(x = SHIELDS, y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Number of SHIELD Centers on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Number of SHIELD Centers",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  scale_x_continuous(breaks = c(1, 2), limits = c(1, 2)) + # Set explicit breaks for the x axis
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Dynamic_filtered$SHIELDS, Dataset_Dynamic_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Other centers on the vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Dynamic_filtered, aes(x=Other, y=Vaccination.Rate, color=factor(zipcode), label=Date)) +
  geom_point() + # Plot points
  geom_smooth(method=lm, aes(group=zipcode)) + # Add a linear trend line for each zipcode
  labs(title="Impact of Number of Other centers on Vaccination Rate for Zipcodes 60604 and 60614",
       x="Number of Other Centers",
       y="Vaccination Rate",
       color="Zipcode") +
  theme_minimal() +
  geom_text(aes(label=Date), check_overlap = TRUE, vjust = 1.5, hjust = 1.5) # Add labels for year and month

Correlation_Result = cor(Dataset_Dynamic_filtered$Other, Dataset_Dynamic_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of SHIELD centers on the Death rate over time*******************************************

ggplot(Dataset_Dynamic, aes(x=SHIELDS, y=Death.Rate, color=factor(zipcode), label=Date)) +
  geom_point() + # Plot points
  geom_smooth(method=lm, aes(group=zipcode)) + # Add a linear trend line for each zipcode
  labs(title="Impact of Number of SHIELD centers on Death Rate for all zipcodes over time",
       x="Number of SHIELD Centers",
       y="Death Rate",
       color="Zipcode") +
  theme_minimal() +
  geom_text(aes(label=Date), check_overlap = TRUE, vjust = 1.5, hjust = 1.5)

Correlation_Result = cor(Dataset_Dynamic$SHIELDS, Dataset_Dynamic$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

ggplot(Dataset_Dynamic_filtered, aes(x = SHIELDS, y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Number of SHIELD Centers on Death Rate for Zipcodes 60621 and 60633",
       x = "Number of SHIELD Centers",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  scale_x_continuous(breaks = c(1, 2), limits = c(1, 2)) + # Set explicit breaks for the x axis
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  )

    
# Bold legend title
# Add labels for year and month

Correlation_Result = cor(Dataset_Dynamic_filtered$SHIELDS, Dataset_Dynamic_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Other centers on the Death rate over time*******************************************

ggplot(Dataset_Dynamic, aes(x=Other, y=Death.Rate, color=factor(zipcode), label=Date)) +
  geom_point() + # Plot points
  geom_smooth(method=lm, aes(group=zipcode)) + # Add a linear trend line for each zipcode
  labs(title="Impact of Number of Other centers on Death Rate for all zipcodes over time",
       x="Number of Other Centers",
       y="Death Rate",
       color="Zipcode") +
  theme_minimal() +
  geom_text(aes(label=Date), check_overlap = TRUE, vjust = 1.5, hjust = 1.5)

Correlation_Result = cor(Dataset_Dynamic$Other, Dataset_Dynamic$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

ggplot(Dataset_Dynamic_filtered, aes(x=Other, y=Death.Rate, color=factor(zipcode), label=Date)) +
  geom_point() + # Plot points
  geom_smooth(method=lm, aes(group=zipcode)) + # Add a linear trend line for each zipcode
  labs(title="Impact of Number of Other centers on Death Rate for Zipcodes 60616 and 60625",
       x="Number of Other Centers",
       y="Death Rate",
       color="Zipcode") +
  theme_minimal() +
  geom_text(aes(label=Date), check_overlap = TRUE, vjust = 1.5, hjust = 1.5) # Add labels for year and month

Correlation_Result = cor(Dataset_Dynamic_filtered$Other, Dataset_Dynamic_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Important Factors on Death Rate *******************************************

library(glm2)
library(broom)
library(knitr)

#Dataset_Dynamic$Population = NULL

death_rate_model = glm(Death.Rate ~ Vaccinated_Cumulative + Vaccination.Rate+ Education..9th.grade.+ Education..at.least.HS.diploma.+
                         Education..at.least.Bachelor.s.degree.+ Population.Uninsured+Population..Insured + SHIELDS+Other+ADI, family = poisson, data = Dataset_Dynamic)

summary(death_rate_model)


Correlation_Result = cor(Dataset_Dynamic$ADI, Dataset_Dynamic$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

ggplot(Dataset_Dynamic, aes(x = ADI, y = Death.Rate)) +
  geom_point(alpha = 0.5) +  # Adjust point transparency with alpha
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Add a linear regression line
  labs(title = "Correlation between Death Rate and Zipcode ADI",
       x = "Zipcode ADI",
       y = "Death Rate per 1000 Population") +
  theme_minimal()  # Use a minimal theme for a cleaner look

library(broom)

model_summary = tidy(death_rate_model)



# Assuming the dataset is loaded into R as Dataset_Dynamic

# Load the necessary library for date conversion
Dataset_Dynamic = read.csv(file.choose(), sep=",", header=T)

library(zoo)

# Convert the Date column from a factor to a character if it's not already
Dataset_Dynamic$DATE = as.character(Dataset_Dynamic$Date)

# Convert the Date column to a Date format, assuming the year is in two-digit format
Dataset_Dynamic$Date_converted = as.Date(paste0("01-", Dataset_Dynamic$DATE), format="%d-%b-%y")

# Create a numeric scale based on the date
# Here we convert the dates to 'yearmon' objects and then to a numeric scale of months
Dataset_Dynamic$Date_numeric = as.numeric(as.yearmon(Dataset_Dynamic$Date_converted)) - 
  as.numeric(as.yearmon(min(Dataset_Dynamic$Date_converted)))

# Assuming the Death Rate column is named 'Death.F'
# Calculate the correlation between the numeric dates and the Death Rate
correlation =  cor(Dataset_Dynamic$Date_numeric, Dataset_Dynamic$Death.Rate)

print(correlation)


ggplot(Dataset_Dynamic, aes(x = Date_converted, y = Death.Rate)) + 
  geom_line() +
  labs(title = "Death Rate Over Time",
       x = "Date",
       y = "Death Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

