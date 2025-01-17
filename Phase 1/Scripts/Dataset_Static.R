Dataset_Static = read.csv(file.choose(), sep=",", header=T)

Dataset_Static$Date = as.Date(paste(Dataset_Static$Year, Dataset_Static$Month, "01", sep="-"), format="%Y-%B-%d")

str(Dataset_Static)

death_rate_model_1 = glm(Death.Rate ~  Age..0.17.+Age..18.64.+Age..65..,family = poisson, data = Dataset_Static)
vaccination_model_1 = glm(Vaccination.Rate ~  Age..0.17.+Age..18.64.+Age..65..,family = poisson, data = Dataset_Static)

summary(death_rate_model_1)
summary(vaccination_model_1)

model_summary_1 = tidy(death_rate_model_1)
model_summary_1_1 = tidy(vaccination_model_1)

Correlation_Result = cor(Dataset_Static$Age..0.17., Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Age..0.17., Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Age..18.64., Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Age..18.64., Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Age..65.., Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Age..65.., Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#**************************************************

death_rate_model_2 = glm(Death.Rate ~  Dataset_Static$Sex..Male.+Dataset_Static$Sex..Female.,family = poisson, data = Dataset_Static)

summary(death_rate_model_2)

model_summary_2 = tidy(death_rate_model_2)

Correlation_Result = cor(Dataset_Static$Sex..Male., Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Sex..Female., Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#**************************************************

death_rate_model_3 = glm(Death.Rate ~  Dataset_Static$Ethnicity..Hispanic.+Dataset_Static$Ethnicity..Non.Hispanic.,family = poisson, data = Dataset_Static)

summary(death_rate_model_3)

model_summary_3 = tidy(death_rate_model_3)

Correlation_Result = cor(Dataset_Static$Ethnicity..Hispanic., Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Ethnicity..Non.Hispanic., Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#**************************************************

death_rate_model_4 = glm(Death.Rate ~  Dataset_Static$X2021.ADI.State.Rank,family = poisson, data = Dataset_Static)

summary(death_rate_model_4)

model_summary_4 = tidy(death_rate_model_4)

Correlation_Result = cor(Dataset_Static$X2021.ADI.State.Rank, Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)


#**************************************************

death_rate_model_5 = glm(Death.Rate ~  Dataset_Static$American.Indian.Alaska.Native+
                           Dataset_Static$Asian+Dataset_Static$Black.African.American+
                           Dataset_Static$Native.Hawaiian.Other.Pacific.Islander+
                           Dataset_Static$White+Dataset_Static$Other,family = poisson, data = Dataset_Static)

summary(death_rate_model_5)

model_summary_5 = tidy(death_rate_model_5)

Correlation_Result = cor(Dataset_Static$American.Indian.Alaska.Native, Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Asian, Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Black.African.American, Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Native.Hawaiian.Other.Pacific.Islander, Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$White, Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Other, Dataset_Static$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)


#***********************************Impact of Age (0-17) on the Death rate over time for two zipcodes*******************************************

zipcodes_of_interest = c("60624", "60633")

library(tidyverse)
# Filter the dataset for only the zipcodes of interest
Dataset_Static_filtered = Dataset_Static %>% 
  filter(zipcode %in% zipcodes_of_interest)

Dataset_Static_filtered$Date = as.Date(paste(Dataset_Static_filtered$Year, Dataset_Static_filtered$Month, "01", sep="-"), format="%Y-%B-%d")


ggplot(Dataset_Static_filtered, aes(x = Age..0.17., y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Age (0-17) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Age (0-17)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Age..0.17., Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Age (18-64) on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Age..18.64., y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Age (18-64) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Age (18-64)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Age..18.64., Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Age (65+) on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Age..65.., y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Age (65+) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Age (65+)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Age..65.., Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Gender (Male) on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Sex..Male., y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Gender (Male) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Gender (Male)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Sex..Male., Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Gender (Female) on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Sex..Female., y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Gender (Female) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Gender (Female)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Sex..Female., Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Race (American Indian/Alaska Native) on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = American.Indian.Alaska.Native, y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Race (American Indian/Alaska Native) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Race (American Indian/Alaska Native)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$American.Indian.Alaska.Native, Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Race (Asian) on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Asian, y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Race (Asian) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Race (Asian)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Asian, Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Race (Black/African American) on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Black.African.American, y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Race (Black/African American) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Race (Black/African American)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Black.African.American, Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Race (Native Hawaiian/Other Pacific Islander) on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Native.Hawaiian.Other.Pacific.Islander, y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Race (Native Hawaiian/Other Pacific Islander) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Race (Native Hawaiian/Other Pacific Islander)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Native.Hawaiian.Other.Pacific.Islander, Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Race (White) on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = White, y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Race (White) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Race (White)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$White, Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Race (Other) on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Other, y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Race (Other) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Race (Other)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Other, Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Ethnic (Hispanic) on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Ethnicity..Hispanic., y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Ethnic (Hispanic) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Ethnic (Hispanic)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Ethnicity..Hispanic., Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Ethnic (Non-Hispanic) on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Ethnicity..Non.Hispanic., y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Ethnic (Non-Hispanic) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Ethnic (Non-Hispanic)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Ethnicity..Non.Hispanic., Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of ADI on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = X2021.ADI.State.Rank, y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of ADI on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the ADI",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$X2021.ADI.State.Rank, Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***************************************************

vaccination_model_1 = glm(Vaccination.Rate ~  Age..0.17.+Age..18.64.+Age..65..,family = poisson, data = Dataset_Static)

summary(vaccination_model_1)

Correlation_Result = cor(Dataset_Static$Age..0.17., Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Age..18.64., Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Age..65.., Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#**************************************************

vaccination_rate_model_2 = glm(Vaccination.Rate ~  Dataset_Static$Sex..Male.+Dataset_Static$Sex..Female.,family = poisson, data = Dataset_Static)

summary(vaccination_rate_model_2)

Correlation_Result = cor(Dataset_Static$Sex..Male., Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Sex..Female., Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#**************************************************

vaccination_rate_model_3 = glm(Vaccination.Rate ~  Dataset_Static$Ethnicity..Hispanic.+Dataset_Static$Ethnicity..Non.Hispanic.,family = poisson, data = Dataset_Static)

summary(death_rate_model_3)

Correlation_Result = cor(Dataset_Static$Ethnicity..Hispanic., Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Ethnicity..Non.Hispanic., Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#**************************************************

vaccination_rate_model_4 = glm(Vaccination.Rate ~  Dataset_Static$X2021.ADI.State.Rank,family = poisson, data = Dataset_Static)

summary(vaccination_rate_model_4)

Correlation_Result = cor(Dataset_Static$X2021.ADI.State.Rank, Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)


#**************************************************

vaccination_rate_model_5 = glm(Vaccination.Rate ~  Dataset_Static$American.Indian.Alaska.Native+
                           Dataset_Static$Asian+Dataset_Static$Black.African.American+
                           Dataset_Static$Native.Hawaiian.Other.Pacific.Islander+
                           Dataset_Static$White+Dataset_Static$Other,family = poisson, data = Dataset_Static)

summary(vaccination_rate_model_5)

Correlation_Result = cor(Dataset_Static$American.Indian.Alaska.Native, Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Asian, Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Black.African.American, Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Native.Hawaiian.Other.Pacific.Islander, Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$White, Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

Correlation_Result = cor(Dataset_Static$Other, Dataset_Static$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)


#***********************************Impact of Age (0-17) on the Vaccination rate over time for two zipcodes*******************************************

zipcodes_of_interest = c("60607", "60621")

library(tidyverse)
# Filter the dataset for only the zipcodes of interest
Dataset_Static_filtered = Dataset_Static %>% 
  filter(zipcode %in% zipcodes_of_interest)

Dataset_Static_filtered$Date = as.Date(paste(Dataset_Static_filtered$Year, Dataset_Static_filtered$Month, "01", sep="-"), format="%Y-%B-%d")


ggplot(Dataset_Static_filtered, aes(x = Age..0.17., y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Age (0-17) on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Age (0-17)",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Age..0.17., Dataset_Static_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Age (18-64) on the Vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Age..18.64., y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Age (18-64) on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Age (18-64)",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Age..18.64., Dataset_Static_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Age (65+) on the Vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Age..65.., y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Age (65+) on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Age (65+)",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Age..65.., Dataset_Static_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Gender (Male) on the Vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Sex..Male., y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Gender (Male) on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Gender (Male)",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Sex..Male., Dataset_Static_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Gender (Female) on the Vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Sex..Female., y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Gender (Female) on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Gender (Female)",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Sex..Female., Dataset_Static_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Race (American Indian/Alaska Native) on the Vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = American.Indian.Alaska.Native, y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Race (American Indian/Alaska Native) on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Race (American Indian/Alaska Native)",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$American.Indian.Alaska.Native, Dataset_Static_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Race (Asian) on the Vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Asian, y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Race (Asian) on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Race (Asian)",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Asian, Dataset_Static_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Race (Black/African American) on the Vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Black.African.American, y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Race (Black/African American) on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Race (Black/African American)",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Black.African.American, Dataset_Static_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Race (Native Hawaiian/Other Pacific Islander) on the Death rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Native.Hawaiian.Other.Pacific.Islander, y = Death.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Race (Native Hawaiian/Other Pacific Islander) on Death Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Race (Native Hawaiian/Other Pacific Islander)",
       y = "Death Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Native.Hawaiian.Other.Pacific.Islander, Dataset_Static_filtered$Death.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Race (White) on the Vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = White, y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Race (White) on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Race (White)",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$White, Dataset_Static_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Race (Other) on the Vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Other, y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Race (Other) on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Race (Other)",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Other, Dataset_Static_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Ethnic (Hispanic) on the Vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Ethnicity..Hispanic., y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Ethnic (Hispanic) on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Ethnic (Hispanic)",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Ethnicity..Hispanic., Dataset_Static_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of Ethnic (Non-Hispanic) on the Vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = Ethnicity..Non.Hispanic., y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of Ethnic (Non-Hispanic) on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the Ethnic (Non-Hispanic)",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$Ethnicity..Non.Hispanic., Dataset_Static_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)

#***********************************Impact of ADI on the Vaccination rate over time for two zipcodes*******************************************

ggplot(Dataset_Static_filtered, aes(x = X2021.ADI.State.Rank, y = Vaccination.Rate, color = factor(zipcode))) +
  geom_point() + # Plot points
  geom_smooth(method = lm, aes(group = zipcode)) + # Add a linear trend line for each zipcode
  labs(title = "Impact of ADI on Vaccination Rate over time for Zipcodes 60607 and 60621",
       x = "Population with the ADI",
       y = "Vaccination Rate",
       color = "Zipcode") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"), # Bold plot title
    axis.title.x = element_text(face = "bold"), # Bold x axis title
    axis.title.y = element_text(face = "bold"), # Bold y axis title
    legend.title = element_text(face = "bold")
  ) # Add labels for year and month

Correlation_Result = cor(Dataset_Static_filtered$X2021.ADI.State.Rank, Dataset_Static_filtered$Vaccination.Rate, method = "pearson", use = "complete.obs")
print(Correlation_Result)
