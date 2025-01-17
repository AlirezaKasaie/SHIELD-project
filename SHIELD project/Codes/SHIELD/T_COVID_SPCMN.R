if(!require(data.table)) install.packages("data.table")

library(data.table)
library('dplyr')
library('ggplot2')
library('tidyverse')
library('lubridate')


T_COVID_SPCMN = fread(file.choose(), sep=",", header=T)


str(T_COVID_SPCMN)
colnames(T_COVID_SPCMN)
sum(is.na(T_COVID_SPCMN))

unique_values = unique(T_COVID_SPCMN$X.NAI.AGENCY...PATIENT)
unique_values = as.data.frame(unique_values)
df = T_COVID_SPCMN[, c("X.NAI.AGENCY...PATIENT", "X.NAI.AGENCY...TEST..EMR.", "X.NAI.LOCATION..EMR.","LAB..Combined.","LOCATION")]
write.csv(df, file = "C:/Users/skasaiesharifi/Documents/T_COVID_SPCMN_NEW.csv", row.names = FALSE)

colSums(is.na(T_COVID_SPCMN))

total_blank_and_empty = colSums(is.na(T_COVID_SPCMN) | T_COVID_SPCMN == "", na.rm = TRUE)

df_DATES = T_COVID_SPCMN[, c("X.NAI.DATE...COLLECTED..EMR.", "X.NAI.DATE...EDW.LOAD", "X.NAI.DATE...LAB.RECEIVED",
"X.NAI.DATE...LAST.MODIFIED","X.NAI.DATE...LAST.UPDATE","X.NAI.DATE...MANIFEST.CREATED","X.NAI.DATE...MANIFEST.SENT","X.NAI.DATE...RESULT..EMR.",
"X.NAI.LAG...COLLECTED.TO.RESULT","X.NAI.LAG...COLLECTED.TO.RESULT.RANGE","X.NAI.LAG...RECEIVED.TO.RESULT","X.NAI.LAG...RECIEVED.TO.RESULT.RANGE",
"LAG...Collected.to.Sent","LAG...Sent.to.Result","DATE...COLLECTED","DATE...RESULT")]

write.csv(df_DATES, file = "C:/Users/skasaiesharifi/Documents/T_COVID_SPCMN_DATES.csv", row.names = FALSE)

df_DATES$X.NAI.LAG...COLLECTED.TO.RESULT.RANGE = as.factor(df_DATES$X.NAI.LAG...COLLECTED.TO.RESULT.RANGE)
df_DATES$X.NAI.LAG...RECIEVED.TO.RESULT.RANGE = as.factor(df_DATES$X.NAI.LAG...RECIEVED.TO.RESULT.RANGE)
levels(df_DATES$X.NAI.LAG...COLLECTED.TO.RESULT.RANGE)
levels(df_DATES$X.NAI.LAG...RECIEVED.TO.RESULT.RANGE)


CollectedToSent_Range_level_counts = table(df_DATES$X.NAI.LAG...COLLECTED.TO.RESULT.RANGE)
print(CollectedToSent_Range_level_counts)

ReceivedToSent_Range_level_counts <- table(df_DATES$X.NAI.LAG...RECIEVED.TO.RESULT.RANGE)
print(ReceivedToSent_Range_level_counts)

df_DATES$X.NAI.LAG...COLLECTED.TO.RESULT = as.numeric(df_DATES$X.NAI.LAG...COLLECTED.TO.RESULT)

T_COVID_SPCMN$X.NAI.LAB.STATUS = as.factor(T_COVID_SPCMN$X.NAI.LAB.STATUS)
levels(T_COVID_SPCMN$X.NAI.LAB.STATUS)
Lab_Status = table(T_COVID_SPCMN$X.NAI.LAB.STATUS)
print(Lab_Status)

X.NAI.LAB.STATUS = T_COVID_SPCMN$X.NAI.LAB.STATUS
X.NAI.SPECIMEN.STATUS = T_COVID_SPCMN$X.NAI.SPECIMEN.STATUS

# Check for discrepancies
discrepancies = X.NAI.LAB.STATUS != X.NAI.SPECIMEN.STATUS

# which rows have discrepancies
which(discrepancies)

# Total number of discrepancies
sum(discrepancies)


df_Trends = T_COVID_SPCMN[, c("LAB","X.NAI.DATE...COLLECTED..EMR.")]

# Assuming your dataframe is named 'df' and you've already read your data into R
# First, convert the date-time column to POSIXct
df_Trends$X.NAI.DATE...COLLECTED..EMR. = as.POSIXct(df_Trends$X.NAI.DATE...COLLECTED..EMR.,
                                          format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")


# Aggregate data for overall daily trends
daily_trends_overall <- df_Trends %>%
  mutate(Date = as.Date(X.NAI.DATE...COLLECTED..EMR.)) %>%
  group_by(Date) %>%
  summarise(Daily_Count = n(), .groups = "drop")

# Aggregate data for daily trends per lab
daily_trends_lab = df_Trends %>%
  mutate(Date = as.Date(X.NAI.DATE...COLLECTED..EMR.)) %>%
  group_by(Date, LAB) %>%
  summarise(Daily_Count = n(), .groups = "drop")

# Aggregate data for overall weekly trends
weekly_trends_overall = df_Trends %>%
  mutate(Week = floor_date(X.NAI.DATE...COLLECTED..EMR., unit = "week")) %>%
  group_by(Week) %>%
  summarise(Weekly_Count = n(), .groups = "drop")

# Aggregate data for weekly trends per lab
weekly_trends_lab = df_Trends %>%
  mutate(Week = floor_date(X.NAI.DATE...COLLECTED..EMR., unit = "week")) %>%
  group_by(Week, LAB) %>%
  summarise(Weekly_Count = n(), .groups = "drop")

weekly_trends_lab <- weekly_trends_lab %>%
  mutate(
    Week = as.Date(Week),  # Make sure the 'Week' column is of Date type
    Month = floor_date(Week, unit = "month"),
    Week_Number = week(Week) - week(Month) + 1
  ) %>%
  arrange(Month, Week)  # Arrange the data by month and week

# Create a label for the x-axis that combines the month name and the week number
weekly_trends_lab$Week_Label <- with(weekly_trends_lab, paste(format(Month, "%B"), "Week", Week_Number))

# Aggregate data for overall monthly trends
monthly_trends_overall = df_Trends %>%
  mutate(Month = floor_date(X.NAI.DATE...COLLECTED..EMR., unit = "month")) %>%
  group_by(Month) %>%
  summarise(Monthly_Count = n(), .groups = "drop")

# Aggregate data for monthly trends per lab
monthly_trends_lab = df_Trends %>%
  mutate(Month = floor_date(X.NAI.DATE...COLLECTED..EMR., unit = "month")) %>%
  group_by(Month, LAB) %>%
  summarise(Monthly_Count = n(), .groups = "drop")

if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")
library(viridis)


ggplot(daily_trends_overall, aes(x=Date, y=Daily_Count)) +
  geom_line() +
  labs(title="Overall Daily Test Collection Trends")


# Plot the trend of weekly counts for each week and each lab
ggplot(weekly_trends_lab, aes(x=Week_Label, y=Weekly_Count, fill=LAB)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Weekly Test Collection Trends per Lab",
       x="Week",
       y="Count of Test Collections") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) 


ggplot(monthly_trends_overall, aes(x=Month, y=Monthly_Count)) +
  geom_line() +
  labs(title="Overall Monthly Test Collection Trends")

# And monthly trends per lab
ggplot(monthly_trends_lab, aes(x=Month, y=Monthly_Count, fill=LAB)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_viridis(discrete = TRUE, option = "D") +  # Use viridis for discrete color scale
  labs(title="Monthly Test Collection Trends per Lab",
       x="Month",
       y="Count of Test Collections") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 



T_COVID_SPCMN$X.NAI.DATE...LAST.UPDATE = as.POSIXct(T_COVID_SPCMN$X.NAI.DATE...LAST.UPDATE, format = "%m/%d/%Y %I:%M:%S %p")
T_COVID_SPCMN$Year = format(T_COVID_SPCMN$X.NAI.DATE...LAST.UPDATE, "%Y")
unique_years = unique(T_COVID_SPCMN$Year)
print(unique_years)


