SPCMN=read.csv(file.choose(), sep=",", header=T)

library('dplyr')
library('ggplot2')
library('tidyverse')

str(SPCMN)
colnames(SPCMN)

SPCMN$Lab.Id = as.factor(SPCMN$Lab.Id)
SPCMN$TEST_RESULT = as.factor(SPCMN$TEST_RESULT)
levels(SPCMN$Lab.Id)
levels(SPCMN$TEST_RESULT)



Lab.Id = SPCMN$Lab.Id
Lab.Id.LAB1 - SPCMN$LAB_ID..LAB1.

# Check for discrepancies
discrepancies = Lab.Id != Lab.Id.LAB1

# which rows have discrepancies
which(discrepancies)

# Total number of discrepancies
sum(discrepancies)


Tech.Id = SPCMN$Tech.Id
TECH_ID.TEST_RESULT1 = SPCMN$TECH_ID..TEST_RESULT1.

# Check for discrepancies
discrepancies = Tech.Id != TECH_ID.TEST_RESULT1

# which rows have discrepancies
which(discrepancies)

# Total number of discrepancies
sum(discrepancies)

# Number of missing values in each column
colSums(is.na(SPCMN))

library("lubridate")

SPCMN$TS = mdy_hms(SPCMN$TS)
SPCMN$TS..TEST_RESULT1. = mdy_hms(SPCMN$TS..TEST_RESULT1.)

# Calculate the time difference and add it as a new column
SPCMN$diff_TS = SPCMN$TS..TEST_RESULT1. - SPCMN$TS
write.csv(SPCMN, file = "C:/Users/skasaiesharifi/Documents/test.csv", row.names = FALSE)

SPCMN_NEW=read.csv(file.choose(), sep=",", header=T)
min(SPCMN_NEW$diff_TS, na.rm = TRUE)
max(SPCMN_NEW$diff_TS, na.rm = TRUE)

all_same = length(unique(SPCMN_NEW$TS..LAB1.)) == 1

if (all_same) {
  print("All values in the column are the same.")
} else {
  print("Not all values in the column are the same.")
}

unique_values = unique(SPCMN_NEW$TS..LAB1.)
print(unique_values)

value_counts = table(SPCMN$TS..LAB1.)
value_counts_df = as.data.frame(value_counts)
names(value_counts_df) = c("Value", "Frequency")
value_counts_df$Percentage = (value_counts_df$Frequency / sum(value_counts_df$Frequency)) * 100
value_counts_df = value_counts_df[order(-value_counts_df$Percentage),]
print(value_counts_df)

all_same = length(unique(SPCMN$SPECIMEN_BARCODE)) == 1

if (all_same) {
  print("All values in the column are the same.")
} else {
  print("Not all values in the column are the same.")
}

value_counts = table(SPCMN$TEST_RESULT)
value_counts_df = as.data.frame(value_counts)
names(value_counts_df) = c("Value", "Frequency")
value_counts_df$Percentage = (value_counts_df$Frequency / sum(value_counts_df$Frequency)) * 100
value_counts_df = value_counts_df[order(-value_counts_df$Percentage),]
print(value_counts_df)


lab_tech_table = SPCMN %>%
  group_by(Lab.Name) %>%
  summarise(Tech.Ids = toString(sort(unique(Tech.Id)))) %>%
  ungroup()

write.csv(lab_tech_table, file = "C:/Users/skasaiesharifi/Documents/lab_tech.csv", row.names = FALSE)

lab_tech_count = SPCMN %>%
  group_by(Lab.Name) %>%
  summarise(TotalTechIds = n_distinct(Tech.Id)) %>%
  ungroup()

print(lab_tech_count)

specimen_count_per_lab = SPCMN %>%
  group_by(Lab.Name, Lab.Id) %>%
  summarise(TotalSpecimens = n()) %>%
  ungroup()

print(specimen_count_per_lab)

test_results_count = SPCMN %>%
  group_by(Lab.Id, TEST_RESULT) %>%
  summarise(Count = n(), .groups = 'drop')  # .groups = 'drop' to remove the grouping structure

print(test_results_count)


SAMPLE_TEST_RESULT_ID = SPCMN$SAMPLE_TEST_RESULT_ID
SPECIMEN_ID.TEST_RESULT1 = SPCMN$SPECIMEN_ID..TEST_RESULT1.

# Check for discrepancies
discrepancies = SAMPLE_TEST_RESULT_ID != SPECIMEN_ID.TEST_RESULT1

# which rows have discrepancies
which(discrepancies)

# Total number of discrepancies
sum(discrepancies)
