TCOVID=read.csv(file.choose(), sep=",", header=T)

SPECIMN=read.csv(file.choose(), sep=",", header=T)

total_missing_values = sum(is.na(Data))
missing_values_per_column = colSums(is.na(Data))
missing_values_df <- as.data.frame(missing_values_per_column)

frequency_table <- table(Data$LAB)
frequency_table = as.data.frame(frequency_table)

frequency_table2 <- table(Data$LOCATION)
frequency_table2 = as.data.frame(frequency_table2)

install.packages("dplyr")
library(dplyr)
install.packages("tidyr")

result_df = Data %>%
  group_by(LAB) %>%
  summarise(All_LOCATION = toString(unique(LOCATION)))


write.csv(result_df, file = "result.csv", row.names = FALSE)