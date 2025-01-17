# Create a complete grid of all months and zipcodes
all_combinations <- expand.grid(
  zipcode = unique(ICU_UPDATED_2$zipcode),
  Date = format(seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "1 month"), "%Y-%m")
)

# Ensure zipcode types match
all_combinations$zipcode <- as.character(all_combinations$zipcode)
ICU_UPDATED_2$zipcode <- as.character(ICU_UPDATED_2$zipcode)

# Perform a full join to get a complete dataset
ICU_UPDATED_2 <- full_join(all_combinations, ICU_UPDATED_2, by = c("zipcode", "Date"))

write.csv(ICU_UPDATED_2, file = "R:/mtootooni/217107/Shield Data/Alireza/Shield/Data/ICU/ICU_UPDATED_2.csv", row.names = FALSE)


ICU_UPDATED_2 = fread(file.choose(), sep=",", header=T)

Dataset2 = fread(file.choose(), sep=",", header=T)

ICU_UPDATED_2$zipcode <- as.character(ICU_UPDATED_2$zipcode)
Dataset2$zipcode <- as.character(Dataset2$zipcode)

ICU_UPDATED_2 <- merge(ICU_UPDATED_2, Dataset2, by.x = "zipcode", by.y = "zipcode", all.x = TRUE)

ICU_UPDATED_2 = ICU_UPDATED_2[,-c(16:36)]



book1 = read.csv(file.choose(), sep=",", header=T)

book1$Date = as.factor(book1$Date)

book1$Date = relevel(book1$Date, ref = "Jan-21")


skewness_ICU_rate = skewness(book1$ICU.Hospitalization.Rate, type = 3)
print(skewness_ICU_rate)


ICU_rate_GLMMmodel_1 = lmer(log(ICU.Hospitalization.Rate + 1) ~ Date +  ADI_STATERNK + Total_SHIELD_centers_per_zipcode_per_month + 
                              + ADI_STATERNK*Total_SHIELD_centers_per_zipcode_per_month + (1|zipcode) , data = book1)

tab_model(ICU_rate_GLMMmodel_1)
