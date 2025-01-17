flowsh_1 = read.csv(file.choose(), sep=",", header=T)

flowsh_1$adm_date_time = gsub("\\[|\\]", "", flowsh_1$adm_date_time) # Remove brackets
flowsh_1$disch_date_time = gsub("\\[|\\]", "", flowsh_1$disch_date_time)
flowsh_1$adm_date_time = sub(":.*", "", flowsh_1$adm_date_time) # Remove time and everything after
flowsh_1$disch_date_time = sub(":.*", "", flowsh_1$disch_date_time)

# Convert to Date object with the original format and then to the desired format
flowsh_1$adm_date_time = format(as.Date(flowsh_1$adm_date_time, format="%Y/%m/%d"), "%m/%d/%Y")
flowsh_1$disch_date_time = format(as.Date(flowsh_1$disch_date_time, format="%Y/%m/%d"), "%m/%d/%Y")


flowsh_1 = flowsh_1 %>%
  mutate(first_icu_dt = mdy(first_icu_dt)) %>%  # Convert to Date format
  filter(st == "IL", 
         year(first_icu_dt) %in% c(2020, 2021))


#flowsh_1$disc_date_time <- NULL
