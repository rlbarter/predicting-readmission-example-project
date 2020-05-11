# load the original data

library(tidyverse)
# load in the data
data_orig <- read_csv("../data/raw_data/diabetic_data.csv")

# convert ? to NA
data_clean <- data_orig %>%
  na_if("?")

# remove features with more than 35% values missing
vars_missing <- data_clean %>% 
  map_dbl(~sum(is.na(.)) / length(.))
vars_keep <- names(vars_missing[vars_missing < 0.35])
data_clean <- data_clean %>%
  select(all_of(vars_keep))

# remove features that have a single unique value
vars_unique <- data_clean %>% 
  map_dbl(n_distinct) 
vars_keep <- names(vars_unique[vars_unique != 1])
data_clean <- data_clean %>%
  select(all_of(vars_keep))

# remove diagnosis codes
data_clean <- data_clean %>%
  select(-diag_1, -diag_2, -diag_3)

# convert age to numeric
data_clean <- data_clean %>%
  mutate(age = case_when(age == "[0-10)" ~ 5,
                         age == "[10-20)" ~ 15,
                         age == "[20-30)" ~ 25,
                         age == "[30-40)" ~ 35,
                         age == "[40-50)" ~ 45,
                         age == "[50-60)" ~ 55,
                         age == "[60-70)" ~ 65,
                         age == "[70-80)" ~ 75,
                         age == "[80-90)" ~ 85,
                         age == "[90-100)" ~ 95))

# convert "unknown/invalid" gender entry to NA
data_clean <- data_clean %>%
  mutate(gender = if_else(gender == "Unknown/Invalid", 
                          as.character(NA), gender))

# convert admission_type to meaningful categories
# note that (4) newborn and (7) trauma have 10 and 21 entries, respectively,
# so I'm going to convert them to NA
data_clean <- data_clean %>%
  mutate(admission_type_id = case_when(admission_type_id == 1 ~ "emergency",
                                       admission_type_id == 2 ~ "urgent",
                                       admission_type_id == 3 ~ "elective",
                                       admission_type_id == 4 ~ as.character(NA), # "newborn"
                                       admission_type_id == 5 ~ as.character(NA),
                                       admission_type_id == 6 ~ as.character(NA),
                                       admission_type_id == 7 ~ as.character(NA), #"trauma"
                                       admission_type_id == 8 ~ as.character(NA)))

# convert discharge_disposition_id to meaningful categories
# I'm purposely leaving out some categories because I want them to be NA
# I'm also purposely grouping some categories together
data_clean <- data_clean %>%
  mutate(discharge_disposition_id = case_when(discharge_disposition_id == 1 ~ "home",
                                              discharge_disposition_id == 2 ~ "hospital",
                                              discharge_disposition_id == 3 ~ "skilled nursing facility",
                                              discharge_disposition_id == 4 ~ "intermediate care facility",
                                              discharge_disposition_id == 5 ~ "other inpatient care facility",
                                              discharge_disposition_id == 6 ~ "home with health service",
                                              discharge_disposition_id == 7 ~ "left against medical advice",
                                              discharge_disposition_id == 8 ~ "home with health service",
                                              discharge_disposition_id == 11 ~ "deceased",
                                              discharge_disposition_id == 13 ~ "hospice",
                                              discharge_disposition_id == 14 ~ "hospice",
                                              discharge_disposition_id == 15 ~ "hospital",
                                              discharge_disposition_id == 16 ~ "hospital",
                                              discharge_disposition_id == 17 ~ "hospital",
                                              discharge_disposition_id == 22 ~ "hospital",
                                              discharge_disposition_id == 23 ~ "hospital",
                                              discharge_disposition_id == 24 ~ "skilled nursing facility",
                                              discharge_disposition_id == 28 ~ "hospital"))

# remove patients who are deceased (because of course they can't be readmitted)
data_clean <- data_clean %>%
  filter(discharge_disposition_id != "deceased")

# convert admission_source_id to meaningful categories
data_clean <- data_clean %>%
  mutate(admission_source_id = case_when(admission_source_id == 1 ~ "phsyician",
                                         admission_source_id == 2 ~ "clinic",
                                         admission_source_id %in% c(3, 8, 11, 12, 13, 14, 18, 19) ~ "other",
                                         admission_source_id %in% c(4, 6, 10, 22, 25) ~ "hospital",
                                         admission_source_id == 5 ~ "skilled nursing facility",
                                         admission_source_id == 7 ~ "emergency room"))

# convert readmitted to binary for readmitted in 30 days
data_clean <- data_clean %>%
  mutate(readmitted = case_when(readmitted == "NO" ~ "not readmitted",
                                readmitted == ">30" ~ "not readmitted",
                                readmitted == "<30" ~ "readmitted"))



write_csv(data_clean, "../data/processed_data/diabetic_data_clean.csv")
