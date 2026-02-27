## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# load clean cross sectional data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# check levels of each variable
levels_list <- lapply(sw_combined_clean, unique)

# Convert to a data frame suitable for Excel
max_length <- max(sapply(levels_list, length))
levels_df <- as.data.frame(
  lapply(levels_list, function(x) {
    length(x) <- max_length
    x
  }),
  stringsAsFactors = FALSE
)

levels(sw_combined_clean$ngo_access_lifetime)

# Write to Excel
write_xlsx(levels_df, "sw_combined_clean_levels.xlsx")

## prepare longitudinal data 

# load appended clean data
sw_data_linkage <- read_excel("SW IBBS linkage.xlsx")

# check date formatting 
sw_combined_clean %>%
  filter(year %in% c(2013, 2015, 2017, 2021)) %>%
  select(year, interview_dte) %>%
  group_by(year) %>%
  slice_head(n = 20) %>%
  ungroup() %>%
  print(n = Inf)

# convert to date from character
sw_combined_clean <- sw_combined_clean %>%
  filter(year %in% c(2013, 2015, 2017, 2021)) %>%
  mutate(interview_dte = as.Date(interview_dte))  

# convert IDs to character
sw_data_linkage <- sw_data_linkage %>%
  mutate(across(ends_with("_id"), as.character),
         id = as.character(id))

# pivot linkage to long
linkage_long <- sw_data_linkage %>%
  pivot_longer(
    cols = ends_with("_id"),
    names_to = "year_column",
    values_to = "year_id"
  ) %>%
  mutate(
    year = case_when(
      year_column == "2013_id" ~ 2013,
      year_column == "2015_id" ~ 2015,
      year_column == "2017_id" ~ 2017,
      year_column == "2021_id" ~ 2021
    )
  ) %>%
  filter(!is.na(year_id))

# combine linkage keys and cleaned data
sw_data_long <- linkage_long %>%
  left_join(
    sw_combined_clean,
    by = c("year" = "year", "year_id" = "id")
  ) %>%
  arrange(id, year)

# check number of unique participants
length(unique(sw_data_long$id))
nrow(sw_data_long)

sw_data_long %>%
  count(hiv_test_rslt_bin)

# id sequence variable
sw_data_long <- sw_data_long %>%
  group_by(id) %>%
  mutate(id_seq = row_number()) %>%
  ungroup()

# check HIV test results at first visit
first_hiv_test <- sw_data_long %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  slice_min(interview_dte, n = 1) %>%
  ungroup()

# negative and positive first test
first_hiv_test %>%
  count(hiv_test_rslt_bin)

# IDs negative at first visit
negative_first_ids <- first_hiv_test %>%
  filter(hiv_test_rslt_bin == "Negative") %>%
  pull(id)

# define incidence dataset
sw_negative_cohort <- sw_data_long %>%
  filter(id %in% negative_first_ids) %>%
  arrange(id, year)

length(unique(sw_negative_cohort$id))
table(sw_negative_cohort$year)

# negative and positive first test
sw_negative_cohort %>%
  count(hiv_test_rslt_bin)

# save incidence dataset 
saveRDS(sw_negative_cohort, "sw_long_raw.rds")
write_xlsx(sw_negative_cohort, "sw_long_raw.xlsx")

# load incidence dataset
sw_long_raw <- readRDS("sw_long_raw.rds")

# sequence by date and id
sw_long_raw <- sw_long_raw %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  mutate(visit_number = row_number()) %>%
  ungroup()

# lag hiv_test_reslt and lab_test_dte
sw_negative_cohort <- sw_negative_cohort %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(
    hiv_test_rslt_start = lag(hiv_test_rslt_bin),
    interview_dte_start = lag(interview_dte),
    hiv_test_rslt_end = hiv_test_rslt_bin,
    interview_dte_end = interview_dte,
    visit_number = row_number()
  ) %>%
  ungroup()

# remove rows where participant tested positive twice
sum(sw_negative_cohort$hiv_test_rslt_start == "Positive" & 
    sw_negative_cohort$hiv_test_rslt_end == "Positive", na.rm = TRUE)

sw_negative_cohort <- sw_negative_cohort %>%
  filter(!(hiv_test_rslt_start == "Positive" & hiv_test_rslt_end == "Positive"))

# remove first row of each study_id
sw_negative_cohort <- sw_negative_cohort %>%
  filter(visit_number != 1)

# days at risk
sw_negative_cohort <- sw_negative_cohort %>%
  mutate(
    days_risk = ifelse(
      hiv_test_rslt_end == "Positive" & hiv_test_rslt_start == "Negative",
      as.numeric(interview_dte_end - interview_dte_start) / 2,
      as.numeric(interview_dte_end - interview_dte_start)
    )
  )

# check for negative days_risk
sum(sw_negative_cohort$days_risk < 0, na.rm = TRUE)
sum(sw_negative_cohort$days_risk == 0, na.rm = TRUE)

# remove the missing outcome row
sw_negative_cohort <- sw_negative_cohort %>%
  filter(!is.na(hiv_test_rslt_end))
  
# Count incident HIV cases (assuming each "Positive" is an incident case)
num_incident_cases_hiv <- sum(sw_negative_cohort$hiv_test_rslt_end == "Positive", na.rm = TRUE)

# Calculate total person-time in years
total_person_years_hiv <- sum(sw_negative_cohort$days_risk, na.rm = TRUE) / 365.25

# Incidence rate per 100 person-years
incidence_rate_hiv <- (num_incident_cases_hiv / total_person_years_hiv) * 100

num_incident_cases_hiv
total_person_years_hiv
incidence_rate_hiv

# calculate person-years
sw_negative_cohort <- sw_negative_cohort %>%
    mutate(py = days_risk/365.25)

# save incidence dataset
saveRDS(sw_negative_cohort, "sw_incidence_dataset.rds")

## violence incidence

# rape 

# load appended clean data
sw_data_linkage <- read_excel("SW IBBS linkage.xlsx")

# check date formatting 
sw_combined_clean %>%
  filter(year %in% c(2013, 2015, 2017, 2021)) %>%
  select(year, interview_dte) %>%
  group_by(year) %>%
  slice_head(n = 20) %>%
  ungroup() %>%
  print(n = Inf)

# convert to date from character
sw_combined_clean <- sw_combined_clean %>%
  filter(year %in% c(2013, 2015, 2017, 2021)) %>%
  mutate(interview_dte = as.Date(interview_dte))  

# convert IDs to character
sw_data_linkage <- sw_data_linkage %>%
  mutate(across(ends_with("_id"), as.character),
         id = as.character(id))

# pivot linkage to long
linkage_long <- sw_data_linkage %>%
  pivot_longer(
    cols = ends_with("_id"),
    names_to = "year_column",
    values_to = "year_id"
  ) %>%
  mutate(
    year = case_when(
      year_column == "2013_id" ~ 2013,
      year_column == "2015_id" ~ 2015,
      year_column == "2017_id" ~ 2017,
      year_column == "2021_id" ~ 2021
    )
  ) %>%
  filter(!is.na(year_id))

# combine linkage keys and cleaned data
sw_data_long <- linkage_long %>%
  left_join(
    sw_combined_clean,
    by = c("year" = "year", "year_id" = "id")
  ) %>%
  arrange(id, year)

# id sequence variable
sw_data_long <- sw_data_long %>%
  group_by(id) %>%
  mutate(id_seq = row_number()) %>%
  ungroup()

# --------------------------------------------------
# DEFINE BASELINE (FIRST VISIT) FOR VIOLENCE
# --------------------------------------------------

first_violence <- sw_data_long %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  slice_min(interview_dte, n = 1) %>%
  ungroup()

# check baseline violence
first_violence %>%
  count(violence_rape_ever)

# IDs with NO rape at first visit
no_rape_first_ids <- first_violence %>%
  filter(violence_rape_ever == "No") %>%
  pull(id)

# define incidence cohort (violence-free at baseline)
sw_negative_cohort <- sw_data_long %>%
  filter(id %in% no_rape_first_ids) %>%
  arrange(id, year)

# --------------------------------------------------
# CREATE START–END STRUCTURE
# --------------------------------------------------

sw_negative_cohort <- sw_negative_cohort %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  mutate(
    visit_number = row_number(),
    rape_start = lag(violence_rape_ever),
    interview_dte_start = lag(interview_dte),
    rape_end = violence_rape_ever,
    interview_dte_end = interview_dte
  ) %>%
  ungroup()

# remove first visit (no prior interval)
sw_negative_cohort <- sw_negative_cohort %>%
  filter(visit_number != 1)

# remove rows where rape already Yes at start and end
sw_negative_cohort <- sw_negative_cohort %>%
  filter(!(rape_start == "Yes" & rape_end == "Yes"))

# remove missing outcome
sw_negative_cohort <- sw_negative_cohort %>%
  filter(!is.na(rape_end))

# --------------------------------------------------
# CALCULATE DAYS AT RISK
# --------------------------------------------------

sw_negative_cohort <- sw_negative_cohort %>%
  mutate(
    days_risk = ifelse(
      rape_end == "Yes" & rape_start == "No",
      as.numeric(interview_dte_end - interview_dte_start) / 2,
      as.numeric(interview_dte_end - interview_dte_start)
    )
  )

# check
sum(sw_negative_cohort$days_risk < 0, na.rm = TRUE)
sum(sw_negative_cohort$days_risk == 0, na.rm = TRUE)

# --------------------------------------------------
# INCIDENT VIOLENCE CALCULATION
# --------------------------------------------------

num_incident_cases_rape <- sum(sw_negative_cohort$rape_end == "Yes", na.rm = TRUE)

total_person_years_rape <- sum(sw_negative_cohort$days_risk, na.rm = TRUE) / 365.25

incidence_rate_rape <- (num_incident_cases_rape / total_person_years_rape) * 100

num_incident_cases_rape
total_person_years_rape
incidence_rate_rape

# --------------------------------------------------
# PREPARE FOR COX MODELS
# --------------------------------------------------

vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat", "ngo_access_lifetime_3cat", "alcohol_30d_bin", "city_travel_12m_cat",
  "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_beaten_ever",
  "violence_physical_abuse_ever", "violence_police", "used_syringe_last_3cat", "underage_first_sw_bin"
)

# binary outcome for Cox
sw_negative_cohort$rape_bin <- ifelse(sw_negative_cohort$rape_end == "Yes", 1, 0)

# recode exposures to binary
sw_negative_cohort <- sw_negative_cohort %>%
  mutate(across(all_of(vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"), levels = c("No", "Yes"))
  ))

# calculate person-years
sw_negative_cohort <- sw_negative_cohort %>%
  mutate(py = days_risk / 365.25)

# save dataset
saveRDS(sw_negative_cohort, "sw_incident_rape_dataset.rds")
write_xlsx(sw_negative_cohort, "sw_incident_rape_dataset.xlsx")

# beating 

# load appended clean data
sw_data_linkage <- read_excel("SW IBBS linkage.xlsx")

# check date formatting 
sw_combined_clean %>%
  filter(year %in% c(2013, 2015, 2017, 2021)) %>%
  select(year, interview_dte) %>%
  group_by(year) %>%
  slice_head(n = 20) %>%
  ungroup() %>%
  print(n = Inf)

# convert to date from character
sw_combined_clean <- sw_combined_clean %>%
  filter(year %in% c(2013, 2015, 2017, 2021)) %>%
  mutate(interview_dte = as.Date(interview_dte))  

# convert IDs to character
sw_data_linkage <- sw_data_linkage %>%
  mutate(across(ends_with("_id"), as.character),
         id = as.character(id))

# pivot linkage to long
linkage_long <- sw_data_linkage %>%
  pivot_longer(
    cols = ends_with("_id"),
    names_to = "year_column",
    values_to = "year_id"
  ) %>%
  mutate(
    year = case_when(
      year_column == "2013_id" ~ 2013,
      year_column == "2015_id" ~ 2015,
      year_column == "2017_id" ~ 2017,
      year_column == "2021_id" ~ 2021
    )
  ) %>%
  filter(!is.na(year_id))

# combine linkage keys and cleaned data
sw_data_long <- linkage_long %>%
  left_join(
    sw_combined_clean,
    by = c("year" = "year", "year_id" = "id")
  ) %>%
  arrange(id, year)

# id sequence variable
sw_data_long <- sw_data_long %>%
  group_by(id) %>%
  mutate(id_seq = row_number()) %>%
  ungroup()

# --------------------------------------------------
# DEFINE BASELINE (FIRST VISIT) FOR VIOLENCE
# --------------------------------------------------

first_violence <- sw_data_long %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  slice_min(interview_dte, n = 1) %>%
  ungroup()

# check baseline violence
first_violence %>%
  count(violence_beaten_ever)

# IDs with NO beating at first visit
no_beating_first_ids <- first_violence %>%
  filter(violence_beaten_ever == "No") %>%
  pull(id)

# define incidence cohort (violence-free at baseline)
sw_negative_cohort <- sw_data_long %>%
  filter(id %in% no_beating_first_ids) %>%
  arrange(id, year)

# --------------------------------------------------
# CREATE START–END STRUCTURE
# --------------------------------------------------

sw_negative_cohort <- sw_negative_cohort %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  mutate(
    visit_number = row_number(),
    beating_start = lag(violence_beaten_ever),
    interview_dte_start = lag(interview_dte),
    beating_end = violence_beaten_ever,
    interview_dte_end = interview_dte
  ) %>%
  ungroup()

# remove first visit (no prior interval)
sw_negative_cohort <- sw_negative_cohort %>%
  filter(visit_number != 1)

# remove rows where beating already Yes at start and end
sw_negative_cohort <- sw_negative_cohort %>%
  filter(!(beating_start == "Yes" & beating_end == "Yes"))

# remove missing outcome
sw_negative_cohort <- sw_negative_cohort %>%
  filter(!is.na(beating_end))

# --------------------------------------------------
# CALCULATE DAYS AT RISK
# --------------------------------------------------

sw_negative_cohort <- sw_negative_cohort %>%
  mutate(
    days_risk = ifelse(
      beating_end == "Yes" & beating_start == "No",
      as.numeric(interview_dte_end - interview_dte_start) / 2,
      as.numeric(interview_dte_end - interview_dte_start)
    )
  )

# check
sum(sw_negative_cohort$days_risk < 0, na.rm = TRUE)
sum(sw_negative_cohort$days_risk == 0, na.rm = TRUE)

# --------------------------------------------------
# INCIDENT VIOLENCE CALCULATION
# --------------------------------------------------

num_incident_cases_beating <- sum(sw_negative_cohort$beating_end == "Yes", na.rm = TRUE)

total_person_years_beating <- sum(sw_negative_cohort$days_risk, na.rm = TRUE) / 365.25

incidence_rate_beating <- (num_incident_cases_beating / total_person_years_beating) * 100

num_incident_cases_beating
total_person_years_beating
incidence_rate_beating

# --------------------------------------------------
# PREPARE FOR COX MODELS
# --------------------------------------------------

vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat", "ngo_access_lifetime_3cat", "alcohol_30d_bin", "city_travel_12m_cat",
  "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever",
  "violence_physical_abuse_ever", "violence_police", "used_syringe_last_3cat", "underage_first_sw_bin"
)

# binary outcome for Cox
sw_negative_cohort$beating_bin <- ifelse(sw_negative_cohort$beating_end == "Yes", 1, 0)

# recode exposures to binary
sw_negative_cohort <- sw_negative_cohort %>%
  mutate(across(all_of(vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"), levels = c("No", "Yes"))
  ))

# calculate person-years
sw_negative_cohort <- sw_negative_cohort %>%
  mutate(py = days_risk / 365.25)

# save dataset
saveRDS(sw_negative_cohort, "sw_incident_beating_dataset.rds")
write_xlsx(sw_negative_cohort, "sw_incident_beating_dataset.xlsx")

# condom use 

# load appended clean data
sw_data_linkage <- read_excel("SW IBBS linkage.xlsx")

# check date formatting 
sw_combined_clean %>%
  filter(year %in% c(2013, 2015, 2017, 2021)) %>%
  select(year, interview_dte) %>%
  group_by(year) %>%
  slice_head(n = 20) %>%
  ungroup() %>%
  print(n = Inf)

# convert to date from character
sw_combined_clean <- sw_combined_clean %>%
  filter(year %in% c(2013, 2015, 2017, 2021)) %>%
  mutate(interview_dte = as.Date(interview_dte))  

# convert IDs to character
sw_data_linkage <- sw_data_linkage %>%
  mutate(across(ends_with("_id"), as.character),
         id = as.character(id))

# pivot linkage to long
linkage_long <- sw_data_linkage %>%
  pivot_longer(
    cols = ends_with("_id"),
    names_to = "year_column",
    values_to = "year_id"
  ) %>%
  mutate(
    year = case_when(
      year_column == "2013_id" ~ 2013,
      year_column == "2015_id" ~ 2015,
      year_column == "2017_id" ~ 2017,
      year_column == "2021_id" ~ 2021
    )
  ) %>%
  filter(!is.na(year_id))

# combine linkage keys and cleaned data
sw_data_long <- linkage_long %>%
  left_join(
    sw_combined_clean,
    by = c("year" = "year", "year_id" = "id")
  ) %>%
  arrange(id, year)

# id sequence variable
sw_data_long <- sw_data_long %>%
  group_by(id) %>%
  mutate(id_seq = row_number()) %>%
  ungroup()

# --------------------------------------------------
# DEFINE BASELINE (FIRST VISIT) FOR VIOLENCE
# --------------------------------------------------

# define inconsistent condom use 
sw_data_long <- sw_data_long %>%
    inconsistent_condom_use = case_when(
      client_condom_lastsex_3cat == "No"  ~ 1,
      client_condom_lastsex_3cat == "Yes" ~ 0,
      client_condom_lastsex_3cat == "Missing / Unknown" ~ NA_real_
    )
    
first_violence <- sw_data_long %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  slice_min(interview_dte, n = 1) %>%
  ungroup()

# check baseline violence
first_violence %>%
  count(violence_beaten_ever)

# IDs with NO beating at first visit
no_beating_first_ids <- first_violence %>%
  filter(violence_beaten_ever == "No") %>%
  pull(id)

# define incidence cohort (violence-free at baseline)
sw_negative_cohort <- sw_data_long %>%
  filter(id %in% no_beating_first_ids) %>%
  arrange(id, year)

# --------------------------------------------------
# CREATE START–END STRUCTURE
# --------------------------------------------------

sw_negative_cohort <- sw_negative_cohort %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  mutate(
    visit_number = row_number(),
    beating_start = lag(violence_beaten_ever),
    interview_dte_start = lag(interview_dte),
    beating_end = violence_beaten_ever,
    interview_dte_end = interview_dte
  ) %>%
  ungroup()

# remove first visit (no prior interval)
sw_negative_cohort <- sw_negative_cohort %>%
  filter(visit_number != 1)

# remove rows where beating already Yes at start and end
sw_negative_cohort <- sw_negative_cohort %>%
  filter(!(beating_start == "Yes" & beating_end == "Yes"))

# remove missing outcome
sw_negative_cohort <- sw_negative_cohort %>%
  filter(!is.na(beating_end))

# --------------------------------------------------
# CALCULATE DAYS AT RISK
# --------------------------------------------------

sw_negative_cohort <- sw_negative_cohort %>%
  mutate(
    days_risk = ifelse(
      beating_end == "Yes" & beating_start == "No",
      as.numeric(interview_dte_end - interview_dte_start) / 2,
      as.numeric(interview_dte_end - interview_dte_start)
    )
  )

# check
sum(sw_negative_cohort$days_risk < 0, na.rm = TRUE)
sum(sw_negative_cohort$days_risk == 0, na.rm = TRUE)

# --------------------------------------------------
# INCIDENT VIOLENCE CALCULATION
# --------------------------------------------------

num_incident_cases_beating <- sum(sw_negative_cohort$beating_end == "Yes", na.rm = TRUE)

total_person_years_beating <- sum(sw_negative_cohort$days_risk, na.rm = TRUE) / 365.25

incidence_rate_beating <- (num_incident_cases_beating / total_person_years_beating) * 100

num_incident_cases_beating
total_person_years_beating
incidence_rate_beating

# --------------------------------------------------
# PREPARE FOR COX MODELS
# --------------------------------------------------

vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat", "ngo_access_lifetime_3cat", "alcohol_30d_bin", "city_travel_12m_cat",
  "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever",
  "violence_physical_abuse_ever", "violence_police", "used_syringe_last_3cat", "underage_first_sw_bin"
)

# binary outcome for Cox
sw_negative_cohort$beating_bin <- ifelse(sw_negative_cohort$beating_end == "Yes", 1, 0)

# recode exposures to binary
sw_negative_cohort <- sw_negative_cohort %>%
  mutate(across(all_of(vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"), levels = c("No", "Yes"))
  ))

# calculate person-years
sw_negative_cohort <- sw_negative_cohort %>%
  mutate(py = days_risk / 365.25)

# save dataset
saveRDS(sw_negative_cohort, "sw_incident_beating_dataset.rds")
write_xlsx(sw_negative_cohort, "sw_incident_beating_dataset.xlsx")
