## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# load clean cross sectional data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# check levels of each variable
levels_list <- lapply(sw_combined_clean, unique)

# convert to df
max_length <- max(sapply(levels_list, length))
levels_df <- as.data.frame(
  lapply(levels_list, function(x) {
    length(x) <- max_length
    x
  }),
  stringsAsFactors = FALSE
)

levels(sw_combined_clean$ngo_access_lifetime)

# save
write_xlsx(levels_df, "sw_combined_clean_levels.xlsx")

## prepare longitudinal data 

# load appended clean data
sw_data_linkage <- read_excel("SW IBBS linkage.xlsx")
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# indicator for skipped years
sw_data_linkage <- sw_data_linkage %>%
  mutate(
    w2013 = !is.na(`2013_id`),
    w2015 = !is.na(`2015_id`),
    w2017 = !is.na(`2017_id`),
    w2021 = !is.na(`2021_id`)
  )

skip_flags <- sw_data_linkage %>%
  group_by(UIC) %>%
  summarise(
    p2013 = any(w2013),
    p2015 = any(w2015),
    p2017 = any(w2017),
    p2021 = any(w2021),
    .groups = "drop"
  ) %>%
  mutate(
    skip_2015 = p2013 & !p2015 & (p2017 | p2021),
    skip_2017 = (p2013 | p2015) & !p2017 & p2021,
    skip_any = skip_2015 | skip_2017
  ) %>%
  select(UIC, skip_any)

# attach skip flag
sw_data_linkage <- sw_data_linkage %>%
  left_join(skip_flags, by = "UIC")

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

# delete those with missing survey years
sw_data_long <- sw_data_long %>%
  filter(skip_any == "FALSE")

# check number of unique participants
length(unique(sw_data_long$id))
nrow(sw_data_long)

# id sequence variable
sw_data_long <- sw_data_long %>%
  group_by(id) %>%
  mutate(id_seq = row_number()) %>%
  ungroup()

saveRDS(sw_data_long, "sw_data_long.rds")

## HIV incidence analysis

# load data
sw_data_long_hiv <- readRDS("sw_data_long.rds")

# tab positive and negative tests
sw_data_long_hiv %>%
  count(hiv_test_rslt_bin) # 94 positives

# number of unique ids with positive hiv test 
sw_data_long_hiv %>%
  filter(hiv_test_rslt_bin == "Positive") %>%
  distinct(id) %>%
  count() # 60 unique ids with a positive test

# check HIV test results at first visit
first_hiv_test <- sw_data_long_hiv %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  slice_min(interview_dte, n = 1) %>%
  ungroup()

# negative and positive first test
first_hiv_test %>%
  count(hiv_test_rslt_bin) ## 36 positive at first test

# IDs negative at first visit
negative_first_ids <- first_hiv_test %>%
  filter(hiv_test_rslt_bin == "Negative") %>%
  pull(id)

# define incidence dataset
sw_negative_cohort_hiv <- sw_data_long %>%
  filter(id %in% negative_first_ids) %>%
  arrange(id, year)

# tab positive and negative tests
sw_negative_cohort_hiv %>%
  count(hiv_test_rslt_bin) # 26 positives

# number of unique ids with positive hiv test 
sw_negative_cohort_hiv %>%
  filter(hiv_test_rslt_bin == "Positive") %>%
  distinct(id) %>%
  count() # 24 unique ids with a positive test

length(unique(sw_negative_cohort_hiv$id))
table(sw_negative_cohort_hiv$year)

# negative and positive first test
sw_negative_cohort_hiv %>%
  count(hiv_test_rslt_bin)

# sequence by date and id
sw_negative_cohort_hiv <- sw_negative_cohort_hiv %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  mutate(visit_number = row_number()) %>%
  ungroup()

# lag hiv_test_reslt and lab_test_dte
sw_negative_cohort_hiv <- sw_negative_cohort_hiv %>%
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
sum(sw_negative_cohort_hiv$hiv_test_rslt_start == "Positive" & 
    sw_negative_cohort_hiv$hiv_test_rslt_end == "Positive", na.rm = TRUE)

sw_negative_cohort_hiv <- sw_negative_cohort_hiv %>%
  filter(!(hiv_test_rslt_start == "Positive" & hiv_test_rslt_end == "Positive"))

# remove first row of each study_id
sw_negative_cohort_hiv <- sw_negative_cohort_hiv %>%
  filter(visit_number != 1)

# days at risk
sw_negative_cohort_hiv <- sw_negative_cohort_hiv %>%
  mutate(
    days_risk = ifelse(
      hiv_test_rslt_end == "Positive" & hiv_test_rslt_start == "Negative", ## if positive at end of f/u
      as.numeric(interview_dte_end - interview_dte_start) / 2, ## then divide time at risk in half
      as.numeric(interview_dte_end - interview_dte_start) ## otherwise leave as is
    )
  )

# check for negative days_risk
sum(sw_negative_cohort_hiv$days_risk < 0, na.rm = TRUE)
sum(sw_negative_cohort_hiv$days_risk == 0, na.rm = TRUE)

# remove the missing outcome row
sw_negative_cohort_hiv <- sw_negative_cohort_hiv %>%
  filter(!is.na(hiv_test_rslt_end))
  
# incident HIV cases
num_incident_cases_hiv <- sum(sw_negative_cohort_hiv$hiv_test_rslt_end == "Positive", na.rm = TRUE)

# total person-time in years
total_person_years_hiv <- sum(sw_negative_cohort_hiv$days_risk, na.rm = TRUE) / 365.25

# incidence rate per 100 person-years
incidence_rate_hiv <- (num_incident_cases_hiv / total_person_years_hiv) * 100

num_incident_cases_hiv
total_person_years_hiv
incidence_rate_hiv

# calculate person-years
sw_negative_cohort_hiv <- sw_negative_cohort_hiv %>%
    mutate(py = days_risk/365.25)

# save incidence dataset
saveRDS(sw_negative_cohort_hiv, "sw_incidence_hiv_dataset.rds")

## rape incidence analysis

# load data
sw_data_long_rape <- readRDS("sw_data_long.rds")

# check rape at first visit
first_rape <- sw_data_long_rape %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  slice_min(interview_dte, n = 1) %>%
  ungroup()

# check baseline violence
first_rape %>%
  count(violence_rape_ever)

# IDs with no rape at first visit
no_rape_first_ids <- first_rape %>%
  filter(violence_rape_ever == "No") %>%
  pull(id)

# define incidence cohort (violence-free at baseline)
sw_norape_cohort <- sw_data_long_rape %>%
  filter(id %in% no_rape_first_ids) %>%
  arrange(id, year)

# lag rape date and interview date
sw_norape_cohort <- sw_norape_cohort %>%
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

# remove first visit
sw_norape_cohort <- sw_norape_cohort %>%
  filter(visit_number != 1)

# remove rows where rape yes at start and yes at end
#sw_norape_cohort <- sw_norape_cohort %>%
#  filter(!(rape_start == "Yes" & rape_end == "Yes"))

# remove missing outcome
sw_norape_cohort <- sw_norape_cohort %>%
  filter(!is.na(rape_end))

# days at risk
sw_norape_cohort <- sw_norape_cohort %>%
  mutate(
    days_risk = ifelse(
      rape_end == "Yes" & rape_start == "No", ## if positive at end of f/u
      as.numeric(interview_dte_end - interview_dte_start) / 2, ## then divide time at risk in half
      as.numeric(interview_dte_end - interview_dte_start) ## otherwise leave as is
    )
  )

# check
sum(sw_norape_cohort$days_risk < 0, na.rm = TRUE)
sum(sw_norape_cohort$days_risk == 0, na.rm = TRUE)

# incident rape cases
num_incident_cases_rape <- sum(sw_norape_cohort$rape_end == "Yes", na.rm = TRUE)

# total person time in years
total_person_years_rape <- sum(sw_norape_cohort$days_risk, na.rm = TRUE) / 365.25

# incidence rate per 100 person-years
incidence_rate_rape <- (num_incident_cases_rape / total_person_years_rape) * 100

num_incident_cases_rape
total_person_years_rape
incidence_rate_rape

# calculate person-years
sw_norape_cohort <- sw_norape_cohort %>%
  mutate(py = days_risk / 365.25)

# save dataset
saveRDS(sw_norape_cohort, "sw_incident_rape_dataset.rds")
write_xlsx(sw_norape_cohort, "sw_incident_rape_dataset.xlsx")

## beating analysis

# load data
sw_data_long_beating <- readRDS("sw_data_long.rds")

# check beating at first visit
first_beating <- sw_data_long_beating %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  slice_min(interview_dte, n = 1) %>%
  ungroup()

# check baseline violence
first_beating %>%
  count(violence_beaten_ever) # 121 beaten at first visit

# IDs with no beating at first visit
no_beating_first_ids <- first_beating %>%
  filter(violence_beaten_ever == "No") %>%
  pull(id)

# define incidence cohort (no beating at baseline)
sw_nobeating_cohort <- sw_data_long_beating %>%
  filter(id %in% no_beating_first_ids) %>%
  arrange(id, year)

# lag beating date and interview date
sw_nobeating_cohort <- sw_nobeating_cohort %>%
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

# remove first visit
sw_nobeating_cohort <- sw_nobeating_cohort %>%
  filter(visit_number != 1)

# remove rows where beating already Yes at start and end
sw_nobeating_cohort <- sw_nobeating_cohort %>%
  filter(!(beating_start == "Yes" & beating_end == "Yes"))

# remove missing outcome
sw_nobeating_cohort <- sw_nobeating_cohort %>%
  filter(!is.na(beating_end))

# days at risk
sw_nobeating_cohort <- sw_nobeating_cohort %>%
  mutate(
    days_risk = ifelse(
      beating_end == "Yes" & beating_start == "No", ## if positive at end of f/u
      as.numeric(interview_dte_end - interview_dte_start) / 2, ## then divide time at risk in half
      as.numeric(interview_dte_end - interview_dte_start) ## otherwise leave as is
    )
  )
# check
sum(sw_nobeating_cohort$days_risk < 0, na.rm = TRUE)
sum(sw_nobeating_cohort$days_risk == 0, na.rm = TRUE)

# incident cases of beating
num_incident_cases_beating <- sum(sw_nobeating_cohort$beating_end == "Yes", na.rm = TRUE)

# time at risk 
total_person_years_beating <- sum(sw_nobeating_cohort$days_risk, na.rm = TRUE) / 365.25

# incident rate
incidence_rate_beating <- (num_incident_cases_beating / total_person_years_beating) * 100

num_incident_cases_beating
total_person_years_beating
incidence_rate_beating

# calculate person-years
sw_nobeating_cohort <- sw_nobeating_cohort %>%
  mutate(py = days_risk / 365.25)

# save dataset
saveRDS(sw_nobeating_cohort, "sw_incident_beating_dataset.rds")
write_xlsx(sw_nobeating_cohort, "sw_incident_beating_dataset.xlsx")

# condom use 

# load data
sw_data_long_condom <- readRDS("sw_data_long.rds")

# define inconsistent condom use
sw_data_long_condom <- sw_data_long_condom %>%
    mutate(
      condom_lastsex_bin = case_when(
      client_condom_lastsex_3cat == "No"  ~ "Yes",
      client_condom_lastsex_3cat == "Yes" ~ "No",
      client_condom_lastsex_3cat == "Missing / Unknown" ~ NA_character_
    ))

# check past 30 days
table(sw_data_long_condom$condom_lastsex_bin, sw_data_long_condom$year)

# check condom use at first visit
first_condom <- sw_data_long_condom %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  slice_min(interview_dte, n = 1) %>%
  ungroup()

# check baseline condom use prevalence
first_condom %>%
  count(condom_lastsex_bin)

# ids with inconsistent condom use at first visit
no_condom_first_ids <- first_condom %>%
  filter(condom_lastsex_bin == 1) %>%
  pull(id)

# define condom use incidence cohort
sw_nocondom_cohort <- sw_data_long_condom %>%
  filter(id %in% no_condom_first_ids) %>%
  arrange(id, year)

# lag between end and start 
sw_nocondom_cohort <- sw_nocondom_cohort %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  mutate(
    visit_number = row_number(),
    condom_start = lag(condom_lastsex_bin),
    interview_dte_start = lag(interview_dte),
    condom_end = condom_lastsex_bin,
    interview_dte_end = interview_dte
  ) %>%
  ungroup()

# remove first visit (no prior interval)
sw_nocondom_cohort <- sw_nocondom_cohort %>%
  filter(visit_number != 1)

# remove rows where condom use is inconsistent at start and end
sw_nocondom_cohort <- sw_nocondom_cohort %>%
  filter(!(condom_start == "Yes" & condom_end == "Yes"))

# remove missing outcome
sw_nocondom_cohort <- sw_nocondom_cohort %>%
  filter(!is.na(condom_end))

# days at risk
sw_nocondom_cohort <- sw_nocondom_cohort %>%
  mutate(
    days_risk = ifelse(
      condom_end == "Yes" & condom_start == "No", ## if positive at end of f/u
      as.numeric(interview_dte_end - interview_dte_start) / 2, ## then divide time at risk in half
      as.numeric(interview_dte_end - interview_dte_start) ## otherwise leave as is
    )
  )

# check
sum(sw_nocondom_cohort$days_risk < 0, na.rm = TRUE)
sum(sw_nocondom_cohort$days_risk == 0, na.rm = TRUE)

# incident rape cases
num_incident_cases_condom <- sum(sw_nocondom_cohort$condom_end == "Yes", na.rm = TRUE)

# total person time in years
total_person_years_condom <- sum(sw_nocondom_cohort$days_risk, na.rm = TRUE) / 365.25

# incidence rate per 100 person-years
incidence_rate_condom <- (num_incident_cases_condom / total_person_years_condom) * 100

num_incident_cases_condom
total_person_years_condom
incidence_rate_condom

# save dataset
saveRDS(sw_negative_cohort, "sw_incident_condom_dataset.rds")
write_xlsx(sw_negative_cohort, "sw_incident_condom_dataset.xlsx")

## injecting drug use analysis

# load data
sw_data_long_idu <- readRDS("sw_data_long.rds")

# check past 12 months
table(sw_data_long_idu$idu_12m_3cat, sw_data_long_idu$source_year)

# check lifetime
table(sw_data_long_idu$idu_ever_3cat, sw_data_long_idu$source_year)

# check rape at first visit
first_idu <- sw_data_long_idu %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  slice_min(interview_dte, n = 1) %>%
  ungroup()

# check baseline violence
first_idu %>%
  count(idu_ever_3cat)

# IDs with no injecting at first visit
no_idu_first_ids <- first_idu %>%
  filter(idu_ever_3cat == "No") %>%
  pull(id)

# define incidence cohort (violence-free at baseline)
sw_noidu_cohort <- sw_data_long_idu %>%
  filter(id %in% no_idu_first_ids) %>%
  arrange(id, year)

# lag rape date and interview date
sw_noidu_cohort <- sw_noidu_cohort %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  mutate(
    visit_number = row_number(),
    idu_start = lag(idu_ever_3cat),
    interview_dte_start = lag(interview_dte),
    idu_end = idu_ever_3cat,
    interview_dte_end = interview_dte
  ) %>%
  ungroup()

# remove first visit
sw_noidu_cohort <- sw_noidu_cohort %>%
  filter(visit_number != 1)

# remove rows where idu yes at start and yes at end
sw_noidu_cohort <- sw_noidu_cohort %>%
  filter(!(idu_start == "Yes" & idu_end == "Yes"))

# remove missing outcome
sw_noidu_cohort <- sw_noidu_cohort %>%
  filter(!is.na(idu_end))

# days at risk
sw_noidu_cohort <- sw_noidu_cohort %>%
  mutate(
    days_risk = ifelse(
      idu_end == "Yes" & idu_start == "No", ## if positive at end of f/u
      as.numeric(interview_dte_end - interview_dte_start) / 2, ## then divide time at risk in half
      as.numeric(interview_dte_end - interview_dte_start) ## otherwise leave as is
    )
  )

# check
sum(sw_noidu_cohort$days_risk < 0, na.rm = TRUE)
sum(sw_noidu_cohort$days_risk == 0, na.rm = TRUE)

# incident rape cases
num_incident_cases_idu <- sum(sw_noidu_cohort$idu_end == "Yes", na.rm = TRUE)

# total person time in years
total_person_years_idu <- sum(sw_noidu_cohort$days_risk, na.rm = TRUE) / 365.25

# incidence rate per 100 person-years
incidence_rate_idu <- (num_incident_cases_idu / total_person_years_idu) * 100

num_incident_cases_idu
total_person_years_idu
incidence_rate_idu

# calculate person-years
sw_noidu_cohort <- sw_noidu_cohort %>%
  mutate(py = days_risk / 365.25)

# save dataset
saveRDS(sw_noidu_cohort, "sw_incident_idu_dataset.rds")
write_xlsx(sw_noidu_cohort, "sw_incident_idu_dataset.xlsx")


## steet-based sw analysis

# load data
first_sb <- readRDS("sw_data_long.rds")

# baseline = street (Yes)
ids_sb_baseline <- first_sb %>%
  filter(street_sw_bin == "Yes") %>%
  pull(id)

# cohort
cohort_sb <- sw_data_long_sb %>%
  filter(id %in% ids_sb_baseline) %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  mutate(
    visit_number = row_number(),
    sb_start = lag(street_sw_bin),
    sb_end = street_sw_bin,
    t_start = lag(interview_dte),
    t_end = interview_dte
  ) %>%
  ungroup() %>%
  filter(visit_number != 1) %>%
  filter(sb_start == "Yes") %>%
  filter(!is.na(sb_end))

# event: Yes → No
cohort_sb <- cohort_sb %>%
  mutate(
    event = ifelse(sb_start == "Yes" & sb_end == "No", 1, 0),
    days_risk = ifelse(
      event == 1,
      as.numeric(t_end - t_start) / 2,
      as.numeric(t_end - t_start)
    ),
    py = days_risk / 365.25
  )

# rate
events_sb_to_indoor <- sum(cohort_sb$event)
py_sb_to_indoor <- sum(cohort_sb$py)
rate_sb_to_indoor <- (events_sb_to_indoor / py_sb_to_indoor) * 100

events_sb_to_indoor
py_sb_to_indoor
rate_sb_to_indoor

# calculate person-years
cohort_sb <- cohort_sb %>%
  mutate(py = days_risk / 365.25)

# save dataset
saveRDS(cohort_sb, "sw_incident_nosb_dataset.rds")
write_xlsx(cohort_sb, "sw_incident_nosb_dataset.xlsx")

# load data
sw_data_long_sb <- readRDS("sw_data_long.rds")

# baseline state
first_sb <- sw_data_long_sb %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  slice_min(interview_dte, n = 1) %>%
  ungroup()

# baseline = indoor (No)
ids_indoor_baseline <- first_sb %>%
  filter(street_sw_bin == "No") %>%
  pull(id)

# cohort
cohort_indoor <- sw_data_long_sb %>%
  filter(id %in% ids_indoor_baseline) %>%
  arrange(id, interview_dte) %>%
  group_by(id) %>%
  mutate(
    visit_number = row_number(),
    sb_start = lag(street_sw_bin),
    sb_end = street_sw_bin,
    t_start = lag(interview_dte),
    t_end = interview_dte
  ) %>%
  ungroup() %>%
  filter(visit_number != 1) %>%
  filter(sb_start == "No") %>%
  filter(!is.na(sb_end))

# event: No → Yes
cohort_indoor <- cohort_indoor %>%
  mutate(
    event = ifelse(sb_start == "No" & sb_end == "Yes", 1, 0),
    days_risk = ifelse(
      event == 1,
      as.numeric(t_end - t_start) / 2,
      as.numeric(t_end - t_start)
    ),
    py = days_risk / 365.25
  )

# rate
events_indoor_to_sb <- sum(cohort_indoor$event)
py_indoor_to_sb <- sum(cohort_indoor$py)
rate_indoor_to_sb <- (events_indoor_to_sb / py_indoor_to_sb) * 100
events_indoor_to_sb
py_indoor_to_sb
rate_indoor_to_sb

# calculate person-years
sw_sb_cohort <- sw_sb_cohort %>%
  mutate(py = days_risk / 365.25)

# save dataset
saveRDS(sw_sb_cohort, "sw_incident_sb_dataset.rds")
write_xlsx(sw_sb_cohort, "sw_incident_sb_dataset.xlsx")






















calc_transition_rate <- function(data,
                                 id_var = "id",
                                 date_var = "interview_dte",
                                 state_var,
                                 baseline_state,
                                 start_state,
                                 end_state,
                                 scale = 100,
                                 save_data = FALSE,
                                 rds_name = NULL,
                                 xlsx_name = NULL) {

  id <- sym(id_var)
  date <- sym(date_var)
  state <- sym(state_var)
  
  # baseline
  first <- data %>%
    arrange(!!id, !!date) %>%
    group_by(!!id) %>%
    slice_min(!!date, n = 1) %>%
    ungroup()
  
  baseline_ids <- first %>%
    filter(!!state == baseline_state) %>%
    pull(!!id)
  
  # cohort
  cohort <- data %>%
    filter((!!id) %in% baseline_ids) %>%
    arrange(!!id, !!date) %>%
    group_by(!!id) %>%
    mutate(
      visit_number = row_number(),
      state_start = lag(!!state),
      state_end = !!state,
      t_start = lag(!!date),
      t_end = !!date
    ) %>%
    ungroup() %>%
    filter(visit_number != 1) %>%
    filter(state_start == start_state) %>%
    filter(!is.na(state_end)) %>%
    mutate(
      event = ifelse(state_start == start_state & state_end == end_state, 1, 0),
      days_risk = ifelse(
        event == 1,
        as.numeric(t_end - t_start) / 2,
        as.numeric(t_end - t_start)
      ),
      py = days_risk / 365.25
    )
  
  # results
  events <- sum(cohort$event, na.rm = TRUE)
  person_years <- sum(cohort$py, na.rm = TRUE)
  rate <- (events / person_years) * scale
  
  # print results
  cat("\n--- Transition summary ---\n")
  cat("From:", start_state, "→ To:", end_state, "\n")
  cat("Baseline:", baseline_state, "\n")
  cat("Events:", events, "\n")
  cat("Person-years:", round(person_years, 2), "\n")
  cat("Rate per", scale, "PY:", round(rate, 3), "\n\n")
  
  # save
  if (save_data) {
    if (!is.null(rds_name)) saveRDS(cohort, rds_name)
    if (!is.null(xlsx_name)) write_xlsx(cohort, xlsx_name)
  }
  
  return(list(
    events = events,
    person_years = person_years,
    rate = rate,
    cohort = cohort
  ))
}

## hiv incidence
hiv_incidence <- calc_transition_rate(
  data = sw_data_long,
  state_var = "hiv_test_rslt_bin",
  baseline_state = "Negative",
  start_state = "Negative",
  end_state = "Positive",
  save_data = TRUE,
  rds_name = "sw_incidence_hiv_dataset.rds",
  xlsx_name = "sw_incidence_hiv_dataset.xlsx"
)

res_indoor_to_sb <- calc_transition_rate(
  data = sw_data_long,
  state_var = "street_sw_bin",
  baseline_state = "No",
  start_state = "No",
  end_state = "Yes",
  save_data = TRUE,
  rds_name = "sw_incident_sb_dataset.rds",
  xlsx_name = "sw_incident_sb_dataset.xlsx"
)
