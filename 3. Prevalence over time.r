## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales, openxlsx, readr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# Load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# Ensure idu_ever_3cat is a factor
sw_combined_clean <- sw_combined_clean %>%
  mutate(idu_ever_3cat = factor(idu_ever_3cat, levels = c("No", "Yes")))

# Variables to summarize
vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat", "ngo_access_lifetime_3cat",
  "city_travel_12m_cat", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "sw_partners_total_24h_5cat", "violence_any_ever_3cat", "violence_rape_12m_3cat",
  "violence_rape_ever", "violence_beaten_ever", "violence_physical_abuse_ever",
  "violence_police", "violence_pimp", "violence_support_ngo", "age_bin",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence",
  "avoided_healthcare_12m_police", "avoided_hiv_test_12m_police",
  "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

# Add "Overall" pseudo-city
sw_combined_clean <- bind_rows(sw_combined_clean, sw_combined_clean %>% mutate(city = "Overall"))

# Create a workbook
wb <- createWorkbook()

for (var in vars) {
  
  cat("Processing:", var, "\n")
  
  # Summarize by city/year and IDU status
  summary_df <- sw_combined_clean %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(city, year) %>%
    summarise(
      IDU_n = sum(.data[[var]] == "Yes" & idu_ever_3cat == "Yes", na.rm = TRUE),
      IDU_denom = sum(idu_ever_3cat == "Yes", na.rm = TRUE),
      IDU = ifelse(IDU_denom > 0,
                   paste0(IDU_n, " (", round(IDU_n/IDU_denom*100,1), "%)"), NA),
      
      No_IDU_n = sum(.data[[var]] == "Yes" & idu_ever_3cat == "No", na.rm = TRUE),
      No_IDU_denom = sum(idu_ever_3cat == "No", na.rm = TRUE),
      No_IDU = ifelse(No_IDU_denom > 0,
                      paste0(No_IDU_n, " (", round(No_IDU_n/No_IDU_denom*100,1), "%)"), NA),
      
      Overall_n = sum(.data[[var]] == "Yes", na.rm = TRUE),
      Overall_denom = n(),
      Overall = paste0(Overall_n, " (", round(Overall_n/Overall_denom*100,1), "%)"),
      
      .groups = "drop"
    ) %>%
    arrange(city, year) %>%
    select(city, year, IDU, No_IDU, Overall)
  
  # Name the sheet as first 10 characters of variable
  sheet_name <- substr(var, 1, 25)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, summary_df)
}

# Save workbook
saveWorkbook(wb, "IDU_stratified_summary.xlsx", overwrite = TRUE)
cat("Saved all summaries to IDU_stratified_summary.xlsx\n")

## idu over time

sw_combined_clean <- readRDS("sw_combined_clean.rds")

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    idu_ever_3cat = case_when(
      idu_ever_3cat %in% c("Yes", "1") ~ "Yes",
      idu_ever_3cat %in% c("No", "0") ~ "No",
      TRUE ~ NA_character_
    )
  )

city_prev <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(idu_ever_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(idu_ever_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(idu_ever_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "City",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

overall_prev <- sw_combined_clean %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(idu_ever_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(idu_ever_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(idu_ever_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "Overall",
    city = "All cities",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

idu_prev <- bind_rows(city_prev, overall_prev) %>%
  arrange(level, city, year)

wb <- createWorkbook()
addWorksheet(wb, "idu_prevalence")
writeData(wb, "idu_prevalence", idu_prev)
saveWorkbook(wb, "idu_prevalence_by_city_year.xlsx", overwrite = TRUE)

## idu 12m

# Load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    idu_12m_3cat = case_when(
      idu_12m_3cat %in% c("Yes", "1") ~ "Yes",
      idu_12m_3cat %in% c("No", "0") ~ "No",
      TRUE ~ NA_character_
    )
  )

city_prev <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(idu_12m_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(idu_12m_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(idu_12m_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "City",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

overall_prev <- sw_combined_clean %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(idu_12m_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(idu_12m_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(idu_12m_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "Overall",
    city = "All cities",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

idu_12m_prev <- bind_rows(city_prev, overall_prev) %>%
  arrange(level, city, year)

wb <- createWorkbook()
addWorksheet(wb, "idu_12m_prevalence")
writeData(wb, "idu_12m_prevalence", idu_12m_prev)
saveWorkbook(wb, "idu_12m_prevalence_by_city_year.xlsx", overwrite = TRUE)