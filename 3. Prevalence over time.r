## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales, openxlsx, readr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# Load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# factor
sw_combined_clean <- sw_combined_clean %>%
  mutate(idu_ever_3cat = factor(idu_ever_3cat, levels = c("No", "Yes")),
        hiv_test_rslt_bin = case_when(
          hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ "Yes",
          hiv_test_rslt_bin %in% c("Negative","0","No") ~ "No",
          TRUE ~ NA_character_),
        years_in_sw_3cat = as.factor(years_in_sw_3cat))

# Variables to summarize
vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat", "ngo_access_lifetime_3cat",
  "ngo_condom_12m_bin", "ngo_syringe_12m_bin", "hiv_test_rslt_bin", "years_in_sw_3cat",
  "city_travel_12m_cat", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "violence_any_ever_3cat", "violence_rape_12m_3cat",
  "violence_rape_ever", "violence_beaten_ever", "violence_physical_abuse_ever",
  "violence_police", "violence_pimp", "violence_support_ngo", "age_bin",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence",
  "avoided_healthcare_12m_police", "avoided_hiv_test_12m_police",
  "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

# Add "Overall" pseudo-city
sw_combined_clean <- bind_rows(sw_combined_clean, sw_combined_clean %>% mutate(city = "Overall"))

sw_combined_clean <- bind_rows(
  sw_combined_clean,
  sw_combined_clean %>% mutate(street_sw_bin = "Overall")
)

sw_combined_clean <- sw_combined_clean %>%
  mutate(street_sw_bin = factor(street_sw_bin, levels = c("No", "Yes", "Overall")))

# Create a workbook
wb <- createWorkbook()

for (var in vars) {
  
  cat("Processing:", var, "\n")
  
  # Summarise by city/year and IDU status
  summary_df <- sw_combined_clean %>%
    filter(!is.na(.data[[var]]), !is.na(street_sw_bin)) %>%
    group_by(city, year, street_sw_bin) %>%
    summarise(
      IDU_n = sum(.data[[var]] == "Yes" & idu_ever_3cat == "Yes", na.rm = TRUE),
      IDU_denom = sum(idu_ever_3cat == "Yes", na.rm = TRUE),
      IDU_pct = ifelse(IDU_denom > 0, round(IDU_n/IDU_denom*100,1), NA),
      
      No_IDU_n = sum(.data[[var]] == "Yes" & idu_ever_3cat == "No", na.rm = TRUE),
      No_IDU_denom = sum(idu_ever_3cat == "No", na.rm = TRUE),
      No_IDU_pct = ifelse(No_IDU_denom > 0, round(No_IDU_n/No_IDU_denom*100,1), NA),
      
      Overall_n = sum(.data[[var]] == "Yes", na.rm = TRUE),
      Overall_denom = n(),
      Overall_pct = round(Overall_n/Overall_denom*100,1),
      
      .groups = "drop"
    ) %>%
    arrange(city, year, street_sw_bin)
  
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

## hiv prevalence tables#

# Load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# Keep only selected cities
cities_keep <- c("Cherkasy","Dnipro", "Kropyvnytskyi","Kyiv", "Mariupol","Odesa")

sw_combined_clean <- sw_combined_clean %>%
  filter(city %in% cities_keep)

# Clean variables
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    idu_ever_3cat = factor(idu_ever_3cat, levels = c("No", "Yes")),
    street_sw_bin = factor(street_sw_bin, levels = c("No", "Yes")),
    hiv = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    )
  )

# Helper function to return "n/N (pct)"
calc_str <- function(x) {
  N <- sum(!is.na(x))
  n <- sum(x == 1, na.rm = TRUE)
  if (N == 0) return(NA_character_)
  paste0(n, "/", N, " (", round(n/N*100, 1), "%)")
}

# Create HIV table (by city)
hiv_table <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(
    Overall = calc_str(hiv),

    Street = calc_str(hiv[street_sw_bin == "Yes"]),
    Indoor = calc_str(hiv[street_sw_bin == "No"]),

    IDU = calc_str(hiv[idu_ever_3cat == "Yes"]),
    Non_IDU = calc_str(hiv[idu_ever_3cat == "No"]),

    Street_IDU = calc_str(hiv[street_sw_bin == "Yes" & idu_ever_3cat == "Yes"]),
    Street_non_IDU = calc_str(hiv[street_sw_bin == "Yes" & idu_ever_3cat == "No"]),

    Indoor_IDU = calc_str(hiv[street_sw_bin == "No" & idu_ever_3cat == "Yes"]),
    Indoor_non_IDU = calc_str(hiv[street_sw_bin == "No" & idu_ever_3cat == "No"]),

    .groups = "drop"
  )

# Create overall table (all cities combined)
overall_table <- sw_combined_clean %>%
  group_by(year) %>%
  summarise(
    Overall = calc_str(hiv),

    Street = calc_str(hiv[street_sw_bin == "Yes"]),
    Indoor = calc_str(hiv[street_sw_bin == "No"]),

    IDU = calc_str(hiv[idu_ever_3cat == "Yes"]),
    Non_IDU = calc_str(hiv[idu_ever_3cat == "No"]),

    Street_IDU = calc_str(hiv[street_sw_bin == "Yes" & idu_ever_3cat == "Yes"]),
    Street_non_IDU = calc_str(hiv[street_sw_bin == "Yes" & idu_ever_3cat == "No"]),

    Indoor_IDU = calc_str(hiv[street_sw_bin == "No" & idu_ever_3cat == "Yes"]),
    Indoor_non_IDU = calc_str(hiv[street_sw_bin == "No" & idu_ever_3cat == "No"]),

    .groups = "drop"
  )

# Create Excel workbook
wb <- createWorkbook()

# Write one sheet per city
for (ct in cities_keep) {
  
  df_city <- hiv_table %>%
    filter(city == ct) %>%
    select(-city)
  
  sheet_name <- substr(ct, 1, 31)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df_city)
}

# Add overall sheet
addWorksheet(wb, "Overall")
writeData(wb, "Overall", overall_table)

# Save workbook
saveWorkbook(wb, "HIV_prevalence_by_city.xlsx", overwrite = TRUE)

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

## ngo access over time

sw_combined_clean <- readRDS("sw_combined_clean.rds")

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ngo_access_lifetime_3cat = case_when(
      ngo_access_lifetime_3cat %in% c("Yes", "1") ~ "Yes",
      ngo_access_lifetime_3cat %in% c("No", "0") ~ "No",
      TRUE ~ NA_character_
    )
  )

city_prev <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(ngo_access_lifetime_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(ngo_access_lifetime_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(ngo_access_lifetime_3cat)) / n_total,
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
    no_prop = sum(ngo_access_lifetime_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(ngo_access_lifetime_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(ngo_access_lifetime_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "Overall",
    city = "All cities",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

ngo_prev <- bind_rows(city_prev, overall_prev) %>%
  arrange(level, city, year)

wb <- createWorkbook()
addWorksheet(wb, "ngo_prevalence")
writeData(wb, "ngo_prevalence", ngo_prev)
saveWorkbook(wb, "ngo_prevalence_by_city_year.xlsx", overwrite = TRUE)
