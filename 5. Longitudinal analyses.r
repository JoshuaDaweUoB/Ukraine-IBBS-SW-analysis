## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

## rate ratios

# load long hiv data
sw_negative_cohort <- readRDS("sw_incidence_dataset.rds")

sw_negative_cohort <- sw_negative_cohort %>%
  mutate(
    hiv_test_rslt_bin = ifelse(hiv_test_rslt_bin == "Positive", 1, 0),
    hiv_test_rslt_bin = as.numeric(hiv_test_rslt_bin)
  )

# ensure adjustment variables are factors
sw_negative_cohort <- sw_negative_cohort %>%
  mutate(
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )


# -----------------------------
# DEFINE EXPOSURE VARIABLES
# -----------------------------

exposure_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_any_ever_3cat",
  "violence_beaten_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo",
  "sw_partners_total_24h_5cat",
  "sw_partners_clients_30d_4cat"
)

# recode true binary Yes/No variables
binary_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_any_ever_3cat",
  "violence_beaten_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo"
)

sw_negative_cohort <- sw_negative_cohort %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# -----------------------------
# RUN REGION-ADJUSTED COX MODELS
# -----------------------------

results_list <- list()

for (var in exposure_vars) {
  
  # make sure exposure is factor
  sw_negative_cohort[[var]] <- as.factor(sw_negative_cohort[[var]])
  
  formula <- as.formula(
    paste("Surv(py, hiv_test_rslt_bin) ~", var, "+ ukraine_region + year")
  )
  
  model <- coxph(formula, data = sw_negative_cohort)
  
  tidy_mod <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # keep only exposure rows
  tidy_mod <- tidy_mod %>%
    filter(grepl(paste0("^", var), term))
  
  # get all exposure levels
  levels_var <- levels(sw_negative_cohort[[var]])
  
  for (lev in levels_var) {
    
    subset_data <- sw_negative_cohort %>%
      filter(!is.na(.data[[var]]),
             .data[[var]] == lev)
    
    cases <- sum(subset_data$hiv_test_rslt_bin == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    
    # reference level
    if (lev == levels_var[1]) {
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = 1,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years
      )
      
    } else {
      
      row_match <- tidy_mod %>%
        filter(grepl(lev, term))
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = row_match$estimate,
        CI_lower = row_match$conf.low,
        CI_upper = row_match$conf.high,
        Cases = cases,
        Person_Years = person_years
      )
    }
  }
}

results_df <- bind_rows(results_list)

write_xlsx(results_df, "cox_model_results_hiv.xlsx")

## rape incidence

# Load dataset
sw_negative_cohort_rape <- readRDS("sw_incident_rape_dataset.rds")

# Ensure correct types
sw_negative_cohort_rape <- sw_negative_cohort_rape %>%
  mutate(
    rape_bin = as.numeric(rape_bin),
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# -----------------------------
# DEFINE EXPOSURE VARIABLES
# -----------------------------

exposure_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_beaten_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo",
  "sw_partners_total_24h_5cat",
  "sw_partners_clients_30d_4cat"
)

# recode true binary Yes/No variables
binary_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_beaten_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo"
)

sw_negative_cohort_rape <- sw_negative_cohort_rape %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# -----------------------------
# RUN REGION-ADJUSTED COX MODELS
# -----------------------------

results_list <- list()

for (var in exposure_vars) {
  
  # make sure exposure is factor
  sw_negative_cohort_rape[[var]] <- as.factor(sw_negative_cohort_rape[[var]])
  
  formula <- as.formula(
    paste("Surv(py, rape_bin) ~", var, "+ ukraine_region + year")
  )
  
  model <- coxph(formula, data = sw_negative_cohort_rape)
  
  tidy_mod <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # keep only exposure rows
  tidy_mod <- tidy_mod %>%
    filter(grepl(paste0("^", var), term))
  
  # get all exposure levels
  levels_var <- levels(sw_negative_cohort_rape[[var]])
  
  for (lev in levels_var) {
    
    subset_data <- sw_negative_cohort_rape %>%
      filter(!is.na(.data[[var]]),
             .data[[var]] == lev)
    
    cases <- sum(subset_data$rape_bin == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    
    # reference level
    if (lev == levels_var[1]) {
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = 1,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years
      )
      
    } else {
      
      row_match <- tidy_mod %>%
        filter(grepl(lev, term))
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = row_match$estimate,
        CI_lower = row_match$conf.low,
        CI_upper = row_match$conf.high,
        Cases = cases,
        Person_Years = person_years
      )
    }
  }
}


results_df <- bind_rows(results_list)

write_xlsx(results_df, "cox_model_results_rape.xlsx")

## beating incidence

# Load dataset
sw_negative_cohort_beating <- readRDS("sw_incident_beating_dataset.rds")

# Ensure correct types
sw_negative_cohort_beating <- sw_negative_cohort_beating %>%
  mutate(
    beating_bin = as.numeric(beating_bin),
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# -----------------------------
# DEFINE EXPOSURE VARIABLES
# -----------------------------

exposure_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_rape_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo",
  "sw_partners_total_24h_5cat",
  "sw_partners_clients_30d_4cat"
)

# recode true binary Yes/No variables
binary_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_rape_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo"
)

sw_negative_cohort_beating <- sw_negative_cohort_beating %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# -----------------------------
# RUN REGION-ADJUSTED COX MODELS
# -----------------------------

results_list <- list()

for (var in exposure_vars) {
  
  # make sure exposure is factor
  sw_negative_cohort_beating[[var]] <- as.factor(sw_negative_cohort_beating[[var]])
  
  formula <- as.formula(
    paste("Surv(py, beating_bin) ~", var, "+ ukraine_region + year")
  )
  
  model <- coxph(formula, data = sw_negative_cohort_beating)
  
  tidy_mod <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # keep only exposure rows
  tidy_mod <- tidy_mod %>%
    filter(grepl(paste0("^", var), term))
  
  # get all exposure levels
  levels_var <- levels(sw_negative_cohort_beating[[var]])
  
  for (lev in levels_var) {
    
    subset_data <- sw_negative_cohort_beating %>%
      filter(!is.na(.data[[var]]),
             .data[[var]] == lev)
    
    cases <- sum(subset_data$beating_bin == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    
    # reference level
    if (lev == levels_var[1]) {
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = 1,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years
      )
      
    } else {
      
      row_match <- tidy_mod %>%
        filter(grepl(lev, term))
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = row_match$estimate,
        CI_lower = row_match$conf.low,
        CI_upper = row_match$conf.high,
        Cases = cases,
        Person_Years = person_years
      )
    }
  }
}


results_df <- bind_rows(results_list)

write_xlsx(results_df, "cox_model_results_beating.xlsx")


## condom use incidence

# Load dataset
sw_negative_cohort_beating <- readRDS("sw_incident_beating_dataset.rds")

# Ensure correct types
sw_negative_cohort_beating <- sw_negative_cohort_beating %>%
  mutate(
    beating_bin = as.numeric(beating_bin),
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# -----------------------------
# DEFINE EXPOSURE VARIABLES
# -----------------------------

exposure_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_rape_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo",
  "sw_partners_total_24h_5cat",
  "sw_partners_clients_30d_4cat"
)

# recode true binary Yes/No variables
binary_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_rape_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo"
)

sw_negative_cohort_beating <- sw_negative_cohort_beating %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# -----------------------------
# RUN REGION-ADJUSTED COX MODELS
# -----------------------------

results_list <- list()

for (var in exposure_vars) {
  
  # make sure exposure is factor
  sw_negative_cohort_beating[[var]] <- as.factor(sw_negative_cohort_beating[[var]])
  
  formula <- as.formula(
    paste("Surv(py, beating_bin) ~", var, "+ ukraine_region + year")
  )
  
  model <- coxph(formula, data = sw_negative_cohort_beating)
  
  tidy_mod <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # keep only exposure rows
  tidy_mod <- tidy_mod %>%
    filter(grepl(paste0("^", var), term))
  
  # get all exposure levels
  levels_var <- levels(sw_negative_cohort_beating[[var]])
  
  for (lev in levels_var) {
    
    subset_data <- sw_negative_cohort_beating %>%
      filter(!is.na(.data[[var]]),
             .data[[var]] == lev)
    
    cases <- sum(subset_data$beating_bin == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    
    # reference level
    if (lev == levels_var[1]) {
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = 1,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years
      )
      
    } else {
      
      row_match <- tidy_mod %>%
        filter(grepl(lev, term))
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = row_match$estimate,
        CI_lower = row_match$conf.low,
        CI_upper = row_match$conf.high,
        Cases = cases,
        Person_Years = person_years
      )
    }
  }
}


results_df <- bind_rows(results_list)

write_xlsx(results_df, "cox_model_results_beating.xlsx")

# condom incidence
