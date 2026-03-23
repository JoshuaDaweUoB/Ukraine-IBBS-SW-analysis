## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# function to calculate IR 
calc_ir <- function(cases, person_years) {
  ir <- (cases / person_years) * 100
  # 95% CI using Poisson approximation
  lower <- (qchisq(0.025, 2 * cases) / 2) / person_years * 100
  upper <- (qchisq(0.975, 2 * (cases + 1)) / 2) / person_years * 100
  if (cases == 0) {
    lower <- 0
    upper <- (-log(0.05) / person_years) * 100
  }
  return(list(IR = ir, IR_lower = lower, IR_upper = upper))
}

## rate ratios

# load long hiv data
sw_negative_cohort_hiv <- readRDS("sw_incidence_hiv_dataset.rds")

# list of dataframes
data_frames <- list(
  overall = sw_negative_cohort_hiv,
  idu = sw_negative_cohort_hiv %>% filter(idu_ever_3cat == "Yes"),
  noidu = sw_negative_cohort_hiv %>% filter(idu_ever_3cat == "No"),
  street = sw_negative_cohort_hiv %>% filter(street_sw_bin == "Yes"),
  nostreet = sw_negative_cohort_hiv %>% filter(street_sw_bin == "No")
)

# exposures
exposure_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_any_ever_3cat",
  "violence_rape_ever",
  "violence_beaten_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "idu_ever_3cat",
  "idu_12m_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo",
  "sw_partners_clients_30d_3cat"
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
  "violence_rape_ever",
  "violence_beaten_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "idu_ever_3cat",
  "idu_12m_3cat",  
  "underage_first_sw_bin",
  "violence_support_ngo"
)

# loop over dataframes
results_all <- list()

for (dataset_name in names(data_frames)) {

  df <- data_frames[[dataset_name]]

  # recode outcome
  df <- df %>%
    mutate(
      hiv_test_rslt_bin = ifelse(hiv_test_rslt_bin == "Positive", 1, 0),
      hiv_test_rslt_bin = as.numeric(hiv_test_rslt_bin)
    )

  # adjustment variables
  df <- df %>%
    mutate(
      ukraine_region = as.factor(ukraine_region),
      year = as.factor(year),
      years_in_sw_3cat = as.factor(years_in_sw_3cat)
    )

  # binary recode
  df <- df %>%
    mutate(across(all_of(binary_vars),
      ~ factor(ifelse(. == "Yes", "Yes", "No"),
               levels = c("No", "Yes"))
    ))

  results_list <- list()

}

results_all <- list()

# hazard ratios

results_list <- list()

for (dataset_name in names(data_frames)) {

  df <- data_frames[[dataset_name]]

  # recode outcome
  df <- df %>%
    mutate(
      hiv_test_rslt_bin = ifelse(hiv_test_rslt_bin == "Positive", 1, 0),
      hiv_test_rslt_bin = as.numeric(hiv_test_rslt_bin)
    )

  # adjustment variables
  df <- df %>%
    mutate(
      ukraine_region = as.factor(ukraine_region),
      year = as.factor(year),
      years_in_sw_3cat = as.factor(years_in_sw_3cat)
    )

  # binary recode
  df <- df %>%
    mutate(across(all_of(binary_vars),
      ~ factor(ifelse(. == "Yes", "Yes", "No"),
               levels = c("No", "Yes"))
    ))

  for (var in exposure_vars) {
    df[[var]] <- as.factor(df[[var]])
    formula <- as.formula(
      paste("Surv(py, hiv_test_rslt_bin) ~", var, "+ ukraine_region + year + years_in_sw_3cat")
    )
    model <- coxph(formula, data = df)
    tidy_mod <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    tidy_mod <- tidy_mod %>% filter(grepl(paste0("^", var), term))
    levels_var <- levels(df[[var]])
    for (lev in levels_var) {
      subset_data <- df %>%
        filter(!is.na(.data[[var]]), .data[[var]] == lev)
      cases <- sum(subset_data$hiv_test_rslt_bin == 1, na.rm = TRUE)
      person_years <- sum(subset_data$py, na.rm = TRUE)
      ir_res <- calc_ir(cases, person_years)
      if (lev == levels_var[1]) {
        results_list[[length(results_list) + 1]] <- data.frame(
          Variable = var,
          Level = lev,
          HR = 1,
          CI_lower = NA,
          CI_upper = NA,
          Cases = cases,
          Person_Years = person_years,
          IR_100PY = ir_res$IR,
          IR_100PY_lower = ir_res$IR_lower,
          IR_100PY_upper = ir_res$IR_upper
        )
      } else {
        row_match <- tidy_mod %>% filter(grepl(lev, term))
        results_list[[length(results_list) + 1]] <- data.frame(
          Variable = var,
          Level = lev,
          HR = row_match$estimate,
          CI_lower = row_match$conf.low,
          CI_upper = row_match$conf.high,
          Cases = cases,
          Person_Years = person_years,
          IR_100PY = ir_res$IR,
          IR_100PY_lower = ir_res$IR_lower,
          IR_100PY_upper = ir_res$IR_upper
        )
      }
    }

    results_all[[dataset_name]] <- bind_rows(results_list)
  }
}

write_xlsx(results_all, "cox_model_results_hiv.xlsx")

## rape incidence

# load dataset
sw_negative_cohort_rape <- readRDS("sw_incident_rape_dataset.rds")

# binary outcome for Cox
sw_negative_cohort_rape$rape_bin <- ifelse(sw_negative_cohort_rape$rape_end == "Yes", 1, 0)

# variable types
sw_negative_cohort_rape <- sw_negative_cohort_rape %>%
  mutate(
    rape_bin = as.numeric(rape_bin),
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year),
    years_in_sw_3cat = as.factor(years_in_sw_3cat)
  )


# exposures
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
  "sw_partners_clients_30d_3cat",
  "idu_ever_3cat",
  "idu_12m_3cat"   
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
  "violence_support_ngo",
  "idu_ever_3cat",
  "idu_12m_3cat"  
)

sw_negative_cohort_rape <- sw_negative_cohort_rape %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# hazard ratios

results_list <- list()

for (var in exposure_vars) {
  sw_negative_cohort_rape[[var]] <- as.factor(sw_negative_cohort_rape[[var]])
  formula <- as.formula(
    paste("Surv(py, rape_bin) ~", var, "+ ukraine_region + year + years_in_sw_3cat")
  )
  model <- coxph(formula, data = sw_negative_cohort_rape)
  tidy_mod <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  tidy_mod <- tidy_mod %>% filter(grepl(paste0("^", var), term))
  levels_var <- levels(sw_negative_cohort_rape[[var]])
  for (lev in levels_var) {
    subset_data <- sw_negative_cohort_rape %>%
      filter(!is.na(.data[[var]]), .data[[var]] == lev)
    cases <- sum(subset_data$rape_bin == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    ir_res <- calc_ir(cases, person_years)
    if (lev == levels_var[1]) {
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = 1,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years,
        IR_100PY = ir_res$IR,
        IR_100PY_lower = ir_res$IR_lower,
        IR_100PY_upper = ir_res$IR_upper
      )
    } else {
      row_match <- tidy_mod %>% filter(grepl(lev, term))
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = row_match$estimate,
        CI_lower = row_match$conf.low,
        CI_upper = row_match$conf.high,
        Cases = cases,
        Person_Years = person_years,
        IR_100PY = ir_res$IR,
        IR_100PY_lower = ir_res$IR_lower,
        IR_100PY_upper = ir_res$IR_upper
      )
    }
  }
}

results_df <- bind_rows(results_list)

write_xlsx(results_df, "cox_model_results_rape.xlsx")

## beating incidence

# load dataset
sw_negative_cohort_beating <- readRDS("sw_incident_beating_dataset.rds")

# binary outcome for Cox
sw_negative_cohort_beating$beating_bin <- ifelse(sw_negative_cohort_beating$beating_end == "Yes", 1, 0)

# variable types
sw_negative_cohort_beating <- sw_negative_cohort_beating %>%
  mutate(
    beating_bin = as.numeric(beating_bin),
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# exposures
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
  "sw_partners_clients_30d_3cat",
  "idu_ever_3cat",
  "idu_12m_3cat"    
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
  "violence_support_ngo",
  "idu_ever_3cat",
  "idu_12m_3cat"    
)

sw_negative_cohort_beating <- sw_negative_cohort_beating %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# hazard ratios

results_list <- list()

for (var in exposure_vars) {
  sw_negative_cohort_beating[[var]] <- as.factor(sw_negative_cohort_beating[[var]])
  formula <- as.formula(
    paste("Surv(py, beating_bin) ~", var, "+ ukraine_region + year + years_in_sw_3cat")
  )
  model <- coxph(formula, data = sw_negative_cohort_beating)
  tidy_mod <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  tidy_mod <- tidy_mod %>% filter(grepl(paste0("^", var), term))
  levels_var <- levels(sw_negative_cohort_beating[[var]])
  for (lev in levels_var) {
    subset_data <- sw_negative_cohort_beating %>%
      filter(!is.na(.data[[var]]), .data[[var]] == lev)
    cases <- sum(subset_data$beating_bin == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    ir_res <- calc_ir(cases, person_years)
    if (lev == levels_var[1]) {
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = 1,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years,
        IR_100PY = ir_res$IR,
        IR_100PY_lower = ir_res$IR_lower,
        IR_100PY_upper = ir_res$IR_upper
      )
    } else {
      row_match <- tidy_mod %>% filter(grepl(lev, term))
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = row_match$estimate,
        CI_lower = row_match$conf.low,
        CI_upper = row_match$conf.high,
        Cases = cases,
        Person_Years = person_years,
        IR_100PY = ir_res$IR,
        IR_100PY_lower = ir_res$IR_lower,
        IR_100PY_upper = ir_res$IR_upper
      )
    }
  }
}

results_df <- bind_rows(results_list)

write_xlsx(results_df, "cox_model_results_beating.xlsx")

# injecting drug use incidence

# load dataset
sw_negative_cohort_idu <- readRDS("sw_incident_idu_dataset.rds")

# binary outcome for Cox
sw_negative_cohort_idu$idu_bin <- ifelse(sw_negative_cohort_idu$idu_end == "Yes", 1, 0)

# variable types
sw_negative_cohort_idu <- sw_negative_cohort_idu %>%
  mutate(
    idu_bin = as.numeric(idu_bin),
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# exposures
exposure_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_any_ever_3cat",
  "violence_rape_ever",
  "violence_beaten_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo",
  "sw_partners_clients_30d_3cat"
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
  "violence_rape_ever",
  "violence_beaten_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo"
)

sw_negative_cohort_idu <- sw_negative_cohort_idu %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# hazard ratios
results_list <- list()

for (var in exposure_vars) {
  sw_negative_cohort_idu[[var]] <- as.factor(sw_negative_cohort_idu[[var]])
  formula <- as.formula(
    paste("Surv(py, idu_bin) ~", var, "+ ukraine_region + year + years_in_sw_3cat")
  )
  model <- coxph(formula, data = sw_negative_cohort_idu)
  tidy_mod <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  tidy_mod <- tidy_mod %>% filter(grepl(paste0("^", var), term))
  levels_var <- levels(sw_negative_cohort_idu[[var]])
  for (lev in levels_var) {
    subset_data <- sw_negative_cohort_idu %>%
      filter(!is.na(.data[[var]]), .data[[var]] == lev)
    cases <- sum(subset_data$idu_bin == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    ir_res <- calc_ir(cases, person_years)
    if (lev == levels_var[1]) {
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = 1,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years,
        IR_100PY = ir_res$IR,
        IR_100PY_lower = ir_res$IR_lower,
        IR_100PY_upper = ir_res$IR_upper
      )
    } else {
      row_match <- tidy_mod %>% filter(grepl(lev, term))
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = row_match$estimate,
        CI_lower = row_match$conf.low,
        CI_upper = row_match$conf.high,
        Cases = cases,
        Person_Years = person_years,
        IR_100PY = ir_res$IR,
        IR_100PY_lower = ir_res$IR_lower,
        IR_100PY_upper = ir_res$IR_upper
      )
    }
  }
}

results_df <- bind_rows(results_list)

write_xlsx(results_df, "cox_model_results_idu.xlsx")