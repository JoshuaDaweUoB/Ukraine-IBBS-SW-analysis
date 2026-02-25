## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

table(sw_combined_raw$year, sw_combined_clean$typology_primary_3cat)

# List of variables to test
vars <- c(
  "year", "condom_access_12m_3cat", "ngo_access_lifetime_3cat", "city_travel_12m",
  "sw_days_total_7d_3cat", "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp", "violence_support_ngo",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence", "avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma"
)

# Ensure outcome is a factor
sw_combined_clean$hiv_test_rslt_bin <- as.factor(sw_combined_clean$hiv_test_rslt_bin)

# Prepare to store results
results <- data.frame(
  Variable = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# loop over vars
for (v in vars) {
  formula <- as.formula(paste("hiv_test_rslt_bin ~ ukraine_region + year +", v))
  model <- glm(formula, data = sw_combined_clean, family = binomial)
  tidy_mod <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  # Exclude intercept
  tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
  for (i in 1:nrow(tidy_mod)) {
    or <- tidy_mod$estimate[i]
    lci <- tidy_mod$conf.low[i]
    uci <- tidy_mod$conf.high[i]
    or_fmt <- sprintf("OR: %.2f; (95%%CI: %.2f-%.2f)", or, lci, uci)
    results <- rbind(results, data.frame(Variable = paste(v, tidy_mod$term[i], sep=":"), OR = or_fmt))
  }
}

# remove region coefficients
results <- results[!grepl("^.*:ukraine_region", results$Variable), ]
results <- results[!grepl("^.*:year", results$Variable), ]

# Save to Excel
writexl::write_xlsx(results, "univariate_logistic_results.xlsx")

## rate ratios

# load long hiv data
sw_negative_cohort <- readRDS("sw_negative_cohort.rds")

vars <- c(
  "condom_access_12m_3cat", "ngo_access_lifetime_3cat", "street_sw_bin", "alcohol_30d_bin",
  "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever",
  "violence_physical_abuse_ever", "violence_police", "used_syringe_last_3cat", "underage_first_sw_bin"
)

# check missingness over years
for (var in vars) {
    print(table(sw_negative_cohort$year, sw_negative_cohort[[var]], useNA = "ifany"))
}


results_list <- list()

for (var in vars) {
  # If variable is a factor, get levels; otherwise, use unique values
  levels_var <- if (is.factor(sw_negative_cohort[[var]])) {
    levels(sw_negative_cohort[[var]])
  } else {
    unique(sw_negative_cohort[[var]])
  }
  
  for (lev in levels_var) {
    subset_data <- sw_negative_cohort[sw_negative_cohort[[var]] == lev & !is.na(sw_negative_cohort[[var]]), ]
    cases <- sum(subset_data$hiv_test_rslt_bin == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    
    # Fit Cox model for the variable (overall, not per level)
    formula <- as.formula(paste("Surv(py, hiv_test_rslt_bin) ~", var))
    model <- coxph(formula, data = sw_negative_cohort)
    hr <- exp(coef(model))
    ci <- exp(confint(model))
    
    # Only add HR/CI for the current level if it matches the coefficient name
    if (paste0(var, lev) %in% names(hr)) {
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = hr[paste0(var, lev)],
        CI_lower = ci[paste0(var, lev), 1],
        CI_upper = ci[paste0(var, lev), 2],
        Cases = cases,
        Person_Years = person_years
      )
    } else {
      # For reference level (usually not shown in HR output)
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = NA,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years
      )
    }
  }
}

results_df <- do.call(rbind, results_list)
write_xlsx(results_df, "cox_model_results_hiv.xlsx")

## rape incidence

# load long rape data
sw_negative_cohort_rape <- readRDS("sw_incident_rape_dataset.rds")

vars <- c(
  "condom_access_12m_3cat", "ngo_access_lifetime_3cat", "street_sw_bin"
)

results_list <- list()

for (var in vars) {
  # If variable is a factor, get levels; otherwise, use unique values
  levels_var <- if (is.factor(sw_negative_cohort_rape[[var]])) {
    levels(sw_negative_cohort_rape[[var]])
  } else {
    unique(sw_negative_cohort_rape[[var]])
  }
  
  for (lev in levels_var) {
    subset_data <- sw_negative_cohort_rape[sw_negative_cohort_rape[[var]] == lev & !is.na(sw_negative_cohort_rape[[var]]), ]
    cases <- sum(subset_data$rape_bin == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    
    # Fit Cox model for the variable (overall, not per level)
    formula <- as.formula(paste("Surv(py, rape_bin) ~", var))
    model <- coxph(formula, data = sw_negative_cohort_rape)
    hr <- exp(coef(model))
    ci <- exp(confint(model))
    
    # Only add HR/CI for the current level if it matches the coefficient name
    if (paste0(var, lev) %in% names(hr)) {
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = hr[paste0(var, lev)],
        CI_lower = ci[paste0(var, lev), 1],
        CI_upper = ci[paste0(var, lev), 2],
        Cases = cases,
        Person_Years = person_years
      )
    } else {
      # For reference level (usually not shown in HR output)
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = NA,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years
      )
    }
  }
}

results_df <- do.call(rbind, results_list)
write_xlsx(results_df, "cox_model_results_rape.xlsx")
