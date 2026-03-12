## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# List of variables to test
vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat", "ngo_access_lifetime_3cat", "city_travel_12m", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "sw_partners_total_24h_5cat", "sw_partners_clients_30d_4cat", "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp", "violence_support_ngo", "age_bin", "occupied", "occupied_partial",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence", "avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

# binary variables
binary_vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat",  "ngo_access_lifetime_3cat", "city_travel_12m", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever", "occupied", "occupied_partial",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp", "violence_support_ngo", "age_bin", "underage_first_sw_bin",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence", "avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma",
)

sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(binary_vars),
    ~ factor(case_when(
      . == "Yes" ~ "Yes",
      . == "No" ~ "No",
      TRUE ~ NA_character_
    ), levels = c("No", "Yes"))
  ))

table(sw_combined_clean$avoided_healthcare_12m_stigma, useNA = "ifany")

# Ensure outcome is a factor
sw_combined_clean$hiv_test_rslt_bin <- as.factor(sw_combined_clean$hiv_test_rslt_bin)

# ensure adjustment variables are factors
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )



# Prepare to store results
results <- data.frame(
  Variable = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# loop over vars
for (v in vars) {
  cat("Running:", v, "\n")
  
  model_vars <- c("hiv_test_rslt_bin", "ukraine_region", "year", v)
  subset_data <- sw_combined_clean %>%
    select(all_of(model_vars)) %>%
    filter(complete.cases(.))
  
  # Check number of unique years in subset
  if (length(unique(subset_data$year)) == 1) {
    formula <- as.formula(paste("hiv_test_rslt_bin ~ ukraine_region +", v))
  } else {
    formula <- as.formula(paste("hiv_test_rslt_bin ~ ukraine_region + year +", v))
  }
  
  model <- try(glm(formula, data = subset_data, family = binomial), silent = TRUE)
  
  if (inherits(model, "try-error")) {
    cat("FAILED:", v, "\n")
    next
  }
  
  tidy_mod <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
  
  for (i in 1:nrow(tidy_mod)) {
    or <- tidy_mod$estimate[i]
    lci <- tidy_mod$conf.low[i]
    uci <- tidy_mod$conf.high[i]
    or_fmt <- sprintf("OR: %.2f; (95%%CI: %.2f-%.2f)", or, lci, uci)
    results <- rbind(results,
                     data.frame(Variable = paste(v, tidy_mod$term[i], sep=":"),
                                OR = or_fmt))
  }
}

# remove region coefficients
results <- results[!grepl("^.*:ukraine_region", results$Variable), ]
results <- results[!grepl("^.*:year", results$Variable), ]

# Save to Excel
write_xlsx(results, "univariate_logistic_results.xlsx")

# summary statistics

# load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

table(sw_combined_clean$hiv_test_rslt_bin, useNA = "ifany")

# Ensure outcome is numeric for proportions
sw_combined_clean$hiv_test_rslt_bin_num <- as.numeric(as.character(sw_combined_clean$hiv_test_rslt_bin))

print(levels(sw_combined_clean$hiv_test_rslt_bin))
print(table(sw_combined_clean$hiv_test_rslt_bin, useNA = "ifany"))

summary_list <- list()

for (v in vars) {
  tab <- sw_combined_clean %>%
    group_by(!!sym(v)) %>%
    summarise(
      n = n(),
      outcome_n = sum(hiv_test_rslt_bin == "Positive", na.rm = TRUE),
      outcome_prop = mean(hiv_test_rslt_bin == "Positive", na.rm = TRUE)
    ) %>%
    mutate(variable = v) %>%
    rename(level = !!sym(v))
  summary_list[[v]] <- tab
}

summary_df <- bind_rows(summary_list) %>%
  select(variable, level, n, outcome_n, outcome_prop)

write_xlsx(summary_df, "exposure_summary.xlsx")

# condom use 

# load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# List of variables to test
vars <- c(
  "ngo_access_lifetime_3cat", "city_travel_12m", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "sw_partners_total_24h_5cat", "sw_partners_clients_30d_4cat", "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp", "violence_support_ngo", "age_bin",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence", "avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

# binary variables
binary_vars <- c(
  "ngo_access_lifetime_3cat", "city_travel_12m", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp", "violence_support_ngo", "age_bin", "underage_first_sw_bin"
)

sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"), levels = c("No", "Yes"))
  ))
table(sw_combined_clean$avoided_healthcare_12m_stigma)

sw_combined_clean$condom_access_12m_3cat <- factor(
  sw_combined_clean$condom_access_12m_3cat,
  levels = c("No", "Yes")
)

sw_combined_clean$client_condom_lastsex_3cat <- factor(
  sw_combined_clean$client_condom_lastsex_3cat,
  levels = c("No", "Yes")
)

# ensure adjustment variables are factors
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# Prepare to store results
results <- data.frame(
  Variable = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# loop over vars
for (v in vars) {

  cat("Running:", v, "\n")

  if (length(unique(tmp$year)) == 1) {
    formula <- as.formula(paste("condom_access_12m_3cat ~ ukraine_region +", v))
  } else {
    formula <- as.formula(paste("condom_access_12m_3cat ~ ukraine_region + year +", v))
  }
  

  model <- try(glm(formula, data = sw_combined_clean, family = binomial), silent = TRUE)

  if (inherits(model, "try-error")) {
    cat("FAILED:", v, "\n")
    next
  }

  tidy_mod <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

  for (i in 1:nrow(tidy_mod)) {
    or <- tidy_mod$estimate[i]
    lci <- tidy_mod$conf.low[i]
    uci <- tidy_mod$conf.high[i]
    or_fmt <- sprintf("OR: %.2f; (95%%CI: %.2f-%.2f)", or, lci, uci)
    results <- rbind(results,
                     data.frame(
                       Variable = paste(v, tidy_mod$term[i], sep=":"),
                       OR = or_fmt
                     ))
  }
}

# remove region coefficients
results <- results[!grepl("^.*:ukraine_region", results$Variable), ]
results <- results[!grepl("^.*:year", results$Variable), ]

# Save to Excel
write_xlsx(results, "univariate_logistic_results_condom_access.xlsx")

# Prepare to store results
results <- data.frame(
  Variable = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# loop over vars
for (v in vars) {

  cat("Running:", v, "\n")

  if (length(unique(tmp$year)) == 1) {
    formula <- as.formula(paste("client_condom_lastsex_3cat ~ ukraine_region +", v))
  } else {
    formula <- as.formula(paste("client_condom_lastsex_3cat ~ ukraine_region + year +", v))
  }
  

  model <- try(glm(formula, data = sw_combined_clean, family = binomial), silent = TRUE)

  if (inherits(model, "try-error")) {
    cat("FAILED:", v, "\n")
    next
  }

  tidy_mod <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

  for (i in 1:nrow(tidy_mod)) {
    or <- tidy_mod$estimate[i]
    lci <- tidy_mod$conf.low[i]
    uci <- tidy_mod$conf.high[i]
    or_fmt <- sprintf("OR: %.2f; (95%%CI: %.2f-%.2f)", or, lci, uci)
    results <- rbind(results,
                     data.frame(
                       Variable = paste(v, tidy_mod$term[i], sep=":"),
                       OR = or_fmt
                     ))
  }
}

# remove region coefficients
results <- results[!grepl("^.*:ukraine_region", results$Variable), ]
results <- results[!grepl("^.*:year", results$Variable), ]

# Save to Excel
write_xlsx(results, "univariate_logistic_results_condom_lastsex.xlsx")

# rape

# List of variables to test
vars <- c(
  "year", "condom_access_12m_3cat", "ngo_access_lifetime_3cat", "city_travel_12m",
  "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "sw_partners_total_24h_5cat", "sw_partners_clients_30d_4cat",
  "violence_beaten_ever", "violence_physical_abuse_ever", "violence_police",
  "violence_pimp", "violence_support_ngo",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence",
  "avoided_healthcare_12m_police", "age_bin",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence",
  "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

# binary variables
binary_vars <- c(
  "condom_access_12m_3cat", "ngo_access_lifetime_3cat", "street_sw_bin",
  "alcohol_30d_bin", "violence_support_ngo",
  "violence_beaten_ever", "violence_physical_abuse_ever",
  "violence_police", "violence_pimp", "age_bin",
  "used_syringe_last_3cat", "underage_first_sw_bin", "city_travel_12m"
)

# Recode binary predictors
sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# ---- OUTCOME: RAPE ----
# Recode rape outcome to binary 0/1
sw_combined_clean$rape_bin <- ifelse(
  sw_combined_clean$violence_rape_ever == "Yes", 1, 0
)

# ensure adjustment variables are factors
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# Prepare results storage
results <- data.frame(
  Variable = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# loop over vars
for (v in vars) {

  formula <- as.formula(
    paste("rape_bin ~ ukraine_region + year +", v)
  )

  model <- glm(formula, data = sw_combined_clean, family = binomial)

  tidy_mod <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)

  # remove intercept
  tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

  for (i in 1:nrow(tidy_mod)) {

    or <- tidy_mod$estimate[i]
    lci <- tidy_mod$conf.low[i]
    uci <- tidy_mod$conf.high[i]

    or_fmt <- sprintf("OR: %.2f; (95%%CI: %.2f-%.2f)", or, lci, uci)

    results <- rbind(
      results,
      data.frame(
        Variable = paste(v, tidy_mod$term[i], sep=":"),
        OR = or_fmt
      )
    )
  }
}

# remove region and year coefficients from output
results <- results[!grepl("^.*:ukraine_region", results$Variable), ]
results <- results[!grepl("^.*:year", results$Variable), ]

# Save to Excel
write_xlsx(results, "univariate_logistic_results_rape.xlsx")

# rape

# List of variables to test
vars <- c(
  "year", "condom_access_12m_3cat", "ngo_access_lifetime_3cat", "city_travel_12m",
  "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "sw_partners_total_24h_5cat", "sw_partners_clients_30d_4cat",
  "violence_beaten_ever", "violence_physical_abuse_ever", "violence_police",
  "violence_pimp", "violence_support_ngo",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence",
  "avoided_healthcare_12m_police", "age_bin",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence",
  "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

# binary variables
binary_vars <- c(
  "condom_access_12m_3cat", "ngo_access_lifetime_3cat", "street_sw_bin",
  "alcohol_30d_bin", "violence_support_ngo",
  "violence_beaten_ever", "violence_physical_abuse_ever",
  "violence_police", "violence_pimp", "age_bin",
  "used_syringe_last_3cat", "underage_first_sw_bin", "city_travel_12m"
)

# Recode binary predictors
sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# ---- OUTCOME: RAPE ----
# Recode rape outcome to binary 0/1
sw_combined_clean$rape_bin <- ifelse(
  sw_combined_clean$art_current_3cat == "Yes", 1, 0
)

# ensure adjustment variables are factors
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# Prepare results storage
results <- data.frame(
  Variable = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# loop over vars
for (v in vars) {

  formula <- as.formula(
    paste("art_current_3cat ~ ukraine_region + year +", v)
  )

  model <- glm(formula, data = sw_combined_clean, family = binomial)

  tidy_mod <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)

  # remove intercept
  tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

  for (i in 1:nrow(tidy_mod)) {

    or <- tidy_mod$estimate[i]
    lci <- tidy_mod$conf.low[i]
    uci <- tidy_mod$conf.high[i]

    or_fmt <- sprintf("OR: %.2f; (95%%CI: %.2f-%.2f)", or, lci, uci)

    results <- rbind(
      results,
      data.frame(
        Variable = paste(v, tidy_mod$term[i], sep=":"),
        OR = or_fmt
      )
    )
  }
}

# remove region and year coefficients from output
results <- results[!grepl("^.*:ukraine_region", results$Variable), ]
results <- results[!grepl("^.*:year", results$Variable), ]

# Save to Excel
write_xlsx(results, "univariate_logistic_results_art.xlsx")

