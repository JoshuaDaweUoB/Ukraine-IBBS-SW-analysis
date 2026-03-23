## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# List of variables to test
vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat", "ngo_access_lifetime_3cat", "city_travel_12m", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "idu_ever_3cat", "idu_12m_3cat", "sw_partners_clients_30d_3cat", "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp", "violence_support_ngo", "age_bin", "occupied", "occupied_partial",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence", "avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

# binary variables
binary_vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat",  "ngo_access_lifetime_3cat", "city_travel_12m", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "idu_ever_3cat", "idu_12m_3cat", "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever", "occupied", "occupied_partial",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp", "violence_support_ngo", "age_bin", "underage_first_sw_bin",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence", "avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma"
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

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    )
  )

# Prepare results container
results <- data.frame(
  Variable = character(),
  Level = character(),
  N = numeric(),
  Cases = numeric(),
  Percent = numeric(),
  CountPercent = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# Loop over variables
for (v in vars) {
  cat("Running:", v, "\n")
  
  # subset relevant columns and complete cases
  model_vars <- c("hiv_test_rslt_bin", "ukraine_region", "year", v)
  subset_data <- sw_combined_clean %>%
    select(all_of(model_vars)) %>%
    filter(complete.cases(.))
  
  # Check if multiple years for formula
  if (length(unique(subset_data$year)) == 1) {
    formula <- as.formula(paste("hiv_test_rslt_bin ~ ukraine_region +", v))
  } else {
    formula <- as.formula(paste("hiv_test_rslt_bin ~ ukraine_region + year +", v))
  }
  
  # fit model
  model <- try(glm(formula, data = subset_data, family = binomial), silent = TRUE)
  if (inherits(model, "try-error")) {
    cat("FAILED:", v, "\n")
    next
  }
  
  tidy_mod <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
  
  # loop over levels
  levels_var <- levels(subset_data[[v]])
  
  for (lev in levels_var) {
    sub <- subset_data %>% filter(.data[[v]] == lev)
    n <- nrow(sub)
    cases <- sum(sub$hiv_test_rslt_bin, na.rm = TRUE)      # numeric outcome
    pct <- round(cases / n * 100, 1)
    count_pct <- sprintf("%d (%.1f)", cases, pct)           # new column
    
    # reference level
    if (lev == levels_var[1]) {
      or_fmt <- "ref."
    } else {
      term_match <- tidy_mod$term[grepl(lev, tidy_mod$term)]
      if (length(term_match) == 1) {
        row <- tidy_mod[tidy_mod$term == term_match, ]
        or_fmt <- sprintf("%.2f (%.2f-%.2f)", row$estimate, row$conf.low, row$conf.high)
      } else {
        or_fmt <- NA
      }
    }
    
    # add row to results
    results <- rbind(results,
                     data.frame(
                       Variable = v,
                       Level = lev,
                       N = n,
                       Cases = cases,
                       Percent = pct,
                       CountPercent = count_pct,
                       OR = or_fmt,
                       stringsAsFactors = FALSE
                     ))
  }
}

# Save to Excel
write_xlsx(results, "univariate_logistic_results_with_counts.xlsx")

# condom use 

# load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# clean condom use
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    client_condom_lastsex_bin = case_when(
      client_condom_lastsex_3cat %in% c("Yes","1") ~ 1,
      client_condom_lastsex_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    )
  )

# variables to test
vars <- c(
  "condom_access_12m_3cat", "ngo_access_lifetime_3cat", "city_travel_12m", "street_sw_bin",
  "alcohol_30d_bin", "used_syringe_last_3cat", "idu_ever_3cat", "idu_12m_3cat",
  "sw_partners_clients_30d_3cat", "violence_any_ever_3cat", "violence_rape_12m_3cat",
  "violence_rape_ever", "violence_beaten_ever", "violence_physical_abuse_ever",
  "violence_police", "violence_pimp", "violence_support_ngo", "age_bin",
  "occupied", "occupied_partial", "avoided_healthcare_12m_stigma",
  "avoided_healthcare_12m_violence", "avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence",
  "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

# ensure adjustment variables are factors
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# store results
results <- data.frame(
  Variable = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# Prepare results container
results <- data.frame(
  Variable = character(),
  Level = character(),
  N = numeric(),
  Cases = numeric(),
  Percent = numeric(),
  CountPercent = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

for (v in vars) {

  cat("Running:", v, "\n")

  model_vars <- c("client_condom_lastsex_bin","ukraine_region","year",v)

  subset_data <- sw_combined_clean %>%
    select(all_of(model_vars)) %>%
    filter(complete.cases(.))

  if (length(unique(subset_data$year)) == 1) {
    formula <- as.formula(paste("client_condom_lastsex_bin ~ ukraine_region +", v))
  } else {
    formula <- as.formula(paste("client_condom_lastsex_bin ~ ukraine_region + year +", v))
  }

  model <- try(glm(formula, data=subset_data, family=binomial), silent=TRUE)

  if (inherits(model,"try-error")) {
    cat("FAILED:",v,"\n")
    next
  }

  tidy_mod <- broom::tidy(model, conf.int=TRUE, exponentiate=TRUE)
  tidy_mod <- tidy_mod[tidy_mod$term!="(Intercept)",]

  levels_var <- levels(subset_data[[v]])

  for (lev in levels_var) {

    sub <- subset_data %>% filter(.data[[v]] == lev)

    n <- nrow(sub)
    cases <- sum(sub$client_condom_lastsex_bin, na.rm=TRUE)
    pct <- round(cases/n*100,1)
    count_pct <- sprintf("%d (%.1f)",cases,pct)

    if (lev == levels_var[1]) {
      or_fmt <- "ref."
    } else {

      term_match <- tidy_mod$term[grepl(lev,tidy_mod$term)]

      if (length(term_match)==1) {
        row <- tidy_mod[tidy_mod$term==term_match,]
        or_fmt <- sprintf("%.2f (%.2f-%.2f)",
                          row$estimate,row$conf.low,row$conf.high)
      } else {
        or_fmt <- NA
      }
    }

    results <- rbind(results,
      data.frame(
        Variable=v,
        Level=lev,
        N=n,
        Cases=cases,
        Percent=pct,
        CountPercent=count_pct,
        OR=or_fmt,
        stringsAsFactors=FALSE
      )
    )
  }
}

write_xlsx(results,"univariate_logistic_condomlastsex_results.xlsx")

# rape

# load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# ensure adjustment variables are factors
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )
# create rape_bin
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    rape_bin = case_when(
      violence_rape_ever %in% c("Yes","1") ~ 1,
      violence_rape_ever %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# variables to test
vars <- c(
  "client_condom_lastsex", "condom_access_12m_3cat", "ngo_access_lifetime_3cat",
  "city_travel_12m", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "idu_ever_3cat", "idu_12m_3cat", "sw_partners_clients_30d_3cat",
  "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_beaten_ever",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp",
  "violence_support_ngo", "age_bin", "occupied", "occupied_partial",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence",
  "avoided_healthcare_12m_police", "avoided_hiv_test_12m_police",
  "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma",
  "underage_first_sw_bin"
)

# results container
results <- data.frame(
  Variable = character(),
  Level = character(),
  N = numeric(),
  Cases = numeric(),
  Percent = numeric(),
  CountPercent = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# loop over variables
for (v in vars) {

  if (!v %in% names(sw_combined_clean)) next

  subset_data <- sw_combined_clean %>%
    select(rape_bin, ukraine_region, year, all_of(v)) %>%
    filter(!is.na(rape_bin) & !is.na(ukraine_region) & !is.na(.data[[v]]))

  if (nrow(subset_data) == 0) next

  subset_data[[v]] <- as.factor(subset_data[[v]])

  formula <- if (length(unique(subset_data$year)) == 1) {
    as.formula(paste("rape_bin ~ ukraine_region +", v))
  } else {
    as.formula(paste("rape_bin ~ ukraine_region + year +", v))
  }

  model <- try(glm(formula, data = subset_data, family = binomial), silent = TRUE)
  if (inherits(model, "try-error")) next

  tidy_mod <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

  for (lev in levels(subset_data[[v]])) {

    sub <- subset_data %>% filter(.data[[v]] == lev)
    n <- nrow(sub)
    cases <- sum(sub$rape_bin, na.rm = TRUE)
    pct <- ifelse(n > 0, round(cases / n * 100, 1), NA)
    count_pct <- ifelse(n > 0, sprintf("%d (%.1f)", cases, pct), NA)

    if (lev == levels(subset_data[[v]])[1]) {
      or_fmt <- "ref."
    } else {
      term_match <- tidy_mod$term[grepl(paste0(v, lev), tidy_mod$term)]
      if (length(term_match) == 1) {
        row <- tidy_mod[tidy_mod$term == term_match, ]
        or_fmt <- sprintf("%.2f (%.2f-%.2f)", row$estimate, row$conf.low, row$conf.high)
      } else {
        or_fmt <- NA
      }
    }

    results <- rbind(results, data.frame(
      Variable = v,
      Level = lev,
      N = n,
      Cases = cases,
      Percent = pct,
      CountPercent = count_pct,
      OR = or_fmt,
      stringsAsFactors = FALSE
    ))
  }
}

write_xlsx(results, "univariate_logistic_rape_bin_results.xlsx")










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

# recode binary predictors
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

