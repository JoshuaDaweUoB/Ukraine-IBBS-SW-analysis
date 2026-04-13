## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales)

## set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# hiv status

## load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean_kharkiv.rds")

## variables to test
vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat", "ngo_access_lifetime_3cat", "city_travel_12m_cat", "street_sw_bin", "typology_primary_6m", "typology_primary_30d_3cat", "typology_primary_30d", "alcohol_30d_bin", "used_syringe_last_3cat",
  "idu_ever_3cat", "idu_12m_3cat", "sw_partners_clients_30d_3cat", "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp", "violence_support_ngo", "age_bin", "occupied", "occupied_partial",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence", "avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

## binary variables
binary_vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat",  "ngo_access_lifetime_3cat", "city_travel_12m_cat", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "idu_ever_3cat", "idu_12m_3cat", "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever", "occupied", "occupied_partial",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp", "violence_support_ngo", "age_bin", "underage_first_sw_bin",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence", "avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma"
)

## convert binary variables
sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(binary_vars),
                ~ factor(case_when(
                  . == "Yes" ~ "Yes",
                  . == "No" ~ "No",
                  TRUE ~ NA_character_
                ), levels = c("No", "Yes"))
  ))

## outcome to numeric 0/1
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    hiv_test_rslt_bin = as.factor(hiv_test_rslt_bin),
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    )
  )

## adjustment variables
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    year = as.factor(year),
    years_in_sw = as.factor(years_in_sw)
  )


# Stratified datasets
datasets <- list(
  Overall = sw_combined_clean,
  IDU = sw_combined_clean %>% filter(idu_ever_3cat == "Yes"),
  No_IDU = sw_combined_clean %>% filter(idu_ever_3cat == "No")
)

# Function to run logistic regression + summary table
compute_results <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    kharkiv_data <- df %>%
      select(hiv_test_rslt_bin, city, year, years_in_sw, all_of(v)) %>%
      filter(
        !is.na(hiv_test_rslt_bin),
        !is.na(.data[[v]]),
        !is.na(years_in_sw)
      )

    if (nrow(kharkiv_data) == 0 ||
        length(unique(kharkiv_data$hiv_test_rslt_bin)) < 2 ||
        length(unique(kharkiv_data[[v]])) < 2) next

    # ----------------------------
    # MODEL FORMULA
    # ----------------------------
    rhs <- paste("years_in_sw", v, sep = " + ")

    if (length(unique(kharkiv_data$year)) > 1) {
      rhs <- paste(rhs, "+ year")
    }

    formula <- as.formula(paste("hiv_test_rslt_bin ~", rhs))

    model <- try(glm(formula, data = kharkiv_data, family = binomial), silent = TRUE)

    tidy_mod <- NULL
    if (!inherits(model, "try-error")) {
      tidy_mod <- try(broom::tidy(model, conf.int = TRUE, exponentiate = TRUE), silent = TRUE)

      if (!inherits(tidy_mod, "try-error")) {
        tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
      } else {
        tidy_mod <- NULL
      }
    }

    # safer level handling
    levels_var <- sort(unique(kharkiv_data[[v]]))

    for (lev in levels_var) {

      sub <- kharkiv_data %>% filter(.data[[v]] == lev)
      n <- nrow(sub)

      if (n == 0) next

      cases <- sum(sub$hiv_test_rslt_bin, na.rm = TRUE)
      count_pct <- sprintf("%d (%.1f%%)", cases, round(cases / n * 100, 1))

      # ----------------------------
      # OR EXTRACTION (FIXED)
      # ----------------------------
      if (lev == levels_var[1]) {

        or_fmt <- "ref."

      } else if (!is.null(tidy_mod)) {

        term_match <- tidy_mod[grepl(paste0("^", v), tidy_mod$term), ]
        term_match <- term_match[grepl(lev, term_match$term), ]

        if (nrow(term_match) == 1) {

          row <- term_match[1, ]

          if (!is.na(row$conf.low)) {
            or_fmt <- sprintf("%.2f (%.2f-%.2f)",
                              row$estimate,
                              row$conf.low,
                              row$conf.high)
          } else {
            or_fmt <- sprintf("%.2f", row$estimate)
          }

        } else {
          or_fmt <- NA
        }

      } else {
        or_fmt <- NA
      }

      results <- rbind(
        results,
        data.frame(
          Variable = v,
          Level = lev,
          N = n,
          Cases = cases,
          `x (x.x%)` = count_pct,
          OR = or_fmt,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  return(results)
}

# Loop over stratified datasets
all_sheets <- list()
for (name in names(datasets)) {
  cat("Processing:", name, "\n")
  all_sheets[[name]] <- compute_results(datasets[[name]], vars)
}

# Save Excel with 3 sheets: Overall, IDU, No_IDU
write_xlsx(all_sheets, "hiv_results_idu_stratified.xlsx")

# condom use

# Load data
sw_combined_clean <- readRDS("sw_combined_clean_kharkiv.rds")

# Outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    condom_access_12m_3cat = case_when(
      condom_access_12m_3cat %in% c("Yes","1") ~ 1,
      condom_access_12m_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    city = as.factor(city),
    year = as.factor(year)
  )

# Exposure variables
vars <- c(
  "ngo_access_lifetime_3cat","city_travel_12m_cat","street_sw_bin",
  "alcohol_30d_bin","used_syringe_last_3cat","idu_ever_3cat","idu_12m_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat","violence_rape_12m_3cat",
  "violence_rape_ever","violence_beaten_ever","violence_physical_abuse_ever",
  "violence_police","violence_pimp","violence_support_ngo","age_bin",
  "occupied","occupied_partial","avoided_healthcare_12m_stigma",
  "avoided_healthcare_12m_violence","avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police","avoided_hiv_test_12m_violence",
  "avoided_hiv_test_12m_stigma","underage_first_sw_bin"
)

# Convert binary variables
sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(vars),
                ~ factor(case_when(
                  . == "Yes" ~ "Yes",
                  . == "No" ~ "No",
                  TRUE ~ NA_character_
                ), levels = c("No", "Yes"))
  ))

# Stratified datasets
datasets <- list(
  Overall = sw_combined_clean,
  IDU = sw_combined_clean %>% filter(idu_ever_3cat == "Yes"),
  No_IDU = sw_combined_clean %>% filter(idu_ever_3cat == "No")
)

# Function to compute counts, percentages, and ORs
compute_results <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    kharkiv_data <- df %>%
      select(condom_access_12m_3cat, city, year, years_in_sw, all_of(v)) %>%
      filter(
        !is.na(condom_access_12m_3cat),
        !is.na(.data[[v]]),
        !is.na(years_in_sw)
      )

    if (nrow(kharkiv_data) == 0 ||
        length(unique(kharkiv_data$condom_access_12m_3cat)) < 2 ||
        length(unique(kharkiv_data[[v]])) < 2) next

    # ----------------------------
    # MODEL FORMULA
    # ----------------------------
    rhs <- paste("years_in_sw", v, sep = " + ")

    if (length(unique(kharkiv_data$year)) > 1) {
      rhs <- paste(rhs, "+ year")
    }

    formula <- as.formula(paste("condom_access_12m_3cat ~", rhs))

    model <- try(glm(formula, data = kharkiv_data, family = binomial), silent = TRUE)

    tidy_mod <- NULL
    if (!inherits(model, "try-error")) {
      tidy_mod <- try(broom::tidy(model, conf.int = TRUE, exponentiate = TRUE), silent = TRUE)

      if (!inherits(tidy_mod, "try-error")) {
        tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
      } else {
        tidy_mod <- NULL
      }
    }

    # safer level handling
    levels_var <- sort(unique(kharkiv_data[[v]]))

    for (lev in levels_var) {

      sub <- kharkiv_data %>% filter(.data[[v]] == lev)
      n <- nrow(sub)

      if (n == 0) next

      cases <- sum(sub$condom_access_12m_3cat, na.rm = TRUE)
      count_pct <- sprintf("%d (%.1f%%)", cases, round(cases / n * 100, 1))

      # ----------------------------
      # OR EXTRACTION (FIXED)
      # ----------------------------
      if (lev == levels_var[1]) {

        or_fmt <- "ref."

      } else if (!is.null(tidy_mod)) {

        term_match <- tidy_mod[grepl(paste0("^", v), tidy_mod$term), ]
        term_match <- term_match[grepl(lev, term_match$term), ]

        if (nrow(term_match) == 1) {

          row <- term_match[1, ]

          if (!is.na(row$conf.low)) {
            or_fmt <- sprintf("%.2f (%.2f-%.2f)",
                              row$estimate,
                              row$conf.low,
                              row$conf.high)
          } else {
            or_fmt <- sprintf("%.2f", row$estimate)
          }

        } else {
          or_fmt <- NA
        }

      } else {
        or_fmt <- NA
      }

      results <- rbind(
        results,
        data.frame(
          Variable = v,
          Level = lev,
          N = n,
          Cases = cases,
          `x (x.x%)` = count_pct,
          OR = or_fmt,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  return(results)
}

# Loop over datasets and generate sheets
all_sheets <- list()
for (name in names(datasets)) {
  cat("Processing:", name, "\n")
  all_sheets[[name]] <- compute_results(datasets[[name]], vars)
}

# Save Excel with 3 sheets: Overall, IDU, No_IDU
write_xlsx(all_sheets, "condom_12m_idu_stratified.xlsx")

## rape

# load data
sw_combined_clean <- readRDS("sw_combined_clean_kharkiv.rds")

# outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    rape_bin = case_when(
      violence_rape_ever %in% c("Yes","1") ~ 1,
      violence_rape_ever %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    city = as.factor(city),
    year = as.factor(year),
    years_in_sw = as.factor(years_in_sw)
  )

# exposure variables
vars <- c(
  "client_condom_lastsex","condom_access_12m_3cat","ngo_access_lifetime_3cat",
  "city_travel_12m_cat","street_sw_bin","alcohol_30d_bin","used_syringe_last_3cat",
  "idu_ever_3cat","idu_12m_3cat","sw_partners_clients_30d_3cat",
  "violence_beaten_ever","violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "avoided_healthcare_12m_stigma","avoided_healthcare_12m_violence",
  "avoided_healthcare_12m_police","avoided_hiv_test_12m_police",
  "avoided_hiv_test_12m_violence","avoided_hiv_test_12m_stigma",
  "underage_first_sw_bin"
)

# convert exposures to factors
sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(vars),
    ~ factor(case_when(
      . == "Yes" ~ "Yes",
      . == "No" ~ "No",
      TRUE ~ NA_character_
    ), levels = c("No","Yes"))
  ))

# stratified datasets
datasets <- list(
  overall = sw_combined_clean,
  idu = sw_combined_clean %>% filter(idu_ever_3cat == "Yes"),
  no_idu = sw_combined_clean %>% filter(idu_ever_3cat == "No")
)

# function to run logistic models and build table
compute_results <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    # subset complete cases
    kharkiv_data <- df %>%
      select(rape_bin, city, year, years_in_sw, all_of(v)) %>%
      filter(
        !is.na(rape_bin),
        !is.na(.data[[v]]),
        !is.na(years_in_sw)
      )

    # skip low variation
    if (nrow(kharkiv_data) == 0 ||
        length(unique(kharkiv_data$rape_bin)) < 2 ||
        length(unique(kharkiv_data[[v]])) < 2) next

    # model formula
    rhs <- paste("years_in_sw", v, sep = " + ")

    if (length(unique(kharkiv_data$year)) > 1) {
      rhs <- paste(rhs, "+ year")
    }

    model <- try(
      glm(as.formula(paste("rape_bin ~", rhs)),
          data = kharkiv_data,
          family = binomial),
      silent = TRUE
    )

    # tidy model safely
    tidy_mod <- NULL

    if (!inherits(model, "try-error")) {
      tidy_mod <- try(
        broom::tidy(model, conf.int = TRUE, exponentiate = TRUE),
        silent = TRUE
      )

      if (!inherits(tidy_mod, "try-error")) {
        tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
      } else {
        tidy_mod <- NULL
      }
    }

    # exposure levels
    levels_var <- sort(unique(kharkiv_data[[v]]))

    for (lev in levels_var) {

      sub <- kharkiv_data %>% filter(.data[[v]] == lev)
      n <- nrow(sub)

      if (n == 0) next

      cases <- sum(sub$rape_bin, na.rm = TRUE)
      count_pct <- sprintf("%d (%.1f%%)", cases, round(cases / n * 100, 1))

      # reference category handling
      if (lev == levels_var[1]) {

        or_fmt <- "ref."

      } else if (!is.null(tidy_mod)) {

        term_match <- tidy_mod[grepl(paste0("^", v), tidy_mod$term), ]
        term_match <- term_match[grepl(lev, term_match$term), ]

        if (nrow(term_match) == 1) {

          row <- term_match[1, ]

          if (!is.na(row$conf.low)) {
            or_fmt <- sprintf(
              "%.2f (%.2f-%.2f)",
              row$estimate,
              row$conf.low,
              row$conf.high
            )
          } else {
            or_fmt <- sprintf("%.2f", row$estimate)
          }

        } else {
          or_fmt <- NA
        }

      } else {
        or_fmt <- NA
      }

      results <- rbind(
        results,
        data.frame(
          variable = v,
          level = lev,
          n = n,
          cases = cases,
          pct = count_pct,
          or = or_fmt
        )
      )
    }
  }

  results
}

# run models
all_sheets <- list()

for (name in names(datasets)) {
  cat("processing:", name, "\n")
  all_sheets[[name]] <- compute_results(datasets[[name]], vars)
}

# export results
write_xlsx(all_sheets, "rape_idu_stratified.xlsx")

# idu

## load data
sw_combined_clean <- readRDS("sw_combined_clean_kharkiv.rds")

## outcome: idu_ever_3cat
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    idu_ever_bin = case_when(
      idu_ever_3cat %in% c("Yes","1") ~ 1,
      idu_ever_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    city = as.factor(city),
    year = as.factor(year),
    years_in_sw = as.factor(years_in_sw)
  )
table(sw_combined_clean$idu_ever_3cat,
      sw_combined_clean$year,
      useNA = "ifany")

prop.table(
  table(sw_combined_clean$idu_ever_3cat,
        sw_combined_clean$year),
  margin = 2
)      

## variables
vars <- c(
  "client_condom_lastsex","condom_access_12m_3cat","ngo_access_lifetime_3cat",
  "city_travel_12m_cat","street_sw_bin","alcohol_30d_bin","used_syringe_last_3cat",
  "idu_12m_3cat","sw_partners_clients_30d_3cat",
  "violence_any_ever_3cat","violence_rape_12m_3cat","violence_beaten_ever",
  "violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "avoided_healthcare_12m_stigma","avoided_healthcare_12m_violence",
  "avoided_healthcare_12m_police","avoided_hiv_test_12m_police",
  "avoided_hiv_test_12m_violence","avoided_hiv_test_12m_stigma",
  "underage_first_sw_bin"
)

## convert binary variables
sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(vars),
                ~ factor(case_when(
                  . == "Yes" ~ "Yes",
                  . == "No" ~ "No",
                  TRUE ~ NA_character_
                ), levels = c("No", "Yes"))
  ))

## containers
all_sheets <- list()
cities <- c(levels(sw_combined_clean$city), "Overall")

## loop
for (cty in cities) {

  cat("Processing:", cty, "\n")

  if (cty == "Overall") {
    city_data <- sw_combined_clean
  } else {
    city_data <- sw_combined_clean %>% filter(city == cty)
  }

  results_list <- list()
  row_id <- 1

  for (v in vars) {

    if (!v %in% names(city_data)) next

    kharkiv_data <- city_data %>%
      select(idu_ever_bin, city, year, years_in_sw, all_of(v)) %>%
      filter(
        !is.na(idu_ever_bin),
        !is.na(.data[[v]]),
        !is.na(years_in_sw)
      )

    if (nrow(kharkiv_data) == 0) next
    if (length(unique(kharkiv_data$idu_ever_bin)) < 2) next
    if (length(unique(kharkiv_data[[v]])) < 2) next

    ## MODEL (UPDATED: years_in_sw ALWAYS INCLUDED)
    if (cty == "Overall") {
      formula <- if (length(unique(kharkiv_data$year)) == 1) {
        as.formula(paste("idu_ever_bin ~ city + years_in_sw +", v))
      } else {
        as.formula(paste("idu_ever_bin ~ city + year + years_in_sw +", v))
      }
    } else {
      formula <- if (length(unique(kharkiv_data$year)) == 1) {
        as.formula(paste("idu_ever_bin ~ years_in_sw +", v))
      } else {
        as.formula(paste("idu_ever_bin ~ year + years_in_sw +", v))
      }
    }

    model <- try(glm(formula, data = kharkiv_data, family = binomial), silent = TRUE)

    ## SAFE tidy
    tidy_mod <- NULL
    if (!inherits(model, "try-error")) {
      tidy_mod <- try(
        broom::tidy(model, conf.int = TRUE, exponentiate = TRUE),
        silent = TRUE
      )
      if (!inherits(tidy_mod, "try-error")) {
        tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
      } else {
        tidy_mod <- NULL
      }
    }

    levels_var <- levels(factor(kharkiv_data[[v]]))

    for (lev in levels_var) {

      sub <- kharkiv_data %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$idu_ever_bin)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {

        or_fmt <- "ref."

      } else if (!is.null(tidy_mod)) {

        term_match <- tidy_mod$term[grepl(lev, tidy_mod$term)]

        if (length(term_match) == 1) {

          row <- tidy_mod[tidy_mod$term == term_match, ]

          if (!is.na(row$conf.low)) {
            or_fmt <- sprintf(
              "%.2f (%.2f-%.2f)",
              row$estimate,
              row$conf.low,
              row$conf.high
            )
          } else {
            or_fmt <- sprintf("%.2f", row$estimate)
          }

        } else {
          or_fmt <- NA
        }

      } else {
        or_fmt <- NA
      }

      results_list[[row_id]] <- data.frame(
        Variable = v,
        Level = lev,
        N = n,
        Cases = cases,
        Percent = pct,
        CountPercent = count_pct,
        OR = or_fmt,
        stringsAsFactors = FALSE
      )

      row_id <- row_id + 1
    }
  }

  all_sheets[[cty]] <- dplyr::bind_rows(results_list)
}

## save
write_xlsx(all_sheets, "idu_ever_by_city.xlsx")

# art

## load data
sw_combined_clean <- readRDS("sw_combined_clean_kharkiv.rds")

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    art_current_bin = case_when(
      art_current_3cat %in% c("Yes","1") ~ 1,
      art_current_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    city = as.factor(city),
    year = as.factor(year)
  )

table(sw_combined_clean$art_current_bin)

## variables
vars <- c(
  "condom_access_12m_3cat","ngo_access_lifetime_3cat","city_travel_12m_cat","street_sw_bin",
  "alcohol_30d_bin","used_syringe_last_3cat","idu_ever_3cat","idu_12m_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat","violence_rape_12m_3cat",
  "violence_rape_ever","violence_beaten_ever","violence_physical_abuse_ever",
  "violence_police","violence_pimp","violence_support_ngo","age_bin",
  "occupied","occupied_partial","avoided_healthcare_12m_stigma",
  "avoided_healthcare_12m_violence","avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police","avoided_hiv_test_12m_violence",
  "avoided_hiv_test_12m_stigma","underage_first_sw_bin"
)

## convert binary variables
sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(vars),
                ~ factor(case_when(
                  . == "Yes" ~ "Yes",
                  . == "No" ~ "No",
                  TRUE ~ NA_character_
                ), levels = c("No", "Yes"))
  ))

## containers
all_sheets <- list()
cities <- c(levels(sw_combined_clean$city), "Overall")

## loop
for (cty in cities) {

  cat("Processing:", cty, "\n")

  if (cty == "Overall") {
    city_data <- sw_combined_clean
  } else {
    city_data <- sw_combined_clean %>% filter(city == cty)
  }

  results <- data.frame()

  for (v in vars) {

    if (!v %in% names(city_data)) next

    kharkiv_data <- city_data %>%
      select(art_current_bin, city, year, all_of(v)) %>%
      filter(!is.na(art_current_bin), !is.na(.data[[v]]))

    if (nrow(kharkiv_data) == 0) next
    if (length(unique(kharkiv_data$art_current_bin)) < 2) next
    if (length(unique(kharkiv_data[[v]])) < 2) next

    ## MODEL
    if (cty == "Overall") {
      formula <- if (length(unique(kharkiv_data$year)) == 1) {
        as.formula(paste("art_current_bin ~ city +", v))
      } else {
        as.formula(paste("art_current_bin ~ city + year +", v))
      }
    } else {
      formula <- if (length(unique(kharkiv_data$year)) == 1) {
        as.formula(paste("art_current_bin ~", v))
      } else {
        as.formula(paste("art_current_bin ~ year +", v))
      }
    }

    model <- try(glm(formula, data = kharkiv_data, family = binomial), silent = TRUE)

    ## SAFE tidy (FIXED)
    tidy_mod <- NULL

    if (!inherits(model, "try-error")) {

      tidy_mod <- try(
        broom::tidy(model, conf.int = TRUE, exponentiate = TRUE),
        silent = TRUE
      )

      if (!inherits(tidy_mod, "try-error")) {
        tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
      } else {
        tidy_mod <- NULL
      }
    }

    levels_var <- levels(factor(kharkiv_data[[v]]))

    for (lev in levels_var) {

      sub <- kharkiv_data %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$art_current_bin)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      ## SAFE OR extraction
      if (lev == levels_var[1]) {

        or_fmt <- "ref."

      } else if (!is.null(tidy_mod)) {

        term_match <- tidy_mod$term[grepl(lev, tidy_mod$term)]

        if (length(term_match) == 1) {

          row <- tidy_mod[tidy_mod$term == term_match, ]

          if (!is.na(row$conf.low)) {
            or_fmt <- sprintf("%.2f (%.2f-%.2f)",
                              row$estimate, row$conf.low, row$conf.high)
          } else {
            or_fmt <- sprintf("%.2f", row$estimate)
          }

        } else {
          or_fmt <- NA
        }

      } else {
        or_fmt <- NA
      }

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

  all_sheets[[cty]] <- results
}

## save
write_xlsx(all_sheets, "art_current_by_city.xlsx")





## packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, broom)

## load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# variables
vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat", "ngo_access_lifetime_3cat",
  "city_travel_12m_cat", "street_sw_bin", "typology_primary_6m",
  "typology_primary_30d_3cat", "typology_primary_30d", "alcohol_30d_bin",
  "used_syringe_last_3cat", "idu_ever_3cat", "idu_12m_3cat",
  "sw_partners_clients_30d_3cat", "violence_any_ever_3cat",
  "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp",
  "violence_support_ngo", "age_bin", "occupied", "occupied_partial",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence",
  "avoided_healthcare_12m_police", "avoided_hiv_test_12m_police",
  "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma",
  "underage_first_sw_bin"
)

# binary conversion
binary_vars <- vars

sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(binary_vars),
    ~ factor(case_when(
      . == "Yes" ~ "Yes",
      . == "No" ~ "No",
      TRUE ~ NA_character_
    ), levels = c("No", "Yes"))
  ))

# outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    ),
    year = as.factor(year),
    years_in_sw = as.factor(years_in_sw)
  )

# datasets
datasets <- list(
  Overall = sw_combined_clean,
  IDU = sw_combined_clean %>% filter(idu_ever_3cat == "Yes"),
  No_IDU = sw_combined_clean %>% filter(idu_ever_3cat == "No")
)

## function
compute_results <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(hiv_test_rslt_bin, years_in_sw, year, all_of(v)) %>%
      filter(
        !is.na(hiv_test_rslt_bin),
        !is.na(.data[[v]]),
        !is.na(years_in_sw)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$hiv_test_rslt_bin)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw", v, sep = " + ")

    if (length(unique(dat$year)) > 1) {
      rhs <- paste(rhs, "+ year")
    }

    fml <- as.formula(paste("hiv_test_rslt_bin ~", rhs))

    model <- try(glm(fml, data = dat, family = binomial), silent = TRUE)

    tidy_mod <- NULL
    if (!inherits(model, "try-error")) {
      tidy_mod <- try(
        broom::tidy(model, conf.int = TRUE, exponentiate = TRUE),
        silent = TRUE
      )
      if (!inherits(tidy_mod, "try-error")) {
        tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
      } else {
        tidy_mod <- NULL
      }
    }

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$hiv_test_rslt_bin, na.rm = TRUE)
      count_pct <- sprintf("%d (%.1f%%)", cases, round(cases / n * 100, 1))

      if (lev == levels_var[1]) {
        or_fmt <- "ref."
      } else if (!is.null(tidy_mod)) {

        term_match <- tidy_mod[grepl(paste0("^", v), tidy_mod$term), ]
        term_match <- term_match[grepl(lev, term_match$term), ]

        if (nrow(term_match) == 1) {
          row <- term_match[1, ]

          if (!is.na(row$conf.low)) {
            or_fmt <- sprintf("%.2f (%.2f-%.2f)",
              row$estimate, row$conf.low, row$conf.high)
          } else {
            or_fmt <- sprintf("%.2f", row$estimate)
          }

        } else {
          or_fmt <- NA
        }

      } else {
        or_fmt <- NA
      }

      results <- rbind(results, data.frame(
        Variable = v,
        Level = lev,
        N = n,
        Cases = cases,
        `x (x.x%)` = count_pct,
        OR = or_fmt
      ))
    }
  }

  results
}

## run
all_sheets <- list()

for (name in names(datasets)) {
  cat("processing:", name, "\n")
  all_sheets[[name]] <- compute_results(datasets[[name]], vars)
}

write_xlsx(all_sheets, "hiv_results_idu_stratified.xlsx")

## condom use

## load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    condom_access_12m_3cat = case_when(
      condom_access_12m_3cat %in% c("Yes","1") ~ 1,
      condom_access_12m_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    city = as.factor(city),
    year = as.factor(year),
    years_in_sw = as.factor(years_in_sw)
  )

## variables
vars <- c(
  "ngo_access_lifetime_3cat","city_travel_12m_cat","street_sw_bin",
  "alcohol_30d_bin","used_syringe_last_3cat","idu_ever_3cat","idu_12m_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat","violence_rape_12m_3cat",
  "violence_rape_ever","violence_beaten_ever","violence_physical_abuse_ever",
  "violence_police","violence_pimp","violence_support_ngo","age_bin",
  "occupied","occupied_partial","avoided_healthcare_12m_stigma",
  "avoided_healthcare_12m_violence","avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police","avoided_hiv_test_12m_violence",
  "avoided_hiv_test_12m_stigma","underage_first_sw_bin"
)

## binary conversion
sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(vars),
    ~ factor(case_when(
      . == "Yes" ~ "Yes",
      . == "No" ~ "No",
      TRUE ~ NA_character_
    ), levels = c("No","Yes"))
  ))

## datasets
datasets <- list(
  Overall = sw_combined_clean,
  IDU = sw_combined_clean %>% filter(idu_ever_3cat == "Yes"),
  No_IDU = sw_combined_clean %>% filter(idu_ever_3cat == "No")
)

## function
compute_results <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(condom_access_12m_3cat, city, year, years_in_sw, all_of(v)) %>%
      filter(
        !is.na(condom_access_12m_3cat),
        !is.na(.data[[v]]),
        !is.na(years_in_sw)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$condom_access_12m_3cat)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw", v, sep = " + ")

    if (length(unique(dat$year)) > 1) {
      rhs <- paste(rhs, "+ year")
    }

    fml <- as.formula(paste("condom_access_12m_3cat ~", rhs))

    model <- try(glm(fml, data = dat, family = binomial), silent = TRUE)

    tidy_mod <- NULL
    if (!inherits(model, "try-error")) {
      tidy_mod <- try(
        broom::tidy(model, conf.int = TRUE, exponentiate = TRUE),
        silent = TRUE
      )
      if (!inherits(tidy_mod, "try-error")) {
        tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
      } else {
        tidy_mod <- NULL
      }
    }

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$condom_access_12m_3cat, na.rm = TRUE)
      count_pct <- sprintf("%d (%.1f%%)", cases, round(cases / n * 100, 1))

      if (lev == levels_var[1]) {
        or_fmt <- "ref."
      } else if (!is.null(tidy_mod)) {

        term_match <- tidy_mod[grepl(paste0("^", v), tidy_mod$term), ]
        term_match <- term_match[grepl(lev, term_match$term), ]

        if (nrow(term_match) == 1) {
          row <- term_match[1, ]

          if (!is.na(row$conf.low)) {
            or_fmt <- sprintf("%.2f (%.2f-%.2f)",
              row$estimate, row$conf.low, row$conf.high)
          } else {
            or_fmt <- sprintf("%.2f", row$estimate)
          }

        } else {
          or_fmt <- NA
        }

      } else {
        or_fmt <- NA
      }

      results <- rbind(results, data.frame(
        variable = v,
        level = lev,
        n = n,
        cases = cases,
        pct = count_pct,
        or = or_fmt
      ))
    }
  }

  results
}

## run
all_sheets <- list()

for (name in names(datasets)) {
  cat("processing:", name, "\n")
  all_sheets[[name]] <- compute_results(datasets[[name]], vars)
}

write_xlsx(all_sheets, "condom_12m_idu_stratified.xlsx")

## rape

# load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    rape_bin = case_when(
      violence_rape_ever %in% c("Yes","1") ~ 1,
      violence_rape_ever %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    city = as.factor(city),
    year = as.factor(year),
    years_in_sw = as.factor(years_in_sw)
  )

# exposure variables
vars <- c(
  "client_condom_lastsex","condom_access_12m_3cat","ngo_access_lifetime_3cat",
  "city_travel_12m_cat","street_sw_bin","alcohol_30d_bin","used_syringe_last_3cat",
  "idu_ever_3cat","idu_12m_3cat","sw_partners_clients_30d_3cat",
  "violence_beaten_ever","violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "avoided_healthcare_12m_stigma","avoided_healthcare_12m_violence",
  "avoided_healthcare_12m_police","avoided_hiv_test_12m_police",
  "avoided_hiv_test_12m_violence","avoided_hiv_test_12m_stigma",
  "underage_first_sw_bin"
)

# convert exposures to factors
sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(vars),
    ~ factor(case_when(
      . == "Yes" ~ "Yes",
      . == "No" ~ "No",
      TRUE ~ NA_character_
    ), levels = c("No","Yes"))
  ))

# stratified datasets
datasets <- list(
  overall = sw_combined_clean,
  idu = sw_combined_clean %>% filter(idu_ever_3cat == "Yes"),
  no_idu = sw_combined_clean %>% filter(idu_ever_3cat == "No")
)

# function to run logistic models and build table
compute_results <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    # subset complete cases
    data_sub <- df %>%
      select(rape_bin, city, year, years_in_sw, all_of(v)) %>%
      filter(
        !is.na(rape_bin),
        !is.na(.data[[v]]),
        !is.na(years_in_sw)
      )

    # skip low variation
    if (nrow(data_sub) == 0 ||
        length(unique(data_sub$rape_bin)) < 2 ||
        length(unique(data_sub[[v]])) < 2) next

    # model formula
    rhs <- paste("years_in_sw", v, sep = " + ")

    if (length(unique(data_sub$year)) > 1) {
      rhs <- paste(rhs, "+ year")
    }

    model <- try(
      glm(as.formula(paste("rape_bin ~", rhs)),
          data = data_sub,
          family = binomial),
      silent = TRUE
    )

    # tidy model safely
    tidy_mod <- NULL

    if (!inherits(model, "try-error")) {
      tidy_mod <- try(
        broom::tidy(model, conf.int = TRUE, exponentiate = TRUE),
        silent = TRUE
      )

      if (!inherits(tidy_mod, "try-error")) {
        tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
      } else {
        tidy_mod <- NULL
      }
    }

    # exposure levels
    levels_var <- sort(unique(data_sub[[v]]))

    for (lev in levels_var) {

      sub <- data_sub %>% filter(.data[[v]] == lev)
      n <- nrow(sub)

      if (n == 0) next

      cases <- sum(sub$rape_bin, na.rm = TRUE)
      count_pct <- sprintf("%d (%.1f%%)", cases, round(cases / n * 100, 1))

      # reference category handling
      if (lev == levels_var[1]) {

        or_fmt <- "ref."

      } else if (!is.null(tidy_mod)) {

        term_match <- tidy_mod[grepl(paste0("^", v), tidy_mod$term), ]
        term_match <- term_match[grepl(lev, term_match$term), ]

        if (nrow(term_match) == 1) {

          row <- term_match[1, ]

          if (!is.na(row$conf.low)) {
            or_fmt <- sprintf(
              "%.2f (%.2f-%.2f)",
              row$estimate,
              row$conf.low,
              row$conf.high
            )
          } else {
            or_fmt <- sprintf("%.2f", row$estimate)
          }

        } else {
          or_fmt <- NA
        }

      } else {
        or_fmt <- NA
      }

      results <- rbind(
        results,
        data.frame(
          variable = v,
          level = lev,
          n = n,
          cases = cases,
          pct = count_pct,
          or = or_fmt
        )
      )
    }
  }

  results
}

# run models
all_sheets <- list()

for (name in names(datasets)) {
  cat("processing:", name, "\n")
  all_sheets[[name]] <- compute_results(datasets[[name]], vars)
}

# export results
write_xlsx(all_sheets, "rape_idu_stratified.xlsx")

## art

## load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    art_current_bin = case_when(
      art_current_3cat %in% c("Yes","1") ~ 1,
      art_current_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    city = as.factor(city),
    year = as.factor(year)
  )

## variables
vars <- c(
  "condom_access_12m_3cat","ngo_access_lifetime_3cat","city_travel_12m_cat","street_sw_bin",
  "alcohol_30d_bin","used_syringe_last_3cat","idu_ever_3cat","idu_12m_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat","violence_rape_12m_3cat",
  "violence_rape_ever","violence_beaten_ever","violence_physical_abuse_ever",
  "violence_police","violence_pimp","violence_support_ngo","age_bin",
  "occupied","occupied_partial","avoided_healthcare_12m_stigma",
  "avoided_healthcare_12m_violence","avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police","avoided_hiv_test_12m_violence",
  "avoided_hiv_test_12m_stigma","underage_first_sw_bin"
)

## convert binary variables
sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(vars),
    ~ factor(case_when(
      . == "Yes" ~ "Yes",
      . == "No" ~ "No",
      TRUE ~ NA_character_
    ), levels = c("No", "Yes"))
  ))

## containers
all_sheets <- list()
cities <- c(levels(sw_combined_clean$city), "Overall")

## loop over cities
for (cty in cities) {

  cat("processing:", cty, "\n")

  city_data <- if (cty == "Overall") {
    sw_combined_clean
  } else {
    sw_combined_clean %>% filter(city == cty)
  }

  results <- data.frame()

  for (v in vars) {

    if (!v %in% names(city_data)) next

    dat <- city_data %>%
      select(art_current_bin, city, year, all_of(v)) %>%
      filter(!is.na(art_current_bin), !is.na(.data[[v]]))

    if (nrow(dat) == 0) next
    if (length(unique(dat$art_current_bin)) < 2) next
    if (length(unique(dat[[v]])) < 2) next

    ## model
    if (cty == "Overall") {
      formula <- if (length(unique(dat$year)) == 1) {
        as.formula(paste("art_current_bin ~ city +", v))
      } else {
        as.formula(paste("art_current_bin ~ city + year +", v))
      }
    } else {
      formula <- if (length(unique(dat$year)) == 1) {
        as.formula(paste("art_current_bin ~", v))
      } else {
        as.formula(paste("art_current_bin ~ year +", v))
      }
    }

    model <- try(glm(formula, data = dat, family = binomial), silent = TRUE)

    tidy_mod <- NULL

    if (!inherits(model, "try-error")) {
      tidy_mod <- try(
        broom::tidy(model, conf.int = TRUE, exponentiate = TRUE),
        silent = TRUE
      )

      if (!inherits(tidy_mod, "try-error")) {
        tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
      } else {
        tidy_mod <- NULL
      }
    }

    levels_var <- levels(factor(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$art_current_bin, na.rm = TRUE)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {

        or_fmt <- "ref."

      } else if (!is.null(tidy_mod)) {

        term_match <- tidy_mod$term[grepl(lev, tidy_mod$term)]

        if (length(term_match) == 1) {

          row <- tidy_mod[tidy_mod$term == term_match, ]

          if (!is.na(row$conf.low)) {
            or_fmt <- sprintf(
              "%.2f (%.2f-%.2f)",
              row$estimate, row$conf.low, row$conf.high
            )
          } else {
            or_fmt <- sprintf("%.2f", row$estimate)
          }

        } else {
          or_fmt <- NA
        }

      } else {
        or_fmt <- NA
      }

      results <- rbind(
        results,
        data.frame(
          Variable = v,
          Level = lev,
          N = n,
          Cases = cases,
          Percent = pct,
          CountPercent = count_pct,
          OR = or_fmt
        )
      )
    }
  }

  all_sheets[[cty]] <- results
}

## save
write_xlsx(all_sheets, "art_current_by_city.xlsx")