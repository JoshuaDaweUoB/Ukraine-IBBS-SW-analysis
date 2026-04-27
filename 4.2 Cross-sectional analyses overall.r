## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, 
               lubridate, broom, survival, ggplot2, scales, sandwich, lmtest, lme4, 
               broom.mixed) 
               
## set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

## hiv 

## load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

sw_combined_clean %>%
  group_by(city) %>%
  summarise(n_years = n_distinct(year))

## variables
vars <- c(
  "condom_access_12m_3cat","client_condom_lastsex_3cat","ngo_client_lifetime_3cat",
  "ngo_condom_rec_bin", "ngo_syringe_12m_bin",
  "city_travel_12m_cat","street_sw_bin", "typology_primary_6m_3cat",
  "typology_primary_30d_3cat","alcohol_30d_bin",
  "used_syringe_last_3cat","idu_ever_3cat","idu_12m_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat",
  "violence_rape_12m_3cat","violence_rape_ever","violence_beaten_ever",
  "violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "underage_first_sw_bin"
)

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    ),
    years_in_sw_3cat = as.factor(years_in_sw_3cat),
    city = as.factor(city),
    year = as.factor(year)
  )

table(sw_combined_clean$hiv_test_rslt_bin, useNA = "ifany")

## function (Logistic OR model with glmer and mixed effects for city and year)
compute_results_overall <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(hiv_test_rslt_bin, years_in_sw_3cat, year, city, all_of(v)) %>%
      filter(
        !is.na(hiv_test_rslt_bin),
        !is.na(.data[[v]]),
        !is.na(years_in_sw_3cat)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$hiv_test_rslt_bin)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(
      glmer(as.formula(paste("hiv_test_rslt_bin ~", rhs, "+ (1 | year) + (1 | city)")),
            data = dat,
            family = binomial(link = "logit"),
            control = glmerControl(optimizer = "bobyqa")),
      silent = TRUE
    )

    if (inherits(model, "try-error")) next

    ## Extract model coefficients with Wald-based CIs (exponentiated for ORs)
    tidy_mod <- try(broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE, exponentiate = TRUE),
                    silent = TRUE)
    
    if (inherits(tidy_mod, "try-error")) next
    
    # Remove intercept
    tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$hiv_test_rslt_bin, na.rm = TRUE)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        or_fmt <- "ref."
      } else if (nrow(tidy_mod) > 0) {

        term <- tidy_mod[
          grepl(v, tidy_mod$term) &
          grepl(lev, tidy_mod$term),
        ]

        if (nrow(term) == 1) {
          or_fmt <- sprintf("%.2f (%.2f-%.2f)",
                            term$estimate,
                            term$conf.low,
                            term$conf.high)
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
        Percent = pct,
        CountPercent = count_pct,
        OR = or_fmt
      ))
    }
  }

  results
}

## overall datasets
overall_data <- sw_combined_clean %>%
  filter(!is.na(hiv_test_rslt_bin), !is.na(years_in_sw_3cat))

idu_data <- overall_data %>%
  filter(idu_ever_3cat == "Yes")

no_idu_data <- overall_data %>%
  filter(idu_ever_3cat == "No")

street_data <- overall_data %>%
  filter(street_sw_bin == "Yes")

no_street_data <- overall_data %>%
  filter(street_sw_bin == "No")

ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "Yes")

no_ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "No")

overall_out    <- compute_results_overall(overall_data, vars)
idu_out        <- compute_results_overall(idu_data, vars)
no_idu_out     <- compute_results_overall(no_idu_data, vars)
street_out     <- compute_results_overall(street_data, vars)
no_street_out  <- compute_results_overall(no_street_data, vars)
ngo_out        <- compute_results_overall(ngo_data, vars)
no_ngo_out     <- compute_results_overall(no_ngo_data, vars)

## save
final_output <- list(
  Overall = overall_out,
  IDU = idu_out,
  No_IDU = no_idu_out,
  Street = street_out,
  No_Street = no_street_out,
  NGO = ngo_out,
  No_NGO = no_ngo_out
)

write_xlsx(final_output,
           "outputs_by_city/hiv_results_OVERALL.xlsx")

## hiv testing past 12 months

## load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## variables
vars <- c(
  "condom_access_12m_3cat","ngo_client_lifetime_3cat", "client_condom_lastsex_3cat",
  "ngo_condom_rec_bin", "ngo_syringe_12m_bin", "hiv_test_rslt_bin",
  "city_travel_12m_cat","street_sw_bin", "typology_primary_6m_3cat",
  "typology_primary_30d_3cat","alcohol_30d_bin",
  "used_syringe_last_3cat","idu_ever_3cat","idu_12m_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat",
  "violence_rape_12m_3cat","violence_rape_ever","violence_beaten_ever",
  "violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "underage_first_sw_bin"
)

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    hiv_tested_12m_3cat = case_when(
      hiv_tested_12m_3cat %in% c("Yes","1") ~ 1,
      hiv_tested_12m_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    years_in_sw_3cat = as.factor(years_in_sw_3cat),
    city = as.factor(city),
    year = as.factor(year)
  )

table(sw_combined_clean$hiv_tested_12m_3cat, useNA = "ifany")

compute_results_overall <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(hiv_tested_12m_3cat, years_in_sw_3cat, year, city, all_of(v)) %>%
      filter(
        !is.na(hiv_tested_12m_3cat),
        !is.na(.data[[v]]),
        !is.na(years_in_sw_3cat)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$hiv_tested_12m_3cat)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(
      glmer(as.formula(paste("hiv_tested_12m_3cat ~", rhs, "+ (1 | year) + (1 | city)")),
            data = dat,
            family = poisson(link = "log"),
            control = glmerControl(optimizer = "bobyqa")),
      silent = TRUE
    )

    if (inherits(model, "try-error")) next

    tidy_mod <- try(
      broom.mixed::tidy(model, effects = "fixed",
                        conf.int = TRUE, exponentiate = TRUE),
      silent = TRUE
    )

    if (inherits(tidy_mod, "try-error")) next

    tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$hiv_tested_12m_3cat, na.rm = TRUE)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        pr_fmt <- "ref."
      } else if (nrow(tidy_mod) > 0) {

        term <- tidy_mod[
          grepl(v, tidy_mod$term) &
          grepl(lev, tidy_mod$term),
        ]

        if (nrow(term) == 1) {
          pr_fmt <- sprintf("%.2f (%.2f-%.2f)",
                            term$estimate,
                            term$conf.low,
                            term$conf.high)
        } else {
          pr_fmt <- NA
        }
      } else {
        pr_fmt <- NA
      }

      results <- rbind(results, data.frame(
        Variable = v,
        Level = lev,
        N = n,
        Cases = cases,
        Percent = pct,
        CountPercent = count_pct,
        PR = pr_fmt
      ))
    }
  }

  results
}

## overall datasets
overall_data <- sw_combined_clean %>%
  filter(!is.na(hiv_test_rslt_bin), !is.na(years_in_sw_3cat))

idu_data <- overall_data %>%
  filter(idu_ever_3cat == "Yes")

no_idu_data <- overall_data %>%
  filter(idu_ever_3cat == "No")

street_data <- overall_data %>%
  filter(street_sw_bin == "Yes")

no_street_data <- overall_data %>%
  filter(street_sw_bin == "No")

ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "Yes")

no_ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "No")

overall_out    <- compute_results_overall(overall_data, vars)
idu_out        <- compute_results_overall(idu_data, vars)
no_idu_out     <- compute_results_overall(no_idu_data, vars)
street_out     <- compute_results_overall(street_data, vars)
no_street_out  <- compute_results_overall(no_street_data, vars)
ngo_out        <- compute_results_overall(ngo_data, vars)
no_ngo_out     <- compute_results_overall(no_ngo_data, vars)

## save
final_output <- list(
  Overall = overall_out,
  IDU = idu_out,
  No_IDU = no_idu_out,
  Street = street_out,
  No_Street = no_street_out,
  NGO = ngo_out,
  No_NGO = no_ngo_out
)

write_xlsx(final_output,
           "outputs_by_city/hiv_tested_12m_results_OVERALL.xlsx")

## art use 

## load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

sw_combined_clean %>%
  group_by(city) %>%
  summarise(n_years = n_distinct(year))

## variables
vars <- c(
  "condom_access_12m_3cat","client_condom_lastsex_3cat","ngo_client_lifetime_3cat",
  "ngo_condom_rec_bin", "ngo_syringe_12m_bin",
  "city_travel_12m_cat","street_sw_bin", "typology_primary_6m_3cat",
  "typology_primary_30d_3cat","alcohol_30d_bin",
  "used_syringe_last_3cat","idu_ever_3cat","idu_12m_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat",
  "violence_rape_12m_3cat","violence_rape_ever","violence_beaten_ever",
  "violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "underage_first_sw_bin"
)

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    art_current_3cat = case_when(
      art_current_3cat %in% c("Positive","1","Yes") ~ 1,
      art_current_3cat %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    ),
    years_in_sw_3cat = as.factor(years_in_sw_3cat),
    city = as.factor(city),
    year = as.factor(year)
  )

table(sw_combined_clean$art_current_3cat, useNA = "ifany")

## function (Logistic OR model with glmer and mixed effects for city and year)
compute_results_overall <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(art_current_3cat, years_in_sw_3cat, year, city, all_of(v)) %>%
      filter(
        !is.na(art_current_3cat),
        !is.na(.data[[v]]),
        !is.na(years_in_sw_3cat)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$art_current_3cat)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(
      glmer(as.formula(paste("art_current_3cat ~", rhs, "+ (1 | year) + (1 | city)")),
            data = dat,
            family = binomial(link = "logit"),
            control = glmerControl(optimizer = "bobyqa")),
      silent = TRUE
    )

    if (inherits(model, "try-error")) next

    ## Extract model coefficients with Wald-based CIs (exponentiated for ORs)
    tidy_mod <- try(broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE, exponentiate = TRUE),
                    silent = TRUE)
    
    if (inherits(tidy_mod, "try-error")) next
    
    # Remove intercept
    tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$art_current_3cat, na.rm = TRUE)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        or_fmt <- "ref."
      } else if (nrow(tidy_mod) > 0) {

        term <- tidy_mod[
          grepl(v, tidy_mod$term) &
          grepl(lev, tidy_mod$term),
        ]

        if (nrow(term) == 1) {
          or_fmt <- sprintf("%.2f (%.2f-%.2f)",
                            term$estimate,
                            term$conf.low,
                            term$conf.high)
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
        Percent = pct,
        CountPercent = count_pct,
        OR = or_fmt
      ))
    }
  }

  results
}

## overall datasets
overall_data <- sw_combined_clean %>%
  filter(!is.na(art_current_3cat), !is.na(years_in_sw_3cat))

idu_data <- overall_data %>%
  filter(idu_ever_3cat == "Yes")

no_idu_data <- overall_data %>%
  filter(idu_ever_3cat == "No")

street_data <- overall_data %>%
  filter(street_sw_bin == "Yes")

no_street_data <- overall_data %>%
  filter(street_sw_bin == "No")

ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "Yes")

no_ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "No")

overall_out    <- compute_results_overall(overall_data, vars)
idu_out        <- compute_results_overall(idu_data, vars)
no_idu_out     <- compute_results_overall(no_idu_data, vars)
street_out     <- compute_results_overall(street_data, vars)
no_street_out  <- compute_results_overall(no_street_data, vars)
ngo_out        <- compute_results_overall(ngo_data, vars)
no_ngo_out     <- compute_results_overall(no_ngo_data, vars)

## save
final_output <- list(
  Overall = overall_out,
  IDU = idu_out,
  No_IDU = no_idu_out,
  Street = street_out,
  No_Street = no_street_out,
  NGO = ngo_out,
  No_NGO = no_ngo_out
)

write_xlsx(final_output,
           "outputs_by_city/art_results_OVERALL.xlsx")



## condom access 12m

## load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## variables
vars <- c(
  "client_condom_lastsex_3cat","ngo_client_lifetime_3cat",
  "ngo_condom_rec_bin", "ngo_syringe_12m_bin", "hiv_test_rslt_bin",
  "city_travel_12m_cat","street_sw_bin", "typology_primary_6m_3cat",
  "typology_primary_30d_3cat","alcohol_30d_bin",
  "used_syringe_last_3cat","idu_ever_3cat","idu_12m_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat",
  "violence_rape_12m_3cat","violence_rape_ever","violence_beaten_ever",
  "violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "underage_first_sw_bin"
)

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    condom_access_12m_bin = case_when(
      condom_access_12m_3cat %in% c("Yes","1") ~ 1,
      condom_access_12m_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    ),
    years_in_sw_3cat = as.factor(years_in_sw_3cat),
    city = as.factor(city),
    year = as.factor(year)
  )

table(sw_combined_clean$condom_access_12m_bin, useNA = "ifany")

## function (Logistic OR model with glmer and mixed effects for city and year)
compute_results_overall <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(condom_access_12m_bin, years_in_sw_3cat, year, city, all_of(v)) %>%
      filter(
        !is.na(condom_access_12m_bin),
        !is.na(.data[[v]]),
        !is.na(years_in_sw_3cat)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$condom_access_12m_bin)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(
      glmer(as.formula(paste("condom_access_12m_bin ~", rhs, "+ (1 | year) + (1 | city)")),
            data = dat,
            family = binomial(link = "logit"),
            control = glmerControl(optimizer = "bobyqa")),
      silent = TRUE
    )

    if (inherits(model, "try-error")) next

    ## Extract model coefficients with Wald-based CIs (exponentiated for ORs)
    tidy_mod <- try(broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE, exponentiate = TRUE),
                    silent = TRUE)
    
    if (inherits(tidy_mod, "try-error")) next
    
    # Remove intercept
    tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$condom_access_12m_bin, na.rm = TRUE)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        or_fmt <- "ref."
      } else if (nrow(tidy_mod) > 0) {

        term <- tidy_mod[
          grepl(v, tidy_mod$term) &
          grepl(lev, tidy_mod$term),
        ]

        if (nrow(term) == 1) {
          or_fmt <- sprintf("%.2f (%.2f-%.2f)",
                            term$estimate,
                            term$conf.low,
                            term$conf.high)
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
        Percent = pct,
        CountPercent = count_pct,
        OR = or_fmt
      ))
    }
  }

  results
}

## overall datasets
overall_data <- sw_combined_clean %>%
  filter(!is.na(hiv_test_rslt_bin), !is.na(years_in_sw_3cat))

idu_data <- overall_data %>%
  filter(idu_ever_3cat == "Yes")

no_idu_data <- overall_data %>%
  filter(idu_ever_3cat == "No")

street_data <- overall_data %>%
  filter(street_sw_bin == "Yes")

no_street_data <- overall_data %>%
  filter(street_sw_bin == "No")

ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "Yes")

no_ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "No")

overall_out    <- compute_results_overall(overall_data, vars)
idu_out        <- compute_results_overall(idu_data, vars)
no_idu_out     <- compute_results_overall(no_idu_data, vars)
street_out     <- compute_results_overall(street_data, vars)
no_street_out  <- compute_results_overall(no_street_data, vars)
ngo_out        <- compute_results_overall(ngo_data, vars)
no_ngo_out     <- compute_results_overall(no_ngo_data, vars)

## save
final_output <- list(
  Overall = overall_out,
  IDU = idu_out,
  No_IDU = no_idu_out,
  Street = street_out,
  No_Street = no_street_out,
  NGO = ngo_out,
  No_NGO = no_ngo_out
)

write_xlsx(final_output,
           "outputs_by_city/condom_access_results_OVERALL.xlsx")

## condom use last sex

## load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## variables
vars <- c(
  "condom_access_12m_3cat","ngo_client_lifetime_3cat",
  "ngo_condom_rec_bin", "ngo_syringe_12m_bin", "hiv_test_rslt_bin",
  "city_travel_12m_cat","street_sw_bin", "typology_primary_6m_3cat",
  "typology_primary_30d_3cat","alcohol_30d_bin",
  "used_syringe_last_3cat","idu_ever_3cat","idu_12m_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat",
  "violence_rape_12m_3cat","violence_rape_ever","violence_beaten_ever",
  "violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "underage_first_sw_bin"
)

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    client_condom_lastsex_3cat = case_when(
      client_condom_lastsex_3cat %in% c("Yes","1") ~ 1,
      client_condom_lastsex_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    ),
    years_in_sw_3cat = as.factor(years_in_sw_3cat),
    city = as.factor(city),
    year = as.factor(year)
  )

table(sw_combined_clean$client_condom_lastsex_3cat, useNA = "ifany")

compute_results_overall <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(client_condom_lastsex_3cat, years_in_sw_3cat, year, city, all_of(v)) %>%
      filter(
        !is.na(client_condom_lastsex_3cat),
        !is.na(.data[[v]]),
        !is.na(years_in_sw_3cat)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$client_condom_lastsex_3cat)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(
      glmer(as.formula(paste("client_condom_lastsex_3cat ~", rhs, "+ (1 | year) + (1 | city)")),
            data = dat,
            family = poisson(link = "log"),
            control = glmerControl(optimizer = "bobyqa")),
      silent = TRUE
    )

    if (inherits(model, "try-error")) next

    tidy_mod <- try(
      broom.mixed::tidy(model, effects = "fixed",
                        conf.int = TRUE, exponentiate = TRUE),
      silent = TRUE
    )

    if (inherits(tidy_mod, "try-error")) next

    tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$client_condom_lastsex_3cat, na.rm = TRUE)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        pr_fmt <- "ref."
      } else if (nrow(tidy_mod) > 0) {

        term <- tidy_mod[
          grepl(v, tidy_mod$term) &
          grepl(lev, tidy_mod$term),
        ]

        if (nrow(term) == 1) {
          pr_fmt <- sprintf("%.2f (%.2f-%.2f)",
                            term$estimate,
                            term$conf.low,
                            term$conf.high)
        } else {
          pr_fmt <- NA
        }
      } else {
        pr_fmt <- NA
      }

      results <- rbind(results, data.frame(
        Variable = v,
        Level = lev,
        N = n,
        Cases = cases,
        Percent = pct,
        CountPercent = count_pct,
        PR = pr_fmt
      ))
    }
  }

  results
}

## overall datasets
overall_data <- sw_combined_clean %>%
  filter(!is.na(hiv_test_rslt_bin), !is.na(years_in_sw_3cat))

idu_data <- overall_data %>%
  filter(idu_ever_3cat == "Yes")

no_idu_data <- overall_data %>%
  filter(idu_ever_3cat == "No")

street_data <- overall_data %>%
  filter(street_sw_bin == "Yes")

no_street_data <- overall_data %>%
  filter(street_sw_bin == "No")

ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "Yes")

no_ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "No")

overall_out    <- compute_results_overall(overall_data, vars)
idu_out        <- compute_results_overall(idu_data, vars)
no_idu_out     <- compute_results_overall(no_idu_data, vars)
street_out     <- compute_results_overall(street_data, vars)
no_street_out  <- compute_results_overall(no_street_data, vars)
ngo_out        <- compute_results_overall(ngo_data, vars)
no_ngo_out     <- compute_results_overall(no_ngo_data, vars)

## save
final_output <- list(
  Overall = overall_out,
  IDU = idu_out,
  No_IDU = no_idu_out,
  Street = street_out,
  No_Street = no_street_out,
  NGO = ngo_out,
  No_NGO = no_ngo_out
)

write_xlsx(final_output,
           "outputs_by_city/condom_lastsex_results_OVERALL.xlsx")

## NGO client

## load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## variables
vars <- c(
  "condom_access_12m_3cat",
  "ngo_condom_rec_bin", "ngo_syringe_12m_bin", "hiv_test_rslt_bin",
  "city_travel_12m_cat","street_sw_bin", "typology_primary_6m_3cat",
  "typology_primary_30d_3cat","alcohol_30d_bin",
  "used_syringe_last_3cat","idu_ever_3cat","idu_12m_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat",
  "violence_rape_12m_3cat","violence_rape_ever","violence_beaten_ever",
  "violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "underage_first_sw_bin"
)

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ngo_client_lifetime_3cat = case_when(
      ngo_client_lifetime_3cat %in% c("Yes","1") ~ 1,
      ngo_client_lifetime_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    ),
    years_in_sw_3cat = as.factor(years_in_sw_3cat),
    city = as.factor(city),
    year = as.factor(year)
  )

table(sw_combined_clean$ngo_client_lifetime_3cat, useNA = "ifany")

compute_results_overall <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(ngo_client_lifetime_3cat, years_in_sw_3cat, year, city, all_of(v)) %>%
      filter(
        !is.na(ngo_client_lifetime_3cat),
        !is.na(.data[[v]]),
        !is.na(years_in_sw_3cat)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$ngo_client_lifetime_3cat)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(
      glmer(as.formula(paste("ngo_client_lifetime_3cat ~", rhs, "+ (1 | year) + (1 | city)")),
            data = dat,
            family = poisson(link = "log"),
            control = glmerControl(optimizer = "bobyqa")),
      silent = TRUE
    )

    if (inherits(model, "try-error")) next

    tidy_mod <- try(
      broom.mixed::tidy(model, effects = "fixed",
                        conf.int = TRUE, exponentiate = TRUE),
      silent = TRUE
    )

    if (inherits(tidy_mod, "try-error")) next

    tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$ngo_client_lifetime_3cat, na.rm = TRUE)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        pr_fmt <- "ref."
      } else if (nrow(tidy_mod) > 0) {

        term <- tidy_mod[
          grepl(v, tidy_mod$term) &
          grepl(lev, tidy_mod$term),
        ]

        if (nrow(term) == 1) {
          pr_fmt <- sprintf("%.2f (%.2f-%.2f)",
                            term$estimate,
                            term$conf.low,
                            term$conf.high)
        } else {
          pr_fmt <- NA
        }
      } else {
        pr_fmt <- NA
      }

      results <- rbind(results, data.frame(
        Variable = v,
        Level = lev,
        N = n,
        Cases = cases,
        Percent = pct,
        CountPercent = count_pct,
        PR = pr_fmt
      ))
    }
  }

  results
}

## overall datasets
overall_data <- sw_combined_clean %>%
  filter(!is.na(hiv_test_rslt_bin), !is.na(years_in_sw_3cat))

idu_data <- overall_data %>%
  filter(idu_ever_3cat == "Yes")

no_idu_data <- overall_data %>%
  filter(idu_ever_3cat == "No")

street_data <- overall_data %>%
  filter(street_sw_bin == "Yes")

no_street_data <- overall_data %>%
  filter(street_sw_bin == "No")

ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "Yes")

no_ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "No")

overall_out    <- compute_results_overall(overall_data, vars)
idu_out        <- compute_results_overall(idu_data, vars)
no_idu_out     <- compute_results_overall(no_idu_data, vars)
street_out     <- compute_results_overall(street_data, vars)
no_street_out  <- compute_results_overall(no_street_data, vars)
ngo_out        <- compute_results_overall(ngo_data, vars)
no_ngo_out     <- compute_results_overall(no_ngo_data, vars)

## save
final_output <- list(
  Overall = overall_out,
  IDU = idu_out,
  No_IDU = no_idu_out,
  Street = street_out,
  No_Street = no_street_out,
  NGO = ngo_out,
  No_NGO = no_ngo_out
)

write_xlsx(final_output,
           "outputs_by_city/ngo_client_results_OVERALL.xlsx")

## unsterile syringe use

## load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## variables
vars <- c(
  "condom_access_12m_3cat", "ngo_client_lifetime_3cat",
  "ngo_condom_rec_bin", "ngo_syringe_12m_bin", "hiv_test_rslt_bin",
  "city_travel_12m_cat","street_sw_bin", "typology_primary_6m_3cat",
  "typology_primary_30d_3cat","alcohol_30d_bin",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat",
  "violence_rape_12m_3cat","violence_rape_ever","violence_beaten_ever",
  "violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "underage_first_sw_bin"
)

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    used_syringe_last_3cat = case_when(
      used_syringe_last_3cat %in% c("Yes","1") ~ 1,
      used_syringe_last_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    ),
    years_in_sw_3cat = as.factor(years_in_sw_3cat),
    city = as.factor(city),
    year = as.factor(year)
  )

table(sw_combined_clean$used_syringe_last_3cat, useNA = "ifany")

compute_results_overall <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(used_syringe_last_3cat, years_in_sw_3cat, year, city, all_of(v)) %>%
      filter(
        !is.na(used_syringe_last_3cat),
        !is.na(.data[[v]]),
        !is.na(years_in_sw_3cat)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$used_syringe_last_3cat)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(
      glmer(as.formula(paste("used_syringe_last_3cat ~", rhs, "+ (1 | year) + (1 | city)")),
            data = dat,
            family = poisson(link = "log"),
            control = glmerControl(optimizer = "bobyqa")),
      silent = TRUE
    )

    if (inherits(model, "try-error")) next

    tidy_mod <- try(
      broom.mixed::tidy(model, effects = "fixed",
                        conf.int = TRUE, exponentiate = TRUE),
      silent = TRUE
    )

    if (inherits(tidy_mod, "try-error")) next

    tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$used_syringe_last_3cat, na.rm = TRUE)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        pr_fmt <- "ref."
      } else if (nrow(tidy_mod) > 0) {

        term <- tidy_mod[
          grepl(v, tidy_mod$term) &
          grepl(lev, tidy_mod$term),
        ]

        if (nrow(term) == 1) {
          pr_fmt <- sprintf("%.2f (%.2f-%.2f)",
                            term$estimate,
                            term$conf.low,
                            term$conf.high)
        } else {
          pr_fmt <- NA
        }
      } else {
        pr_fmt <- NA
      }

      results <- rbind(results, data.frame(
        Variable = v,
        Level = lev,
        N = n,
        Cases = cases,
        Percent = pct,
        CountPercent = count_pct,
        PR = pr_fmt
      ))
    }
  }

  results
}

## overall datasets
overall_data <- sw_combined_clean %>%
  filter(!is.na(used_syringe_last_3cat), !is.na(years_in_sw_3cat))

overall_data <- overall_data %>%
  filter(idu_ever_3cat == "Yes")

street_data <- overall_data %>%
  filter(street_sw_bin == "Yes")

no_street_data <- overall_data %>%
  filter(street_sw_bin == "No")

ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "Yes")

no_ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "No")

overall_out    <- compute_results_overall(overall_data, vars)
street_out     <- compute_results_overall(street_data, vars)
no_street_out  <- compute_results_overall(no_street_data, vars)
ngo_out        <- compute_results_overall(ngo_data, vars)
no_ngo_out     <- compute_results_overall(no_ngo_data, vars)

## save
final_output <- list(
  Overall = overall_out,
  Street = street_out,
  No_Street = no_street_out,
  NGO = ngo_out,
  No_NGO = no_ngo_out
)

write_xlsx(final_output,
           "outputs_by_city/used_syringe_results_OVERALL.xlsx")

## ngo for syringe

## load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## variables
vars <- c(
  "condom_access_12m_3cat", "ngo_client_lifetime_3cat",
  "ngo_condom_rec_bin", "hiv_test_rslt_bin",
  "city_travel_12m_cat","street_sw_bin", "typology_primary_6m_3cat",
  "typology_primary_30d_3cat","alcohol_30d_bin",
  "used_syringe_last_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat",
  "violence_rape_12m_3cat","violence_rape_ever","violence_beaten_ever",
  "violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "underage_first_sw_bin"
)

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ngo_syringe_12m_bin = case_when(
      ngo_syringe_12m_bin %in% c("Yes","1") ~ 1,
      ngo_syringe_12m_bin %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    ),
    years_in_sw_3cat = as.factor(years_in_sw_3cat),
    city = as.factor(city),
    year = as.factor(year)
  )

table(sw_combined_clean$ngo_syringe_12m_bin, useNA = "ifany")

compute_results_overall <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(ngo_syringe_12m_bin, years_in_sw_3cat, year, city, all_of(v)) %>%
      filter(
        !is.na(ngo_syringe_12m_bin),
        !is.na(.data[[v]]),
        !is.na(years_in_sw_3cat)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$ngo_syringe_12m_bin)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(
      glmer(as.formula(paste("ngo_syringe_12m_bin ~", rhs, "+ (1 | year) + (1 | city)")),
            data = dat,
            family = poisson(link = "log"),
            control = glmerControl(optimizer = "bobyqa")),
      silent = TRUE
    )

    if (inherits(model, "try-error")) next

    tidy_mod <- try(
      broom.mixed::tidy(model, effects = "fixed",
                        conf.int = TRUE, exponentiate = TRUE),
      silent = TRUE
    )

    if (inherits(tidy_mod, "try-error")) next

    tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$ngo_syringe_12m_bin, na.rm = TRUE)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        pr_fmt <- "ref."
      } else if (nrow(tidy_mod) > 0) {

        term <- tidy_mod[
          grepl(v, tidy_mod$term) &
          grepl(lev, tidy_mod$term),
        ]

        if (nrow(term) == 1) {
          pr_fmt <- sprintf("%.2f (%.2f-%.2f)",
                            term$estimate,
                            term$conf.low,
                            term$conf.high)
        } else {
          pr_fmt <- NA
        }
      } else {
        pr_fmt <- NA
      }

      results <- rbind(results, data.frame(
        Variable = v,
        Level = lev,
        N = n,
        Cases = cases,
        Percent = pct,
        CountPercent = count_pct,
        PR = pr_fmt
      ))
    }
  }

  results
}


## overall datasets
overall_data <- sw_combined_clean %>%
  filter(!is.na(ngo_syringe_12m_bin), !is.na(years_in_sw_3cat))

overall_data <- overall_data %>%
  filter(idu_ever_3cat == "Yes")

street_data <- overall_data %>%
  filter(street_sw_bin == "Yes")

no_street_data <- overall_data %>%
  filter(street_sw_bin == "No")

ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "Yes")

no_ngo_data <- overall_data %>%
  filter(ngo_client_lifetime_3cat == "No")

overall_out    <- compute_results_overall(overall_data, vars)
street_out     <- compute_results_overall(street_data, vars)
no_street_out  <- compute_results_overall(no_street_data, vars)
ngo_out        <- compute_results_overall(ngo_data, vars)
no_ngo_out     <- compute_results_overall(no_ngo_data, vars)

## save
final_output <- list(
  Overall = overall_out,
  Street = street_out,
  No_Street = no_street_out,
  NGO = ngo_out,
  No_NGO = no_ngo_out
)

write_xlsx(final_output,
           "outputs_by_city/ngo_syringe_results_OVERALL.xlsx")

