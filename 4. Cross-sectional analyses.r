## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales, sandwich, lmtest, lme4)

## set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

## load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# cities in ukraine in 2021
sw_combined_clean_2021 <- sw_combined_clean %>% filter(year == "2021")
table(sw_combined_clean_2021$city)

# kyiv trends in HIV
sw_combined_clean_kyiv <- sw_combined_clean %>% filter(city == "Kyiv")
sw_combined_clean_kyiv <- sw_combined_clean_kyiv %>% filter(gender == "Female")
prop.table(table(sw_combined_clean_kyiv$hiv_test_rslt_bin, sw_combined_clean_kyiv$year), margin = 2)

## variables
vars <- c(
  "condom_access_12m_3cat","client_condom_lastsex_3cat","ngo_access_lifetime_3cat",
  "ngo_condom_12m_bin", "ngo_syringe_12m_bin",
  "city_travel_12m_cat","street_sw_bin", "typology_primary_6m_3cat",
  "typology_primary_30d_3cat","alcohol_30d_bin",
  "used_syringe_last_3cat","idu_ever_3cat","idu_12m_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat",
  "violence_rape_12m_3cat","violence_rape_ever","violence_beaten_ever",
  "violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "underage_first_sw_bin"
)

sw_combined_clean <- sw_combined_clean %>%
  mutate(city = as.factor(city))

# hiv status

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    ),
    years_in_sw_3cat = as.factor(years_in_sw_3cat)
  )

class(sw_combined_clean[["ngo_syringe_12m_bin"]])
levels(sw_combined_clean[["ngo_syringe_12m_bin"]])

## function
compute_results <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(hiv_test_rslt_bin, years_in_sw_3cat, year, all_of(v)) %>%
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
      glmer(as.formula(paste("hiv_test_rslt_bin ~", rhs, "+ (1 | year)")),
            data = dat,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa")),
      silent = TRUE
    )

    tidy_mod <- if (!inherits(model, "try-error")) {
      tmp <- try(broom::tidy(model, conf.int = TRUE, exponentiate = TRUE),
                 silent = TRUE)
      if (!inherits(tmp, "try-error")) tmp[tmp$term != "(Intercept)", ] else NULL
    } else NULL

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
      } else if (!is.null(tidy_mod)) {

        term <- tidy_mod[
          grepl(v, tidy_mod$term) &
          grepl(lev, tidy_mod$term),
        ]

        if (nrow(term) == 1) {
          or_fmt <- if (!is.na(term$conf.low)) {
            sprintf("%.2f (%.2f-%.2f)",
                    term$estimate,
                    term$conf.low,
                    term$conf.high)
          } else {
            sprintf("%.2f", term$estimate)
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
        Percent = pct,
        CountPercent = count_pct,
        OR = or_fmt
      ))
    }
  }

  results
}

## create base output folder
dir.create("outputs_by_city", showWarnings = FALSE)


## loop over cities
for (cty in levels(sw_combined_clean$city)) {

  cat("City:", cty, "\n")

  city_data <- sw_combined_clean %>% filter(city == cty)

  datasets <- list(
    Overall = city_data,
    IDU = city_data %>% filter(idu_ever_3cat == "Yes"),
    No_IDU = city_data %>% filter(idu_ever_3cat == "No")
  )

  out <- lapply(names(datasets), function(name) {
    compute_results(datasets[[name]], vars)
  })

  names(out) <- names(datasets)

  dir.create(file.path("outputs_by_city", cty), showWarnings = FALSE)

  write_xlsx(out, file.path("outputs_by_city", cty, "hiv_results.xlsx"))
}

## overall

overall_data <- sw_combined_clean %>%
  filter(!is.na(hiv_test_rslt_bin), !is.na(years_in_sw_3cat))

idu_data <- overall_data %>%
  filter(idu_ever_3cat == "Yes")

no_idu_data <- overall_data %>%
  filter(idu_ever_3cat == "No")

overall_out <- compute_results(overall_data, vars)
idu_out     <- compute_results(idu_data, vars)
no_idu_out  <- compute_results(no_idu_data, vars)

## save

final_output <- list(
  Overall = overall_out,
  IDU = idu_out,
  No_IDU = no_idu_out
)

write_xlsx(final_output, "outputs_by_city/hiv_results_OVERALL.xlsx")

## condom use

## load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## variables
vars <- c(
  "condom_access_12m_3cat", "ngo_access_lifetime_3cat",
  "ngo_condom_12m_bin", "ngo_syringe_12m_bin",
  "city_travel_12m_cat","street_sw_bin", "typology_primary_6m_3cat",
  "typology_primary_30d_3cat","alcohol_30d_bin",
  "used_syringe_last_3cat","idu_ever_3cat","idu_12m_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat",
  "violence_rape_12m_3cat","violence_rape_ever","violence_beaten_ever",
  "violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "underage_first_sw_bin", "year"
)

# hiv status

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    client_condom_lastsex_3cat = case_when(
      client_condom_lastsex_3cat %in% c("1","Yes") ~ 1,
      client_condom_lastsex_3cat %in% c("0","No") ~ 0,
      TRUE ~ NA_real_
    ),
    years_in_sw_3cat = as.factor(years_in_sw_3cat),
    city = as.factor(city),
    year = as.factor(year)
  )


## function (Poisson PR model with glmer and Wald-based CIs)
compute_results <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(client_condom_lastsex_3cat, years_in_sw_3cat, year, all_of(v)) %>%
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
      glmer(as.formula(paste("client_condom_lastsex_3cat ~", rhs, "+ (1 | year)")),
            data = dat,
            family = poisson(link = "log"),
            control = glmerControl(optimizer = "bobyqa")),
      silent = TRUE
    )

    if (inherits(model, "try-error")) next

    ## Extract model coefficients with Wald-based CIs (exponentiated for PRs)
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

## create base output folder
dir.create("outputs_by_city", showWarnings = FALSE)

## loop over cities
for (cty in levels(sw_combined_clean$city)) {

  cat("City:", cty, "\n")

  city_data <- sw_combined_clean %>% filter(city == cty)

  datasets <- list(
    Overall = city_data,
    IDU = city_data %>% filter(idu_ever_3cat == "Yes"),
    No_IDU = city_data %>% filter(idu_ever_3cat == "No")
  )

  out <- lapply(names(datasets), function(name) {
    compute_results(datasets[[name]], vars)
  })

  names(out) <- names(datasets)

  dir.create(file.path("outputs_by_city", cty), showWarnings = FALSE)

  write_xlsx(out,
             file.path("outputs_by_city", cty, "condom_results.xlsx"))
}

## function (Poisson PR model with glmer and mixed effects for city and year)
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

    ## Extract model coefficients with Wald-based CIs (exponentiated for PRs)
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
  filter(!is.na(client_condom_lastsex_3cat), !is.na(years_in_sw_3cat))

idu_data <- overall_data %>%
  filter(idu_ever_3cat == "Yes")

no_idu_data <- overall_data %>%
  filter(idu_ever_3cat == "No")

overall_out <- compute_results_overall(overall_data, vars)
idu_out     <- compute_results_overall(idu_data, vars)
no_idu_out  <- compute_results_overall(no_idu_data, vars)

## save
final_output <- list(
  Overall = overall_out,
  IDU = idu_out,
  No_IDU = no_idu_out
)

write_xlsx(final_output,
           "outputs_by_city/condom_results_OVERALL.xlsx")

## rape

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    rape_bin = case_when(
      violence_rape_ever %in% c("Yes","1") ~ 1,
      violence_rape_ever %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ))

compute_results_rape <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(rape_bin, years_in_sw_3cat, all_of(v)) %>%
      filter(!is.na(rape_bin), !is.na(.data[[v]]), !is.na(years_in_sw_3cat))

    if (nrow(dat) == 0 ||
        length(unique(dat$rape_bin)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(glm(as.formula(paste("rape_bin ~", rhs)),
                     data = dat, family = binomial), silent = TRUE)

    tidy_mod <- if (!inherits(model, "try-error")) {
      tmp <- try(broom::tidy(model, conf.int = TRUE, exponentiate = TRUE), silent = TRUE)
      if (!inherits(tmp, "try-error")) tmp[tmp$term != "(Intercept)", ] else NULL
    } else NULL

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {
      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub); if (n == 0) next

      cases <- sum(sub$rape_bin)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        or_fmt <- "ref."
      } else if (!is.null(tidy_mod)) {
        term <- tidy_mod[grepl(v, tidy_mod$term) & grepl(lev, tidy_mod$term), ]
        if (nrow(term) == 1) {
          or_fmt <- sprintf("%.2f (%.2f-%.2f)", term$estimate, term$conf.low, term$conf.high)
        } else or_fmt <- NA
      } else or_fmt <- NA

      results <- rbind(results, data.frame(
        Variable = v, Level = lev, N = n, Cases = cases,
        Percent = pct, CountPercent = count_pct, OR = or_fmt
      ))
    }
  }
  results
}

for (cty in levels(sw_combined_clean$city)) {

  cat("City:", cty, "\n")

  city_data <- sw_combined_clean %>% filter(city == cty)

  datasets <- list(
    Overall = city_data,
    IDU = city_data %>% filter(idu_ever_3cat == "Yes"),
    No_IDU = city_data %>% filter(idu_ever_3cat == "No")
  )

  out <- lapply(names(datasets), function(name) {
    compute_results_rape(datasets[[name]], vars)
  })

  names(out) <- names(datasets)

  dir.create(file.path("outputs_by_city", cty), showWarnings = FALSE)
  write_xlsx(out, file.path("outputs_by_city", cty, "rape_results.xlsx"))
}

## overall 

overall_out <- compute_results_rape(overall_data, vars)
idu_out     <- compute_results_rape(idu_data, vars)
no_idu_out  <- compute_results_rape(no_idu_data, vars)

## save overall

final_output <- list(
  Overall = overall_out,
  IDU = idu_out,
  No_IDU = no_idu_out
)

write_xlsx(final_output, "outputs_by_city/rape_results_OVERALL.xlsx")

## street based sw

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    street_sw_bin = case_when(
      street_sw_bin %in% c("Yes", "1") ~ 1,
      street_sw_bin %in% c("No", "0") ~ 0,
      TRUE ~ NA_real_
    ))

compute_results_street <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(street_sw_bin, years_in_sw_3cat, all_of(v)) %>%
      filter(
        !is.na(street_sw_bin),
        !is.na(.data[[v]]),
        !is.na(years_in_sw_3cat)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$street_sw_bin)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(glm(as.formula(paste("street_sw_bin ~", rhs)),
                     data = dat, family = binomial), silent = TRUE)

    tidy_mod <- if (!inherits(model, "try-error")) {
      tmp <- try(broom::tidy(model, conf.int = TRUE, exponentiate = TRUE),
                 silent = TRUE)
      if (!inherits(tmp, "try-error")) tmp[tmp$term != "(Intercept)", ] else NULL
    } else NULL

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$street_sw_bin, na.rm = TRUE)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        or_fmt <- "ref."
      } else if (!is.null(tidy_mod)) {

        term <- tidy_mod[grepl(v, tidy_mod$term) & grepl(lev, tidy_mod$term), ]

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

## run by city
for (cty in levels(sw_combined_clean$city)) {

  cat("City:", cty, "\n")

  city_data <- sw_combined_clean %>% filter(city == cty)

  datasets <- list(
    Overall = city_data,
    IDU = city_data %>% filter(idu_ever_3cat == "Yes"),
    No_IDU = city_data %>% filter(idu_ever_3cat == "No")
  )

  out <- lapply(names(datasets), function(name) {
    compute_results_street(datasets[[name]], vars)
  })

  names(out) <- names(datasets)

  dir.create(file.path("outputs_by_city", cty), showWarnings = FALSE)

  write_xlsx(out, file.path("outputs_by_city", cty, "street_sw_results.xlsx"))
}

## ngo access

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ngo_access_lifetime_3cat = case_when(
      ngo_access_lifetime_3cat %in% c("Yes", "1") ~ 1,
      ngo_access_lifetime_3cat %in% c("No", "0") ~ 0,
      TRUE ~ NA_real_
    ))

compute_results_ngo <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(ngo_access_lifetime_3cat, years_in_sw_3cat, all_of(v)) %>%
      filter(
        !is.na(ngo_access_lifetime_3cat),
        !is.na(.data[[v]]),
        !is.na(years_in_sw_3cat)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$ngo_access_lifetime_3cat)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(glm(as.formula(paste("ngo_access_lifetime_3cat ~", rhs)),
                     data = dat, family = binomial), silent = TRUE)

    tidy_mod <- if (!inherits(model, "try-error")) {
      tmp <- try(broom::tidy(model, conf.int = TRUE, exponentiate = TRUE),
                 silent = TRUE)
      if (!inherits(tmp, "try-error")) tmp[tmp$term != "(Intercept)", ] else NULL
    } else NULL

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$ngo_access_lifetime_3cat, na.rm = TRUE)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        or_fmt <- "ref."
      } else if (!is.null(tidy_mod)) {

        term <- tidy_mod[grepl(v, tidy_mod$term) & grepl(lev, tidy_mod$term), ]

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

## run by city
for (cty in levels(sw_combined_clean$city)) {

  cat("City:", cty, "\n")

  city_data <- sw_combined_clean %>% filter(city == cty)

  datasets <- list(
    Overall = city_data,
    IDU = city_data %>% filter(idu_ever_3cat == "Yes"),
    No_IDU = city_data %>% filter(idu_ever_3cat == "No")
  )

  out <- lapply(names(datasets), function(name) {
    compute_results_ngo(datasets[[name]], vars)
  })

  names(out) <- names(datasets)

  dir.create(file.path("outputs_by_city", cty), showWarnings = FALSE)

  write_xlsx(out, file.path("outputs_by_city", cty, "ngo_access_results.xlsx"))
}

## ngo condom 12m

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ngo_condom_12m_bin = case_when(
      ngo_condom_12m %in% c("Yes", "1") ~ 1,
      ngo_condom_12m %in% c("No", "0") ~ 0,
      TRUE ~ NA_real_
    ))

compute_results_ngo_condom <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(ngo_condom_12m_bin, years_in_sw_3cat, all_of(v)) %>%
      filter(
        !is.na(ngo_condom_12m_bin),
        !is.na(.data[[v]]),
        !is.na(years_in_sw_3cat)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$ngo_condom_12m_bin)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(glm(as.formula(paste("ngo_condom_12m_bin ~", rhs)),
                     data = dat, family = binomial), silent = TRUE)

    tidy_mod <- if (!inherits(model, "try-error")) {
      tmp <- try(broom::tidy(model, conf.int = TRUE, exponentiate = TRUE),
                 silent = TRUE)
      if (!inherits(tmp, "try-error")) tmp[tmp$term != "(Intercept)", ] else NULL
    } else NULL

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$ngo_condom_12m_bin, na.rm = TRUE)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        or_fmt <- "ref."
      } else if (!is.null(tidy_mod)) {

        term <- tidy_mod[grepl(v, tidy_mod$term) & grepl(lev, tidy_mod$term), ]

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

## run by city
for (cty in levels(sw_combined_clean$city)) {

  cat("City:", cty, "\n")

  city_data <- sw_combined_clean %>% filter(city == cty)

  datasets <- list(
    Overall = city_data,
    IDU = city_data %>% filter(idu_ever_3cat == "Yes"),
    No_IDU = city_data %>% filter(idu_ever_3cat == "No")
  )

  out <- lapply(names(datasets), function(name) {
    compute_results_ngo_condom(datasets[[name]], vars)
  })

  names(out) <- names(datasets)

  dir.create(file.path("outputs_by_city", cty), showWarnings = FALSE)

  write_xlsx(out, file.path("outputs_by_city", cty, "ngo_condom_results.xlsx"))
}

## ngo syringe 12m

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ngo_syringe_12m_bin = case_when(
      ngo_syringe_12m %in% c("Yes", "1") ~ 1,
      ngo_syringe_12m %in% c("No", "0") ~ 0,
      TRUE ~ NA_real_
    ))

compute_results_ngo_syringe <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(ngo_syringe_12m_bin, years_in_sw_3cat, all_of(v)) %>%
      filter(
        !is.na(ngo_syringe_12m_bin),
        !is.na(.data[[v]]),
        !is.na(years_in_sw_3cat)
      )

    if (nrow(dat) == 0 ||
        length(unique(dat$ngo_syringe_12m_bin)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(glm(as.formula(paste("ngo_syringe_12m_bin ~", rhs)),
                     data = dat, family = binomial), silent = TRUE)

    tidy_mod <- if (!inherits(model, "try-error")) {
      tmp <- try(broom::tidy(model, conf.int = TRUE, exponentiate = TRUE),
                 silent = TRUE)
      if (!inherits(tmp, "try-error")) tmp[tmp$term != "(Intercept)", ] else NULL
    } else NULL

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {

      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub)
      if (n == 0) next

      cases <- sum(sub$ngo_syringe_12m_bin, na.rm = TRUE)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        or_fmt <- "ref."
      } else if (!is.null(tidy_mod)) {

        term <- tidy_mod[grepl(v, tidy_mod$term) & grepl(lev, tidy_mod$term), ]

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

## run by city
for (cty in levels(sw_combined_clean$city)) {

  cat("City:", cty, "\n")

  city_data <- sw_combined_clean %>% filter(city == cty)

  datasets <- list(
    Overall = city_data,
    IDU = city_data %>% filter(idu_ever_3cat == "Yes"),
    No_IDU = city_data %>% filter(idu_ever_3cat == "No")
  )

  out <- lapply(names(datasets), function(name) {
    compute_results_ngo_syringe(datasets[[name]], vars)
  })

  names(out) <- names(datasets)

  dir.create(file.path("outputs_by_city", cty), showWarnings = FALSE)

  write_xlsx(out, file.path("outputs_by_city", cty, "ngo_syringe_results.xlsx"))
}

## art use
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    art_current_bin = case_when(
      art_current_3cat %in% c("Yes","1") ~ 1,
      art_current_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ))

compute_results_art <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(art_current_bin, years_in_sw_3cat, all_of(v)) %>%
      filter(!is.na(art_current_bin), !is.na(.data[[v]]), !is.na(years_in_sw_3cat))

    if (nrow(dat) == 0 ||
        length(unique(dat$art_current_bin)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw_3cat", v, sep = " + ")

    model <- try(glm(as.formula(paste("art_current_bin ~", rhs)),
                     data = dat, family = binomial), silent = TRUE)

    tidy_mod <- if (!inherits(model, "try-error")) {
      tmp <- try(broom::tidy(model, conf.int = TRUE, exponentiate = TRUE), silent = TRUE)
      if (!inherits(tmp, "try-error")) tmp[tmp$term != "(Intercept)", ] else NULL
    } else NULL

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {
      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub); if (n == 0) next

      cases <- sum(sub$art_current_bin)
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

      if (lev == levels_var[1]) {
        or_fmt <- "ref."
      } else if (!is.null(tidy_mod)) {
        term <- tidy_mod[grepl(v, tidy_mod$term) & grepl(lev, tidy_mod$term), ]
        if (nrow(term) == 1) {
          or_fmt <- sprintf("%.2f (%.2f-%.2f)", term$estimate, term$conf.low, term$conf.high)
        } else or_fmt <- NA
      } else or_fmt <- NA

      results <- rbind(results, data.frame(
        Variable = v, Level = lev, N = n, Cases = cases,
        Percent = pct, CountPercent = count_pct, OR = or_fmt
      ))
    }
  }
  results
}

## loop ART
for (cty in levels(sw_combined_clean$city)) {

  cat("City:", cty, "\n")

  city_data <- sw_combined_clean %>% filter(city == cty)

  datasets <- list(
    Overall = city_data,
    IDU = city_data %>% filter(idu_ever_3cat == "Yes"),
    No_IDU = city_data %>% filter(idu_ever_3cat == "No")
  )

  out <- lapply(names(datasets), function(name) {
    compute_results_art(datasets[[name]], vars)
  })

  names(out) <- names(datasets)

  dir.create(file.path("outputs_by_city", cty), showWarnings = FALSE)
  write_xlsx(out, file.path("outputs_by_city", cty, "art_results.xlsx"))
}