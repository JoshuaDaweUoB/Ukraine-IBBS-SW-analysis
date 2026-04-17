## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales)

## set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# hiv status

## load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## variables
vars <- c(
  "condom_access_12m_3cat","client_condom_lastsex_3cat","ngo_access_lifetime_3cat",
  "city_travel_12m_cat","street_sw_bin","typology_primary_6m", "typology_primary_6m_3cat",
  "typology_primary_30d_3cat","typology_primary_30d","alcohol_30d_bin", 
  "used_syringe_last_3cat","idu_ever_3cat","idu_12m_3cat",
  "sw_partners_clients_30d_3cat","violence_any_ever_3cat",
  "violence_rape_12m_3cat","violence_rape_ever","violence_beaten_ever",
  "violence_physical_abuse_ever","violence_police","violence_pimp",
  "violence_support_ngo","age_bin","occupied","occupied_partial",
  "avoided_healthcare_12m_stigma","avoided_healthcare_12m_violence",
  "avoided_healthcare_12m_police","avoided_hiv_test_12m_police",
  "avoided_hiv_test_12m_violence","avoided_hiv_test_12m_stigma",
  "underage_first_sw_bin"
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

## outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    ),
    year = as.factor(year),
    years_in_sw = as.factor(years_in_sw),
    city = as.factor(city)
  )

names(sw_combined_clean)

## function
compute_results <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(hiv_test_rslt_bin, year, years_in_sw, all_of(v)) %>%
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

    model <- try(glm(as.formula(paste("hiv_test_rslt_bin ~", rhs)),
                     data = dat, family = binomial), silent = TRUE)

    tidy_mod <- NULL
    if (!inherits(model, "try-error")) {
      tidy_mod <- try(broom::tidy(model, conf.int = TRUE, exponentiate = TRUE),
                      silent = TRUE)
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
      pct <- round(cases / n * 100, 1)
      count_pct <- sprintf("%d (%.1f%%)", cases, pct)

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
cities <- levels(sw_combined_clean$city)

for (cty in cities) {

  cat("Processing:", cty, "\n")

  city_data <- sw_combined_clean %>% filter(city == cty)

  datasets <- list(
    Overall = city_data,
    IDU = city_data %>% filter(idu_ever_3cat == "Yes"),
    No_IDU = city_data %>% filter(idu_ever_3cat == "No")
  )

  results_list <- list()

  for (name in names(datasets)) {
    results_list[[name]] <- compute_results(datasets[[name]], vars)
  }

  ## create city folder
  city_dir <- file.path("outputs_by_city", cty)
  dir.create(city_dir, showWarnings = FALSE)

  ## save file
  write_xlsx(
    results_list,
    path = file.path(city_dir, "hiv_results.xlsx")
  )
}

## condom use
sw_combined_clean <- readRDS("sw_combined_clean.rds") %>%
  mutate(
    condom_access_12m_3cat = case_when(
      condom_access_12m_3cat %in% c("Yes","1") ~ 1,
      condom_access_12m_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    year = as.factor(year),
    years_in_sw = as.factor(years_in_sw),
    city = as.factor(city)
  )

compute_results_condom <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(condom_access_12m_3cat, year, years_in_sw, all_of(v)) %>%
      filter(!is.na(condom_access_12m_3cat), !is.na(.data[[v]]), !is.na(years_in_sw))

    if (nrow(dat) == 0 ||
        length(unique(dat$condom_access_12m_3cat)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw", v, sep = " + ")
    if (length(unique(dat$year)) > 1) rhs <- paste(rhs, "+ year")

    model <- try(glm(as.formula(paste("condom_access_12m_3cat ~", rhs)),
                     data = dat, family = binomial), silent = TRUE)

    tidy_mod <- if (!inherits(model, "try-error")) {
      tmp <- try(broom::tidy(model, conf.int = TRUE, exponentiate = TRUE), silent = TRUE)
      if (!inherits(tmp, "try-error")) tmp[tmp$term != "(Intercept)", ] else NULL
    } else NULL

    levels_var <- sort(unique(dat[[v]]))

    for (lev in levels_var) {
      sub <- dat %>% filter(.data[[v]] == lev)
      n <- nrow(sub); if (n == 0) next

      cases <- sum(sub$condom_access_12m_3cat)
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

## loop
for (cty in levels(sw_combined_clean$city)) {

  cat("City:", cty, "\n")

  city_data <- sw_combined_clean %>% filter(city == cty)

  datasets <- list(
    Overall = city_data,
    IDU = city_data %>% filter(idu_ever_3cat == "Yes"),
    No_IDU = city_data %>% filter(idu_ever_3cat == "No")
  )

  out <- lapply(names(datasets), function(name) {
    cat("  Dataset:", name, "\n")
    compute_results_condom(datasets[[name]], vars)
  })

  names(out) <- names(datasets)

  dir.create(file.path("outputs_by_city", cty), showWarnings = FALSE)
  write_xlsx(out, file.path("outputs_by_city", cty, "condom_results.xlsx"))
}

## rape

sw_combined_clean <- readRDS("sw_combined_clean.rds") %>%
  mutate(
    rape_bin = case_when(
      violence_rape_ever %in% c("Yes","1") ~ 1,
      violence_rape_ever %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    year = as.factor(year),
    years_in_sw = as.factor(years_in_sw),
    city = as.factor(city)
  )

compute_results_rape <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(rape_bin, year, years_in_sw, all_of(v)) %>%
      filter(!is.na(rape_bin), !is.na(.data[[v]]), !is.na(years_in_sw))

    if (nrow(dat) == 0 ||
        length(unique(dat$rape_bin)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- paste("years_in_sw", v, sep = " + ")
    if (length(unique(dat$year)) > 1) rhs <- paste(rhs, "+ year")

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
    cat("  Dataset:", name, "\n")
    compute_results_rape(datasets[[name]], vars)
  })

  names(out) <- names(datasets)

  dir.create(file.path("outputs_by_city", cty), showWarnings = FALSE)
  write_xlsx(out, file.path("outputs_by_city", cty, "rape_results.xlsx"))
}

## art use 

sw_combined_clean <- readRDS("sw_combined_clean.rds") %>%
  mutate(
    art_current_bin = case_when(
      art_current_3cat %in% c("Yes","1") ~ 1,
      art_current_3cat %in% c("No","0") ~ 0,
      TRUE ~ NA_real_
    ),
    year = as.factor(year),
    city = as.factor(city)
  )

compute_results_art <- function(df, vars) {

  results <- data.frame()

  for (v in vars) {

    dat <- df %>%
      select(art_current_bin, year, all_of(v)) %>%
      filter(!is.na(art_current_bin), !is.na(.data[[v]]))

    if (nrow(dat) == 0 ||
        length(unique(dat$art_current_bin)) < 2 ||
        length(unique(dat[[v]])) < 2) next

    rhs <- v
    if (length(unique(dat$year)) > 1) rhs <- paste(rhs, "+ year")

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

for (cty in levels(sw_combined_clean$city)) {

  cat("City:", cty, "\n")

  city_data <- sw_combined_clean %>% filter(city == cty)

  datasets <- list(
    Overall = city_data,
    IDU = city_data %>% filter(idu_ever_3cat == "Yes"),
    No_IDU = city_data %>% filter(idu_ever_3cat == "No")
  )

  out <- lapply(names(datasets), function(name) {
    cat("  Dataset:", name, "\n")
    compute_results_art(datasets[[name]], vars)
  })

  names(out) <- names(datasets)

  dir.create(file.path("outputs_by_city", cty), showWarnings = FALSE)
  write_xlsx(out, file.path("outputs_by_city", cty, "art_results.xlsx"))
}