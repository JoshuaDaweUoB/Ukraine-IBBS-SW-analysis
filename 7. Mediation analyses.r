# load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, 
               lubridate, broom, survival, ggplot2, scales, sandwich, lmtest, lme4,
               broom.mixed, openxlsx, mediation, lavaan, semPlot, knitr, purrr) 

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## mediation using mediation package

# function to run mediation model
run_mediation <- function(data,
                          treat,
                          mediator,
                          outcome = "hiv_test_rslt_bin",
                          covariates = c("years_in_sw_3cat", "year", "city"),
                          sims = 500,
                          robustSE = TRUE,
                          seed = 123) {

  set.seed(seed)

  covars <- paste(covariates, collapse = " + ")

  med.formula <- as.formula(paste(mediator, "~", treat, "+", covars))
  out.formula <- as.formula(paste(outcome, "~", mediator, "+", treat, "+", covars))

  med.fit <- glm(med.formula, data = data, family = binomial)
  out.fit <- glm(out.formula, data = data, family = binomial)

  med.out <- mediation::mediate(
    model.m = med.fit,
    model.y = out.fit,
    treat = treat,
    mediator = mediator,
    robustSE = robustSE,
    sims = sims
  )

  list(
    model = med.out,
    summary = summary(med.out)
  )
}

# street-based → HIV via lifetime IDU
m1 <- run_mediation(sw_combined_clean,
                    treat = "street_sw_bin",
                    mediator = "idu_ever_bin")

# street-based → HIV via 12m IDU
m2 <- run_mediation(sw_combined_clean,
                    treat = "street_sw_bin",
                    mediator = "idu_12m_bin")

# street-based → HIV via rape
m3 <- run_mediation(sw_combined_clean,
                    treat = "street_sw_bin",
                    mediator = "violence_rape_ever_bin")

# street-based → HIV via forced sex (12m)
m4 <- run_mediation(sw_combined_clean,
                    treat = "street_sw_bin",
                    mediator = "violence_forced_any_12m_bin")

# IDU (lifetime) → HIV via street-based SW
m5 <- run_mediation(sw_combined_clean,
                    treat = "idu_ever_bin",
                    mediator = "street_sw_bin")

# IDU (12m) → HIV via street-based
m6 <- run_mediation(sw_combined_clean,
                    treat = "idu_12m_bin",
                    mediator = "street_sw_bin")

# IDU → HIV via rape
m7 <- run_mediation(sw_combined_clean,
                    treat = "idu_ever_bin",
                    mediator = "violence_rape_ever_bin")

# IDU (12m) → HIV via rape
m8 <- run_mediation(sw_combined_clean,
                    treat = "idu_12m_bin",
                    mediator = "violence_rape_ever_bin")


# IDU (12m) → HIV via forced sex (12m)
m9 <- run_mediation(sw_combined_clean,
                    treat = "idu_12m_bin",
                    mediator = "violence_forced_any_12m_bin")

# IDU → HIV via forced sex (lifetime)
m10 <- run_mediation(sw_combined_clean,
                    treat = "idu_ever_bin",
                    mediator = "violence_forced_any_ever_bin")

# create table
fmt_ci <- function(est, ci) {

  ci <- as.numeric(ci)

  paste0(
    round(as.numeric(est), 5), " (",
    round(ci[1], 5), ", ",
    round(ci[2], 5), ")"
  )
}

extract_med <- function(m) {

  s <- summary(m$model)

  data.frame(
    exposure = m$model$treat,
    mediator = m$model$mediator,

    ACME = fmt_ci(s$d0, s$d0.ci),
    ADE = fmt_ci(s$z0, s$z0.ci),
    Total_Effect = fmt_ci(s$tau.coef, s$tau.coef.ci),
    Prop_Mediated = fmt_ci(s$n0, s$n0.ci)
  )
}

models <- list(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10)

results_sheet <- do.call(rbind, lapply(models, extract_med))

results_sheet$model <- paste0("m", 1:9)

results_sheet <- results_sheet[, c(
  "model", "exposure", "mediator",
  "ACME", "ADE", "Total_Effect", "Prop_Mediated"
)]

write.csv(results_sheet, "mediation_results_all_models.csv", row.names = FALSE)

# structural equation model with rape and idu

sw_combined_clean$years_in_sw_3cat <- as.numeric(sw_combined_clean$years_in_sw_3cat)

violence_vars <- c(
  "violence_forced_any_ever_bin",
  "violence_rape_ever_bin"
)

run_sem <- function(v) {

  model <- paste0('
    idu_ever_bin ~ a1*street_sw_bin +
                   years_in_sw_3cat + year + city

    ', v, ' ~ a2*street_sw_bin +
              years_in_sw_3cat + year + city

    hiv_test_rslt_bin ~ cprime*street_sw_bin +
                        b1*idu_ever_bin +
                        b2*', v, ' +
                        years_in_sw_3cat + year + city

    ind_idu := a1*b1
    ind_violence := a2*b2

    total_ind := ind_idu + ind_violence
    total := cprime + total_ind
  ')

  fit <- sem(
    model,
    data = sw_combined_clean,
    ordered = c("idu_ever_bin", v, "hiv_test_rslt_bin"),
    estimator = "WLSMV"
  )

  path_table <- lavaan::parameterEstimates(fit, standardized = TRUE) %>%
    dplyr::filter(op == "~") %>%
    dplyr::select(lhs, rhs, est, se, z, pvalue, ci.lower, ci.upper, std.all)

  med_table <- lavaan::parameterEstimates(fit, standardized = TRUE) %>%
    dplyr::filter(op == ":=") %>%
    dplyr::select(lhs, est, se, z, pvalue, ci.lower, ci.upper, std.all)

  list(
    model = v,
    fit = fit,
    paths = path_table,
    mediation = med_table
  )
}

results <- lapply(violence_vars, run_sem)

# view results for first model
results[[1]]$paths
results[[1]]$mediation

# combine mediation tables for all models
all_med <- bind_rows(lapply(results, function(x) {
  x$mediation %>% mutate(model = x$model)
}))

kable(all_med, digits = 3)

## moderation analysis of idu and rape

# interaction term
moderation.model.rape <- glm(
  hiv_test_rslt_bin ~ idu_ever_bin*violence_rape_ever_bin + years_in_sw_3cat + 
    year + city,
  data = sw_combined_clean,
  family = binomial
)

# moderation results
mod_table <- tidy(
  moderation.model.rape,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(term %in% c(
    "idu_ever_binYes",
    "violence_rape_ever_binYes",
    "idu_ever_binYes:violence_rape_ever_binYes"
  )) %>%
  mutate(
    term = c(
      "IDU (Yes vs No)",
      "Lifetime rape (Yes vs No)",
      "IDU x Lifetime rape"
    )
  ) %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)

kable(
  mod_table,
  digits = 2,
  col.names = c(
    "Variable",
    "Odds Ratio",
    "95% CI Lower",
    "95% CI Upper",
    "P-value"
  )
)

# moderation of idu and street-based sw

# interaction term
moderation.model.street <- glm(
  hiv_test_rslt_bin ~ idu_ever_bin*street_sw_bin + years_in_sw_3cat + 
    year + city,
  data = sw_combined_clean,
  family = binomial
)

# moderation results
mod_table <- tidy(
  moderation.model.street,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(term %in% c(
    "idu_ever_binYes",
    "street_sw_binYes",
    "idu_ever_binYes:street_sw_binYes"
  )) %>%
  mutate(
    term = c(
      "IDU (Yes vs No)",
      "Street-based sex work (Yes vs No)",
      "IDU x Street-based sex work"
    )
  ) %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)

kable(
  mod_table,
  digits = 2,
  col.names = c(
    "Variable",
    "Odds Ratio",
    "95% CI Lower",
    "95% CI Upper",
    "P-value"
  )
)

## moderation analysis of idu and forced sex

# interaction term
moderation.model.forced <- glm(
  hiv_test_rslt_bin ~ idu_ever_bin*violence_forced_any_12m_bin + years_in_sw_3cat + 
    year + city,
  data = sw_combined_clean,
  family = binomial
)

# moderation results
mod_table <- tidy(
  moderation.model.forced,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(term %in% c(
    "idu_ever_binYes",
    "violence_forced_any_12m_binYes",
    "idu_ever_binYes:violence_forced_any_12m_binYes"
  )) %>%
  mutate(
    term = c(
      "IDU (Yes vs No)",
      "Forced sex (Yes vs No)",
      "IDU x Forced sex"
    )
  ) %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)

kable(
  mod_table,
  digits = 2,
  col.names = c(
    "Variable",
    "Odds Ratio",
    "95% CI Lower",
    "95% CI Upper",
    "P-value"
  )
)


# sensitivity: partially sequential modelling with IDU -> rape

sw_combined_clean$years_in_sw_3cat <- as.numeric(sw_combined_clean$years_in_sw_3cat)

violence_vars <- c(
  "violence_forced_any_ever_bin",
  "violence_rape_ever_bin"
)

run_sem <- function(v) {

  model <- paste0('
    idu_ever_bin ~ a1*street_sw_bin +
                   years_in_sw_3cat + year + city

    ', v, ' ~ a2*street_sw_bin +
              d*idu_ever_bin +
              years_in_sw_3cat + year + city

    hiv_test_rslt_bin ~ cprime*street_sw_bin +
                        b1*idu_ever_bin +
                        b2*', v, ' +
                        years_in_sw_3cat + year + city

    ind_idu := a1*b1

    ind_violence := a2*b2

    ind_chain := a1*d*b2

    total_ind := ind_idu + ind_violence + ind_chain

    total := cprime + total_ind
  ')

  fit <- sem(
    model,
    data = sw_combined_clean,
    ordered = c("idu_ever_bin", v, "hiv_test_rslt_bin"),
    estimator = "WLSMV"
  )

  path_table <- lavaan::parameterEstimates(fit, standardized = TRUE) %>%
    dplyr::filter(op == "~") %>%
    dplyr::select(lhs, rhs, est, se, z, pvalue, ci.lower, ci.upper, std.all)

  med_table <- lavaan::parameterEstimates(fit, standardized = TRUE) %>%
    dplyr::filter(op == ":=") %>%
    dplyr::select(lhs, est, se, z, pvalue, ci.lower, ci.upper, std.all)

  list(
    model = v,
    fit = fit,
    paths = path_table,
    mediation = med_table
  )
}

results <- lapply(violence_vars, run_sem)

all_med <- bind_rows(lapply(results, function(x) {
  x$mediation %>% mutate(model = x$model)
}))

kable(all_med, digits = 3)
