# load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, 
               lubridate, broom, survival, ggplot2, scales, sandwich, lmtest, lme4,
               broom.mixed, openxlsx, mediation, lavaan, semPlot) 

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## mediation using mediation package

# street-based mediated through idu

total.effect <- glm(
  hiv_test_rslt_bin ~ street_sw_bin + years_in_sw_3cat + 
    year + city,
  data = sw_combined_clean,
  family = binomial
)

med.fit <- glm(
  idu_ever_bin ~ street_sw_bin + years_in_sw_3cat + 
    year + city,
  data = sw_combined_clean,
  family = binomial
)

out.fit <- glm(
  hiv_test_rslt_bin ~ idu_ever_bin + street_sw_bin + years_in_sw_3cat +
    year + city,
  data = sw_combined_clean,
  family = binomial
)

med.out <- mediate(
  med.fit,
  out.fit,
  treat = "street_sw_bin",
  mediator = "idu_ever_bin",
  robustSE = TRUE,
  sims = 500
)

summary(med.out)

# street-based mediated through rape

med.fit2 <- glm(violence_rape_ever_bin ~ street_sw_bin + years_in_sw_3cat + year + city,
                family = binomial, data = sw_combined_clean)

out.fit2 <- glm(hiv_test_rslt_bin ~ violence_rape_ever_bin + street_sw_bin +
                  years_in_sw_3cat + year + city,
                family = binomial, data = sw_combined_clean)

med2 <- mediate(med.fit2, out.fit2,
                treat = "street_sw_bin",
                mediator = "violence_rape_ever_bin",
                sims = 500)

med.out2 <- mediate(
  med.fit2,
  out.fit2,
  treat = "street_sw_bin",
  mediator = "violence_rape_ever_bin",
  robustSE = TRUE,
  sims = 500
)

summary(med.out2)

# idu -> mediated through sb

total.effect <- glm(
  hiv_test_rslt_bin ~ idu_ever_bin + years_in_sw_3cat + 
    year + city,
  data = sw_combined_clean,
  family = binomial
)

med.fit <- glm(
  street_sw_bin ~ idu_ever_bin  + years_in_sw_3cat + 
    year + city,
  data = sw_combined_clean,
  family = binomial
)

out.fit <- glm(
  hiv_test_rslt_bin ~ street_sw_bin + idu_ever_bin + years_in_sw_3cat +
    year + city,
  data = sw_combined_clean,
  family = binomial
)

med.out <- mediate(
  med.fit,
  out.fit,
  treat = "idu_ever_bin",
  mediator = "street_sw_bin",
  robustSE = TRUE,
  sims = 500
)

summary(med.out)


# structural equation model with correlation between rape and idu

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

# example: view results for first model
results[[1]]$paths
results[[1]]$mediation

# combine mediation tables for all models
all_med <- bind_rows(lapply(results, function(x) {
  x$mediation %>% mutate(model = x$model)
}))

kable(all_med, digits = 3)

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
