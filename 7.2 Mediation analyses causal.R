# load packages
pacman::p_load(dplyr, tidyr, boot, openxlsx, dagitty, ggdag, ggplot2, nnet)

# set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Chapter Writing/Chapter 6 - Ukraine FSWs IDU violence and HIV/data/SW data")

# load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# restrict to 2013 to 2021
sw_combined_clean <- sw_combined_clean %>%
  filter(as.numeric(as.character(year)) >= 2013)

# variables
outcome    <- "hiv_test_rslt_bin"
exposure   <- "street_sw_bin"
mediators  <- c("idu_12m_bin", "violence_rape_ever_bin")
covariates <- c("age_cat", "education_3cat", "alcohol_30d_bin",
                "sw_partners_clients_30d_3cat", "years_in_sw_3cat", "year", "city")

# complete case data, yes/no variables converted to 0/1
dat <- sw_combined_clean %>%
  dplyr::select(all_of(c(outcome, exposure, mediators, covariates))) %>%
  mutate(across(all_of(c(outcome, exposure, mediators)),
                ~ if_else(is.na(.x), NA_integer_, as.integer(.x %in% c("Yes", "Positive"))))) %>%
  drop_na() %>%
  mutate(across(all_of(covariates), ~ droplevels(as.factor(.x))))

nrow(dat)

# interventional effects by g-computation
# alcohol use and volume of clients are affected by street-based SW and confound mediator-HIV paths,
# so they are simulated under each exposure level rather than held fixed

# model formulas
base <- "age_cat + education_3cat + years_in_sw_3cat + year + city"
f_m1  <- as.formula(paste("idu_12m_bin ~ street_sw_bin +", base))
f_m2  <- as.formula(paste("violence_rape_ever_bin ~ street_sw_bin +", base))
f_alc <- as.formula(paste("alcohol_30d_bin ~ street_sw_bin +", base))
f_vol <- as.formula(paste("sw_partners_clients_30d_3cat ~ street_sw_bin +", base))
f_y   <- as.formula(paste("hiv_test_rslt_bin ~ street_sw_bin + idu_12m_bin + violence_rape_ever_bin +",
                          "alcohol_30d_bin + sw_partners_clients_30d_3cat +", base))

# category levels
alc_lv <- levels(dat$alcohol_30d_bin)
vol_lv <- levels(dat$sw_partners_clients_30d_3cat)

# function to fit models and calculate effects (used by boot)
med_fun <- function(d, i) {
  d <- d[i, ]
  n <- nrow(d)

  # fit models
  fit_m1  <- glm(f_m1, data = d, family = binomial)
  fit_m2  <- glm(f_m2, data = d, family = binomial)
  fit_alc <- glm(f_alc, data = d, family = binomial)
  fit_vol <- multinom(f_vol, data = d, trace = FALSE)
  fit_y   <- glm(f_y, data = d, family = binomial)

  # everyone set to street-based (d1) or not (d0)
  d1 <- mutate(d, street_sw_bin = 1L)
  d0 <- mutate(d, street_sw_bin = 0L)

  # random numbers, shared across scenarios
  u1 <- runif(n); u2 <- runif(n); ua <- runif(n); uv <- runif(n)

  # mediators drawn under each exposure level
  idu1  <- as.integer(u1 < predict(fit_m1, d1, type = "response"))
  idu0  <- as.integer(u1 < predict(fit_m1, d0, type = "response"))
  rape1 <- as.integer(u2 < predict(fit_m2, d1, type = "response"))
  rape0 <- as.integer(u2 < predict(fit_m2, d0, type = "response"))

  # alcohol use and volume of clients drawn under each exposure level
  alc1 <- factor(if_else(ua < predict(fit_alc, d1, type = "response"), alc_lv[2], alc_lv[1]), levels = alc_lv)
  alc0 <- factor(if_else(ua < predict(fit_alc, d0, type = "response"), alc_lv[2], alc_lv[1]), levels = alc_lv)
  pv1  <- predict(fit_vol, d1, type = "probs")
  pv0  <- predict(fit_vol, d0, type = "probs")
  vol1 <- factor(vol_lv[1 + (uv > pv1[, 1]) + (uv > pv1[, 1] + pv1[, 2])], levels = vol_lv)
  vol0 <- factor(vol_lv[1 + (uv > pv0[, 1]) + (uv > pv0[, 1] + pv0[, 2])], levels = vol_lv)

  # mean HIV risk: exposure level, then IDU and rape drawn under exposure level 1 or 0
  y_111 <- mean(predict(fit_y, mutate(d1, alcohol_30d_bin = alc1, sw_partners_clients_30d_3cat = vol1,
                                      idu_12m_bin = idu1, violence_rape_ever_bin = rape1), type = "response"))
  y_110 <- mean(predict(fit_y, mutate(d1, alcohol_30d_bin = alc1, sw_partners_clients_30d_3cat = vol1,
                                      idu_12m_bin = idu1, violence_rape_ever_bin = rape0), type = "response"))
  y_100 <- mean(predict(fit_y, mutate(d1, alcohol_30d_bin = alc1, sw_partners_clients_30d_3cat = vol1,
                                      idu_12m_bin = idu0, violence_rape_ever_bin = rape0), type = "response"))
  y_000 <- mean(predict(fit_y, mutate(d0, alcohol_30d_bin = alc0, sw_partners_clients_30d_3cat = vol0,
                                      idu_12m_bin = idu0, violence_rape_ever_bin = rape0), type = "response"))

  # total, direct and indirect effects (risk differences) and proportion mediated
  c(total      = y_111 - y_000,
    direct     = y_100 - y_000,
    ind_idu    = y_110 - y_100,
    ind_rape   = y_111 - y_110,
    total_ind  = y_111 - y_100,
    prop_idu   = (y_110 - y_100) / (y_111 - y_000),
    prop_rape  = (y_111 - y_110) / (y_111 - y_000),
    prop_total = (y_111 - y_100) / (y_111 - y_000))
}

# parallel bootstrap
n_cores <- max(1, parallel::detectCores() - 1)
cl <- parallel::makeCluster(n_cores)
parallel::clusterEvalQ(cl, {library(dplyr); library(nnet)})
parallel::clusterExport(cl, c("f_m1", "f_m2", "f_alc", "f_vol", "f_y", "alc_lv", "vol_lv"))
parallel::clusterSetRNGStream(cl, 123)

# bootstrap confidence intervals
set.seed(123)
bt_ce <- boot(dat, med_fun, R = 1000, parallel = "snow", ncpus = n_cores, cl = cl)
parallel::stopCluster(cl)

# results
ce_results <- data.frame(
  label    = names(bt_ce$t0),
  est      = bt_ce$t0,
  ci.lower = apply(bt_ce$t, 2, quantile, 0.025, na.rm = TRUE),
  ci.upper = apply(bt_ce$t, 2, quantile, 0.975, na.rm = TRUE),
  row.names = NULL
)
ce_results

# save results
write.csv(ce_results, "mediation_causal_results.csv", row.names = FALSE)