# load packages
pacman::p_load(dplyr, tidyr, boot, openxlsx, dagitty, ggdag, ggplot2)

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
covariates <- c("age_cat", "education_3cat", "years_in_sw_3cat", "year", "city")

# complete case data, yes/no variables converted to 0/1
sw_combined_clean_cc <- sw_combined_clean %>%
  dplyr::select(all_of(c(outcome, exposure, mediators, covariates, "years_in_sw"))) %>%
  mutate(across(all_of(c(outcome, exposure, mediators)),
                ~ if_else(is.na(.x), NA_integer_, as.integer(.x %in% c("Yes", "Positive"))))) %>%
  drop_na() %>%
  # drop FSWs with 0 years in sex work
  filter(years_in_sw >= 1) %>%
  mutate(across(all_of(covariates), ~ droplevels(as.factor(.x))))

nrow(sw_combined_clean_cc)

# number of FSWs with 0 and 1 years in sex work
table(sw_combined_clean$years_in_sw, useNA = "ifany")
sum(sw_combined_clean$years_in_sw == 0, na.rm = TRUE)
sum(sw_combined_clean$years_in_sw == 1, na.rm = TRUE)
sum(sw_combined_clean_cc$years_in_sw <= 3)

# sensitivity analysis samples: in sex work 1-4 years, and 5 or more years
sw_combined_clean_cc_sa1 <- sw_combined_clean_cc %>%
  filter(years_in_sw <= 4) %>%
  mutate(across(all_of(covariates), droplevels))
sw_combined_clean_cc_sa2 <- sw_combined_clean_cc %>%
  filter(years_in_sw >= 5) %>%
  mutate(across(all_of(covariates), droplevels))

nrow(sw_combined_clean_cc_sa1)
nrow(sw_combined_clean_cc_sa2)

# hiv prevalence by street-based sex work and idu, with exact 95% ci
hiv_by_sb_idu <- sw_combined_clean_cc %>%
  mutate(Typology = if_else(street_sw_bin == 1, "Street-based", "Other typologies"),
         IDU      = if_else(idu_12m_bin == 1, "Yes", "No")) %>%
  group_by(Typology, IDU) %>%
  summarise(N   = n(),
            HIV = sum(hiv_test_rslt_bin),
            .groups = "drop") %>%
  mutate(prev  = 100 * HIV / N,
         lower = 100 * qbeta(0.025, HIV, N - HIV + 1),
         upper = 100 * qbeta(0.975, HIV + 1, N - HIV),
         `HIV prevalence, % (95% CI)` = sprintf("%.1f (%.1f, %.1f)", prev, lower, upper)) %>%
  select(Typology, IDU, N, HIV, `HIV prevalence, % (95% CI)`)
hiv_by_sb_idu

# save table
openxlsx::write.xlsx(hiv_by_sb_idu, "hiv_by_typology_idu.xlsx")

# interventional effects by g-computation

# model formulas
base <- "age_cat + education_3cat + years_in_sw_3cat + year + city"
f_m1  <- as.formula(paste("idu_12m_bin ~ street_sw_bin +", base))
f_m2  <- as.formula(paste("violence_rape_ever_bin ~ street_sw_bin +", base))
f_y   <- as.formula(paste("hiv_test_rslt_bin ~ street_sw_bin + idu_12m_bin + violence_rape_ever_bin +",
                          "street_sw_bin:idu_12m_bin + street_sw_bin:violence_rape_ever_bin +",  # street-based by IDU and by rape interactions
                          base))

# function to fit models and calculate effects (used by boot)
med_fun <- function(d, i) {
  d <- d[i, ]
  n <- nrow(d)

  # fit models
  fit_m1  <- glm(f_m1, data = d, family = binomial)
  fit_m2  <- glm(f_m2, data = d, family = binomial)
  fit_y   <- glm(f_y, data = d, family = binomial)

  # everyone set to street-based (d1) or not (d0)
  d1 <- mutate(d, street_sw_bin = 1L)
  d0 <- mutate(d, street_sw_bin = 0L)

  # random numbers, shared across scenarios
  u1 <- runif(n); u2 <- runif(n)

  # mediators drawn under each exposure level
  idu1  <- as.integer(u1 < predict(fit_m1, d1, type = "response"))
  idu0  <- as.integer(u1 < predict(fit_m1, d0, type = "response"))
  rape1 <- as.integer(u2 < predict(fit_m2, d1, type = "response"))
  rape0 <- as.integer(u2 < predict(fit_m2, d0, type = "response"))

  # mean HIV risk: exposure level, then IDU and rape drawn under exposure level 1 or 0
  y_111 <- mean(predict(fit_y, mutate(d1, idu_12m_bin = idu1, violence_rape_ever_bin = rape1), type = "response"))
  y_110 <- mean(predict(fit_y, mutate(d1, idu_12m_bin = idu1, violence_rape_ever_bin = rape0), type = "response"))
  y_100 <- mean(predict(fit_y, mutate(d1, idu_12m_bin = idu0, violence_rape_ever_bin = rape0), type = "response"))
  y_000 <- mean(predict(fit_y, mutate(d0, idu_12m_bin = idu0, violence_rape_ever_bin = rape0), type = "response"))

  # total, direct and indirect effects (risk differences) and proportion mediated
  c(total      = y_111 - y_000,
    direct     = y_100 - y_000,
    ind_idu    = y_110 - y_100,
    ind_rape   = y_111 - y_110,
    total_ind  = y_111 - y_100,
    prop_idu   = (y_110 - y_100) / (y_111 - y_000),
    prop_rape  = (y_111 - y_110) / (y_111 - y_000),
    prop_total = (y_111 - y_100) / (y_111 - y_000),
    # interaction odds ratios from the HIV model
    int_idu_or  = exp(coef(fit_y)[["street_sw_bin:idu_12m_bin"]]),
    int_rape_or = exp(coef(fit_y)[["street_sw_bin:violence_rape_ever_bin"]]))
}

# parallel bootstrap
n_cores <- max(1, parallel::detectCores() - 1)
cl <- parallel::makeCluster(n_cores)
parallel::clusterEvalQ(cl, library(dplyr))
parallel::clusterExport(cl, c("f_m1", "f_m2", "f_y"))

# function to bootstrap, save the bootstrap and save results
run_ce <- function(d, name) {
  parallel::clusterSetRNGStream(cl, 123)
  set.seed(123)
  bt <- boot(d, med_fun, R = 1000, parallel = "snow", ncpus = n_cores, cl = cl)

  # save bootstrap so it doesn't need rerunning (reload with readRDS)
  saveRDS(bt, paste0(name, "_boot.rds"))

  res <- data.frame(
    label    = names(bt$t0),
    est      = bt$t0,
    ci.lower = apply(bt$t, 2, quantile, 0.025, na.rm = TRUE),
    ci.upper = apply(bt$t, 2, quantile, 0.975, na.rm = TRUE),
    row.names = NULL
  )
  write.csv(res, paste0(name, "_results.csv"), row.names = FALSE)
  res
}

# main analysis: in sex work more than 1 year
ce_results <- run_ce(sw_combined_clean_cc, "mediation_causal")
ce_results

# idu-hiv odds ratio by typology
fit_y <- glm(f_y, data = sw_combined_clean_cc, family = binomial)
b <- coef(fit_y); v <- vcov(fit_y)
k <- c("idu_12m_bin", "street_sw_bin:idu_12m_bin")
se_street <- sqrt(v[k[1], k[1]] + v[k[2], k[2]] + 2 * v[k[1], k[2]])
exp(b[k[1]] + c(0, -1.96, 1.96) * sqrt(v[k[1], k[1]]))   # other typologies
exp(sum(b[k]) + c(0, -1.96, 1.96) * se_street)            # street-based


# sensitivity analysis: in sex work 1-4 years
ce_results_1to4 <- run_ce(sw_combined_clean_cc_sa1, "mediation_causal_sw1to4")
ce_results_1to4

# sensitivity analysis: in sex work 5 or more years
ce_results_5plus <- run_ce(sw_combined_clean_cc_sa2, "mediation_causal_sw5plus")
ce_results_5plus

parallel::stopCluster(cl)

