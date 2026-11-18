## load packages
pacman::p_load(dplyr, tidyr, tibble, haven)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Chapter Writing/Chapter 6 - Ukraine FSWs IDU violence and HIV/data/SW data")

# load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")
clients_combined_clean <- readRDS("clients_combined_clean.rds")
pwid <- read_dta("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Chapter Writing/Chapter 6 - Ukraine FSWs IDU violence and HIV/data/PWID data/PWID_combined_clean.dta") %>%
  mutate(across(c(sex, hiv_test_rslt), as_factor),
         age  = as.numeric(age),
         wave = as.numeric(wave))

## model strata

# strata order used in the model
strata <- c("nonIDU_nonSB", "nonIDU_SB", "IDU_nonSB", "IDU_SB")

# assign each FSW to a model stratum
strata_df <- sw_combined_clean %>%
  filter(!is.na(idu_12m_bin), !is.na(street_sw_bin)) %>%
  mutate(
    stratum = factor(case_when(
      idu_12m_bin == "No"  & street_sw_bin == "No"  ~ "nonIDU_nonSB",
      idu_12m_bin == "No"  & street_sw_bin == "Yes" ~ "nonIDU_SB",
      idu_12m_bin == "Yes" & street_sw_bin == "No"  ~ "IDU_nonSB",
      idu_12m_bin == "Yes" & street_sw_bin == "Yes" ~ "IDU_SB"
    ), levels = strata)
  )

table(strata_df$stratum, strata_df$years_in_sw_3cat, useNA = "ifany")

### parameters

## population and entry parameters

# population size of FSWs
N_F <- tibble(N = 67425,
              lower = 50024,
              upper = 84826)
N_F

# observed proportion of FSWs in each stratum, by time in SW
omega_by_year <- bind_rows(
  strata_df %>% filter(years_in_sw <= 1)             %>% mutate(group = "0-1"),
  strata_df %>% filter(years_in_sw_3cat == "0-2")    %>% mutate(group = "0-2"),
  strata_df                                          %>% mutate(group = "All")
) %>%
  count(year, group, stratum, .drop = FALSE) %>%
  group_by(year, group) %>%
  mutate(N     = sum(n),
         omega = n / N,
         # exact (Clopper-Pearson) 95% CI, same as binom.test
         lower = qbeta(0.025, n, N - n + 1),
         upper = qbeta(0.975, n + 1, N - n)) %>%
  ungroup()

print(omega_by_year, n = Inf)

# new FSWs (<= 1 year in SW)
new1_strata_df <- strata_df %>% filter(years_in_sw <= 1)

# proportion entering as IDU (phi_F_I)
fit_idu_1y    <- glm(I(idu_12m_bin == "Yes") ~ year, ## model idu by year
                     data = new1_strata_df, family = binomial)
summary(fit_idu_1y)

fit_idu_const <- glm(I(idu_12m_bin == "Yes") ~ 1, ## proportion of new FSWs who inject (intercept only)
                     data = new1_strata_df, family = binomial)

summary(fit_idu_const) ## 4.5%

anova(fit_idu_const, fit_idu_1y, test = "Chisq") ## no change in idu over time, keep idu constant at 4.5%

# constant phi_F_I
phi_F_coef <- coef(summary(fit_idu_const))[1, ] ## first row of fit_idu_const matrix
phi_F_coef

phi_F_I <- tibble(phi   = plogis(phi_F_coef[["Estimate"]]), # first column is proportion
                  lower = plogis(phi_F_coef[["Estimate"]] - 1.96 * phi_F_coef[["Std. Error"]]), # second column is LB
                  upper = plogis(phi_F_coef[["Estimate"]] + 1.96 * phi_F_coef[["Std. Error"]])) # third column is UB

phi_F_I

# proportion entering as former injector (phi_F_X)
new1_status_df <- new1_strata_df %>% filter(!is.na(idu_status_3cat))
table(new1_status_df$year, new1_status_df$idu_status_3cat)

fit_former_const <- glm(I(idu_status_3cat == "Former") ~ 1, ## proportion of new FSWs who formerly injected (intercept only)
                        data = new1_status_df, family = binomial)

summary(fit_former_const)

# constant phi_F_X
phi_F_X_coef <- coef(summary(fit_former_const))[1, ]

phi_F_X <- tibble(phi    = plogis(phi_F_X_coef[["Estimate"]]),
                  lower  = plogis(phi_F_X_coef[["Estimate"]] - 1.96 * phi_F_X_coef[["Std. Error"]]),
                  upper  = plogis(phi_F_X_coef[["Estimate"]] + 1.96 * phi_F_X_coef[["Std. Error"]]))

phi_F_X

# proportion entering as never injector (phi_F_N)
phi_F_N <- tibble(phi   = 1 - phi_F_I[["phi"]]   - phi_F_X[["phi"]],
                  lower = 1 - phi_F_I[["upper"]] - phi_F_X[["upper"]],
                  upper = 1 - phi_F_I[["lower"]] - phi_F_X[["lower"]])

phi_F_N

# tibble of entry into injecting strata (phi_F)
phi_F <- bind_rows(phi_F_I %>% mutate(status = "Current"),
                  phi_F_X %>% mutate(status = "Former"),
                  phi_F_N %>% mutate(status = "Never"))
phi_F

# proportion of new FSWs entering as SB (phi_F_SB_2009 and phi_F_SB_2021)
fit_sb_1y <- glm(I(street_sw_bin == "Yes") ~ year + idu_12m_bin, ## include idu so entry as SB differs by idu
                 data = new1_strata_df, family = binomial)

summary(fit_sb_1y) ## .84 reduction in odds of SB per year, 3-fold higher odds of SB among IDU vs. non-IDU 

# non-IDU SB in 2009 and 2021 
phi_F_SB <- tibble(year = c(2009, 2021), idu_12m_bin = "No") ## create phi_F_SB df
phi_F_SB

# predictions in 2009 and 2021 on logit scale
sb_pred <- predict(fit_sb_1y, newdata = phi_F_SB, type = "link", se.fit = TRUE) ## newdata inputs values to predict from sb_ends
as.data.frame(sb_pred[c("fit", "se.fit")]) ## row 1 is 2009 row 2 is 2021

# add predicted proportions to phi_F_SB
phi_F_SB <- phi_F_SB %>%
  mutate(s     = plogis(sb_pred$fit),
         lower = plogis(sb_pred$fit - 1.96 * sb_pred$se.fit),
         upper = plogis(sb_pred$fit + 1.96 * sb_pred$se.fit))

phi_F_SB # proportion street-based non-IDU for 2009 & 2021

# IDU vs non-IDU difference in SB on the log scale
beta_F_SB_I <- coef(fit_sb_1y)[["idu_12m_binYes"]] ## coefficient for increase in odds of SB among current IDU
beta_F_SB_I ## log odds ratio 0.94

# odds ratio with 95% CI
or_F_SB_I <- tibble(or    = exp(beta_F_SB_I),
                    lower = exp(confint.default(fit_sb_1y)["idu_12m_binYes", 1]),
                    upper = exp(confint.default(fit_sb_1y)["idu_12m_binYes", 2]))

or_F_SB_I ## OR 2.56

# former vs never injector difference in SB
fit_sb_status <- glm(I(street_sw_bin == "Yes") ~ year + idu_status_3cat, ## sb sw as outcome and year and idu as predictors
                     data = new1_status_df, family = binomial)

beta_F_SB_X <- coef(fit_sb_status)[["idu_status_3catFormer"]] ## log odds ratio, former vs never
beta_F_SB_X

# odds ratio with 95% CI
beta_F_SB_X <- tibble(or    = exp(beta_F_SB_X),
                    lower = exp(confint.default(fit_sb_status)["idu_status_3catFormer", 1]),
                    upper = exp(confint.default(fit_sb_status)["idu_status_3catFormer", 2]))

beta_F_SB_X ## OR 1.14 (0.139-9.35)

# HIV prevalence at entry among FSWs (pi_F_I)

# age groups, same breaks for PWID and FSWs
age_breaks <- c(0, 20, 25, 30, 35, 100)

# HIV among female PWID by wave and age group
pwid_age <- pwid %>%
  filter(sex == "female", hiv_test_rslt %in% c("Negative", "Positive")) %>%
  mutate(agegrp = as.character(cut(age, breaks = age_breaks, right = FALSE))) %>%
  group_by(wave, agegrp) %>%
  summarise(pos  = sum(hiv_test_rslt == "Positive"),
            n    = n(),
            prev = pos / n,
            .groups = "drop")

pwid_age

# new FSWs (<= 1 year in SW) with a valid HIV test result, who currently inject
new1_idu_df <- strata_df %>%
  filter(years_in_sw <= 1,
         hiv_test_rslt_bin %in% c("Positive", "Negative"),
         idu_12m_bin == "Yes")

# age distribution of new FSWs who currently inject
fsw_age_weights <- new1_idu_df %>%
  mutate(age_num = suppressWarnings(as.numeric(age)),
         agegrp  = as.character(cut(age_num, breaks = age_breaks, right = FALSE))) %>%
  filter(!is.na(agegrp)) %>%
  count(agegrp) %>%
  mutate(weight = n / sum(n)) %>%
  select(agegrp, weight)

fsw_age_weights

# standardised prevalence by wave
pwid_std <- pwid_age %>%
  left_join(fsw_age_weights, by = "agegrp") %>%
  group_by(wave) %>%
  summarise(prev_std = sum(weight * prev),
            se       = sqrt(sum(weight^2 * prev * (1 - prev) / n)),
            .groups  = "drop") %>%
  mutate(lower = prev_std - 1.96 * se,
         upper = prev_std + 1.96 * se)

pwid_std

# female PWID HIV prevalence standardised to the age distribution of new IDU FSWs
pi_F_I <- pwid_age %>%
  group_by(agegrp) %>%
  summarise(pos = sum(pos), n = sum(n), .groups = "drop") %>%
  mutate(prev = pos / n) %>%
  left_join(fsw_age_weights, by = "agegrp") %>%
  summarise(pi = sum(weight * prev),
            se = sqrt(sum(weight^2 * prev * (1 - prev) / n))) %>%
  mutate(lower = pi - 1.96 * se,
         upper = pi + 1.96 * se) %>%
  select(pi, lower, upper)

pi_F_I

# hiv prevalence at entry by injecting status
pi_F_D <- bind_rows(pi_idu %>% mutate(status = "Current"),
                  pi_idu %>% mutate(status = "Former"),
                  tibble(status = "Never", pi = 0, lower = 0, upper = 0)) %>%
  select(status, pi, lower, upper)

pi_F_D

# ART coverage among FSWs living with HIV: new (0-2 years in SW) vs all (r_F_zeta)
art_ratio_df <- strata_df %>%
  filter(hiv_test_rslt_bin == "Positive", art_current_bin %in% c("Yes", "No")) %>%
  mutate(new_fsw = years_in_sw_3cat == "0-2")

# pooled ratio across survey years
art_ratio <- art_ratio_df %>%
  summarise(cov_all = mean(art_current_bin == "Yes"),
            cov_new = mean(art_current_bin[new_fsw %in% TRUE] == "Yes"),
            ratio   = cov_new / cov_all)

art_ratio

# 95% CI for the ratio
set.seed(1)
boot_ratio <- replicate(2000, {
  d <- art_ratio_df[sample(nrow(art_ratio_df), replace = TRUE), ]
  mean(d$art_current_bin[d$new_fsw %in% TRUE] == "Yes") / mean(d$art_current_bin == "Yes")
})

r_F_zeta <- art_ratio %>%
  mutate(lower = quantile(boot_ratio, 0.025),
         upper = quantile(boot_ratio, 0.975))

r_F_zeta

# proportion of clients who inject (phi_C_I)
table(clients_combined_clean$year, clients_combined_clean$idu_12m_bin, useNA = "ifany")

fit_client_idu_const <- glm(I(idu_12m_bin == "Yes") ~ 1,
                            data = clients_combined_clean %>% filter(!is.na(idu_12m_bin)),
                            family = binomial)

summary(fit_client_idu_const)

# constant proportion of clients who inject
phi_client_coef <- coef(summary(fit_client_idu_const))[1, ]

phi_C_I <- tibble(phi = plogis(phi_client_coef[["Estimate"]]),
                     lower      = plogis(phi_client_coef[["Estimate"]] - 1.96 * phi_client_coef[["Std. Error"]]),
                     upper      = plogis(phi_client_coef[["Estimate"]] + 1.96 * phi_client_coef[["Std. Error"]]))

phi_C_I

# proportion of clients who formerly injected drugs (phi_C_X)
fsw_former_ratio <- phi_F_X[["phi"]] / phi_F_I[["phi"]]
fsw_former_ratio

sd_logit <- function(x) (qlogis(x$upper) - qlogis(x$lower)) / (2 * 1.96)

set.seed(1)
sims <- plogis(rnorm(1e5, qlogis(phi_C_I$phi), sd_logit(phi_C_I))) *
        plogis(rnorm(1e5, qlogis(phi_F_X$phi), sd_logit(phi_F_X))) /
        plogis(rnorm(1e5, qlogis(phi_F_I$phi), sd_logit(phi_F_I)))

phi_C_X <- tibble(phi   = phi_C_I$phi * fsw_former_ratio,
                  lower = quantile(sims, 0.025),
                  upper = quantile(sims, 0.975))

phi_C_X

# proportion entering as never injector (phi_F_N)
phi_C_N <- tibble(phi   = 1 - phi_C_I[["phi"]]   - phi_C_X[["phi"]],
                  lower = 1 - phi_C_I[["upper"]] - phi_C_X[["upper"]],
                  upper = 1 - phi_C_I[["lower"]] - phi_C_X[["lower"]])

phi_C_N

# tibble of entry into injecting strata (phi_C)
phi_C <- bind_rows(phi_C_I %>% mutate(status = "Current"),
                  phi_C_X %>% mutate(status = "Former"),
                  phi_C_N %>% mutate(status = "Never"))
phi_C

# HIV prevalence among new clients (pi_C_I)

# age distribution of clients who inject
client_age_weights <- clients_combined_clean %>%
  filter(idu_12m_bin == "Yes") %>%
  mutate(agegrp = as.character(cut(age_numeric, breaks = age_breaks, right = FALSE))) %>%
  filter(!is.na(agegrp)) %>%
  count(agegrp) %>%
  mutate(weight = n / sum(n)) %>%
  select(agegrp, weight)

client_age_weights

# HIV among male PWID by age group
pwid_male_age <- pwid %>%
  filter(sex == "male", hiv_test_rslt %in% c("Negative", "Positive")) %>%
  mutate(agegrp = as.character(cut(age, breaks = age_breaks, right = FALSE))) %>%
  group_by(agegrp) %>%
  summarise(pos = sum(hiv_test_rslt == "Positive"), n = n(), .groups = "drop") %>%
  mutate(prev = pos / n)

# standardised prevalence
pi_C_I <- pwid_male_age %>%
  left_join(client_age_weights, by = "agegrp") %>%
  mutate(weight = coalesce(weight, 0)) %>%
  summarise(pi = sum(weight * prev),
            se = sqrt(sum(weight^2 * prev * (1 - prev) / n))) %>%
  mutate(lower = pi - 1.96 * se,
         upper = pi + 1.96 * se) %>%
  select(pi, lower, upper)

pi_C_I

# ART coverage among clients living with HIV: new (0-2 years in SW) vs all (r_C_zeta)
r_C_zeta <- r_F_zeta
r_C_zeta

# entry proportions at the point estimates to compare with observed
omega_entry <- tibble(year = c(2009, 2011, 2013, 2015, 2017, 2021)) %>% ## one row per survey year
  mutate(s_never   = plogis(qlogis(phi_F_SB$s[1]) + (qlogis(phi_F_SB$s[2]) - qlogis(phi_F_SB$s[1])) * (year - 2009) / 12),
         s_current = plogis(qlogis(s_never) + beta_F_SB_I),   
         s_former  = s_never,                                
         never_nonSB   = phi_F_N$phi * (1 - s_never),
         never_SB      = phi_F_N$phi * s_never,
         current_nonSB = phi_F_I$phi * (1 - s_current),
         current_SB    = phi_F_I$phi * s_current,
         former_nonSB  = phi_F_X$phi * (1 - s_former),
         former_SB     = phi_F_X$phi * s_former)

print(omega_entry, width = Inf)

## injecting risk parameters

# per-inection HIV transmission probability with shared syringe (beta_inj)
beta_inj <- tibble(P = 0.063,
                   lower = 0.041,
                   upper = 0.02)
beta_inj

# percent of injections with sterile syringe among FSWs (q_F)
q_F <- tibble(P = 0.96,
              lower = 0.94,
              upper = 0.97)
q_F

# percent of injections with sterile syringe among clients (q_C)
q_C <- tibble(P = 0.95,
              lower = 0.94,
              upper = 0.96)
q_C

# percent of injections with sterile syringe among FSWs and clients (q)
q <- bind_rows(q_F %>% mutate(status = "FSWs"),
               q_C %>% mutate(status = "Clients"))
q

# median times injected past month among FSWs (J_F)
J_F <- pwid %>%
  filter(sex == "female", sw_sell_90d_bin == 1) %>%
  summarise(n      = sum(!is.na(inject_times_30d)),
            mean   = mean(inject_times_30d, na.rm = TRUE),
            se     = sd(inject_times_30d, na.rm = TRUE) / sqrt(n),
            median = median(inject_times_30d, na.rm = TRUE),
            q1     = quantile(inject_times_30d, 0.25, na.rm = TRUE),
            q3     = quantile(inject_times_30d, 0.75, na.rm = TRUE)) %>%
  mutate(lower = mean - 1.96 * se,
         upper = mean + 1.96 * se)

J_F

# median times injected past month among clients (J_C)
J_C <- pwid %>%
  filter(sex == "male", sw_buy_90d_bin == 1) %>%
  summarise(n      = sum(!is.na(inject_times_30d)),
            mean   = mean(inject_times_30d, na.rm = TRUE),
            se     = sd(inject_times_30d, na.rm = TRUE) / sqrt(n),
            median = median(inject_times_30d, na.rm = TRUE),
            q1     = quantile(inject_times_30d, 0.25, na.rm = TRUE),
            q3     = quantile(inject_times_30d, 0.75, na.rm = TRUE)) %>%
  mutate(lower = mean - 1.96 * se,
         upper = mean + 1.96 * se)

J_C

# median times injected past month among FSWs and clients (J_C)
J <- bind_rows(J_F %>% mutate(status = "FSWs"),
               J_C %>% mutate(status = "Clients"))
J

# HIV prevalence among PWID (z_PWID), by wave and pooled
z_PWID_wave <- pwid %>%
  filter(hiv_test_rslt %in% c("Negative", "Positive")) %>%
  group_by(wave) %>%
  summarise(n_hiv = sum(hiv_test_rslt == "Positive"),
            n     = n(),
            z     = n_hiv / n,
            lower = binom.test(n_hiv, n)$conf.int[1],
            upper = binom.test(n_hiv, n)$conf.int[2])

z_PWID_wave

z_PWID <- pwid %>%
  filter(hiv_test_rslt %in% c("Negative", "Positive")) %>%
  summarise(n_hiv = sum(hiv_test_rslt == "Positive"),
            n     = n(),
            z     = n_hiv / n,
            lower = binom.test(n_hiv, n)$conf.int[1],
            upper = binom.test(n_hiv, n)$conf.int[2])

z_PWID

# ART coverage among PWID living with HIV (w_PWID)

# HIV-positive PWID: on ART, with those unaware of status or not registered at an AIDS centre counted as not on ART
pwid_art_df <- pwid %>%
  filter(hiv_test_rslt == "Positive") %>%
  mutate(art_on = case_when(
    art_use_bin %in% c(0, 1) ~ art_use_bin,   # answered ART question
    hiv_disclose == 97       ~ 0,             # never tested: undiagnosed
    hiv_status == 0          ~ 0,             # self-reported negative: unaware
    aids_center == 0         ~ 0,             # not registered at AIDS centre: not on ART
    TRUE                     ~ NA_real_)) %>% # refused to disclose / no answer: unknown
  filter(!is.na(art_on))

fit_art_pwid <- glm(art_on ~ wave, data = pwid_art_df, family = binomial)

w_PWID <- tibble(wave = c(2011, 2020)) %>%
  bind_cols(as_tibble(predict(fit_art_pwid, newdata = ., type = "link", se.fit = TRUE)[1:2])) %>%
  mutate(w     = plogis(fit),
         lower = plogis(fit - 1.96 * se.fit),
         upper = plogis(fit + 1.96 * se.fit))

w_PWID

# proportion of FSWs who inject with high NSP coverage (n_F)
n_F <- pwid %>%
  filter(sex == "female", sw_sell_90d_bin == 1, !is.na(syringe_coverage_4cat)) %>%
  summarise(n_high = sum(syringe_coverage_4cat == 3),
            n      = n(),
            p      = n_high / n,
            lower  = binom.test(n_high, n)$conf.int[1],
            upper  = binom.test(n_high, n)$conf.int[2])

n_F

# proportion of FSWs who inject with high NSP coverage (n_F)
n_C <- pwid %>%
  filter(sex == "male", sw_buy_90d_bin == 1, !is.na(syringe_coverage_4cat)) %>%
  summarise(n_high = sum(syringe_coverage_4cat == 3),
            n      = n(),
            p      = n_high / n,
            lower  = binom.test(n_high, n)$conf.int[1],
            upper  = binom.test(n_high, n)$conf.int[2])

n_C

# proportion of FSWs and clients who inject with high NSP coverage (n)
n <- bind_rows(n_F %>% mutate(status = "FSWs"),
               n_C %>% mutate(status = "Clients"))
n

# relative risk of HIV acquisition with high NSP coverage
RR_NSP <- tibble(rr = 0.42,
                 lower = 0.22,
                 upper = 0.81)
RR_NSP

# reduction in per-injection HIV transmission when the injecting partner living with HIV is on ART


# reduction in per-injection transmission when the PWID partner is on ART: proportion of PWID on ART who are virally suppressed (<1000 copies/mL)
e_ART <- pwid_art_df %>%
  filter(art_on == 1, vs_art %in% c(0, 1)) %>%
  summarise(n_vs  = sum(vs_art == 1),
            n     = n(),
            e     = n_vs / n,
            lower = binom.test(n_vs, n)$conf.int[1],
            upper = binom.test(n_vs, n)$conf.int[2])

e_ART

## sexual risk parameters

# per-act probability of HIV transmission from male to female without a condom (beta_F_sex)
beta_F_sex <- tibble(P = 0.08,
                     lower = 0.06,
                     upper = 0.11)
beta_F_sex

# RR of HIV transmission from male to female when condom is used
RR_F_cond <- tibble(RR = 0.31,
                    lower = 0.20,
                    upper = 0.48)
RR_F_cond


# per-act probability of HIV transmission from female to male without a condom (beta_C_sex)
beta_C_sex <- tibble(P = 0.04,
                     lower = 0.01,
                     upper = 0.14)
beta_C_sex

# RR of HIV transmission from female to male when condom is used (RR_C_cond)
RR_C_cond <- tibble(RR = 44,
                    lower = 0.24,
                    upper = 0.80)
RR_C_cond

# mean number of clients in past 30 days (k_F_ip)

# keep FSWs with typology, IDU and client numbers
k_data <- sw_combined_clean %>%
  filter(street_sw_bin %in% c("Yes", "No"),
         idu_12m_bin %in% c("Yes", "No"),
         !is.na(sw_partners_clients_30d))

# clients in past 30 days by typology and period only
k_model_df <- k_data %>%
  mutate(period = factor(if_else(year < 2017, "2009-2015", "2017-2021")))

fit_k_sb <- glm(sw_partners_clients_30d ~ street_sw_bin * period,
                data = k_model_df, family = quasipoisson)

k_F_ip <- tibble(term  = names(coef(fit_k_sb)),
               est   = exp(coef(fit_k_sb)),
               lower = exp(confint.default(fit_k_sb)[, 1]),
               upper = exp(confint.default(fit_k_sb)[, 2]))

k_F_ip


# sex acts per client (s)

# keep FSWs with typology, IDU and acts per client
acts_data <- sw_combined_clean %>%
  filter(street_sw_bin %in% c("Yes", "No"),
         idu_12m_bin %in% c("Yes", "No"),
         !is.na(sw_acts_per_client))

# pooled across all groups
s <- acts_data %>%
  summarise(n    = n(),
            mean = mean(sw_acts_per_client),
            se   = sd(sw_acts_per_client) / sqrt(n)) %>%
  mutate(lower = mean - 1.96 * se,
         upper = mean + 1.96 * se)

s

# condom use at last sex with a client

# keep FSWs with typology, IDU and condom use
condom_data <- sw_combined_clean %>%
  filter(street_sw_bin %in% c("Yes", "No"),
         idu_12m_bin %in% c("Yes", "No"),
         client_condom_lastsex_bin %in% c("Yes", "No"))

# condom use at last sex with a client among FSWs who currently inject (u_F_I)
u_F_I <- condom_data %>%
  filter(idu_12m_bin == "Yes") %>%
  summarise(n_yes = sum(client_condom_lastsex_bin == "Yes"),
            n     = n(),
            prop  = n_yes / n,
            lower = binom.test(n_yes, n)$conf.int[1],
            upper = binom.test(n_yes, n)$conf.int[2])

u_F_I

# condom use at last sex with a client among former injectors (u_F_X)
u_F_X <- sw_combined_clean %>%
  filter(idu_status_3cat == "Former",
         client_condom_lastsex_bin %in% c("Yes", "No")) %>%
  summarise(n_yes = sum(client_condom_lastsex_bin == "Yes"),
            n     = n(),
            prop  = n_yes / n,
            lower = binom.test(n_yes, n)$conf.int[1],
            upper = binom.test(n_yes, n)$conf.int[2])

u_F_X

# condom use at last sex with a client among FSWs who do not currently inject (u_F_N)
u_F_N <- condom_data %>%
  filter(idu_12m_bin == "No") %>%
  summarise(n_yes = sum(client_condom_lastsex_bin == "Yes"),
            n     = n(),
            prop  = n_yes / n,
            lower = binom.test(n_yes, n)$conf.int[1],
            upper = binom.test(n_yes, n)$conf.int[2])

u_F_N

u_F <- bind_rows(u_F_I %>% mutate(status = "Current"),
                 u_F_X %>% mutate(status = "Former"),
                 u_F_N %>% mutate(status = "Never"))
u_F

# increased hazard of hiv among FSWs who have experienced rape (HR_rape)
HR_rape <- tibble(HR = 2.99,
                  lower = 1.08,
                  upper = 8.30)

# rate of rape among indoor-based FSWs (V_F_indoor)
v_F_indoor <- tibble(rate = 2.3,
                     lower = 1.2,
                     upper = 3.9)

# increased risk of rape among street-based FSWs (RR_rapeSB)
RR_rapeSB <- tibble(RR = 2.79,
                  lower = 1.43,
                  upper = 5.43)

# viral suppression (<1000 copies/mL) among FSWs on ART
vs_fsw_df <- sw_combined_clean %>%
  filter(hiv_test_rslt_bin == "Positive", art_current_bin == "Yes") %>%
  mutate(vl_num = suppressWarnings(as.numeric(hiv_vl)),
         vs = case_when(hiv_vl %in% c("not detected", "<839", "<1000 copies/ml") ~ 1,
                        hiv_vl == "1000> copies/ml"                            ~ 0,
                        !is.na(vl_num) & vl_num <  1000                         ~ 1,
                        !is.na(vl_num) & vl_num >= 1000                         ~ 0,
                        TRUE                                                    ~ NA_real_)) %>%
  filter(!is.na(vs))

e_ART_S <- vs_fsw_df %>%
  summarise(n_vs  = sum(vs),
            n     = n(),
            e     = n_vs / n,
            lower = binom.test(n_vs, n)$conf.int[1],
            upper = binom.test(n_vs, n)$conf.int[2])

e_ART_S

# assortative mixing by IDU status (a)

# step 1
# restrict to FSWs with at least one client in past month and a yes/no answer on IDU clients
mixing <- sw_combined_clean %>%
  filter(idu_12m_bin %in% c("Yes", "No"),
         client_inject_30d_3cat %in% c("Yes", "No"),
         sw_partners_clients_30d > 0) %>%

# step 2
# by IDU status: P = proportion of FSWs with any IDU client, k = median clients in past month
group_by(idu_12m_bin) %>%
summarise(n = n(),
          P = mean(client_inject_30d_3cat == "Yes"),
          k = median(sw_partners_clients_30d)) %>%

# step 3
# proportion of clients who inject, from P = 1 - (1 - p)^k
mutate(p = 1 - (1 - P)^(1 / k),

# step 4
# scale to proportion of clients who inject in client IBBS
p_adj = p * phi_C_I$phi / weighted.mean(p, n))

mixing

# step 5
# a = p_adj among IDU FSWs minus p_adj among non-IDU FSWs
a <- diff(mixing$p_adj)
a

## FSW parameters

















# years in sex work by stratum (for exit)
years_summary <- strata_df %>%
  group_by(stratum) %>%
  summarise(
    n      = sum(!is.na(years_in_sw)),
    mean   = mean(years_in_sw, na.rm = TRUE),
    median = median(years_in_sw, na.rm = TRUE),
    sd     = sd(years_in_sw, na.rm = TRUE),
    var    = var(years_in_sw, na.rm = TRUE),
    min    = min(years_in_sw, na.rm = TRUE),
    max    = max(years_in_sw, na.rm = TRUE),
    .groups = "drop"
  )

years_summary

# in model strata order
years_prior <- years_summary %>% slice(match(strata, stratum))

# mean FSW age
sw_combined_clean$age_numeric <- suppressWarnings(as.numeric(sw_combined_clean$age))
mean(sw_combined_clean$age_numeric, na.rm = TRUE)                                                       # mean FSW age

# age started sex work overall and by wave
age_first_sw_avg <- mean(sw_combined_clean$age_first_sw_numeric, na.rm = TRUE)
age_first_sw_avg

age_first_sw_avg_year <- sw_combined_clean %>%
  group_by(year) %>%
  summarise(n = sum(!is.na(age_first_sw_numeric)),
            mean = mean(age_first_sw_numeric, na.rm = TRUE),
            se = sd(age_first_sw_numeric, na.rm = TRUE) / sqrt(n)) %>%
  mutate(lci = mean - 1.96 * se,
         uci = mean + 1.96 * se)

age_first_sw_avg_year

# movement between strata from linked datra

## movement between strata (linked cohort, FSW IBBS 2013-2021)

sw_data_long <- readRDS("sw_data_long.rds")

# intervals starting in start_state; covariate taken at the start of the interval (as in calc_transition_rate, script 2)
make_cohort <- function(state_var, start, end, covar) {
  sw_data_long %>%
    arrange(id, interview_dte) %>%
    group_by(id) %>%
    mutate(state_start = lag(.data[[state_var]]),
           state_end   = .data[[state_var]],
           covar_start = lag(.data[[covar]]),
           t_start     = lag(interview_dte),
           t_end       = interview_dte) %>%
    ungroup() %>%
    filter(state_start == start, !is.na(state_end)) %>%
    mutate(event = as.numeric(state_end == end),
           py    = as.numeric(t_end - t_start) / if_else(event == 1, 2, 1) / 365.25)
}

sw_sb_cohort    <- make_cohort("street_sw_bin", "No",  "Yes", "idu_12m_bin")     # indoor -> street-based (psi), by IDU
sw_nosb_cohort  <- make_cohort("street_sw_bin", "Yes", "No",  "idu_12m_bin")     # street-based -> indoor (rho), by IDU
sw_idu_cohort   <- make_cohort("idu_12m_bin",   "No",  "Yes", "street_sw_bin")   # non-IDU -> IDU (tau), by typology
sw_noidu_cohort <- make_cohort("idu_12m_bin",   "Yes", "No",  "street_sw_bin")   # IDU -> non-IDU (eps), by typology

# rate per 100 PY; adds a rate ratio for the other stratum (at start of interval) only if it differs (likelihood ratio test, p < 0.05)
transition_rate <- function(data) {
  df <- data %>%
    filter(covar_start %in% c("Yes", "No"), py > 0) %>%
    mutate(x = factor(covar_start, levels = c("No", "Yes")))

  fit_0 <- glm(event ~ 1 + offset(log(py)), family = poisson, data = df)
  fit_1 <- glm(event ~ x + offset(log(py)), family = poisson, data = df)
  p     <- anova(fit_0, fit_1, test = "Chisq")$`Pr(>Chi)`[2]

  fit <- if (p < 0.05) fit_1 else fit_0
  ci  <- confint.default(fit)
  m   <- exp(matrix(c(coef(fit), ci[, 1], ci[, 2]), ncol = 3))
  m[1, ] <- m[1, ] * 100

  tibble(term   = c("rate per 100 PY", "rate ratio")[seq_len(nrow(m))],
         est    = m[, 1],
         lower  = m[, 2],
         upper  = m[, 3],
         p_diff = p)
}

psi <- transition_rate(sw_sb_cohort)      # indoor -> street-based, by IDU
rho <- transition_rate(sw_nosb_cohort)    # street-based -> indoor, by IDU
tau <- transition_rate(sw_idu_cohort)     # non-IDU -> IDU, by typology
eps <- transition_rate(sw_noidu_cohort)   # IDU -> non-IDU, by typology

psi; rho; tau; eps

# function for rates per 100 PY
transition_rate <- function (data, cover) { ## data is dataset and covar is outcome variable
  df <- data %>%
    filter(.data[[covar]] %in% c("Yes", "No"), py > 0) %>% ## drop missing values
    mutate(event = as.numeric(event %in% c(1, "1", "Yes")),
           x     = factor(.data[[covar]], levels = c("No", "Yes")))

}

## duration of sex work (for cessation, gamma)

# mean years in sex work by IDU
duration_sw <- strata_df %>%
  filter(idu_12m_bin %in% c("Yes", "No"), !is.na(years_in_sw)) %>%
  group_by(idu_12m_bin) %>%
  summarise(n    = n(),
            mean = mean(years_in_sw)) %>%
  mutate(lower      = 0.5 * mean,
         upper      = 2 * mean,
         exit_total = 1 / mean,
         exit_lower = 1 / upper,      
         exit_upper = 1 / lower)

duration_sw

## non-HIV mortality (mu)
e_x      <- 53.2                        # remaining life expectancy, Ukrainian women at age 22
cmr_pwid <- c(0.91, 0.68, 1.14) / 100   # non-AIDS CMR per PY, female PWID (rate, lower, upper) (Mathers & Degenhardt 2014, Table 3)

mu_fsw <- tibble(idu_12m_bin = c("No", "Yes"),
                 mu          = 1 / e_x + c(0, cmr_pwid[1]),
                 lower       = 1 / e_x + c(0, cmr_pwid[2]),
                 upper       = 1 / e_x + c(0, cmr_pwid[3]))

mu_fsw

## cessation of sex work (gamma)
gamma_fsw <- duration_sw %>%
  left_join(select(mu_fsw, idu_12m_bin, mu), by = "idu_12m_bin") %>%
  transmute(idu_12m_bin,
            gamma = exit_total - mu,
            lower = exit_lower - mu,
            upper = exit_upper - mu)

gamma_fsw

## client parameters

# client non-HIV mortality (mu_C)
mu_C <- 388.8 / 100000

mu_C

# years buying sex by IDU (for cessation, gamma_C)
duration_client <- clients_combined_clean %>%
  filter(idu_12m_bin %in% c("Yes", "No"), !is.na(years_buying)) %>%
  group_by(idu_12m_bin) %>%
  summarise(n    = n(),
            mean = mean(years_buying)) %>%
  mutate(lower      = 0.5 * mean,
         upper      = 2 * mean,
         exit_total = 1 / mean,
         exit_lower = 1 / upper,
         exit_upper = 1 / lower)

duration_client

# client non-HIV mortality by IDU: male mortality, plus excess non-AIDS mortality for injecting
cmr_pwid_m <- c(1.35, 1.19, 1.52) / 100   # non-AIDS CMR per PY, male PWID (rate, lower, upper) (Mathers & Degenhardt 2014, Table 3)

mu_client <- tibble(idu_12m_bin = c("No", "Yes"),
                    mu          = mu_C + c(0, cmr_pwid_m[1]),
                    lower       = mu_C + c(0, cmr_pwid_m[2]),
                    upper       = mu_C + c(0, cmr_pwid_m[3]))

mu_client

# cessation of buying sex (gamma_C) = total exit rate minus non-HIV mortality
gamma_client <- duration_client %>%
  left_join(select(mu_client, idu_12m_bin, mu), by = "idu_12m_bin") %>%
  transmute(idu_12m_bin,
            gamma = exit_total - mu,
            lower = exit_lower - mu,
            upper = exit_upper - mu)

gamma_client




# mean client age
mean(clients_combined_clean$age_numeric, na.rm = TRUE)                                                  # mean client age
tapply(clients_combined_clean$age_numeric, clients_combined_clean$year, mean, na.rm = TRUE)             # by survey year

# years buying sex among clients (for exit)
T_C <- clients_combined_clean %>%
  summarise(n     = sum(!is.na(years_buying)),
            T_C   = mean(years_buying, na.rm = TRUE),
            se    = sd(years_buying, na.rm = TRUE) / sqrt(n)) %>%
  mutate(lower = T_C - 1.96 * se,
         upper = T_C + 1.96 * se)

T_C

# fsws seen by clients
client_fsw_partners_12m <- clients_combined_clean %>%
  group_by(year) %>%
  summarise(n = sum(!is.na(fsw_partners_12m_numeric)),
            missing = sum(is.na(fsw_partners_12m_numeric)),
            mean = mean(fsw_partners_12m_numeric, na.rm = TRUE),
            median = median(fsw_partners_12m_numeric, na.rm = TRUE),
            min = min(fsw_partners_12m_numeric, na.rm = TRUE),
            max = max(fsw_partners_12m_numeric, na.rm = TRUE))

client_fsw_partners_12m

# paid sex contacts in past 12 months among clients
client_paid_contacts_12m <- clients_combined_clean %>%
  group_by(year) %>%
  summarise(n = sum(!is.na(paid_sex_contacts_12m)),
            mean = mean(paid_sex_contacts_12m, na.rm = TRUE),
            median = median(paid_sex_contacts_12m, na.rm = TRUE))

client_paid_contacts_12m

### calibration targets

# client population size implied by FSW and client reports, across the plausible range of m
N_F <- 80000
c_C <- weighted.mean(client_fsw_partners_12m$mean, client_fsw_partners_12m$n)

reg_12m   <- mean(as.numeric(c_year_data$sw_partners_reg_clients_30d))
irreg_30d <- mean(as.numeric(c_year_data$sw_partners_irreg_clients_30d))

N_C_by_m <- tibble(m = c(3, 6)) %>%
  mutate(c_F = reg_12m + m * irreg_30d,
         N_C = N_F * c_F / c_C)

N_C_by_m

## FSW calibration targets

# observed shares among all FSWs
obs_shares <- omega_by_year %>%
  filter(group == "All") %>%
  select(year, stratum, N, observed = omega, lower, upper)
obs_shares

# keep FSWs with typology and IDU
strata_data <- sw_combined_clean %>%
  filter(street_sw_bin %in% c("Yes", "No"),
         idu_12m_bin %in% c("Yes", "No"))

# proportion in each group with exact 95% CI
strata_data %>%
  count(year, street_sw_bin, idu_12m_bin) %>%
  group_by(year) %>%
  mutate(N = sum(n),
         prop = n / N,
         lci = qbeta(0.025, n, N - n + 1),
         uci = qbeta(0.975, n + 1, N - n)) %>%
  print(n = Inf)

# HIV prevalence among FSWs, by group and year

# keep FSWs with typology, IDU and an HIV test result
hiv_data <- strata_data %>%
  filter(hiv_test_rslt_bin %in% c("Positive", "Negative", "1", "0")) %>%
  mutate(hiv_pos = hiv_test_rslt_bin %in% c("Positive", "1"))

# prevalence with exact 95% CI
fsw_hiv_prev <- hiv_data %>%
  group_by(year, street_sw_bin, idu_12m_bin) %>%
  summarise(n_hiv = sum(hiv_pos),
            N = n()) %>%
  mutate(prev = n_hiv / N,
         lci = qbeta(0.025, n_hiv, N - n_hiv + 1),
         uci = qbeta(0.975, n_hiv + 1, N - n_hiv))

print(fsw_hiv_prev, n = Inf)

# ART coverage among FSWs living with HIV, by year (ART not recorded 2009, 2011)

# keep FSWs living with HIV with ART status
art_data <- hiv_data %>%
  filter(hiv_pos, art_current_bin %in% c("Yes", "No"))

# by group and year
fsw_art_cov <- art_data %>%
  group_by(year, street_sw_bin, idu_12m_bin) %>%
  summarise(n_art = sum(art_current_bin == "Yes"),
            N = n()) %>%
  mutate(cov = n_art / N,
         lci = qbeta(0.025, n_art, N - n_art + 1),
         uci = qbeta(0.975, n_art + 1, N - n_art))

print(fsw_art_cov, n = Inf)

# all groups together, by year
fsw_art_cov_all <- art_data %>%
  group_by(year) %>%
  summarise(n_art = sum(art_current_bin == "Yes"),
            N = n()) %>%
  mutate(cov = n_art / N,
         lci = qbeta(0.025, n_art, N - n_art + 1),
         uci = qbeta(0.975, n_art + 1, N - n_art))

fsw_art_cov_all

## clients caliobration targets

# HIV prevalence among clients, by year
client_hiv_prev <- clients_combined_clean %>%
  filter(hiv_test_rslt_bin %in% c("Positive", "Negative")) %>%
  group_by(year) %>%
  summarise(n_hiv = sum(hiv_test_rslt_bin == "Positive"),
            N = n()) %>%
  mutate(prev = n_hiv / N,
         lci = qbeta(0.025, n_hiv, N - n_hiv + 1),
         uci = qbeta(0.975, n_hiv + 1, N - n_hiv))

client_hiv_prev

# all HIV+ in 2014 by raw ART answer
clients_combined_clean %>%
  filter(hiv_test_rslt_bin == "Positive", year == 2014) %>%
  count(art_current, art_current_bin)

# ART coverage among clients living with HIV (2014 only)
client_art_cov <- clients_combined_clean %>%
  filter(hiv_test_rslt_bin == "Positive", art_current_bin %in% c("Yes", "No")) %>%
  group_by(year) %>%
  summarise(n_art = sum(art_current_bin == "Yes"),
            N = n()) %>%
  mutate(cov = n_art / N,
         lci = qbeta(0.025, n_art, N - n_art + 1),
         uci = qbeta(0.975, n_art + 1, N - n_art))

client_art_cov

## save model parameters
saveRDS(list(strata                 = strata,
             omega_by_year          = omega_by_year,
             phi                    = phi,
             sb_ends                = sb_ends,
             beta_sb_idu            = beta_sb_idu,
             pi_idu                 = pi_idu,
             art_ratio              = art_ratio,
             fsw_client_inject_12m  = fsw_client_inject_12m,
             fsw_client_inject_30d  = fsw_client_inject_30d,
             client_paid_fsw_inject = client_paid_fsw_inject,
             k_by_year              = k_by_year,
             k_overall              = k_overall,
             k_period               = k_period,
             acts_overall           = acts_overall,
             condom_by_year         = condom_by_year,
             condom_overall         = condom_overall,
             years_prior            = years_prior,
             T_C                    = T_C,
             client_fsw_partners_12m  = client_fsw_partners_12m,
             client_paid_contacts_12m = client_paid_contacts_12m,
             age_first_sw_avg       = age_first_sw_avg,
             mu_fsw                 = mu_fsw,
             duration_sw            = duration_sw,
             gamma_fsw              = gamma_fsw,
             mu_C                   = mu_C,
             clients_reg_irreg      = clients_reg_irreg,
             c_C                    = c_C,
             phi_client             = phi_client),
        "model_parameters.rds")

## print parameters
estimates <- readRDS("model_parameters.rds")

for (nm in names(estimates)) {
  cat("\n", nm, "\n")
  if (is.data.frame(estimates[[nm]])) print(estimates[[nm]], n = Inf, width = Inf) else print(estimates[[nm]])
}

## save calibration targets
saveRDS(list(obs_shares      = obs_shares,
             fsw_hiv_prev    = fsw_hiv_prev,
             fsw_art_cov     = fsw_art_cov,
             fsw_art_cov_all = fsw_art_cov_all,
             client_hiv_prev = client_hiv_prev,
             client_hiv_idu  = client_hiv_idu,
             client_art_cov  = client_art_cov),
        "calibration_targets.rds")
        


## population size estimates

# make a table with 4 rows, one for each FSW group:
fsw_2009 <- tibble(street_sw_bin = c("No", "Yes", "No", "Yes"),
                   idu_12m_bin   = c("No", "No", "Yes", "Yes"),
                   N_F = 67425 * with(filter(omega_entry, year == 2009), ## number of FSWs in each group in 2009 multiplied by FSW size
                                      c(never_nonSB + former_nonSB, never_SB + former_SB, current_nonSB, current_SB))) %>%
  left_join(clients_reg_irreg, by = c("street_sw_bin", "idu_12m_bin")) ## add mean regular and mean irregular clients
fsw_2009

# client population across m = 3-6: partnerships counted by FSWs / FSWs per client per year
tibble(m = 3:6) %>%
  mutate(N_C     = sapply(m, function(x) sum(fsw_2009$N_F * (fsw_2009$R + x * fsw_2009$I)) / c_C),
         N_C_IDU = phi_client$phi_client * N_C,
         N_C_non = N_C - N_C_IDU)


## archive














# proportion of clients injecting, and HIV prevalence by injecting status, by year
client_hiv_idu <- clients_combined_clean %>%
  filter(!is.na(idu_12m_bin), !is.na(hiv_test_rslt_bin)) %>%
  group_by(year, idu_12m_bin) %>%
  summarise(n = n(), prev = mean(hiv_test_rslt_bin == "Positive"), .groups = "drop") ## non-IDU clients ~3-4% HIV+ both years; IDU clients 59% (17/29) in 2009, 18% (23/125) in 2014

client_hiv_idu







# overall, by typology and IDU
c_year_overall <- c_year_data %>%
  group_by(street_sw_bin, idu_12m_bin) %>%
  summarise(n = n(),
            mean = mean(clients_12m),
            se = sd(clients_12m) / sqrt(n)) %>%
  mutate(lci = mean - 1.96 * se,
         uci = mean + 1.96 * se)

c_year_overall

c_year_data %>%
  ungroup() %>%
  summarise(n = n(),
            mean = mean(clients_12m),
            se = sd(clients_12m) / sqrt(n)) %>%
  mutate(lci = mean - 1.96 * se,
         uci = mean + 1.96 * se)

c_year_data %>%
  summarise(reg   = mean(as.numeric(sw_partners_reg_clients_30d)),
            irreg = mean(as.numeric(sw_partners_irreg_clients_30d)) * 12)

# regular and irregular clients per month, by typology and IDU (2015, 2017, 2021)
clients_reg_irreg <- c_year_data %>%
  group_by(street_sw_bin, idu_12m_bin) %>%
  summarise(n = n(),
            R = mean(as.numeric(sw_partners_reg_clients_30d)),
            I = mean(as.numeric(sw_partners_irreg_clients_30d)),
            .groups = "drop")

clients_reg_irreg

