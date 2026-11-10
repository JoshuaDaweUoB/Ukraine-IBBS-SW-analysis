## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales, openxlsx, readr, magick)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Chapter Writing/Chapter 6 - Ukraine FSWs IDU violence and HIV/data/SW data")

# load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")
clients_combined_clean <- readRDS("clients_combined_clean.rds")

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

# proportion entering as IDU (phi)
fit_idu_1y    <- glm(I(idu_12m_bin == "Yes") ~ year, ## model idu by year
                     data = new1_strata_df, family = binomial)
summary(fit_idu_1y)

fit_idu_const <- glm(I(idu_12m_bin == "Yes") ~ 1, ## proportion of new FSWs who inject (intercept only)
                     data = new1_strata_df, family = binomial)

summary(fit_idu_const) ## 4.5%

anova(fit_idu_const, fit_idu_1y, test = "Chisq") ## no change in idu over time, keep idu constant at 4.5%

# constant phi
phi_coef <- coef(summary(fit_idu_const))[1, ] ## first row of fit_idu_const matrix
phi_coef

phi <- tibble(phi   = plogis(phi_coef[["Estimate"]]), # first column is proportion
              lower = plogis(phi_coef[["Estimate"]] - 1.96 * phi_coef[["Std. Error"]]), # second column is LB
              upper = plogis(phi_coef[["Estimate"]] + 1.96 * phi_coef[["Std. Error"]])) # third column is UB

phi

# proportion entering as former injector (phi_former)
new1_status_df <- new1_strata_df %>% filter(!is.na(idu_status_3cat))
table(new1_status_df$year, new1_status_df$idu_status_3cat)

fit_former_const <- glm(I(idu_status_3cat == "Former") ~ 1, ## proportion of new FSWs who formerly injected (intercept only)
                        data = new1_status_df, family = binomial)

summary(fit_former_const)

# constant phi_former
phi_former_coef <- coef(summary(fit_former_const))[1, ]

phi_former <- tibble(phi_former = plogis(phi_former_coef[["Estimate"]]),
                     lower      = plogis(phi_former_coef[["Estimate"]] - 1.96 * phi_former_coef[["Std. Error"]]),
                     upper      = plogis(phi_former_coef[["Estimate"]] + 1.96 * phi_former_coef[["Std. Error"]]))

phi_former

# proportion entering as SB
fit_sb_1y <- glm(I(street_sw_bin == "Yes") ~ year + idu_12m_bin, ## include idu so entry as SB differs by idu
                 data = new1_strata_df, family = binomial)

summary(fit_sb_1y) ## .84 reduction in odds of SB per year, 3-fold higher odds of SB among IDU vs. non-IDU 

# non-IDU SB in 2009 and 2021 
sb_ends <- tibble(year = c(2009, 2021), idu_12m_bin = "No") ## create sb_ends df
sb_ends

# predictions in 2009 and 2021 on logit scale
sb_pred <- predict(fit_sb_1y, newdata = sb_ends, type = "link", se.fit = TRUE) ## newdata inputs values to predict from sb_ends
as.data.frame(sb_pred[c("fit", "se.fit")]) ## row 1 is 2009 row 2 is 2021

# add predicted proportions to sb_ends
sb_ends <- sb_ends %>%
  mutate(s     = plogis(sb_pred$fit),
         lower = plogis(sb_pred$fit - 1.96 * sb_pred$se.fit),
         upper = plogis(sb_pred$fit + 1.96 * sb_pred$se.fit))

sb_ends # proportion street-based non-IDU for 2009 & 2021

# IDU vs non-IDU difference in SB on the log scale
beta_sb_idu <- coef(fit_sb_1y)[["idu_12m_binYes"]] ## coefficient for increase in odds of SB among IDU
beta_sb_idu ## log odds ratio 1.20

# former vs never injector difference in SB on the log scale (2008, 2017, 2021 only)
fit_sb_status <- glm(I(street_sw_bin == "Yes") ~ year + idu_status_3cat,
                     data = new1_status_df, family = binomial)

summary(fit_sb_status)

beta_sb_former <- coef(fit_sb_status)[["idu_status_3catFormer"]] ## log odds ratio, former vs never
beta_sb_former

# entry proportions at the point estimates to compare with observed
omega_entry <- tibble(year = c(2009, 2011, 2013, 2015, 2017, 2021)) %>% ## one row per survey year
  mutate(s_never   = plogis(qlogis(sb_ends$s[1]) + (qlogis(sb_ends$s[2]) - qlogis(sb_ends$s[1])) * (year - 2009) / 12), ## SB among those not currently injecting, straight line on logit scale
         s_current = plogis(qlogis(s_never) + beta_sb_idu),   ## add current IDU gap
         s_former  = s_never,                                 ## former assumed same as never (gap = 0)
         phi_never = 1 - phi$phi - phi_former$phi_former,     ## % never injected
         never_nonSB   = phi_never * (1 - s_never),
         never_SB      = phi_never * s_never,
         current_nonSB = phi$phi * (1 - s_current),
         current_SB    = phi$phi * s_current,
         former_nonSB  = phi_former$phi_former * (1 - s_former),
         former_SB     = phi_former$phi_former * s_former)

print(omega_entry, width = Inf)

## HIV prevalence at entry among FSWs (pi)

# female PWID HIV prevalence, standardised to the age distribution of new FSWs who currently inject

# load PWID data
pwid <- haven::read_dta("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Chapter Writing/Chapter 6 - Ukraine FSWs IDU violence and HIV/data/PWID data/PWID_combined_clean.dta") %>%
  mutate(across(c(sex, hiv_test_rslt), haven::as_factor),
         age  = as.numeric(age),
         wave = as.numeric(wave))

# check labels before filtering
table(pwid$sex, useNA = "ifany")
table(pwid$hiv_test_rslt, useNA = "ifany")

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

# pooled across waves: female PWID HIV prevalence standardised to the age distribution of new IDU FSWs
pi_idu <- pwid_age %>%
  group_by(agegrp) %>%
  summarise(pos = sum(pos), n = sum(n), .groups = "drop") %>%
  mutate(prev = pos / n) %>%
  left_join(fsw_age_weights, by = "agegrp") %>%
  summarise(pi = sum(weight * prev),
            se = sqrt(sum(weight^2 * prev * (1 - prev) / n))) %>%
  mutate(lower = pi - 1.96 * se,
         upper = pi + 1.96 * se) %>%
  select(pi, lower, upper)

pi_idu 

# ART coverage among FSWs living with HIV: new (0-2 years in SW) vs all (zeta)
art_ratio_df <- strata_df %>%
  filter(hiv_test_rslt_bin == "Positive", art_current_bin %in% c("Yes", "No")) %>%
  mutate(new_fsw = years_in_sw_3cat == "0-2")

# pooled ratio across survey years
art_ratio <- art_ratio_df %>%
  summarise(cov_all = mean(art_current_bin == "Yes"),
            cov_new = mean(art_current_bin[new_fsw %in% TRUE] == "Yes"),
            ratio   = cov_new / cov_all)

art_ratio

## injecting risk parameters

# proportion of clients injecting, and HIV prevalence by injecting status, by year
client_hiv_idu <- clients_combined_clean %>%
  filter(!is.na(idu_12m_bin), !is.na(hiv_test_rslt_bin)) %>%
  group_by(year, idu_12m_bin) %>%
  summarise(n = n(), prev = mean(hiv_test_rslt_bin == "Positive"), .groups = "drop") ## non-IDU clients ~3-4% HIV+ both years; IDU clients 59% (17/29) in 2009, 18% (23/125) in 2014

client_hiv_idu

# FSW idu by services to clients who inject, by year (12 months: 2009, 2011)
fsw_client_inject_12m <- sw_combined_clean %>%
  filter(!is.na(client_inject_12m_3cat)) %>%
  count(year, idu_12m_bin, client_inject_12m_3cat) %>%
  pivot_wider(names_from = client_inject_12m_3cat, values_from = n, values_fill = 0) ## ~13-15% of non-IDU vs ~63-69% of IDU FSWs had clients who inject

fsw_client_inject_12m

# FSW idu by services to clients who inject, by year (30 days: 2013-2021)
fsw_client_inject_30d <- sw_combined_clean %>%
  filter(!is.na(client_inject_30d_3cat)) %>%
  count(year, idu_12m_bin, client_inject_30d_3cat) %>%
  pivot_wider(names_from = client_inject_30d_3cat, values_from = n, values_fill = 0) ## ~8-24% of non-IDU vs ~40-74% of IDU FSWs had clients who inject; highest in 2013, IDU % falls over time

fsw_client_inject_30d

# idu by client of idu
client_paid_fsw_inject <- clients_combined_clean %>%
  filter(year == 2014) %>%
  count(idu_12m_bin, paid_fsw_inject_12m) %>%
  pivot_wider(names_from = paid_fsw_inject_12m, values_from = n, values_fill = 0) ## 2014: 50% of IDU clients had paid an FSW who injects

client_paid_fsw_inject

## sexual risk parameters

# mean number of clients in past 30 days, by typology and IDU

# keep FSWs with typology, IDU and client numbers
k_data <- sw_combined_clean %>%
  filter(street_sw_bin %in% c("Yes", "No"),
         idu_12m_bin %in% c("Yes", "No"),
         !is.na(sw_partners_clients_30d))

# by year
k_by_year <- k_data %>%
  group_by(street_sw_bin, idu_12m_bin, year) %>%
  summarise(n = n(),
            mean = mean(sw_partners_clients_30d),
            se = sd(sw_partners_clients_30d) / sqrt(n)) %>%
  mutate(lci = mean - 1.96 * se,
         uci = mean + 1.96 * se)

print(k_by_year, n = Inf)

# overall
k_overall <- k_data %>%
  group_by(street_sw_bin, idu_12m_bin) %>%
  summarise(n = n(),
            mean = mean(sw_partners_clients_30d),
            se = sd(sw_partners_clients_30d) / sqrt(n)) %>%
  mutate(lci = mean - 1.96 * se,
         uci = mean + 1.96 * se)

k_overall

# before 2017 vs 2017 onwards
k_period <- k_data %>%
  mutate(period = if_else(year < 2017, "2009-2015", "2017-2021")) %>%
  group_by(street_sw_bin, idu_12m_bin, period) %>%
  summarise(n = n(),
            mean = mean(sw_partners_clients_30d),
            se = sd(sw_partners_clients_30d) / sqrt(n)) %>%
  mutate(lci = mean - 1.96 * se,
         uci = mean + 1.96 * se)

k_period

# sex acts per client

# keep FSWs with typology, IDU and acts per client
acts_data <- sw_combined_clean %>%
  filter(street_sw_bin %in% c("Yes", "No"),
         idu_12m_bin %in% c("Yes", "No"),
         !is.na(sw_acts_per_client))

# overall, by typology and IDU
acts_overall <- acts_data %>%
  group_by(street_sw_bin, idu_12m_bin) %>%
  summarise(n = n(),
            mean = mean(sw_acts_per_client),
            se = sd(sw_acts_per_client) / sqrt(n)) %>%
  mutate(lci = mean - 1.96 * se,
         uci = mean + 1.96 * se)

acts_overall

# condom use at last sex with a client

# keep FSWs with typology, IDU and condom use
condom_data <- sw_combined_clean %>%
  filter(street_sw_bin %in% c("Yes", "No"),
         idu_12m_bin %in% c("Yes", "No"),
         client_condom_lastsex_bin %in% c("Yes", "No"))

# by year, by typology and IDU
condom_by_year <- condom_data %>%
  group_by(street_sw_bin, idu_12m_bin, year) %>%
  summarise(n = n(),
            prop = mean(client_condom_lastsex_bin == "Yes"),
            se = sqrt(prop * (1 - prop) / n)) %>%
  mutate(lci = prop - 1.96 * se,
         uci = prop + 1.96 * se)

print(condom_by_year, n = Inf)

# overall, by typology and IDU
condom_overall <- condom_data %>%
  group_by(street_sw_bin, idu_12m_bin) %>%
  summarise(n = n(),
            prop = mean(client_condom_lastsex_bin == "Yes"),
            se = sqrt(prop * (1 - prop) / n)) %>%
  mutate(lci = prop - 1.96 * se,
         uci = prop + 1.96 * se)

condom_overall

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

## client parameters

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
pi_client_idu <- pwid_male_age %>%
  left_join(client_age_weights, by = "agegrp") %>%
  mutate(weight = coalesce(weight, 0)) %>%
  summarise(pi = sum(weight * prev),
            se = sqrt(sum(weight^2 * prev * (1 - prev) / n))) %>%
  mutate(lower = pi - 1.96 * se,
         upper = pi + 1.96 * se) %>%
  select(pi, lower, upper)

pi_client_idu

# proportion of clients who inject (phi_C for clients)
table(clients_combined_clean$year, clients_combined_clean$idu_12m_bin, useNA = "ifany")

fit_client_idu_const <- glm(I(idu_12m_bin == "Yes") ~ 1,
                            data = clients_combined_clean %>% filter(!is.na(idu_12m_bin)),
                            family = binomial)

summary(fit_client_idu_const)

# constant proportion of clients who inject
phi_client_coef <- coef(summary(fit_client_idu_const))[1, ]

phi_client <- tibble(phi_client = plogis(phi_client_coef[["Estimate"]]),
                     lower      = plogis(phi_client_coef[["Estimate"]] - 1.96 * phi_client_coef[["Std. Error"]]),
                     upper      = plogis(phi_client_coef[["Estimate"]] + 1.96 * phi_client_coef[["Std. Error"]]))

phi_client

# mean client age
mean(clients_combined_clean$age_numeric, na.rm = TRUE)                                                  # mean client age
tapply(clients_combined_clean$age_numeric, clients_combined_clean$year, mean, na.rm = TRUE)             # by survey year

# years buying sex among clients (for exit)
T_C <- mean(clients_combined_clean$years_buying, na.rm = TRUE)   # mean years buying sex, client IBBS 2009 and 2014
T_C

# 95% CI of mean years buying sex
T_C_se    <- sd(clients_combined_clean$years_buying, na.rm = TRUE) / sqrt(sum(!is.na(clients_combined_clean$years_buying)))
T_C_lower <- T_C - 1.96 * T_C_se
T_C_upper <- T_C + 1.96 * T_C_se

# fsws seen by clients
client_fsw_partners <- clients_combined_clean %>%
  group_by(year) %>%
  summarise(n = sum(!is.na(fsw_partners_12m_numeric)),
            missing = sum(is.na(fsw_partners_12m_numeric)),
            mean = mean(fsw_partners_12m_numeric, na.rm = TRUE),
            median = median(fsw_partners_12m_numeric, na.rm = TRUE),
            min = min(fsw_partners_12m_numeric, na.rm = TRUE),
            max = max(fsw_partners_12m_numeric, na.rm = TRUE))

client_fsw_partners

# paid sex contacts in past 12 months among clients
client_paid_contacts <- clients_combined_clean %>%
  group_by(year) %>%
  summarise(n = sum(!is.na(paid_sex_contacts_12m)),
            mean = mean(paid_sex_contacts_12m, na.rm = TRUE),
            median = median(paid_sex_contacts_12m, na.rm = TRUE))

client_paid_contacts

### calibration targets

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
             pi_idu_years           = pi_idu_years,
             zeta                   = zeta,
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
             T_C_lower              = T_C_lower,
             T_C_upper              = T_C_upper,
             client_fsw_partners    = client_fsw_partners,
             client_paid_contacts   = client_paid_contacts,
             age_first_sw_avg       = age_first_sw_avg),
        "model_parameters.rds")

## save calibration targets
saveRDS(list(obs_shares      = obs_shares,
             fsw_hiv_prev    = fsw_hiv_prev,
             fsw_art_cov     = fsw_art_cov,
             fsw_art_cov_all = fsw_art_cov_all,
             client_hiv_prev = client_hiv_prev,
             client_hiv_idu  = client_hiv_idu,
             client_art_cov  = client_art_cov),
        "calibration_targets.rds")
