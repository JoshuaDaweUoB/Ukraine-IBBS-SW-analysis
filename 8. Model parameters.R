# model inputs: estimates from 7. Model parameters estimation.R, literature values, assumptions and priors
# sourced by 9. Entry and exit into model.r; contains no random draws until draw_params() is called

# load packages
pacman::p_load(dplyr, tidyr)

# set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Chapter Writing/Chapter 6 - Ukraine FSWs IDU violence and HIV/data/SW data")

# load parameters and calibration targets from script 7
estimates <- readRDS("model_parameters.rds")
targets   <- readRDS("calibration_targets.rds")
names(estimates)
names(targets)

## housekeeping

# define strata (order: nonIDU_nonSB, nonIDU_SB, IDU_nonSB, IDU_SB)
strata <- estimates$strata
inj    <- c(0, 0, 1, 1)   # 1 = injected in past 12 months
street <- c(0, 1, 0, 1)   # 1 = street-based

# define survey years
years <- c(2009, 2011, 2013, 2015, 2017, 2021)

# model period
start_year <- 1987
end_year   <- 2036

## starting conditions

# fsw population in 1987
fsw_pop_1987 <- 80000

# client population in 1987
client_pop_1987 <- 284000

## estimates from script 7

phi           <- estimates$phi
sb_ends       <- estimates$sb_ends
beta_sb_idu   <- estimates$beta_sb_idu
obs_shares    <- targets$obs_shares
years_prior   <- estimates$years_prior
T_C           <- estimates$T_C
T_C_lower     <- estimates$T_C_lower
T_C_upper     <- estimates$T_C_upper

# entry proportions by year (2009-2021): constant phi, non-IDU SB linear on the logit scale between 2009 and 2021, IDU SB shifted by beta_sb_idu
omega_entry <- function(phi, s_ends) {
  tibble(year = 2009:2021) %>%
    mutate(s_nonIDU     = plogis(qlogis(s_ends[1]) + (qlogis(s_ends[2]) - qlogis(s_ends[1])) * (year - 2009) / 12),
           s_IDU        = plogis(qlogis(s_nonIDU) + beta_sb_idu),
           nonIDU_nonSB = (1 - phi) * (1 - s_nonIDU),
           nonIDU_SB    = (1 - phi) * s_nonIDU,
           IDU_nonSB    = phi * (1 - s_IDU),
           IDU_SB       = phi * s_IDU) %>%
    select(year, all_of(strata))
}

# entry proportions at the point estimates
omega_years <- omega_entry(phi$phi, sb_ends$s)

# hiv prevalence at entry among fsws: one row per survey year, one column per stratum
pi_years <- estimates$pi_years_1y %>%
  mutate(stratum = paste(if_else(idu_12m_bin == "Yes", "IDU", "nonIDU"),
                         if_else(street_sw_bin == "Yes", "SB", "nonSB"), sep = "_")) %>%
  select(year, stratum, pi) %>%
  pivot_wider(names_from = stratum, values_from = pi) %>%
  arrange(year) %>%
  select(year, all_of(strata))

# proportion of hiv positive fsws who enter on ART
zeta <- estimates$zeta$zeta

## fixed parameters

# fsw non-HIV mortality (mu): 1 / remaining life expectancy at mean age of starting SW, plus excess for injecting
age_first_sw <- estimates$age_first_sw_avg
e_x          <- 53.2                          # remaining life expectancy, Ukrainian women at age round(age_first_sw) (ref)
mort_le_f    <- 1 / e_x                       # non-HIV mortality, all FSWs

cmr_pwid_ci  <- c(0.91, 0.68, 1.14)           # non-AIDS CMR per 100 PY, female PWID (rate, lower, upper) (Mathers & Degenhardt 2014, Table 3)
cmr_pwid     <- cmr_pwid_ci[1] / 100
excess_inj   <- cmr_pwid                      # excess non-AIDS mortality from injecting (PWID CMR; background deaths at young ages negligible)
RR_street    <- 1                             # mortality rate ratio, street-based (no data)

mu <- (mort_le_f + excess_inj * inj) * RR_street^street
mu

# cessation of sex work (gamma) = total exit (1 / mean years in SW) minus non-HIV mortality
exit_total <- 1 / years_prior$mean
gamma <- exit_total - mu
if (any(gamma <= 0)) warning("background mortality >= total exit for some strata")
gamma

# 95% CI for cessation, from the 95% CI of mean years in SW
# (longer time in SW = lower cessation, so the upper years limit gives the lower gamma limit)
years_lower <- years_prior$mean - 1.96 * years_prior$sd / sqrt(years_prior$n)
years_upper <- years_prior$mean + 1.96 * years_prior$sd / sqrt(years_prior$n)

gamma_ci <- tibble(
  stratum = strata,
  gamma   = gamma,
  lower   = 1 / years_upper - mu,
  upper   = 1 / years_lower - mu
)

gamma_ci

# client non-HIV mortality (mu_C) and cessation of buying sex (gamma_C)
mort_rate_m <- 388.8 / 100000   # Ukraine age-specific mortality, men 30-34, mean 2009-2021 (ref)
mu_C <- mort_rate_m

gamma_C <- 1 / T_C - mu_C
gamma_C

# 95% CI for cessation, from the 95% CI of mean years buying sex
# (longer time buying = lower cessation, so the upper years limit gives the lower gamma_C limit)
c(gamma_C = gamma_C,
  lower   = 1 / T_C_upper - mu_C,
  upper   = 1 / T_C_lower - mu_C)

# linked-cohort rates per 100 PY used to set the multipliers and calibration bounds (5. Longitudinal analyses.r)
rho_overall <- 17.0                  # street -> indoor, 2013-2021
rho_2013_15 <- 11.8                  # street -> indoor, 2013-15
rho_2015_17 <- c(29.9, 22.4, 39.3)   # street -> indoor, 2015-17 (rate, lower, upper)
eps_overall <- c(23.6, 16.3, 33.2)   # IDU -> non-IDU, 2013-2021 (rate, lower, upper)

## priors

# movement between strata: linked-cohort rates per 100 PY, with 95% CIs (5. Longitudinal analyses.r)
prior_tau <- tibble(est = c(3.0, 5.2),   lower = c(1.8, 3.4),   upper = c(4.7, 7.7))    # non-IDU -> IDU (indoor, street)
prior_eps <- tibble(est = c(20.7, 25.8), lower = c(9.9, 16.3),  upper = c(38.0, 38.7))  # IDU -> non-IDU (indoor, street)
prior_psi <- tibble(est = c(6.5, 15.0),  lower = c(4.6, 6.8),   upper = c(9.0, 28.4))   # indoor -> street (nonIDU, IDU)
prior_rho <- tibble(est = c(16.7, 18.6), lower = c(13.1, 10.4), upper = c(21.1, 30.6))  # street -> indoor (nonIDU, IDU)

# sdlog for a log-normal prior fitted to the CI
prior_tau$sdlog <- (log(prior_tau$upper) - log(prior_tau$lower)) / (2 * 1.96)
prior_eps$sdlog <- (log(prior_eps$upper) - log(prior_eps$lower)) / (2 * 1.96)
prior_psi$sdlog <- (log(prior_psi$upper) - log(prior_psi$lower)) / (2 * 1.96)
prior_rho$sdlog <- (log(prior_rho$upper) - log(prior_rho$lower)) / (2 * 1.96)

# entry proportions: sd on the logit scale for a logit-normal prior fitted to the CI
phi_sdlogit <- (qlogis(phi$upper) - qlogis(phi$lower)) / (2 * 1.96)
sb_sdlogit  <- (qlogis(sb_ends$upper) - qlogis(sb_ends$lower)) / (2 * 1.96)   # 2009, 2021

## draw_params(): one random set of sampled values from the priors

# keep the order of the draws fixed: results for a given seed depend on it
draw_params <- function() {
  draw <- list()

  # calibrated multipliers
  draw$tau_1987 <- runif(1, 0.1, 0.5)                                                        # assumption
  draw$tau_peak <- runif(1, 0.5, 10)                                                         # no data
  draw$rho_pre  <- runif(1, 0.05, 1.5)                                                       # no data
  draw$rho_post <- runif(1, rho_2015_17[2] / rho_overall, rho_2015_17[3] / rho_overall)      # 2015-17 CI
  draw$eps_post <- runif(1, eps_overall[2] / eps_overall[1], eps_overall[3] / eps_overall[1]) # 2013-21 CI

  # non-AIDS CMR among female PWID: normal, per year
  draw$cmr_pwid <- max(rnorm(1, cmr_pwid_ci[1], (cmr_pwid_ci[3] - cmr_pwid_ci[2]) / (2 * 1.96)), 0.01) / 100

  # mean years in SW: normal, using the standard error of the mean
  draw$years_in_sw <- rnorm(4, years_prior$mean, years_prior$sd / sqrt(years_prior$n))

  # entry proportions: logit-normal, proportion entering as IDU and non-IDU SB in 2009 and 2021
  draw$phi    <- plogis(rnorm(1, qlogis(phi$phi), phi_sdlogit))
  draw$s_ends <- plogis(rnorm(2, qlogis(sb_ends$s), sb_sdlogit))

  # movement between strata: log-normal, per year
  draw$tau <- rlnorm(2, log(prior_tau$est), prior_tau$sdlog) / 100
  draw$eps <- rlnorm(2, log(prior_eps$est), prior_eps$sdlog) / 100
  draw$psi <- rlnorm(2, log(prior_psi$est), prior_psi$sdlog) / 100
  draw$rho <- rlnorm(2, log(prior_rho$est), prior_rho$sdlog) / 100

  draw
}

## build_params(): turn sampled values into the parameter set for the model (no random draws)

# returns NULL if cessation would be negative
build_params <- function(draw) {

  # non-HIV mortality: 1 / life expectancy, plus sampled excess for injecting
  mu_s <- (mort_le_f + draw$cmr_pwid * inj) * RR_street^street

  # cessation: 1 / mean years in SW minus mortality
  gamma_s <- 1 / draw$years_in_sw - mu_s
  if (any(gamma_s <= 0)) return(NULL)

  # entry proportions by year
  omega_draw <- omega_entry(draw$phi, draw$s_ends)

  list(
    omega_years = omega_draw,
    mu    = mu_s,
    gamma = gamma_s,
    tau   = draw$tau,
    eps   = draw$eps,
    psi   = draw$psi,
    rho   = draw$rho,
    # tau: low in 1987, peak 1997-2002, back to linked-cohort rate (1) by 2013
    tau_knots = tibble(year = c(1987, 1997, 2002, 2013), mult = c(draw$tau_1987, draw$tau_peak, draw$tau_peak, 1)),
    # rho: constant to 2009, 2013-15 rate in 2014, rho_post from 2016
    rho_knots = tibble(year = c(1987, 2009, 2014, 2016), mult = c(draw$rho_pre, draw$rho_pre, rho_2013_15 / rho_overall, draw$rho_post)),
    # eps: linked-cohort rate (1) to 2013, eps_post from 2016
    eps_knots = tibble(year = c(1987, 2013, 2016), mult = c(1, 1, draw$eps_post))
  )
}

## print parameters

# estimates from script 7
print(omega_years)
print(pi_years)
print(zeta)
print(years_prior)
print(T_C)

# fixed parameters
print(mu)
print(gamma_ci)
print(c(mu_C = mu_C, gamma_C = gamma_C))

# priors
print(prior_tau)
print(prior_eps)
print(prior_psi)
print(prior_rho)
print(c(phi_sdlogit = phi_sdlogit, sb_sdlogit = sb_sdlogit))

# one example simulation: the sampled values, then the parameter set built from them
example_draw <- draw_params()
print(example_draw)
print(build_params(example_draw))
