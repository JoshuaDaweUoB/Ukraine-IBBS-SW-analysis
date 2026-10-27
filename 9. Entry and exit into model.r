# population model of FSWs by injecting drug use in past 12 months (IDU) and street-based sex work (SB),
# with entry, exit and movement between strata, calibrated to IBBS stratum shares 2009-2021
# strata order: nonIDU_nonSB, nonIDU_SB, IDU_nonSB, IDU_SB

## 0. Setup

pacman::p_load(deSolve, dplyr, tidyr, ggplot2, scales)

setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Chapter Writing/Chapter 6 - Ukraine FSWs IDU violence and HIV/data/SW data")

strata <- c("nonIDU_nonSB", "nonIDU_SB", "IDU_nonSB", "IDU_SB")
inj    <- c(0, 0, 1, 1)   # 1 = injected in past 12 months
street <- c(0, 1, 0, 1)   # 1 = street-based

start_year <- 1987
end_year   <- 2036


## 1. Data and model strata

sw_combined_clean <- readRDS("sw_combined_clean.rds")
sw_combined_clean$age_numeric <- suppressWarnings(as.numeric(sw_combined_clean$age))

clients <- readRDS("clients_combined_clean.rds")

# assign each FSW to a model stratum (drop those with missing IDU or typology)
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


## 2. Parameters estimated from the FSW IBBS

## 2.1 Proportion in each stratum (omega)

# proportion in each stratum by survey year, for three groups of FSWs:
#   "0-1" new FSWs (<= 1 year in SW): entry proportions used in the model
#   "0-3" FSWs in SW 0-3 years: comparison only
#   "All" all FSWs: initial conditions and calibration targets
omega_by_year <- bind_rows(
  strata_df %>% filter(years_in_sw <= 1)             %>% mutate(group = "0-1"),
  strata_df %>% filter(years_in_sw_3cat == "0-3")    %>% mutate(group = "0-3"),
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

# entry proportions used in the model: one row per survey year, one column per stratum
omega_years <- omega_by_year %>%
  filter(group == "0-1") %>%
  select(year, stratum, omega) %>%
  pivot_wider(names_from = stratum, values_from = omega) %>%
  arrange(year) %>%
  select(year, all_of(strata))

omega_years

# observed shares among all FSWs: calibration targets and points on the plot
obs_shares <- omega_by_year %>%
  filter(group == "All") %>%
  select(year, stratum, N, observed = omega, lower, upper)


## 2.2 HIV prevalence at entry (pi)

# new FSWs (<= 1 year in SW) with an HIV test result
new1_df <- strata_df %>%
  filter(years_in_sw <= 1, hiv_test_rslt_bin %in% c("Positive", "Negative"))

# raw prevalence by survey year and stratum (small numbers in IDU strata)
hiv_entry_year_1y <- new1_df %>%
  group_by(year, stratum) %>%
  summarise(
    n     = n(),
    n_hiv = sum(hiv_test_rslt_bin == "Positive"),
    pi    = n_hiv / n,
    .groups = "drop"
  )

print(hiv_entry_year_1y, n = Inf)

# smoothed prevalence: logistic regression on survey year, IDU and SB
fit_pi_1y <- glm(I(hiv_test_rslt_bin == "Positive") ~ year + idu_12m_bin + street_sw_bin,
                 data = new1_df, family = binomial)

summary(fit_pi_1y)

# predicted prevalence with 95% CI, by survey year and stratum (used in the model)
pi_years_1y <- expand_grid(year = c(2009, 2011, 2013, 2015, 2017, 2021),
                           idu_12m_bin = c("No", "Yes"), street_sw_bin = c("No", "Yes"))

pi_pred <- predict(fit_pi_1y, newdata = pi_years_1y, type = "link", se.fit = TRUE)

pi_years_1y <- pi_years_1y %>%
  mutate(pi    = plogis(pi_pred$fit),
         lower = plogis(pi_pred$fit - 1.96 * pi_pred$se.fit),
         upper = plogis(pi_pred$fit + 1.96 * pi_pred$se.fit))

print(pi_years_1y, n = Inf)

# sensitivity: same regression among FSWs in SW 0-3 years
fit_pi_3y <- glm(I(hiv_test_rslt_bin == "Positive") ~ year + idu_12m_bin + street_sw_bin,
                 data = strata_df %>% filter(years_in_sw_3cat == "0-3",
                                             hiv_test_rslt_bin %in% c("Positive", "Negative")),
                 family = binomial)

pi_years_3y <- expand_grid(year = c(2009, 2011, 2013, 2015, 2017, 2021),
                           idu_12m_bin = c("No", "Yes"), street_sw_bin = c("No", "Yes"))
pi_years_3y$pi <- predict(fit_pi_3y, newdata = pi_years_3y, type = "response")

print(pi_years_3y, n = Inf)


## 2.3 ART at entry among new FSWs living with HIV (zeta)

# FSWs in SW 0-3 years who tested HIV-positive; ART not recorded in 2009 and 2011
art_entry_df <- strata_df %>%
  filter(years_in_sw_3cat == "0-3",
         hiv_test_rslt_bin %in% c("Positive", "1", "Yes"),
         !is.na(art_current_bin))

# by stratum (small numbers, for checking)
art_entry <- art_entry_df %>%
  group_by(stratum) %>%
  summarise(
    n     = n(),
    n_art = sum(art_current_bin == "Yes"),
    p_art = n_art / n,
    lower = binom.test(n_art, n)$conf.int[1],
    upper = binom.test(n_art, n)$conf.int[2],
    .groups = "drop"
  )

art_entry

# pooled across strata (used in the model)
zeta <- art_entry_df %>%
  summarise(
    n     = n(),
    n_art = sum(art_current_bin == "Yes"),
    zeta  = n_art / n,
    lower = binom.test(n_art, n)$conf.int[1],
    upper = binom.test(n_art, n)$conf.int[2]
  )

zeta


## 2.4 Years in sex work by stratum (for exit)

# not measurable in the linked cohort, so taken from the cross-sectional surveys
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


## 3. Fixed parameters

## 3.1 FSW non-HIV mortality (mu) and cessation of sex work (gamma)

mean(sw_combined_clean$age_numeric, na.rm = TRUE)   # mean FSW age, used to choose the age group below

mort_rate_f <- 75.3 / 100000            # Ukraine age-specific mortality, women 25-29, mean 2009-2021 (ref)
cmr_pwid    <- 0.91 / 100               # non-AIDS CMR, female PWID (Mathers & Degenhardt 2014)
RR_inj      <- cmr_pwid / mort_rate_f   # mortality rate ratio, injecting past 12 months
RR_street   <- 1                        # mortality rate ratio, street-based (no data)

mu <- mort_rate_f * RR_inj^inj * RR_street^street
mu

# cessation = total exit (1 / mean years in SW) minus non-HIV mortality
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

## 3.2 Client non-HIV mortality (mu_C) and cessation of buying sex (gamma_C)

mean(clients$age_numeric, na.rm = TRUE)                                  # mean client age
tapply(clients$age_numeric, clients$year, mean, na.rm = TRUE)            # by survey year

mort_rate_m <- 388.8 / 100000   # Ukraine age-specific mortality, men 30-34, mean 2009-2021 (ref)
mu_C <- mort_rate_m

T_C <- mean(clients$years_buying, na.rm = TRUE)   # mean years buying sex, client IBBS 2009 and 2014
T_C

gamma_C <- 1 / T_C - mu_C
gamma_C

# 95% CI for cessation, from the 95% CI of mean years buying sex
# (longer time buying = lower cessation, so the upper years limit gives the lower gamma_C limit)
T_C_se    <- sd(clients$years_buying, na.rm = TRUE) / sqrt(sum(!is.na(clients$years_buying)))
T_C_lower <- T_C - 1.96 * T_C_se
T_C_upper <- T_C + 1.96 * T_C_se

c(gamma_C = gamma_C,
  lower   = 1 / T_C_upper - mu_C,
  upper   = 1 / T_C_lower - mu_C)


## 3.3 Movement between strata (linked cohort 2013-2021)

# rates per year, from 2. Long format data.r (incidence_all_sheets.xlsx)
# point estimates for reference; the calibration samples them from their 95% CIs (section 5.1)
tau <- c(3.0, 5.2) / 100     # non-IDU -> IDU (indoor, street)
eps <- c(20.7, 25.8) / 100   # IDU -> non-IDU (indoor, street)
psi <- c(6.5, 15.0) / 100    # indoor -> street (nonIDU, IDU)
rho <- c(16.7, 18.6) / 100   # street -> indoor (nonIDU, IDU)

# linked-cohort rates per 100 PY used to set the multipliers and calibration bounds
rho_overall <- 17.0                  # street -> indoor, 2013-2021
rho_2013_15 <- 11.8                  # street -> indoor, 2013-15
rho_2015_17 <- c(29.9, 22.4, 39.3)   # street -> indoor, 2015-17 (rate, lower, upper)
eps_overall <- c(23.6, 16.3, 33.2)   # IDU -> non-IDU, 2013-2021 (rate, lower, upper)


## 4. Population model

## 4.1 Population model (ODEs)

# deSolve needs the model written as a function of time (t), population (N) and parameters (p)
fsw_pop <- function(t, N, p) {
  yr <- t + start_year

  # entry proportions this year: linear between survey years, constant before 2009 and after 2021
  oy <- p$omega_years
  omega <- c(approx(oy$year, oy$nonIDU_nonSB, xout = yr, rule = 2)$y,
             approx(oy$year, oy$nonIDU_SB,    xout = yr, rule = 2)$y,
             approx(oy$year, oy$IDU_nonSB,    xout = yr, rule = 2)$y,
             approx(oy$year, oy$IDU_SB,       xout = yr, rule = 2)$y)

  # time-varying multipliers: linear between knot years, constant before the first and after the last
  tau_mult <- approx(p$tau_knots$year, p$tau_knots$mult, xout = yr, rule = 2)$y
  rho_mult <- approx(p$rho_knots$year, p$rho_knots$mult, xout = yr, rule = 2)$y
  eps_mult <- approx(p$eps_knots$year, p$eps_knots$mult, xout = yr, rule = 2)$y

  out_rate <- p$mu + p$gamma               # non-HIV death + cessation
  Lambda   <- sum(out_rate * N)            # entry replaces all exits, so N_f stays constant

  start_inj <- p$tau * tau_mult * N[c(1, 2)]   # nonIDU -> IDU (indoor, street)
  stop_inj  <- p$eps * eps_mult * N[c(3, 4)]   # IDU -> nonIDU (indoor, street)
  to_street <- p$psi * N[c(1, 3)]              # indoor -> street (nonIDU, IDU)
  to_indoor <- p$rho * rho_mult * N[c(2, 4)]   # street -> indoor (nonIDU, IDU)

  dN <- omega * Lambda - out_rate * N
  dN[1] <- dN[1] - start_inj[1] + stop_inj[1] - to_street[1] + to_indoor[1]
  dN[2] <- dN[2] - start_inj[2] + stop_inj[2] + to_street[1] - to_indoor[1]
  dN[3] <- dN[3] + start_inj[1] - stop_inj[1] - to_street[2] + to_indoor[2]
  dN[4] <- dN[4] + start_inj[2] - stop_inj[2] + to_street[2] - to_indoor[2]

  list(dN, N_f = sum(N), Lambda = Lambda)
}

## 4.2 Initial population

# 1987: 2009 shares among all FSWs, with injecting set to 5% of the 2009 level (assumption)
N_f <- 1   # population normalised to 1
init <- obs_shares %>%
  filter(year == 2009) %>%
  slice(match(strata, stratum)) %>%
  pull(observed)
init <- c(init[1] + init[3] * 0.95, init[2] + init[4] * 0.95, init[3] * 0.05, init[4] * 0.05)
init <- setNames(N_f * init / sum(init), strata)


## 5. Bayesian calibration (approximate Bayesian computation, rejection sampling)

# each simulation draws every uncertain input from its prior, runs the model to 2021 and
# measures the distance to the observed shares among all FSWs (2009-2021);
# the closest accept_frac of simulations form the posterior
set.seed(2026)
n_sims      <- 10000   # run a smaller number first (e.g. 500) to check run time
accept_frac <- 0.01    # proportion of simulations accepted

## 5.1 Priors

# linked-cohort rates per 100 PY, with 95% CIs; sdlog for a log-normal prior fitted to the CI
prior_tau <- tibble(est = c(3.0, 5.2),   lower = c(1.8, 3.4),   upper = c(4.7, 7.7))    # indoor, street
prior_eps <- tibble(est = c(20.7, 25.8), lower = c(9.9, 16.3),  upper = c(38.0, 38.7))  # indoor, street
prior_psi <- tibble(est = c(6.5, 15.0),  lower = c(4.6, 6.8),   upper = c(9.0, 28.4))   # nonIDU, IDU
prior_rho <- tibble(est = c(16.7, 18.6), lower = c(13.1, 10.4), upper = c(21.1, 30.6))  # nonIDU, IDU

prior_tau$sdlog <- (log(prior_tau$upper) - log(prior_tau$lower)) / (2 * 1.96)
prior_eps$sdlog <- (log(prior_eps$upper) - log(prior_eps$lower)) / (2 * 1.96)
prior_psi$sdlog <- (log(prior_psi$upper) - log(prior_psi$lower)) / (2 * 1.96)
prior_rho$sdlog <- (log(prior_rho$upper) - log(prior_rho$lower)) / (2 * 1.96)

# entry proportions: counts of new FSWs in each stratum by survey year (for a Dirichlet prior)
omega_counts <- omega_by_year %>%
  filter(group == "0-1") %>%
  select(year, stratum, n) %>%
  pivot_wider(names_from = stratum, values_from = n, values_fill = 0) %>%
  arrange(year) %>%
  select(year, all_of(strata))

# distance weights: binomial standard error of each observed share
obs_se <- obs_shares %>%
  mutate(se = sqrt(observed * (1 - observed) / N))

## 5.2 Sample from the priors and run the model

sims_p       <- list()   # parameter set for each simulation
sims_summary <- list()   # sampled values and distance for each simulation

for (k in 1:n_sims) {

  # calibrated multipliers
  tau_1987 <- runif(1, 0.1, 0.5)                                                        # assumption
  tau_peak <- runif(1, 0.5, 10)                                                         # no data
  rho_pre  <- runif(1, 0.05, 1.5)                                                       # no data
  rho_post <- runif(1, rho_2015_17[2] / rho_overall, rho_2015_17[3] / rho_overall)      # 2015-17 CI
  eps_post <- runif(1, eps_overall[2] / eps_overall[1], eps_overall[3] / eps_overall[1]) # 2013-21 CI

  # non-HIV mortality: non-AIDS CMR among female PWID, normal 0.91 (0.68-1.14) per 100 PY
  cmr_draw <- max(rnorm(1, 0.91, (1.14 - 0.68) / (2 * 1.96)), 0.01) / 100
  mu_s     <- ifelse(inj == 1, cmr_draw, mort_rate_f) * RR_street^street

  # cessation: 1 / mean years in SW (normal, using the standard error of the mean) minus mortality
  years_draw <- rnorm(4, years_prior$mean, years_prior$sd / sqrt(years_prior$n))
  gamma_s    <- 1 / years_draw - mu_s
  if (any(gamma_s <= 0)) next   # skip if cessation would be negative

  # entry proportions: Dirichlet per survey year
  g <- matrix(rgamma(nrow(omega_counts) * 4, shape = as.matrix(omega_counts[, strata]) + 0.5),
              nrow = nrow(omega_counts))
  omega_draw <- omega_counts
  omega_draw[, strata] <- g / rowSums(g)

  # parameter set for this simulation
  p <- list(
    omega_years = omega_draw,
    mu    = mu_s,
    gamma = gamma_s,
    tau   = rlnorm(2, log(prior_tau$est), prior_tau$sdlog) / 100,
    eps   = rlnorm(2, log(prior_eps$est), prior_eps$sdlog) / 100,
    psi   = rlnorm(2, log(prior_psi$est), prior_psi$sdlog) / 100,
    rho   = rlnorm(2, log(prior_rho$est), prior_rho$sdlog) / 100,
    # tau: low in 1987, peak 2005-2008, back to linked-cohort rate (1) by 2013
    tau_knots = tibble(year = c(1987, 2005, 2008, 2013), mult = c(tau_1987, tau_peak, tau_peak, 1)),
    # rho: constant to 2009, 2013-15 rate in 2014, rho_post from 2016
    rho_knots = tibble(year = c(1987, 2009, 2014, 2016), mult = c(rho_pre, rho_pre, rho_2013_15 / rho_overall, rho_post)),
    # eps: linked-cohort rate (1) to 2013, eps_post from 2016
    eps_knots = tibble(year = c(1987, 2013, 2016), mult = c(1, 1, eps_post))
  )

  # run the model to 2021 (skip if it fails)
  out <- try(ode(y = init, times = seq(0, 2021 - start_year, by = 1), func = fsw_pop, parms = p), silent = TRUE)
  if (inherits(out, "try-error")) next

  model <- as.data.frame(out) %>%
    mutate(year = time + start_year) %>%
    select(year, all_of(strata)) %>%
    pivot_longer(-year, names_to = "stratum", values_to = "model")

  # distance: squared differences from the observed shares, weighted by their standard errors
  d <- inner_join(model, obs_se, by = c("year", "stratum"))
  dist <- sum(((d$model - d$observed) / d$se)^2)

  sims_p[[k]] <- p
  sims_summary[[k]] <- tibble(
    sim = k, dist = dist,
    tau_1987 = tau_1987, tau_peak = tau_peak, rho_pre = rho_pre, rho_post = rho_post, eps_post = eps_post,
    tau_indoor = p$tau[1], tau_street = p$tau[2],
    eps_indoor = p$eps[1], eps_street = p$eps[2],
    psi_nonIDU = p$psi[1], psi_IDU    = p$psi[2],
    rho_nonIDU = p$rho[1], rho_IDU    = p$rho[2],
    gamma_nonIDU_nonSB = gamma_s[1], gamma_nonIDU_SB = gamma_s[2],
    gamma_IDU_nonSB    = gamma_s[3], gamma_IDU_SB    = gamma_s[4]
  )

  if (k %% 500 == 0) message(k, " of ", n_sims, " simulations")
}

sims_summary <- bind_rows(sims_summary)

## 5.3 Accept the closest simulations

posterior <- sims_summary %>%
  filter(dist <= quantile(dist, accept_frac))

nrow(posterior)   # number of accepted parameter sets

## 5.4 Posterior summaries (calibrated multipliers, linked-cohort rates and cessation, per year)

posterior %>%
  select(-sim, -dist) %>%
  pivot_longer(everything(), names_to = "parameter") %>%
  group_by(parameter) %>%
  summarise(median = median(value),
            lower  = quantile(value, 0.025),
            upper  = quantile(value, 0.975))


## 6. Posterior projections and comparison with data

# run each accepted parameter set to the end of the model period
proj <- list()

for (j in posterior$sim) {
  proj[[j]] <- ode(y = init, times = seq(0, end_year - start_year, by = 1), func = fsw_pop, parms = sims_p[[j]]) %>%
    as.data.frame() %>%
    mutate(year = time + start_year, sim = j) %>%
    select(sim, year, all_of(strata)) %>%
    pivot_longer(all_of(strata), names_to = "stratum", values_to = "model")
}

proj <- bind_rows(proj)

# check total population stays constant (should be 1 in every year and simulation)
proj %>% group_by(sim, year) %>% summarise(total = sum(model), .groups = "drop") %>% pull(total) %>% range()

# median and 95% credible interval of the share in each stratum by year
model_shares <- proj %>%
  group_by(year, stratum) %>%
  summarise(model_lower = quantile(model, 0.025),
            model_upper = quantile(model, 0.975),
            model       = median(model),
            .groups = "drop")

left_join(model_shares, obs_shares %>% select(year, stratum, observed, lower, upper),
          by = c("year", "stratum")) %>%
  filter(!is.na(observed)) %>%
  print(n = Inf)


## 7. Plot model vs observed

stratum_labels <- c(
  nonIDU_nonSB = "Indoor, not injecting",
  nonIDU_SB    = "Street-based, not injecting",
  IDU_nonSB    = "Indoor, injecting",
  IDU_SB       = "Street-based, injecting"
)

plot_strata <- ggplot() +
  geom_ribbon(data = model_shares,
              aes(x = year, ymin = model_lower, ymax = model_upper),
              fill = "#2a78d6", alpha = 0.2) +
  geom_line(data = model_shares,
            aes(x = year, y = model, colour = "Model (median, 95% CrI)"), linewidth = 0.8) +
  geom_errorbar(data = obs_shares,
                aes(x = year, ymin = lower, ymax = upper, colour = "Observed (FSW IBBS, 95% CI)"),
                width = 0, linewidth = 0.5) +
  geom_point(data = obs_shares,
             aes(x = year, y = observed, colour = "Observed (FSW IBBS, 95% CI)"), size = 2.5) +
  facet_wrap(~ factor(stratum, levels = strata, labels = stratum_labels[strata]),
             scales = "free_y") +
  scale_colour_manual(values = c("Model (median, 95% CrI)" = "#2a78d6", "Observed (FSW IBBS, 95% CI)" = "#0b0b0b"),
                      name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
  labs(x = "Year", y = "Proportion of FSWs") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position      = "top",
    legend.justification = "left",
    panel.grid.minor     = element_blank(),
    panel.grid.major     = element_line(colour = "#e1e0d9", linewidth = 0.3),
    strip.text           = element_text(face = "bold", hjust = 0, colour = "#0b0b0b"),
    axis.text            = element_text(colour = "#52514e"),
    axis.title           = element_text(colour = "#52514e"),
    plot.background      = element_rect(fill = "#fcfcfb", colour = NA)
  )

plot_strata

ggsave("model_vs_observed_strata.png", plot_strata, width = 8, height = 6, dpi = 300)
