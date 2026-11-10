# population model of FSWs by injecting drug use in past 12 months (IDU) and street-based sex work (SB),
# with entry, exit and movement between strata, calibrated to IBBS stratum shares 2009-2021
# strata order: nonIDU_nonSB, nonIDU_SB, IDU_nonSB, IDU_SB

## 0. Setup

pacman::p_load(deSolve, dplyr, tidyr, ggplot2, scales)

setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Chapter Writing/Chapter 6 - Ukraine FSWs IDU violence and HIV/data/SW data")

# model inputs, priors, draw_params() and build_params()
source("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Chapter Writing/Chapter 6 - Ukraine FSWs IDU violence and HIV/code/SW code/8. Model parameters.R")


## 1. Population model

## 1.1 Population model (ODEs)

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

## 1.2 Initial population

# 1987: 2009 shares among all FSWs, with injecting set to 5% of the 2009 level (assumption)
N_f <- 1   # population normalised to 1
init <- obs_shares %>%
  filter(year == 2009) %>%
  slice(match(strata, stratum)) %>%
  pull(observed)
init <- c(init[1] + init[3] * 0.95, init[2] + init[4] * 0.95, init[3] * 0.05, init[4] * 0.05)
init <- setNames(N_f * init / sum(init), strata)


## 2. Bayesian calibration (approximate Bayesian computation, rejection sampling)

# each simulation draws every uncertain input from its prior, runs the model to 2021 and
# measures the distance to the observed shares among all FSWs (2009-2021);
# the closest accept_frac of simulations form the posterior
set.seed(2026)
n_sims      <- 500   # run a smaller number first (e.g. 500) to check run time
accept_frac <- 0.01    # proportion of simulations accepted

## 2.1 Distance weights

# distance weights: binomial standard error of each observed share
obs_se <- obs_shares %>%
  mutate(se = sqrt(observed * (1 - observed) / N))

## 2.2 Sample from the priors and run the model

sims_draw    <- list()   # sampled values for each simulation
sims_summary <- list()   # summary of sampled values and distance for each simulation

for (k in 1:n_sims) {

  # sample from the priors and build the parameter set (skip if cessation would be negative)
  draw <- draw_params()
  p    <- build_params(draw)
  if (is.null(p)) next

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

  sims_draw[[k]] <- draw
  sims_summary[[k]] <- tibble(
    sim = k, dist = dist,
    tau_1987 = draw$tau_1987, tau_peak = draw$tau_peak, rho_pre = draw$rho_pre, rho_post = draw$rho_post, eps_post = draw$eps_post,
    tau_indoor = p$tau[1], tau_street = p$tau[2],
    eps_indoor = p$eps[1], eps_street = p$eps[2],
    psi_nonIDU = p$psi[1], psi_IDU    = p$psi[2],
    rho_nonIDU = p$rho[1], rho_IDU    = p$rho[2],
    gamma_nonIDU_nonSB = p$gamma[1], gamma_nonIDU_SB = p$gamma[2],
    gamma_IDU_nonSB    = p$gamma[3], gamma_IDU_SB    = p$gamma[4]
  )

  if (k %% 500 == 0) message(k, " of ", n_sims, " simulations")
}

sims_summary <- bind_rows(sims_summary)

## 2.3 Accept the closest simulations

posterior <- sims_summary %>%
  filter(dist <= quantile(dist, accept_frac))

nrow(posterior)   # number of accepted parameter sets

## 2.4 Posterior summaries (calibrated multipliers, linked-cohort rates and cessation, per year)

posterior %>%
  select(-sim, -dist) %>%
  pivot_longer(everything(), names_to = "parameter") %>%
  group_by(parameter) %>%
  summarise(median = median(value),
            lower  = quantile(value, 0.025),
            upper  = quantile(value, 0.975))


## 3. Posterior projections and comparison with data

# run each accepted parameter set to the end of the model period
proj <- list()

for (j in posterior$sim) {
  proj[[j]] <- ode(y = init, times = seq(0, end_year - start_year, by = 1), func = fsw_pop, parms = build_params(sims_draw[[j]])) %>%
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


## 4. Plot model vs observed

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
