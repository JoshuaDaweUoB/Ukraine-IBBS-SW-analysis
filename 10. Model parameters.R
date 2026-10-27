## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales, openxlsx, readr, magick)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Chapter Writing/Chapter 6 - Ukraine FSWs IDU violence and HIV/data/SW data")

# load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## mean number of clients in past 30 days, by typology and IDU

# keep FSWs with typology, IDU and client numbers
k_data <- sw_combined_clean %>%
  filter(street_sw_bin %in% c("Yes", "No"),
         idu_ever_bin %in% c("Yes", "No"),
         !is.na(sw_partners_clients_30d))

# by year
k_data %>%
  group_by(street_sw_bin, idu_ever_bin, year) %>%
  summarise(n = n(),
            mean = mean(sw_partners_clients_30d),
            se = sd(sw_partners_clients_30d) / sqrt(n)) %>%
  mutate(lci = mean - 1.96 * se,
         uci = mean + 1.96 * se) %>%
  print(n = Inf)

# overall
k_data %>%
  group_by(street_sw_bin, idu_ever_bin) %>%
  summarise(n = n(),
            mean = mean(sw_partners_clients_30d),
            se = sd(sw_partners_clients_30d) / sqrt(n)) %>%
  mutate(lci = mean - 1.96 * se,
         uci = mean + 1.96 * se)

# before 2017 vs 2017 onwards
k_data %>%
  mutate(period = if_else(year < 2017, "2009-2015", "2017-2021")) %>%
  group_by(street_sw_bin, idu_ever_bin, period) %>%
  summarise(n = n(),
            mean = mean(sw_partners_clients_30d),
            se = sd(sw_partners_clients_30d) / sqrt(n)) %>%
  mutate(lci = mean - 1.96 * se,
         uci = mean + 1.96 * se)

## sex acts per client

# keep FSWs with typology, IDU and acts per client
acts_data <- sw_combined_clean %>%
  filter(street_sw_bin %in% c("Yes", "No"),
         idu_ever_bin %in% c("Yes", "No"),
         !is.na(sw_acts_per_client))

# overall, by typology and IDU
acts_data %>%
  group_by(street_sw_bin, idu_ever_bin) %>%
  summarise(n = n(),
            mean = mean(sw_acts_per_client),
            se = sd(sw_acts_per_client) / sqrt(n)) %>%
  mutate(lci = mean - 1.96 * se,
         uci = mean + 1.96 * se)

## condom use at last sex with a client

# keep FSWs with typology, IDU and condom use
condom_data <- sw_combined_clean %>%
  filter(street_sw_bin %in% c("Yes", "No"),
         idu_ever_bin %in% c("Yes", "No"),
         client_condom_lastsex_bin %in% c("Yes", "No"))

# by year, by typology and IDU
condom_data %>%
  group_by(street_sw_bin, idu_ever_bin, year) %>%
  summarise(n = n(),
            prop = mean(client_condom_lastsex_bin == "Yes"),
            se = sqrt(prop * (1 - prop) / n)) %>%
  mutate(lci = prop - 1.96 * se,
         uci = prop + 1.96 * se) %>%
  print(n = Inf)

# overall, by typology and IDU
condom_data %>%
  group_by(street_sw_bin, idu_ever_bin) %>%
  summarise(n = n(),
            prop = mean(client_condom_lastsex_bin == "Yes"),
            se = sqrt(prop * (1 - prop) / n)) %>%
  mutate(lci = prop - 1.96 * se,
         uci = prop + 1.96 * se)

## calibration target: proportion of all FSWs in each group, by year

# keep FSWs with typology and IDU (same groups as the model in 9. Entry and exit into model.r)
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

## calibration target: HIV prevalence among FSWs, by group and year

# keep FSWs with typology, IDU and an HIV test result
hiv_data <- strata_data %>%
  filter(hiv_test_rslt_bin %in% c("Positive", "Negative", "1", "0")) %>%
  mutate(hiv_pos = hiv_test_rslt_bin %in% c("Positive", "1"))

# prevalence with exact 95% CI
hiv_data %>%
  group_by(year, street_sw_bin, idu_12m_bin) %>%
  summarise(n_hiv = sum(hiv_pos),
            N = n()) %>%
  mutate(prev = n_hiv / N,
         lci = qbeta(0.025, n_hiv, N - n_hiv + 1),
         uci = qbeta(0.975, n_hiv + 1, N - n_hiv)) %>%
  print(n = Inf)

## calibration target: ART coverage among FSWs living with HIV, by year (ART not recorded 2009, 2011)

# keep FSWs living with HIV with ART status
art_data <- hiv_data %>%
  filter(hiv_pos, art_current_bin %in% c("Yes", "No"))

# by group and year
art_data %>%
  group_by(year, street_sw_bin, idu_12m_bin) %>%
  summarise(n_art = sum(art_current_bin == "Yes"),
            N = n()) %>%
  mutate(cov = n_art / N,
         lci = qbeta(0.025, n_art, N - n_art + 1),
         uci = qbeta(0.975, n_art + 1, N - n_art)) %>%
  print(n = Inf)

# all groups together, by year
art_data %>%
  group_by(year) %>%
  summarise(n_art = sum(art_current_bin == "Yes"),
            N = n()) %>%
  mutate(cov = n_art / N,
         lci = qbeta(0.025, n_art, N - n_art + 1),
         uci = qbeta(0.975, n_art + 1, N - n_art))

## calibration targets: clients

# load client data
clients_combined_clean <- readRDS("clients_combined_clean.rds")

# HIV prevalence among clients, by year
clients_combined_clean %>%
  filter(hiv_test_rslt_bin %in% c("Positive", "Negative")) %>%
  group_by(year) %>%
  summarise(n_hiv = sum(hiv_test_rslt_bin == "Positive"),
            N = n()) %>%
  mutate(prev = n_hiv / N,
         lci = qbeta(0.025, n_hiv, N - n_hiv + 1),
         uci = qbeta(0.975, n_hiv + 1, N - n_hiv))

# ART coverage among clients living with HIV (2014 only)
clients_combined_clean %>%
  filter(hiv_test_rslt_bin == "Positive", art_current_bin %in% c("Yes", "No")) %>%
  group_by(year) %>%
  summarise(n_art = sum(art_current_bin == "Yes"),
            N = n()) %>%
  mutate(cov = n_art / N,
         lci = qbeta(0.025, n_art, N - n_art + 1),
         uci = qbeta(0.975, n_art + 1, N - n_art))


