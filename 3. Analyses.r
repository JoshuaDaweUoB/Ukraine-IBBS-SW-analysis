## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# List of variables to test
vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat", "ngo_access_lifetime_3cat", "city_travel_12m", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "sw_partners_total_24h_5cat", "sw_partners_clients_30d_4cat", "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp", "violence_support_ngo", "age_bin", "occupied", "occupied_partial",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence", "avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

# binary variables
binary_vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat",  "ngo_access_lifetime_3cat", "city_travel_12m", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever", "occupied", "occupied_partial",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp", "violence_support_ngo", "age_bin", "underage_first_sw_bin",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence", "avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma",
)

sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(binary_vars),
    ~ factor(case_when(
      . == "Yes" ~ "Yes",
      . == "No" ~ "No",
      TRUE ~ NA_character_
    ), levels = c("No", "Yes"))
  ))

table(sw_combined_clean$avoided_healthcare_12m_stigma, useNA = "ifany")

# Ensure outcome is a factor
sw_combined_clean$hiv_test_rslt_bin <- as.factor(sw_combined_clean$hiv_test_rslt_bin)

# ensure adjustment variables are factors
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# HIV prevalence by year and city
sw_combined_clean$hiv_test_rslt_bin_num <- ifelse(sw_combined_clean$hiv_test_rslt_bin == "Positive", 1,
                                                  ifelse(sw_combined_clean$hiv_test_rslt_bin == "Negative", 0, NA))
hiv_prevalence <- sw_combined_clean %>%
  group_by(city, year_num) %>%
  summarise(prevalence = mean(hiv_test_rslt_bin_num, na.rm = TRUE)) %>%
  ungroup()

# Get list of cities
cities <- unique(hiv_prevalence$city)

for (c in cities) {
  city_data <- hiv_prevalence %>% filter(city == c)
  
  # Only plot if there are at least 2 years of data
  if (nrow(city_data) < 2) next
  
  p <- ggplot(city_data, aes(x = year_num, y = prevalence)) +
    geom_line(color = "black", size = 0.8) +
    geom_point(color = "black", size = 1.5) +
    scale_x_continuous(breaks = city_data$year_num) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = paste("HIV Prevalence in", c),
         x = "Year",
         y = "Prevalence") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
    )
  
   ggsave(filename = paste0("hiv_prevalence_", c, ".png"), plot = p, width = 6, height = 4)
}

# Calculate prevalence by region and year
hiv_prevalence <- sw_combined_clean %>%
  group_by(ukraine_region, year_num) %>%
  summarise(prevalence = mean(hiv_test_rslt_bin_num, na.rm = TRUE)) %>%
  ungroup()

# Get list of regions
regions <- unique(hiv_prevalence$ukraine_region)

for (r in regions) {
  region_data <- hiv_prevalence %>% filter(ukraine_region == r)
  
  # Only plot if there are at least 2 years of data
  if (nrow(region_data) < 2) next
  
  p <- ggplot(region_data, aes(x = year_num, y = prevalence)) +
    geom_line(color = "black", size = 0.8) +
    geom_point(color = "black", size = 1.5) +
    scale_x_continuous(breaks = region_data$year_num) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = paste("HIV Prevalence in", r),
         x = "Year",
         y = "Prevalence") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(filename = paste0("hiv_prevalence_", r, ".png"), plot = p, width = 6, height = 4)
}


# Calculate prevalence by occupied_partial and year
hiv_prevalence <- sw_combined_clean %>%
  group_by(occupied_partial, year_num) %>%
  summarise(prevalence = mean(hiv_test_rslt_bin_num, na.rm = TRUE)) %>%
  ungroup()

# Plot all levels in one figure
p <- ggplot(hiv_prevalence, aes(x = year_num, y = prevalence, color = occupied_partial, group = occupied_partial)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = 2008:2021, limits = c(2008, 2021)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "HIV Prevalence by occupied or attacked by Russia",
       x = "Year",
       y = "Prevalence",
       color = "Occupied or attacked") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("hiv_prevalence_by_occupied_partial.png", plot = p, width = 7, height = 5)



# Filter for cities with data both before 2015 and in/after 2015
years_before <- c(2011, 2013)
years_after <- c(2015, 2017, 2021)

# Find cities with at least one year before 2015 and one year in/after 2015
cities_before <- sw_combined_clean %>%
  filter(year_num %in% years_before) %>%
  distinct(city)
cities_after <- sw_combined_clean %>%
  filter(year_num %in% years_after) %>%
  distinct(city)

target_cities <- intersect(cities_before$city, cities_after$city)

# Only keep rows for those cities and years 2011-2021
years_required <- c(2011, 2013, 2015, 2017, 2021)
sw_filtered <- sw_combined_clean %>%
  filter(city %in% target_cities, year_num %in% years_required)

# Calculate prevalence by occupied_partial and year (for filtered cities)
hiv_prevalence <- sw_filtered %>%
  group_by(occupied_partial, year_num) %>%
  summarise(prevalence = mean(hiv_test_rslt_bin_num, na.rm = TRUE)) %>%
  ungroup()

# Plot all levels in one figure
p <- ggplot(hiv_prevalence, aes(x = year_num, y = prevalence, color = occupied_partial, group = occupied_partial)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = years_required, limits = c(2011, 2021)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "HIV Prevalence by occupied or attacked by Russia",
       x = "Year",
       y = "Prevalence",
       color = "Occupied or attacked") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("hiv_prevalence_by_occupied_partial.png", plot = p, width = 7, height = 5)

# Calculate prevalence by region and year
hiv_prevalence <- sw_combined_clean %>%
  group_by(ukraine_region_4cat, year_num) %>%
  summarise(prevalence = mean(hiv_test_rslt_bin_num, na.rm = TRUE)) %>%
  ungroup()

# Plot all levels in one figure
p <- ggplot(hiv_prevalence, aes(x = year_num, y = prevalence, color = ukraine_region_4cat, group = ukraine_region_4cat)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = 2008:2021, limits = c(2008, 2021)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "HIV Prevalence by region",
       x = "Year",
       y = "Prevalence",
       color = "Region") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("hiv_prevalence_by_region_4cat.png", plot = p, width = 7, height = 5)

# Calculate prevalence by city and year
hiv_prevalence <- sw_combined_clean %>%
  group_by(city, year_num) %>%
  summarise(prevalence = mean(hiv_test_rslt_bin_num, na.rm = TRUE)) %>%
  ungroup()

# Plot all levels in one figure
p <- ggplot(hiv_prevalence, aes(x = year_num, y = prevalence, color = city, group = city)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = 2008:2021, limits = c(2008, 2021)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "HIV Prevalence by city",
       x = "Year",
       y = "Prevalence",
       color = "City") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("hiv_prevalence_by_city.png", plot = p, width = 7, height = 5)

# Prepare to store results
results <- data.frame(
  Variable = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# loop over vars
for (v in vars) {
  cat("Running:", v, "\n")
  
  model_vars <- c("hiv_test_rslt_bin", "ukraine_region", "year", v)
  subset_data <- sw_combined_clean %>%
    select(all_of(model_vars)) %>%
    filter(complete.cases(.))
  
  # Check number of unique years in subset
  if (length(unique(subset_data$year)) == 1) {
    formula <- as.formula(paste("hiv_test_rslt_bin ~ ukraine_region +", v))
  } else {
    formula <- as.formula(paste("hiv_test_rslt_bin ~ ukraine_region + year +", v))
  }
  
  model <- try(glm(formula, data = subset_data, family = binomial), silent = TRUE)
  
  if (inherits(model, "try-error")) {
    cat("FAILED:", v, "\n")
    next
  }
  
  tidy_mod <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
  
  for (i in 1:nrow(tidy_mod)) {
    or <- tidy_mod$estimate[i]
    lci <- tidy_mod$conf.low[i]
    uci <- tidy_mod$conf.high[i]
    or_fmt <- sprintf("OR: %.2f; (95%%CI: %.2f-%.2f)", or, lci, uci)
    results <- rbind(results,
                     data.frame(Variable = paste(v, tidy_mod$term[i], sep=":"),
                                OR = or_fmt))
  }
}

# remove region coefficients
results <- results[!grepl("^.*:ukraine_region", results$Variable), ]
results <- results[!grepl("^.*:year", results$Variable), ]

# Save to Excel
write_xlsx(results, "univariate_logistic_results.xlsx")

# summary statistics

# load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

table(sw_combined_clean$hiv_test_rslt_bin, useNA = "ifany")

# Ensure outcome is numeric for proportions
sw_combined_clean$hiv_test_rslt_bin_num <- as.numeric(as.character(sw_combined_clean$hiv_test_rslt_bin))

print(levels(sw_combined_clean$hiv_test_rslt_bin))
print(table(sw_combined_clean$hiv_test_rslt_bin, useNA = "ifany"))

summary_list <- list()

for (v in vars) {
  tab <- sw_combined_clean %>%
    group_by(!!sym(v)) %>%
    summarise(
      n = n(),
      outcome_n = sum(hiv_test_rslt_bin == "Positive", na.rm = TRUE),
      outcome_prop = mean(hiv_test_rslt_bin == "Positive", na.rm = TRUE)
    ) %>%
    mutate(variable = v) %>%
    rename(level = !!sym(v))
  summary_list[[v]] <- tab
}

summary_df <- bind_rows(summary_list) %>%
  select(variable, level, n, outcome_n, outcome_prop)

write_xlsx(summary_df, "exposure_summary.xlsx")

# condom use 

# load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# List of variables to test
vars <- c(
  "ngo_access_lifetime_3cat", "city_travel_12m", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "sw_partners_total_24h_5cat", "sw_partners_clients_30d_4cat", "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp", "violence_support_ngo", "age_bin",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence", "avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

# binary variables
binary_vars <- c(
  "ngo_access_lifetime_3cat", "city_travel_12m", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "violence_any_ever_3cat", "violence_rape_12m_3cat", "violence_rape_ever", "violence_beaten_ever",
  "violence_physical_abuse_ever", "violence_police", "violence_pimp", "violence_support_ngo", "age_bin", "underage_first_sw_bin"
)

sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"), levels = c("No", "Yes"))
  ))
table(sw_combined_clean$avoided_healthcare_12m_stigma)

sw_combined_clean$condom_access_12m_3cat <- factor(
  sw_combined_clean$condom_access_12m_3cat,
  levels = c("No", "Yes")
)

sw_combined_clean$client_condom_lastsex_3cat <- factor(
  sw_combined_clean$client_condom_lastsex_3cat,
  levels = c("No", "Yes")
)

# ensure adjustment variables are factors
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# Prepare to store results
results <- data.frame(
  Variable = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# loop over vars
for (v in vars) {

  cat("Running:", v, "\n")

  if (length(unique(tmp$year)) == 1) {
    formula <- as.formula(paste("condom_access_12m_3cat ~ ukraine_region +", v))
  } else {
    formula <- as.formula(paste("condom_access_12m_3cat ~ ukraine_region + year +", v))
  }
  

  model <- try(glm(formula, data = sw_combined_clean, family = binomial), silent = TRUE)

  if (inherits(model, "try-error")) {
    cat("FAILED:", v, "\n")
    next
  }

  tidy_mod <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

  for (i in 1:nrow(tidy_mod)) {
    or <- tidy_mod$estimate[i]
    lci <- tidy_mod$conf.low[i]
    uci <- tidy_mod$conf.high[i]
    or_fmt <- sprintf("OR: %.2f; (95%%CI: %.2f-%.2f)", or, lci, uci)
    results <- rbind(results,
                     data.frame(
                       Variable = paste(v, tidy_mod$term[i], sep=":"),
                       OR = or_fmt
                     ))
  }
}

# remove region coefficients
results <- results[!grepl("^.*:ukraine_region", results$Variable), ]
results <- results[!grepl("^.*:year", results$Variable), ]

# Save to Excel
write_xlsx(results, "univariate_logistic_results_condom_access.xlsx")


# Prepare to store results
results <- data.frame(
  Variable = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# loop over vars
for (v in vars) {

  cat("Running:", v, "\n")

  if (length(unique(tmp$year)) == 1) {
    formula <- as.formula(paste("client_condom_lastsex_3cat ~ ukraine_region +", v))
  } else {
    formula <- as.formula(paste("client_condom_lastsex_3cat ~ ukraine_region + year +", v))
  }
  

  model <- try(glm(formula, data = sw_combined_clean, family = binomial), silent = TRUE)

  if (inherits(model, "try-error")) {
    cat("FAILED:", v, "\n")
    next
  }

  tidy_mod <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

  for (i in 1:nrow(tidy_mod)) {
    or <- tidy_mod$estimate[i]
    lci <- tidy_mod$conf.low[i]
    uci <- tidy_mod$conf.high[i]
    or_fmt <- sprintf("OR: %.2f; (95%%CI: %.2f-%.2f)", or, lci, uci)
    results <- rbind(results,
                     data.frame(
                       Variable = paste(v, tidy_mod$term[i], sep=":"),
                       OR = or_fmt
                     ))
  }
}

# remove region coefficients
results <- results[!grepl("^.*:ukraine_region", results$Variable), ]
results <- results[!grepl("^.*:year", results$Variable), ]

# Save to Excel
write_xlsx(results, "univariate_logistic_results_condom_lastsex.xlsx")

# rape

# List of variables to test
vars <- c(
  "year", "condom_access_12m_3cat", "ngo_access_lifetime_3cat", "city_travel_12m",
  "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "sw_partners_total_24h_5cat", "sw_partners_clients_30d_4cat",
  "violence_beaten_ever", "violence_physical_abuse_ever", "violence_police",
  "violence_pimp", "violence_support_ngo",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence",
  "avoided_healthcare_12m_police", "age_bin",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence",
  "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

# binary variables
binary_vars <- c(
  "condom_access_12m_3cat", "ngo_access_lifetime_3cat", "street_sw_bin",
  "alcohol_30d_bin", "violence_support_ngo",
  "violence_beaten_ever", "violence_physical_abuse_ever",
  "violence_police", "violence_pimp", "age_bin",
  "used_syringe_last_3cat", "underage_first_sw_bin", "city_travel_12m"
)

# Recode binary predictors
sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# ---- OUTCOME: RAPE ----
# Recode rape outcome to binary 0/1
sw_combined_clean$rape_bin <- ifelse(
  sw_combined_clean$violence_rape_ever == "Yes", 1, 0
)

# ensure adjustment variables are factors
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# Prepare results storage
results <- data.frame(
  Variable = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# loop over vars
for (v in vars) {

  formula <- as.formula(
    paste("rape_bin ~ ukraine_region + year +", v)
  )

  model <- glm(formula, data = sw_combined_clean, family = binomial)

  tidy_mod <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)

  # remove intercept
  tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

  for (i in 1:nrow(tidy_mod)) {

    or <- tidy_mod$estimate[i]
    lci <- tidy_mod$conf.low[i]
    uci <- tidy_mod$conf.high[i]

    or_fmt <- sprintf("OR: %.2f; (95%%CI: %.2f-%.2f)", or, lci, uci)

    results <- rbind(
      results,
      data.frame(
        Variable = paste(v, tidy_mod$term[i], sep=":"),
        OR = or_fmt
      )
    )
  }
}

# remove region and year coefficients from output
results <- results[!grepl("^.*:ukraine_region", results$Variable), ]
results <- results[!grepl("^.*:year", results$Variable), ]

# Save to Excel
write_xlsx(results, "univariate_logistic_results_rape.xlsx")

# rape

# List of variables to test
vars <- c(
  "year", "condom_access_12m_3cat", "ngo_access_lifetime_3cat", "city_travel_12m",
  "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "sw_partners_total_24h_5cat", "sw_partners_clients_30d_4cat",
  "violence_beaten_ever", "violence_physical_abuse_ever", "violence_police",
  "violence_pimp", "violence_support_ngo",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence",
  "avoided_healthcare_12m_police", "age_bin",
  "avoided_hiv_test_12m_police", "avoided_hiv_test_12m_violence",
  "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

# binary variables
binary_vars <- c(
  "condom_access_12m_3cat", "ngo_access_lifetime_3cat", "street_sw_bin",
  "alcohol_30d_bin", "violence_support_ngo",
  "violence_beaten_ever", "violence_physical_abuse_ever",
  "violence_police", "violence_pimp", "age_bin",
  "used_syringe_last_3cat", "underage_first_sw_bin", "city_travel_12m"
)

# Recode binary predictors
sw_combined_clean <- sw_combined_clean %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# ---- OUTCOME: RAPE ----
# Recode rape outcome to binary 0/1
sw_combined_clean$rape_bin <- ifelse(
  sw_combined_clean$art_current_3cat == "Yes", 1, 0
)

# ensure adjustment variables are factors
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# Prepare results storage
results <- data.frame(
  Variable = character(),
  OR = character(),
  stringsAsFactors = FALSE
)

# loop over vars
for (v in vars) {

  formula <- as.formula(
    paste("art_current_3cat ~ ukraine_region + year +", v)
  )

  model <- glm(formula, data = sw_combined_clean, family = binomial)

  tidy_mod <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)

  # remove intercept
  tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]

  for (i in 1:nrow(tidy_mod)) {

    or <- tidy_mod$estimate[i]
    lci <- tidy_mod$conf.low[i]
    uci <- tidy_mod$conf.high[i]

    or_fmt <- sprintf("OR: %.2f; (95%%CI: %.2f-%.2f)", or, lci, uci)

    results <- rbind(
      results,
      data.frame(
        Variable = paste(v, tidy_mod$term[i], sep=":"),
        OR = or_fmt
      )
    )
  }
}

# remove region and year coefficients from output
results <- results[!grepl("^.*:ukraine_region", results$Variable), ]
results <- results[!grepl("^.*:year", results$Variable), ]

# Save to Excel
write_xlsx(results, "univariate_logistic_results_art.xlsx")

## rate ratios

# load long hiv data
sw_negative_cohort <- readRDS("sw_incidence_dataset.rds")

sw_negative_cohort <- sw_negative_cohort %>%
  mutate(
    hiv_test_rslt_bin = ifelse(hiv_test_rslt_bin == "Positive", 1, 0),
    hiv_test_rslt_bin = as.numeric(hiv_test_rslt_bin)
  )

# ensure adjustment variables are factors
sw_negative_cohort <- sw_negative_cohort %>%
  mutate(
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )


# -----------------------------
# DEFINE EXPOSURE VARIABLES
# -----------------------------

exposure_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_any_ever_3cat",
  "violence_beaten_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo",
  "sw_partners_total_24h_5cat",
  "sw_partners_clients_30d_4cat"
)

# recode true binary Yes/No variables
binary_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_any_ever_3cat",
  "violence_beaten_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo"
)

sw_negative_cohort <- sw_negative_cohort %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# -----------------------------
# RUN REGION-ADJUSTED COX MODELS
# -----------------------------

results_list <- list()

for (var in exposure_vars) {
  
  # make sure exposure is factor
  sw_negative_cohort[[var]] <- as.factor(sw_negative_cohort[[var]])
  
  formula <- as.formula(
    paste("Surv(py, hiv_test_rslt_bin) ~", var, "+ ukraine_region + year")
  )
  
  model <- coxph(formula, data = sw_negative_cohort)
  
  tidy_mod <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # keep only exposure rows
  tidy_mod <- tidy_mod %>%
    filter(grepl(paste0("^", var), term))
  
  # get all exposure levels
  levels_var <- levels(sw_negative_cohort[[var]])
  
  for (lev in levels_var) {
    
    subset_data <- sw_negative_cohort %>%
      filter(!is.na(.data[[var]]),
             .data[[var]] == lev)
    
    cases <- sum(subset_data$hiv_test_rslt_bin == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    
    # reference level
    if (lev == levels_var[1]) {
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = 1,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years
      )
      
    } else {
      
      row_match <- tidy_mod %>%
        filter(grepl(lev, term))
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = row_match$estimate,
        CI_lower = row_match$conf.low,
        CI_upper = row_match$conf.high,
        Cases = cases,
        Person_Years = person_years
      )
    }
  }
}

results_df <- bind_rows(results_list)

write_xlsx(results_df, "cox_model_results_hiv.xlsx")

## rape incidence

# Load dataset
sw_negative_cohort_rape <- readRDS("sw_incident_rape_dataset.rds")

# Ensure correct types
sw_negative_cohort_rape <- sw_negative_cohort_rape %>%
  mutate(
    rape_bin = as.numeric(rape_bin),
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# -----------------------------
# DEFINE EXPOSURE VARIABLES
# -----------------------------

exposure_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_beaten_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo",
  "sw_partners_total_24h_5cat",
  "sw_partners_clients_30d_4cat"
)

# recode true binary Yes/No variables
binary_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_beaten_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo"
)

sw_negative_cohort_rape <- sw_negative_cohort_rape %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# -----------------------------
# RUN REGION-ADJUSTED COX MODELS
# -----------------------------

results_list <- list()

for (var in exposure_vars) {
  
  # make sure exposure is factor
  sw_negative_cohort_rape[[var]] <- as.factor(sw_negative_cohort_rape[[var]])
  
  formula <- as.formula(
    paste("Surv(py, rape_bin) ~", var, "+ ukraine_region + year")
  )
  
  model <- coxph(formula, data = sw_negative_cohort_rape)
  
  tidy_mod <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # keep only exposure rows
  tidy_mod <- tidy_mod %>%
    filter(grepl(paste0("^", var), term))
  
  # get all exposure levels
  levels_var <- levels(sw_negative_cohort_rape[[var]])
  
  for (lev in levels_var) {
    
    subset_data <- sw_negative_cohort_rape %>%
      filter(!is.na(.data[[var]]),
             .data[[var]] == lev)
    
    cases <- sum(subset_data$rape_bin == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    
    # reference level
    if (lev == levels_var[1]) {
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = 1,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years
      )
      
    } else {
      
      row_match <- tidy_mod %>%
        filter(grepl(lev, term))
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = row_match$estimate,
        CI_lower = row_match$conf.low,
        CI_upper = row_match$conf.high,
        Cases = cases,
        Person_Years = person_years
      )
    }
  }
}


results_df <- bind_rows(results_list)

write_xlsx(results_df, "cox_model_results_rape.xlsx")

## beating incidence

# Load dataset
sw_negative_cohort_beating <- readRDS("sw_incident_beating_dataset.rds")

# Ensure correct types
sw_negative_cohort_beating <- sw_negative_cohort_beating %>%
  mutate(
    beating_bin = as.numeric(beating_bin),
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# -----------------------------
# DEFINE EXPOSURE VARIABLES
# -----------------------------

exposure_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_rape_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo",
  "sw_partners_total_24h_5cat",
  "sw_partners_clients_30d_4cat"
)

# recode true binary Yes/No variables
binary_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_rape_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo"
)

sw_negative_cohort_beating <- sw_negative_cohort_beating %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# -----------------------------
# RUN REGION-ADJUSTED COX MODELS
# -----------------------------

results_list <- list()

for (var in exposure_vars) {
  
  # make sure exposure is factor
  sw_negative_cohort_beating[[var]] <- as.factor(sw_negative_cohort_beating[[var]])
  
  formula <- as.formula(
    paste("Surv(py, beating_bin) ~", var, "+ ukraine_region + year")
  )
  
  model <- coxph(formula, data = sw_negative_cohort_beating)
  
  tidy_mod <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # keep only exposure rows
  tidy_mod <- tidy_mod %>%
    filter(grepl(paste0("^", var), term))
  
  # get all exposure levels
  levels_var <- levels(sw_negative_cohort_beating[[var]])
  
  for (lev in levels_var) {
    
    subset_data <- sw_negative_cohort_beating %>%
      filter(!is.na(.data[[var]]),
             .data[[var]] == lev)
    
    cases <- sum(subset_data$beating_bin == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    
    # reference level
    if (lev == levels_var[1]) {
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = 1,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years
      )
      
    } else {
      
      row_match <- tidy_mod %>%
        filter(grepl(lev, term))
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = row_match$estimate,
        CI_lower = row_match$conf.low,
        CI_upper = row_match$conf.high,
        Cases = cases,
        Person_Years = person_years
      )
    }
  }
}


results_df <- bind_rows(results_list)

write_xlsx(results_df, "cox_model_results_beating.xlsx")


## condom use incidence

# Load dataset
sw_negative_cohort_beating <- readRDS("sw_incident_beating_dataset.rds")

# Ensure correct types
sw_negative_cohort_beating <- sw_negative_cohort_beating %>%
  mutate(
    beating_bin = as.numeric(beating_bin),
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# -----------------------------
# DEFINE EXPOSURE VARIABLES
# -----------------------------

exposure_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_rape_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo",
  "sw_partners_total_24h_5cat",
  "sw_partners_clients_30d_4cat"
)

# recode true binary Yes/No variables
binary_vars <- c(
  "condom_access_12m_3cat",
  "client_condom_lastsex_3cat",
  "ngo_access_lifetime_3cat",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_cat",
  "violence_rape_ever",
  "violence_physical_abuse_ever",
  "violence_police",
  "used_syringe_last_3cat",
  "underage_first_sw_bin",
  "violence_support_ngo"
)

sw_negative_cohort_beating <- sw_negative_cohort_beating %>%
  mutate(across(all_of(binary_vars),
    ~ factor(ifelse(. == "Yes", "Yes", "No"),
             levels = c("No", "Yes"))
  ))

# -----------------------------
# RUN REGION-ADJUSTED COX MODELS
# -----------------------------

results_list <- list()

for (var in exposure_vars) {
  
  # make sure exposure is factor
  sw_negative_cohort_beating[[var]] <- as.factor(sw_negative_cohort_beating[[var]])
  
  formula <- as.formula(
    paste("Surv(py, beating_bin) ~", var, "+ ukraine_region + year")
  )
  
  model <- coxph(formula, data = sw_negative_cohort_beating)
  
  tidy_mod <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # keep only exposure rows
  tidy_mod <- tidy_mod %>%
    filter(grepl(paste0("^", var), term))
  
  # get all exposure levels
  levels_var <- levels(sw_negative_cohort_beating[[var]])
  
  for (lev in levels_var) {
    
    subset_data <- sw_negative_cohort_beating %>%
      filter(!is.na(.data[[var]]),
             .data[[var]] == lev)
    
    cases <- sum(subset_data$beating_bin == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    
    # reference level
    if (lev == levels_var[1]) {
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = 1,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years
      )
      
    } else {
      
      row_match <- tidy_mod %>%
        filter(grepl(lev, term))
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = row_match$estimate,
        CI_lower = row_match$conf.low,
        CI_upper = row_match$conf.high,
        Cases = cases,
        Person_Years = person_years
      )
    }
  }
}


results_df <- bind_rows(results_list)

write_xlsx(results_df, "cox_model_results_beating.xlsx")