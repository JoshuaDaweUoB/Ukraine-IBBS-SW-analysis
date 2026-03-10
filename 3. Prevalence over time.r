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