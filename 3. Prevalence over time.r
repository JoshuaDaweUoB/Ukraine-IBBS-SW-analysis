## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales, openxlsx, readr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# Load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# factor
sw_combined_clean <- sw_combined_clean %>%
  mutate(idu_ever_3cat = factor(idu_ever_3cat, levels = c("No", "Yes")),
        hiv_test_rslt_bin = case_when(
          hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ "Yes",
          hiv_test_rslt_bin %in% c("Negative","0","No") ~ "No",
          TRUE ~ NA_character_),
        years_in_sw_3cat = as.factor(years_in_sw_3cat))

# variables to summarise
vars <- c(
  "condom_access_12m_3cat", "client_condom_lastsex_3cat", "ngo_access_lifetime_3cat",
  "ngo_condom_rec_bin", "ngo_syringe_12m_bin", "hiv_test_rslt_bin", "years_in_sw_3cat",
  "city_travel_12m_cat", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_3cat",
  "violence_any_ever_3cat", "violence_rape_12m_3cat",
  "violence_rape_ever", "violence_beaten_ever", "violence_physical_abuse_ever",
  "violence_police", "violence_pimp", "violence_support_ngo", "age_bin",
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_violence",
  "avoided_healthcare_12m_police", "avoided_hiv_test_12m_police",
  "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_stigma", "underage_first_sw_bin"
)

# Add "Overall" pseudo-city
sw_combined_clean <- bind_rows(sw_combined_clean, sw_combined_clean %>% mutate(city = "Overall"))

sw_combined_clean <- bind_rows(
  sw_combined_clean,
  sw_combined_clean %>% mutate(street_sw_bin = "Overall")
)

sw_combined_clean <- sw_combined_clean %>%
  mutate(street_sw_bin = factor(street_sw_bin, levels = c("No", "Yes", "Overall")))

# Create a workbook
wb <- createWorkbook()

for (var in vars) {
  
  cat("Processing:", var, "\n")
  
  # Summarise by city/year and IDU status
  summary_df <- sw_combined_clean %>%
    filter(!is.na(.data[[var]]), !is.na(street_sw_bin)) %>%
    group_by(city, year, street_sw_bin) %>%
    summarise(
      IDU_n = sum(.data[[var]] == "Yes" & idu_ever_3cat == "Yes", na.rm = TRUE),
      IDU_denom = sum(idu_ever_3cat == "Yes", na.rm = TRUE),
      IDU_pct = ifelse(IDU_denom > 0, round(IDU_n/IDU_denom*100,1), NA),
      
      No_IDU_n = sum(.data[[var]] == "Yes" & idu_ever_3cat == "No", na.rm = TRUE),
      No_IDU_denom = sum(idu_ever_3cat == "No", na.rm = TRUE),
      No_IDU_pct = ifelse(No_IDU_denom > 0, round(No_IDU_n/No_IDU_denom*100,1), NA),
      
      Overall_n = sum(.data[[var]] == "Yes", na.rm = TRUE),
      Overall_denom = n(),
      Overall_pct = round(Overall_n/Overall_denom*100,1),
      
      .groups = "drop"
    ) %>%
    arrange(city, year, street_sw_bin)
  
  # Name the sheet as first 10 characters of variable
  sheet_name <- substr(var, 1, 25)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, summary_df)
}

# Save workbook
saveWorkbook(wb, "IDU_stratified_summary.xlsx", overwrite = TRUE)
cat("Saved all summaries to IDU_stratified_summary.xlsx\n")

## idu over time

sw_combined_clean <- readRDS("sw_combined_clean.rds")

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    idu_ever_3cat = case_when(
      idu_ever_3cat %in% c("Yes", "1") ~ "Yes",
      idu_ever_3cat %in% c("No", "0") ~ "No",
      TRUE ~ NA_character_
    )
  )

city_prev <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(idu_ever_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(idu_ever_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(idu_ever_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "City",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

overall_prev <- sw_combined_clean %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(idu_ever_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(idu_ever_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(idu_ever_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "Overall",
    city = "All cities",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

idu_prev <- bind_rows(city_prev, overall_prev) %>%
  arrange(level, city, year)

wb <- createWorkbook()
addWorksheet(wb, "idu_prevalence")
writeData(wb, "idu_prevalence", idu_prev)
saveWorkbook(wb, "idu_prevalence_by_city_year.xlsx", overwrite = TRUE)

## hiv prevalence tables

# Load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# Keep only selected cities
cities_keep <- c("Cherkasy","Dnipro", "Kropyvnytskyi","Kyiv", "Mariupol","Odesa")

sw_combined_clean <- sw_combined_clean %>%
  filter(city %in% cities_keep)

# Clean variables
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    idu_ever_3cat = factor(idu_ever_3cat, levels = c("No", "Yes")),
    street_sw_bin = factor(street_sw_bin, levels = c("No", "Yes")),
    hiv = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    )
  )

# Helper function to return "n/N (pct)"
calc_str <- function(x) {
  N <- sum(!is.na(x))
  n <- sum(x == 1, na.rm = TRUE)
  if (N == 0) return(NA_character_)
  paste0(n, "/", N, " (", round(n/N*100, 1), "%)")
}

# Create HIV table (by city)
hiv_table <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(
    Overall = calc_str(hiv),

    Street = calc_str(hiv[street_sw_bin == "Yes"]),
    Indoor = calc_str(hiv[street_sw_bin == "No"]),

    IDU = calc_str(hiv[idu_ever_3cat == "Yes"]),
    Non_IDU = calc_str(hiv[idu_ever_3cat == "No"]),

    Street_IDU = calc_str(hiv[street_sw_bin == "Yes" & idu_ever_3cat == "Yes"]),
    Street_non_IDU = calc_str(hiv[street_sw_bin == "Yes" & idu_ever_3cat == "No"]),

    Indoor_IDU = calc_str(hiv[street_sw_bin == "No" & idu_ever_3cat == "Yes"]),
    Indoor_non_IDU = calc_str(hiv[street_sw_bin == "No" & idu_ever_3cat == "No"]),

    .groups = "drop"
  )

# Create overall table (all cities combined)
overall_table <- sw_combined_clean %>%
  group_by(year) %>%
  summarise(
    Overall = calc_str(hiv),

    Street = calc_str(hiv[street_sw_bin == "Yes"]),
    Indoor = calc_str(hiv[street_sw_bin == "No"]),

    IDU = calc_str(hiv[idu_ever_3cat == "Yes"]),
    Non_IDU = calc_str(hiv[idu_ever_3cat == "No"]),

    Street_IDU = calc_str(hiv[street_sw_bin == "Yes" & idu_ever_3cat == "Yes"]),
    Street_non_IDU = calc_str(hiv[street_sw_bin == "Yes" & idu_ever_3cat == "No"]),

    Indoor_IDU = calc_str(hiv[street_sw_bin == "No" & idu_ever_3cat == "Yes"]),
    Indoor_non_IDU = calc_str(hiv[street_sw_bin == "No" & idu_ever_3cat == "No"]),

    .groups = "drop"
  )

# Create Excel workbook
wb <- createWorkbook()

# Write one sheet per city
for (ct in cities_keep) {
  
  df_city <- hiv_table %>%
    filter(city == ct) %>%
    select(-city)
  
  sheet_name <- substr(ct, 1, 31)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df_city)
}

# Add overall sheet
addWorksheet(wb, "Overall")
writeData(wb, "Overall", overall_table)

# Save workbook
saveWorkbook(wb, "HIV_prevalence_by_city.xlsx", overwrite = TRUE)

## idu 12m

# Load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    idu_12m_3cat = case_when(
      idu_12m_3cat %in% c("Yes", "1") ~ "Yes",
      idu_12m_3cat %in% c("No", "0") ~ "No",
      TRUE ~ NA_character_
    )
  )

city_prev <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(idu_12m_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(idu_12m_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(idu_12m_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "City",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

overall_prev <- sw_combined_clean %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(idu_12m_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(idu_12m_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(idu_12m_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "Overall",
    city = "All cities",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

idu_12m_prev <- bind_rows(city_prev, overall_prev) %>%
  arrange(level, city, year)

wb <- createWorkbook()
addWorksheet(wb, "idu_12m_prevalence")
writeData(wb, "idu_12m_prevalence", idu_12m_prev)
saveWorkbook(wb, "idu_12m_prevalence_by_city_year.xlsx", overwrite = TRUE)

## ngo access over time

sw_combined_clean <- readRDS("sw_combined_clean.rds")

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ngo_access_lifetime_3cat = case_when(
      ngo_access_lifetime_3cat %in% c("Yes", "1") ~ "Yes",
      ngo_access_lifetime_3cat %in% c("No", "0") ~ "No",
      TRUE ~ NA_character_
    )
  )

city_prev <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(ngo_access_lifetime_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(ngo_access_lifetime_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(ngo_access_lifetime_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "City",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

overall_prev <- sw_combined_clean %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(ngo_access_lifetime_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(ngo_access_lifetime_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(ngo_access_lifetime_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "Overall",
    city = "All cities",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

ngo_prev <- bind_rows(city_prev, overall_prev) %>%
  arrange(level, city, year)

wb <- createWorkbook()
addWorksheet(wb, "ngo_prevalence")
writeData(wb, "ngo_prevalence", ngo_prev)
saveWorkbook(wb, "ngo_prevalence_by_city_year.xlsx", overwrite = TRUE)

# prevalence by region

# Add "Overall" pseudo-region
sw_combined_clean <- bind_rows(
  sw_combined_clean,
  sw_combined_clean %>% mutate(ukraine_region_4cat = "Overall")
)

sw_combined_clean <- bind_rows(
  sw_combined_clean,
  sw_combined_clean %>% mutate(street_sw_bin = "Overall")
)

sw_combined_clean <- sw_combined_clean %>%
  mutate(street_sw_bin = factor(street_sw_bin, levels = c("No", "Yes", "Overall")))

wb <- createWorkbook()

for (var in vars) {
  
  cat("Processing:", var, "\n")
  
  summary_df <- sw_combined_clean %>%
    filter(!is.na(.data[[var]]), !is.na(street_sw_bin)) %>%
    group_by(ukraine_region_4cat, year, street_sw_bin) %>%
    summarise(
      IDU_n = sum(.data[[var]] == "Yes" & idu_ever_3cat == "Yes", na.rm = TRUE),
      IDU_denom = sum(idu_ever_3cat == "Yes", na.rm = TRUE),
      IDU_pct = ifelse(IDU_denom > 0, round(IDU_n/IDU_denom*100,1), NA),
      
      No_IDU_n = sum(.data[[var]] == "Yes" & idu_ever_3cat == "No", na.rm = TRUE),
      No_IDU_denom = sum(idu_ever_3cat == "No", na.rm = TRUE),
      No_IDU_pct = ifelse(No_IDU_denom > 0, round(No_IDU_n/No_IDU_denom*100,1), NA),
      
      Overall_n = sum(.data[[var]] == "Yes", na.rm = TRUE),
      Overall_denom = n(),
      Overall_pct = round(Overall_n/Overall_denom*100,1),
      
      .groups = "drop"
    ) %>%
    arrange(ukraine_region_4cat, year, street_sw_bin)
  
  sheet_name <- substr(var, 1, 25)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, summary_df)
}

saveWorkbook(wb, "IDU_stratified_summary_by_region.xlsx", overwrite = TRUE)

sw <- readRDS("sw_combined_clean.rds") %>%
  mutate(
    hiv = ifelse(hiv_test_rslt_bin %in% c("Positive","1","Yes"), 1,
          ifelse(hiv_test_rslt_bin %in% c("Negative","0","No"), 0, NA)),
    ngo_client = ifelse(ngo_client_lifetime %in% c("Yes","1"), 1,
                 ifelse(ngo_client_lifetime %in% c("No","0"), 0, NA)),
    ngo_condom = ifelse(ngo_condom_rec_bin %in% c("Yes","1"), 1,
                 ifelse(ngo_condom_rec_bin %in% c("No","0"), 0, NA)),
    condom_access = ifelse(condom_access_12m %in% c("Yes","1"), 1,
                    ifelse(condom_access_12m %in% c("No","0"), 0, NA))
  )

df <- sw %>%
  group_by(year) %>%
  summarise(
    hiv = mean(hiv, na.rm=TRUE),
    ngo_client = mean(ngo_client, na.rm=TRUE),
    ngo_condom = mean(ngo_condom, na.rm=TRUE),
    condom_access = mean(condom_access, na.rm=TRUE),
    .groups="drop"
  ) %>%
  pivot_longer(-year, names_to="indicator", values_to="value") %>%
  mutate(value = value*100)

p <- ggplot(df, aes(year, value, color=indicator)) +
  geom_line(linewidth=1) +
  labs(x="Year", y="%", color=NULL)

ggsave("overall_trends.png", p, width=8, height=5, dpi=300)

## hiv outcomes table

# load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

## HIV outcome
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    ),
    
    idu_ever_3cat = as.character(idu_ever_3cat),
    street_sw_bin = as.character(street_sw_bin),
    age_bin = as.character(age_bin)
  )

## function
calc_prev <- function(x) {
  sprintf(
    "%d (%.1f%%)",
    sum(x == 1, na.rm = TRUE),
    mean(x == 1, na.rm = TRUE) * 100
  )
}

## summary function
make_summary <- function(df) {

  df_year <- df %>%
    group_by(year)

  base <- df_year %>%
    summarise(
      N = n(),
      hiv_prev = calc_prev(hiv_test_rslt_bin),

      tested_lifetime = sprintf(
        "%d (%.1f%%)",
        sum(hiv_tested_lifetime_3cat == "Yes", na.rm = TRUE),
        mean(hiv_tested_lifetime_3cat == "Yes", na.rm = TRUE) * 100
      ),

      tested_12m = sprintf(
        "%d (%.1f%%)",
        sum(hiv_tested_12m_3cat == "Yes", na.rm = TRUE),
        mean(hiv_tested_12m_3cat == "Yes", na.rm = TRUE) * 100
      ),

      aware = sprintf(
        "%d (%.1f%%)",
        sum(hiv_positive_aware == "HIV-aware", na.rm = TRUE),
        mean(hiv_positive_aware == "HIV-aware", na.rm = TRUE) * 100
      ),
      .groups = "drop"
    )

  idu <- df %>%
    filter(idu_ever_3cat == "Yes") %>%
    group_by(year) %>%
    summarise(
      hiv_idu = calc_prev(hiv_test_rslt_bin),
      .groups = "drop"
    )

  street <- df %>%
    filter(street_sw_bin == "Yes") %>%
    group_by(year) %>%
    summarise(
      hiv_street = calc_prev(hiv_test_rslt_bin),
      .groups = "drop"
    )

  u25 <- df %>%
    filter(age_bin == "No") %>%
    group_by(year) %>%
    summarise(
      hiv_u25 = calc_prev(hiv_test_rslt_bin),
      .groups = "drop"
    )

  art <- df %>%
    filter(hiv_test_rslt_bin == 1) %>%
    group_by(year) %>%
    summarise(
      art_current = sprintf(
        "%d (%.1f%%)",
        sum(art_current_3cat == "Yes", na.rm = TRUE),
        mean(art_current_3cat == "Yes", na.rm = TRUE) * 100
      ),
      .groups = "drop"
    )

  base %>%
    left_join(art, by = "year") %>%
    left_join(idu, by = "year") %>%
    left_join(street, by = "year") %>%
    left_join(u25, by = "year")
}

## tables
year_table <- make_summary(sw_combined_clean) %>%
  mutate(year = as.character(year))

overall_table <- sw_combined_clean %>%
  summarise(
    year = "Overall",
    N = n(),

    hiv_prev = calc_prev(hiv_test_rslt_bin),

    tested_lifetime = sprintf(
      "%d (%.1f%%)",
      sum(hiv_tested_lifetime_3cat == "Yes", na.rm = TRUE),
      mean(hiv_tested_lifetime_3cat == "Yes", na.rm = TRUE) * 100
    ),

    tested_12m = sprintf(
      "%d (%.1f%%)",
      sum(hiv_tested_12m_3cat == "Yes", na.rm = TRUE),
      mean(hiv_tested_12m_3cat == "Yes", na.rm = TRUE) * 100
    ),

    aware = sprintf(
      "%d (%.1f%%)",
      sum(hiv_positive_aware == "HIV-aware", na.rm = TRUE),
      mean(hiv_positive_aware == "HIV-aware", na.rm = TRUE) * 100
    ),

    art_current = {
      sub <- sw_combined_clean %>% filter(hiv_test_rslt_bin == 1)
      sprintf(
        "%d (%.1f%%)",
        sum(sub$art_current_3cat == "Yes", na.rm = TRUE),
        mean(sub$art_current_3cat == "Yes", na.rm = TRUE) * 100
      )
    },

    hiv_idu = {
      sub <- sw_combined_clean %>% filter(idu_ever_3cat == "Yes")
      sprintf(
        "%d (%.1f%%)",
        sum(sub$hiv_test_rslt_bin == 1, na.rm = TRUE),
        mean(sub$hiv_test_rslt_bin == 1, na.rm = TRUE) * 100
      )
    },

    hiv_street = {
      sub <- sw_combined_clean %>% filter(street_sw_bin == "Yes")
      sprintf(
        "%d (%.1f%%)",
        sum(sub$hiv_test_rslt_bin == 1, na.rm = TRUE),
        mean(sub$hiv_test_rslt_bin == 1, na.rm = TRUE) * 100
      )
    },

    hiv_u25 = {
      sub <- sw_combined_clean %>% filter(age_bin == "No")
      sprintf(
        "%d (%.1f%%)",
        sum(sub$hiv_test_rslt_bin == 1, na.rm = TRUE),
        mean(sub$hiv_test_rslt_bin == 1, na.rm = TRUE) * 100
      )
    }
  )

## save
final_table <- bind_rows(year_table, overall_table)

write.csv(final_table, "hiv_summary_table.csv", row.names = FALSE)

## sex work characteristics over time

sw_combined_clean <- readRDS("sw_combined_clean.rds")

table(sw_combined_clean$year, sw_combined_clean$violence_rape_12m_3cat, useNA = "ifany")
table(sw_combined_clean$year, sw_combined_clean$violence_rape_ever, useNA = "ifany")
table(sw_combined_clean$violence_rape_12m_3cat, sw_combined_clean$violence_rape_ever, useNA = "ifany")
prop.table(table(sw_combined_clean$sw_partners_clients_30d_3cat, sw_combined_clean$year), margin = 2)
table(sw_combined_clean$ngo_syringe_12m_bin[sw_combined_clean$idu_ever_3cat == "Yes"])

calc_prev <- function(x) {
  denom <- sum(!is.na(x))
  num <- sum(x == "Yes", na.rm = TRUE)
  sprintf("%d (%.1f%%)", num, (num / denom) * 100)
}

calc_prev_true <- function(x) {
  denom <- sum(!is.na(x))
  num <- sum(x, na.rm = TRUE)
  sprintf("%d (%.1f%%)", num, (num / denom) * 100)
}

make_sw_table <- function(df) {

  df %>%
    group_by(year) %>%
    summarise(
      N = n(),

      age_under25 = calc_prev_true(age_bin == "No"),
      street_sw = calc_prev(street_sw_bin),
      idu = calc_prev(idu_ever_3cat),
      underage_first_sw = calc_prev(underage_first_sw_bin),
      rape_ever = calc_prev(violence_rape_ever),
      rape_12m = calc_prev(violence_rape_12m_3cat),
      beaten_ever = calc_prev(violence_beaten_ever),
      ngo_client_lifetime = calc_prev(ngo_client_lifetime_3cat),
      condom_access_12m = calc_prev(condom_access_12m_3cat),
      ngo_condom_rec = calc_prev(ngo_condom_rec_bin),

      ngo_syringe_12m = calc_prev(ngo_syringe_12m_bin[idu_ever_3cat == "Yes"]),

      high_clients_30d = calc_prev_true(as.character(sw_partners_clients_30d_3cat) == "50+"),

      used_syringe_last_3cat = calc_prev(used_syringe_last_3cat[idu_ever_3cat == "Yes"]),
      client_condom_lastsex_3cat = calc_prev(client_condom_lastsex_3cat),

      .groups = "drop"
    )
}

year_table <- make_sw_table(sw_combined_clean) %>%
  mutate(year = as.character(year))

overall_table <- sw_combined_clean %>%
  summarise(
    year = "Overall",
    N = n(),

    age_under25 = calc_prev_true(age_bin == "No"),
    street_sw = calc_prev(street_sw_bin),
    idu = calc_prev(idu_ever_3cat),
    underage_first_sw = calc_prev(underage_first_sw_bin),
    rape_ever = calc_prev(violence_rape_ever),
    rape_12m = calc_prev(violence_rape_12m_3cat),
    beaten_ever = calc_prev(violence_beaten_ever),
    ngo_client_lifetime = calc_prev(ngo_client_lifetime_3cat),
    condom_access_12m = calc_prev(condom_access_12m_3cat),
    ngo_condom_rec = calc_prev(ngo_condom_rec_bin),

    ngo_syringe_12m = calc_prev(ngo_syringe_12m_bin[idu_ever_3cat == "Yes"]),

    high_clients_30d = calc_prev_true(as.character(sw_partners_clients_30d_3cat) == "50+"),

    used_syringe_last_3cat = calc_prev(used_syringe_last_3cat[idu_ever_3cat == "Yes"]),
    client_condom_lastsex_3cat = calc_prev(client_condom_lastsex_3cat)
  )
  

final_table <- bind_rows(year_table, overall_table)

write.csv(final_table, "sw_characteristics_table.csv", row.names = FALSE)

## stratifications
sw_combined_clean <- readRDS("sw_combined_clean.rds")

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    year = as.character(year),

    hiv_test_rslt_bin = case_when(
      hiv_test_rslt_bin %in% c(1, "1", "Positive", "HIV+") ~ 1,
      hiv_test_rslt_bin %in% c(2, "2", "Negative", "HIV-") ~ 0,
      TRUE ~ NA_real_
    ),

    idu_ever_3cat = as.character(idu_ever_3cat),
    street_sw_bin = as.character(street_sw_bin),
    ngo_client_lifetime_3cat = as.character(ngo_client_lifetime_3cat),
    hiv_positive_aware = as.character(hiv_positive_aware),
    art_current_3cat = as.character(art_current_3cat)
  )

## helper functions
calc_prev_bin <- function(x) {
  sprintf("%d (%.1f%%)",
          sum(x == 1, na.rm = TRUE),
          mean(x == 1, na.rm = TRUE) * 100)
}

calc_prev_cat <- function(x) {
  sprintf("%d (%.1f%%)",
          sum(x == "Yes", na.rm = TRUE),
          mean(x == "Yes", na.rm = TRUE) * 100)
}

calc_prev_aware <- function(x) {
  sprintf("%d (%.1f%%)",
          sum(x == "HIV-aware", na.rm = TRUE),
          mean(x == "HIV-aware", na.rm = TRUE) * 100)
}

## base dataset
overall_data <- sw_combined_clean %>%
  filter(!is.na(hiv_test_rslt_bin))

## stratified datasets
idu_data        <- overall_data %>% filter(idu_ever_3cat == "Yes")
table(idu_data$hiv_test_rslt_bin)
table(idu_data$year[idu_data$hiv_test_rslt_bin == 1], idu_data$art_current_3cat[idu_data$hiv_test_rslt_bin == 1])

no_idu_data     <- overall_data %>% filter(idu_ever_3cat == "No")
table(no_idu_data$year[no_idu_data$hiv_test_rslt_bin == 1], no_idu_data$art_current_3cat[no_idu_data$hiv_test_rslt_bin == 1])

street_data     <- overall_data %>% filter(street_sw_bin == "Yes")
no_street_data  <- overall_data %>% filter(street_sw_bin == "No")

ngo_data        <- overall_data %>% filter(ngo_client_lifetime_3cat == "Yes")
no_ngo_data     <- overall_data %>% filter(ngo_client_lifetime_3cat == "No")

## yearly summary (IMPORTANT: all HIV outcomes included)
make_summary <- function(df) {
  df %>%
    group_by(year) %>%
    summarise(
      N = n(),

      hiv_prev = calc_prev_bin(hiv_test_rslt_bin),

      tested_lifetime = calc_prev_cat(hiv_tested_lifetime_3cat),
      tested_12m = calc_prev_cat(hiv_tested_12m_3cat),
      aware = calc_prev_aware(hiv_positive_aware),

      art_current = calc_prev_cat(ifelse(hiv_test_rslt_bin == 1, art_current_3cat, NA)),

      hiv_idu = calc_prev_bin(ifelse(idu_ever_3cat == "Yes", hiv_test_rslt_bin, NA)),
      hiv_street = calc_prev_bin(ifelse(street_sw_bin == "Yes", hiv_test_rslt_bin, NA)),
      hiv_u25 = calc_prev_bin(ifelse(age_bin == "No", hiv_test_rslt_bin, NA)),

      .groups = "drop"
    )
}

## tables
idu_table       <- make_summary(idu_data) %>% mutate(group = "IDU")

no_idu_table    <- make_summary(no_idu_data) %>% mutate(group = "No IDU")

street_table    <- make_summary(street_data) %>% mutate(group = "Street")
no_street_table <- make_summary(no_street_data) %>% mutate(group = "Non-street")

ngo_table       <- make_summary(ngo_data) %>% mutate(group = "NGO client")
no_ngo_table    <- make_summary(no_ngo_data) %>% mutate(group = "No NGO")

## overall rows
make_overall <- function(df, label) {
  df %>%
    summarise(
      year = "Overall",
      N = n(),
      hiv_prev = calc_prev_bin(hiv_test_rslt_bin),
      tested_lifetime = calc_prev_cat(hiv_tested_lifetime_3cat),
      tested_12m = calc_prev_cat(hiv_tested_12m_3cat),
      aware = calc_prev_aware(hiv_positive_aware),

      art_current = calc_prev_cat(ifelse(hiv_test_rslt_bin == 1, art_current_3cat, NA)),

      hiv_idu = calc_prev_bin(ifelse(idu_ever_3cat == "Yes", hiv_test_rslt_bin, NA)),
      hiv_street = calc_prev_bin(ifelse(street_sw_bin == "Yes", hiv_test_rslt_bin, NA)),
      hiv_u25 = calc_prev_bin(ifelse(age_bin == "No", hiv_test_rslt_bin, NA))      
    ) %>%
    mutate(group = label)
}

overall_strata <- bind_rows(
  make_overall(idu_data, "IDU"),
  make_overall(no_idu_data, "No IDU"),
  make_overall(street_data, "Street"),
  make_overall(no_street_data, "Non-street"),
  make_overall(ngo_data, "NGO client"),
  make_overall(no_ngo_data, "No NGO")
)

## final output
stratified_hiv_table <- bind_rows(
  idu_table, no_idu_table,
  street_table, no_street_table,
  ngo_table, no_ngo_table,
  overall_strata
)

write.csv(stratified_hiv_table, "hiv_stratified_table.csv", row.names = FALSE)
