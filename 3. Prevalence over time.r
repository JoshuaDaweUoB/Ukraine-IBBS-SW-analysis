## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales, openxlsx, readr, magick)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# Load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# proportion of FSWs in each compartment
sw_combined_clean <- sw_combined_clean[sw_combined_clean$idu_ever_bin != "Missing / Unknown", ]

years_cats <- c("0-3", "4-9", "10+")

for (i in years_cats) {

# restrict to new SWs
df <- sw_combined_clean %>%
  filter(years_in_sw_3cat == i)

table(df$years_in_sw_3cat)

# Create contingency table
tab <- table(df$idu_ever_bin, df$street_sw_bin)

# Total N
N <- sum(tab)

# calculate proportions
omega_nonIDU_nonSB <- tab["No", "No"] / N
omega_nonIDU_SB    <- tab["No", "Yes"] / N
omega_IDU_nonSB    <- tab["Yes", "No"] / N
omega_IDU_SB       <- tab["Yes", "Yes"] / N

omega <- c(
  omega_nonIDU_nonSB = omega_nonIDU_nonSB,
  omega_nonIDU_SB    = omega_nonIDU_SB,
  omega_IDU_nonSB    = omega_IDU_nonSB,
  omega_IDU_SB       = omega_IDU_SB
)

# results
print(omega)
}

# years in SW 
sw_combined_clean %>%
  group_by(idu_ever_bin) %>%
  summarise(
    n = sum(!is.na(years_in_sw)),
    mean = mean(years_in_sw, na.rm = TRUE),
    median = median(years_in_sw, na.rm = TRUE),
    sd = sd(years_in_sw, na.rm = TRUE),
    var = var(years_in_sw, na.rm = TRUE),
    min = min(years_in_sw, na.rm = TRUE),
    max = max(years_in_sw, na.rm = TRUE),
    .groups = "drop"
  )

## function to calculate exit rate
exit_rate <- function(mean_years, var) {
  1 / (mean_years + (var/mean_years))
}

## values
idu_mean <- 9.3
idu_var <- 35.9
no_idu_mean <- 6.8
no_idu_var <- 25.8

## run function
idu_exit <- exit_rate(idu_mean, idu_var)
idu_exit
no_idu_exit <- exit_rate(no_idu_mean, no_idu_var)
no_idu_exit

# rate of moving from younger to older FSW
sw_combined_clean %>%
  group_by(idu_ever_bin) %>%
  summarise(
    mean = mean(age_first_sw_numeric, na.rm = TRUE),
    .groups = "drop")

young_to_old <- function(age_first_sw) {
  25 - age_first_sw
}

idu_mean <- 22.7
non_idu_mean <- 21.6

idu_yto <- young_to_old(idu_mean)
idu_yto

non_idu_yto <- young_to_old(non_idu_mean)
non_idu_yto

# years in SW 
sw_combined_clean %>%
  group_by(idu_ever_bin) %>%
  summarise(
    mean = mean(sw_partners_clients_30d, na.rm = TRUE),
    .groups = "drop"
  )

## rate moving from non-NGO client to NGO clients

# rate of ngo access by year
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    years_adj = ifelse(years_in_sw == 0, 0.5, years_in_sw),
    ngo_ever = ifelse(ngo_client_lifetime_bin == "Yes", 1,
                 ifelse(ngo_client_lifetime_bin == "No", 0, NA)),
    rate_ngo = ngo_ever/years_adj,

  )

sw_combined_clean %>%
  summarise(
    p_ngo = mean(ngo_ever, na.rm = TRUE),
    mean_years = mean(years_in_sw, na.rm = TRUE),
    lambda_ngo = -log(1 - p_ngo) / mean_years,
    )

lambda_func <- function(p, years, var_p, var_years) {
  
  lambda <- -log(1 - p) / years
  
  var_lambda <- (1 / (years * (1 - p)))^2 * var_p +
                (log(1 - p) / years^2)^2 * var_years  
  
  ci <- c(
    lambda - 1.96 * sqrt(var_lambda),
    lambda + 1.96 * sqrt(var_lambda)
  )

  return(list(lambda = lambda, ci = ci))
}

# factor
sw_combined_clean <- sw_combined_clean %>%
  mutate(hiv_test_rslt_bin = case_when(
          hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ "Yes",
          hiv_test_rslt_bin %in% c("Negative","0","No") ~ "No",
          TRUE ~ NA_character_),
        years_in_sw_3cat = as.factor(years_in_sw_3cat))

# variables to summarise
vars <- c(
  "condom_access_12m_bin", "client_condom_lastsex_bin", "ngo_client_lifetime",
  "ngo_condom_rec_bin", "ngo_syringe_12m_bin", "hiv_test_rslt_bin", "years_in_sw_3cat",
  "city_travel_12m_cat", "street_sw_bin", "alcohol_30d_bin", "used_syringe_last_bin",
  "violence_any_ever_bin", "violence_forced_free_ever_bin", "violence_forced_perv_ever_bin",
  "violence_forced_any_ever_bin", "violence_forced_any_12m_bin", "violence_forced_inject_ever_bin",
  "violence_rape_ever_bin", "violence_beaten_ever_bin", "violence_physical_abuse_ever_bin",
  "violence_police_bin", "violence_pimp_bin", "violence_support_ngo_bin", "age_bin", "underage_first_sw_bin"
)

## some sort of shit graph thing

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
    
    idu_ever_bin = as.character(idu_ever_bin),
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
        sum(hiv_tested_lifetime_bin == "Yes", na.rm = TRUE),
        mean(hiv_tested_lifetime_bin == "Yes", na.rm = TRUE) * 100
      ),

      tested_12m = sprintf(
        "%d (%.1f%%)",
        sum(hiv_tested_12m_bin == "Yes", na.rm = TRUE),
        mean(hiv_tested_12m_bin == "Yes", na.rm = TRUE) * 100
      ),

      aware = sprintf(
        "%d (%.1f%%)",
        sum(hiv_positive_aware == "HIV-aware", na.rm = TRUE),
        mean(hiv_positive_aware == "HIV-aware", na.rm = TRUE) * 100
      ),
      .groups = "drop"
    )

  idu <- df %>%
    filter(idu_ever_bin == "Yes") %>%
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
        sum(art_current_bin == "Yes", na.rm = TRUE),
        mean(art_current_bin == "Yes", na.rm = TRUE) * 100
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
      sum(hiv_tested_lifetime_bin == "Yes", na.rm = TRUE),
      mean(hiv_tested_lifetime_bin == "Yes", na.rm = TRUE) * 100
    ),

    tested_12m = sprintf(
      "%d (%.1f%%)",
      sum(hiv_tested_12m_bin == "Yes", na.rm = TRUE),
      mean(hiv_tested_12m_bin == "Yes", na.rm = TRUE) * 100
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
        sum(sub$art_current_bin == "Yes", na.rm = TRUE),
        mean(sub$art_current_bin == "Yes", na.rm = TRUE) * 100
      )
    },

    hiv_idu = {
      sub <- sw_combined_clean %>% filter(idu_ever_bin == "Yes")
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
      idu = calc_prev(idu_ever_bin),
      underage_first_sw = calc_prev(underage_first_sw_bin),
      violence_any = calc_prev(violence_any_ever_bin),
      forced_sex_ever = calc_prev(violence_forced_any_ever_bin),
      forced_sex_12m = calc_prev(violence_forced_any_12m_bin),
      rape_ever = calc_prev(violence_rape_ever_bin),
      beaten_ever = calc_prev(violence_beaten_ever_bin),
      ngo_client_lifetime = calc_prev(ngo_client_lifetime_bin),
      condom_access_12m = calc_prev(condom_access_12m_bin),
      ngo_condom_rec = calc_prev(ngo_condom_rec_bin),

      ngo_syringe_12m = calc_prev(ngo_syringe_12m_bin[idu_ever_bin == "Yes"]),

      high_clients_30d = calc_prev_true(as.character(sw_partners_clients_30d_3cat) == "50+"),

      used_syringe_last = calc_prev(used_syringe_last_bin[idu_ever_bin == "Yes"]),
      client_condom_lastsex_bin = calc_prev(client_condom_lastsex_bin),

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
      idu = calc_prev(idu_ever_bin),
      underage_first_sw = calc_prev(underage_first_sw_bin),
      violence_any = calc_prev(violence_any_ever_bin),
      forced_sex_ever = calc_prev(violence_forced_any_ever_bin),
      forced_sex_12m = calc_prev(violence_forced_any_12m_bin),
      rape_ever = calc_prev(violence_rape_ever_bin),
      beaten_ever = calc_prev(violence_beaten_ever_bin),
      ngo_client_lifetime = calc_prev(ngo_client_lifetime_bin),
      condom_access_12m = calc_prev(condom_access_12m_bin),
      ngo_condom_rec = calc_prev(ngo_condom_rec_bin),

      ngo_syringe_12m = calc_prev(ngo_syringe_12m_bin[idu_ever_bin == "Yes"]),

      high_clients_30d = calc_prev_true(as.character(sw_partners_clients_30d_3cat) == "50+"),

      used_syringe_last = calc_prev(used_syringe_last_bin[idu_ever_bin == "Yes"]),
      client_condom_lastsex_bin = calc_prev(client_condom_lastsex_bin),
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

    idu_ever_bin = as.character(idu_ever_bin),
    street_sw_bin = as.character(street_sw_bin),
    ngo_client_lifetime_bin = as.character(ngo_client_lifetime_bin),
    hiv_positive_aware = as.character(hiv_positive_aware),
    art_current_bin = as.character(art_current_bin)
  )

## functions
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
idu_data        <- overall_data %>% filter(idu_ever_bin == "Yes")

no_idu_data     <- overall_data %>% filter(idu_ever_bin == "No")

street_data     <- overall_data %>% filter(street_sw_bin == "Yes")
no_street_data  <- overall_data %>% filter(street_sw_bin == "No")

ngo_data        <- overall_data %>% filter(ngo_client_lifetime_bin == "Yes")
no_ngo_data     <- overall_data %>% filter(ngo_client_lifetime_bin == "No")

## yearly summary
make_summary <- function(df) {
  df %>%
    group_by(year) %>%
    summarise(
      N = n(),

      hiv_prev = calc_prev_bin(hiv_test_rslt_bin),

      tested_lifetime = calc_prev_cat(hiv_tested_lifetime_bin),
      tested_12m = calc_prev_cat(hiv_tested_12m_bin),
      aware = calc_prev_aware(hiv_positive_aware),

      art_current = calc_prev_cat(ifelse(hiv_test_rslt_bin == 1, art_current_bin, NA)),

      hiv_idu = calc_prev_bin(ifelse(idu_ever_bin == "Yes", hiv_test_rslt_bin, NA)),
      hiv_street = calc_prev_bin(ifelse(street_sw_bin == "Yes", hiv_test_rslt_bin, NA)),
      hiv_u25 = calc_prev_bin(ifelse(age_bin == "No", hiv_test_rslt_bin, NA)),

      .groups = "drop"
    )
}

## tables
idu_table_hiv       <- make_summary(idu_data) %>% mutate(group = "IDU")

no_idu_table_hiv    <- make_summary(no_idu_data) %>% mutate(group = "No IDU")

street_table_hiv    <- make_summary(street_data) %>% mutate(group = "Street")
no_street_table_hiv <- make_summary(no_street_data) %>% mutate(group = "Non-street")

ngo_table_hiv       <- make_summary(ngo_data) %>% mutate(group = "NGO client")
no_ngo_table_hiv    <- make_summary(no_ngo_data) %>% mutate(group = "No NGO")

## overall rows
make_overall <- function(df, label) {
  df %>%
    summarise(
      year = "Overall",
      N = n(),
      hiv_prev = calc_prev_bin(hiv_test_rslt_bin),
      tested_lifetime = calc_prev_cat(hiv_tested_lifetime_bin),
      tested_12m = calc_prev_cat(hiv_tested_12m_bin),
      aware = calc_prev_aware(hiv_positive_aware),

      art_current = calc_prev_cat(ifelse(hiv_test_rslt_bin == 1, art_current_bin, NA)),

      hiv_idu = calc_prev_bin(ifelse(idu_ever_bin == "Yes", hiv_test_rslt_bin, NA)),
      hiv_street = calc_prev_bin(ifelse(street_sw_bin == "Yes", hiv_test_rslt_bin, NA)),
      hiv_u25 = calc_prev_bin(ifelse(age_bin == "No", hiv_test_rslt_bin, NA))      
    ) %>%
    mutate(group = label)
}

overall_strata_hiv <- bind_rows(
  make_overall(idu_data, "IDU"),
  make_overall(no_idu_data, "No IDU"),
  make_overall(street_data, "Street"),
  make_overall(no_street_data, "Non-street"),
  make_overall(ngo_data, "NGO client"),
  make_overall(no_ngo_data, "No NGO")
)

## final output
stratified_hiv_table <- bind_rows(
  idu_table_hiv, no_idu_table_hiv,
  street_table_hiv, no_street_table_hiv,
  ngo_table_hiv, no_ngo_table_hiv,
  overall_strata_hiv
)

write.csv(stratified_hiv_table, "hiv_stratified_table.csv", row.names = FALSE)

## overall rows
make_overall_characteristics <- function(df, label) {
  df %>%
    summarise(
      year = "Overall",
      N = n(),


      age_under25 = calc_prev_true(age_bin == "No"),
      street_sw = calc_prev(street_sw_bin),
      idu = calc_prev(idu_ever_bin),
      underage_first_sw = calc_prev(underage_first_sw_bin),
      rape_ever = calc_prev(violence_rape_ever),
      forced_sex_12m = calc_prev(violence_forced_any_12m_bin),
      forced_sex_ever = calc_prev(violence_forced_any_ever_bin),
      beaten_ever = calc_prev(violence_beaten_ever),
      ngo_client_lifetime = calc_prev(ngo_client_lifetime_bin),
      condom_access_12m = calc_prev(condom_access_12m_bin),
      ngo_condom_rec = calc_prev(ngo_condom_rec_bin),

      ngo_syringe_12m = calc_prev(
        ifelse(idu_ever_bin == "Yes", ngo_syringe_12m_bin, NA)
      ),

      high_clients_30d = calc_prev_true(
        as.character(sw_partners_clients_30d_3cat) == "50+"
      ),

      used_syringe_last_bin = calc_prev(
        ifelse(idu_ever_bin == "Yes", used_syringe_last_bin, NA)
      ),

      client_condom_lastsex_bin = calc_prev(client_condom_lastsex_bin)
    ) %>%
    mutate(group = label)
}

## final output
sw_stratified <- bind_rows(
  idu_sw_table,
  no_idu_sw_table,
  street_sw_table,
  no_street_sw_table,
  ngo_sw_table,
  no_ngo_sw_table
)

sw_overall <- bind_rows(
  make_overall_characteristics(idu_data, "IDU"),
  make_overall_characteristics(no_idu_data, "No IDU"),
  make_overall_characteristics(street_data, "Street"),
  make_overall_characteristics(no_street_data, "Non-street"),
  make_overall_characteristics(ngo_data, "NGO client"),
  make_overall_characteristics(no_ngo_data, "No NGO")
)

final_table <- bind_rows(
  sw_stratified,
  sw_overall
)

write.csv(final_table, "stratified_characteristics_table.csv", row.names = FALSE)

## figures of trends for IDU

plot_vars <- c(
  "age_under25",
  "street_sw",
  "underage_first_sw",
  "rape_ever",
  "forced_sex_12m",
  "forced_sex_ever",
  "beaten_ever",
  "ngo_client_lifetime",
  "condom_access_12m",
  "ngo_condom_rec",
  "high_clients_30d",
  "client_condom_lastsex_bin",
  "hiv_prev",
  "tested_lifetime",
  "aware",
  "art_current"
)

idu_plot_df <- bind_rows(
  idu_sw_table,
  no_idu_sw_table,
  idu_table_hiv,
  no_idu_table_hiv
) %>%
  mutate(year = as.numeric(as.character(year)))

dir.create("idu_trend_plots", showWarnings = FALSE)

for (v in plot_vars) {

  plot_df <- idu_plot_df %>%
    filter(!is.na(.data[[v]])) %>%
    mutate(
      prev_num = as.numeric(sub(".*\\((.*)%\\).*", "\\1", .data[[v]])),
      p = prev_num / 100,

      n = 100,

      se = sqrt(p * (1 - p) / n),

      ci_low = (p - 1.96 * se) * 100,
      ci_high = (p + 1.96 * se) * 100
    ) %>%
    filter(!is.na(prev_num))

  year_breaks <- plot_df %>%
    filter(!is.na(prev_num)) %>%
    pull(year) %>%
    unique() %>%
    sort()
    
  p <- ggplot(
    plot_df,
    aes(x = year, y = prev_num, color = group, group = group)
  ) +
    geom_ribbon(
      aes(ymin = ci_low, ymax = ci_high, fill = group),
      alpha = 0.15,
      color = NA,
      show.legend = FALSE
    ) +
    geom_line(linewidth = 1.2, na.rm = TRUE) +
    geom_point(size = 3, na.rm = TRUE) +
    scale_x_continuous(
      breaks = year_breaks,
      limits = range(year_breaks)
    ) +
    labs(
      title = paste0(v),      
      x = "Year",
      y = "Prevalence (%)",
      color = NULL
    ) +
    theme_bw()

  ggsave(
    filename = paste0("idu_trend_plots/", v, "_idu_trend.png"),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}

# combine plots
img1 <- image_read("idu_trend_plots/ngo_client_lifetime_idu_trend.png")
img2 <- image_read("idu_trend_plots/street_sw_idu_trend.png")
img3 <- image_read("idu_trend_plots/underage_first_sw_idu_trend.png")
img4 <- image_read("idu_trend_plots/age_under25_idu_trend.png")

img5 <- image_read("idu_trend_plots/forced_sex_12m_idu_trend.png")
img6 <- image_read("idu_trend_plots/forced_sex_ever_idu_trend.png")
img7 <- image_read("idu_trend_plots/rape_ever_idu_trend.png")
img8 <- image_read("idu_trend_plots/beaten_ever_idu_trend.png")

img9 <- image_read("idu_trend_plots/tested_lifetime_idu_trend.png")
img10 <- image_read("idu_trend_plots/hiv_prev_idu_trend.png")
img11 <- image_read("idu_trend_plots/aware_idu_trend.png")
img12 <- image_read("idu_trend_plots/art_current_idu_trend.png")

#characteristics figure
row1 <- image_append(c(img1, img2), stack = FALSE)
row2 <- image_append(c(img3, img4), stack = FALSE)
combined <- image_append(c(row1, row2), stack = TRUE)
image_write(combined, "idu_trend_plots/combined_characteristics.png")

# violence
row1 <- image_append(c(img5, img6), stack = FALSE)
row2 <- image_append(c(img7, img8), stack = FALSE)
combined <- image_append(c(row1, row2), stack = TRUE)
image_write(combined, "idu_trend_plots/combined_violence.png")

# hiv outcomes
row1 <- image_append(c(img9, img10), stack = FALSE)
row2 <- image_append(c(img11, img12), stack = FALSE)
combined <- image_append(c(row1, row2), stack = TRUE)
image_write(combined, "idu_trend_plots/combined_hiv.png")

## figures of trends for street-based

plot_vars <- c(
  "age_under25",
  "idu",
  "underage_first_sw",
  "rape_ever",
  "forced_sex_12m",
  "forced_sex_ever",
  "beaten_ever",
  "ngo_client_lifetime",
  "condom_access_12m",
  "ngo_condom_rec",
  "high_clients_30d",
  "client_condom_lastsex_bin",
  "hiv_prev",
  "tested_lifetime",
  "aware",
  "art_current"
)

street_based_plot_df <- bind_rows(
  street_sw_table,
  no_street_sw_table,
  street_table_hiv,
  no_street_table_hiv
) %>%
  mutate(year = as.numeric(as.character(year)))

dir.create("street_based_trend_plots", showWarnings = FALSE)

for (v in plot_vars) {

  plot_df <- street_based_plot_df %>%
    filter(!is.na(.data[[v]])) %>%
    mutate(
      prev_num = as.numeric(sub(".*\\((.*)%\\).*", "\\1", .data[[v]])),
      p = prev_num / 100,

      n = 100,

      se = sqrt(p * (1 - p) / n),

      ci_low = (p - 1.96 * se) * 100,
      ci_high = (p + 1.96 * se) * 100
    ) %>%
    filter(!is.na(prev_num))

  year_breaks <- plot_df %>%
    filter(!is.na(prev_num)) %>%
    pull(year) %>%
    unique() %>%
    sort()
    
  p <- ggplot(
    plot_df,
    aes(x = year, y = prev_num, color = group, group = group)
  ) +
    geom_ribbon(
      aes(ymin = ci_low, ymax = ci_high, fill = group),
      alpha = 0.15,
      color = NA,
      show.legend = FALSE
    ) +
    geom_line(linewidth = 1.2, na.rm = TRUE) +
    geom_point(size = 3, na.rm = TRUE) +
    scale_x_continuous(
      breaks = year_breaks,
      limits = range(year_breaks)
    ) +
    labs(
    title = paste0(v),      
      x = "Year",
      y = "Prevalence (%)",
      color = NULL
    ) +
    theme_bw()

  ggsave(
    filename = paste0("street_based_trend_plots/", v, "_street_sw_trend.png"),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}

# combine plots
img1 <- image_read("street_based_trend_plots/ngo_client_lifetime_street_sw_trend.png")
img2 <- image_read("street_based_trend_plots/idu_street_sw_trend.png")
img3 <- image_read("street_based_trend_plots/underage_first_sw_street_sw_trend.png")
img4 <- image_read("street_based_trend_plots/age_under25_street_sw_trend.png")

img5 <- image_read("street_based_trend_plots/forced_sex_12m_street_sw_trend.png")
img6 <- image_read("street_based_trend_plots/forced_sex_ever_street_sw_trend.png")
img7 <- image_read("street_based_trend_plots/rape_ever_street_sw_trend.png")
img8 <- image_read("street_based_trend_plots/beaten_ever_street_sw_trend.png")

img9 <- image_read("street_based_trend_plots/tested_lifetime_street_sw_trend.png")
img10 <- image_read("street_based_trend_plots/hiv_prev_street_sw_trend.png")
img11 <- image_read("street_based_trend_plots/aware_street_sw_trend.png")
img12 <- image_read("street_based_trend_plots/art_current_street_sw_trend.png")

#characteristics figure
row1 <- image_append(c(img1, img2), stack = FALSE)
row2 <- image_append(c(img3, img4), stack = FALSE)
combined <- image_append(c(row1, row2), stack = TRUE)
image_write(combined, "street_based_trend_plots/combined_characteristics.png")

# violence
row1 <- image_append(c(img5, img6), stack = FALSE)
row2 <- image_append(c(img7, img8), stack = FALSE)
combined <- image_append(c(row1, row2), stack = TRUE)
image_write(combined, "street_based_trend_plots/combined_violence.png")

# hiv outcomes
row1 <- image_append(c(img9, img10), stack = FALSE)
row2 <- image_append(c(img11, img12), stack = FALSE)
combined <- image_append(c(row1, row2), stack = TRUE)
image_write(combined, "street_based_trend_plots/combined_hiv.png")

## figures of trends for ngo

plot_vars <- c(
  "age_under25",
  "idu",
  "street_sw",
  "underage_first_sw",
  "rape_ever",
  "forced_sex_12m",
  "forced_sex_ever",
  "beaten_ever",
  "ngo_client_lifetime",
  "condom_access_12m",
  "ngo_condom_rec",
  "high_clients_30d",
  "client_condom_lastsex_bin",
  "hiv_prev",
  "tested_lifetime",
  "aware",
  "art_current"
)

ngo_sw_table_plot_df <- bind_rows(
  ngo_sw_table,
  no_ngo_sw_table,
  ngo_table_hiv,
  no_ngo_table_hiv
) %>%
  mutate(year = as.numeric(as.character(year)))

dir.create("ngo_trend_plots", showWarnings = FALSE)

for (v in plot_vars) {

  plot_df <- ngo_sw_table_plot_df %>%
    filter(!is.na(.data[[v]])) %>%
    mutate(
      prev_num = as.numeric(sub(".*\\((.*)%\\).*", "\\1", .data[[v]])),
      p = prev_num / 100,

      n = 100,

      se = sqrt(p * (1 - p) / n),

      ci_low = (p - 1.96 * se) * 100,
      ci_high = (p + 1.96 * se) * 100
    ) %>%
    filter(!is.na(prev_num))

  year_breaks <- plot_df %>%
    filter(!is.na(prev_num)) %>%
    pull(year) %>%
    unique() %>%
    sort()
    
  p <- ggplot(
    plot_df,
    aes(x = year, y = prev_num, color = group, group = group)
  ) +
    geom_ribbon(
      aes(ymin = ci_low, ymax = ci_high, fill = group),
      alpha = 0.15,
      color = NA,
      show.legend = FALSE
    ) +
    geom_line(linewidth = 1.2, na.rm = TRUE) +
    geom_point(size = 3, na.rm = TRUE) +
    scale_x_continuous(
      breaks = year_breaks,
      limits = range(year_breaks)
    ) +
    labs(
      title = paste0(v),
      x = "Year",
      y = "Prevalence (%)",
      color = NULL
    ) +
    theme_bw()

  ggsave(
    filename = paste0("ngo_trend_plots/", v, "_ngo_trend.png"),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}

# combine plots
img1 <- image_read("ngo_trend_plots/ngo_client_lifetime_ngo_trend.png")
img2 <- image_read("ngo_trend_plots/street_sw_ngo_trend.png")
img3 <- image_read("ngo_trend_plots/underage_first_sw_ngo_trend.png")
img4 <- image_read("ngo_trend_plots/age_under25_ngo_trend.png")

img5 <- image_read("ngo_trend_plots/forced_sex_12m_ngo_trend.png")
img6 <- image_read("ngo_trend_plots/forced_sex_ever_ngo_trend.png")
img7 <- image_read("ngo_trend_plots/rape_ever_ngo_trend.png")
img8 <- image_read("ngo_trend_plots/beaten_ever_ngo_trend.png")

img9 <- image_read("ngo_trend_plots/tested_lifetime_ngo_trend.png")
img10 <- image_read("ngo_trend_plots/hiv_prev_ngo_trend.png")
img11 <- image_read("ngo_trend_plots/aware_ngo_trend.png")
img12 <- image_read("ngo_trend_plots/art_current_ngo_trend.png")

#characteristics figure
row1 <- image_append(c(img1, img2), stack = FALSE)
row2 <- image_append(c(img3, img4), stack = FALSE)
combined <- image_append(c(row1, row2), stack = TRUE)
image_write(combined, "ngo_trend_plots/combined_characteristics.png")

# violence
row1 <- image_append(c(img5, img6), stack = FALSE)
row2 <- image_append(c(img7, img8), stack = FALSE)
combined <- image_append(c(row1, row2), stack = TRUE)
image_write(combined, "ngo_trend_plots/combined_violence.png")

# hiv outcomes
row1 <- image_append(c(img9, img10), stack = FALSE)
row2 <- image_append(c(img11, img12), stack = FALSE)
combined <- image_append(c(row1, row2), stack = TRUE)
image_write(combined, "ngo_trend_plots/combined_hiv.png")

