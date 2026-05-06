## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, 
               lubridate, broom, survival, ggplot2, scales, sandwich, lmtest, lme4, 
               broom.mixed, openxlsx) 
               
## set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

## load appended clean data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# factorise adjustment variables
sw_combined_clean <- sw_combined_clean %>%
  mutate(
      years_in_sw_3cat = as.factor(years_in_sw_3cat),
      city = as.factor(city),
      year = as.factor(year),
    )

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    sw_clients_med_vs_low = case_when(
      sw_partners_clients_30d_3cat == "20-49" ~ 1,
      sw_partners_clients_30d_3cat == "0-19" ~ 0,
      TRUE ~ NA_real_
    )
  )

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    sw_clients_high_vs_low = case_when(
      sw_partners_clients_30d_3cat == "50+" ~ 1,
      sw_partners_clients_30d_3cat == "0-19" ~ 0,
      TRUE ~ NA_real_
    )
  )

# exposures
exposures <- c(
  "condom_access_12m_3cat","client_condom_lastsex_3cat","ngo_client_lifetime_3cat",
  "ngo_condom_rec_bin", "ngo_syringe_12m_bin", "ngo_condom_12m",
  "idu_ever_3cat","idu_12m_3cat", "street_sw_bin",
  "sw_clients_med_vs_low", "sw_clients_high_vs_low", "violence_any_ever_3cat",
  "violence_rape_12m_3cat","violence_rape_ever","violence_beaten_ever",
  "violence_physical_abuse_ever","violence_police","violence_pimp"
)

# outcomes
outcomes <- c(
  "hiv_test_rslt_bin", "idu_ever_3cat", "street_sw_bin", "art_current_3cat",
  "violence_any_ever_3cat", "violence_rape_ever", "violence_beaten_ever",
  "client_condom_lastsex_3cat", "ngo_condom_rec_bin", "ngo_client_lifetime_3cat"
)

# binary function
binary_function <- function(data, col_name){
  data %>%
    mutate(
      !!col_name := case_when(
        .data[[col_name]] %in% c("Positive","1","Yes") ~ 1,
        .data[[col_name]] %in% c("Negative","0","No") ~ 0,
        TRUE ~ NA_real_
      )
    )
}

# make outcomes binary
for (i in outcomes) {
  sw_combined_clean <- binary_function(sw_combined_clean, i)
}

# make exposures binary
for (i in exposures) {
  sw_combined_clean <- binary_function(sw_combined_clean, i)
}
table(sw_combined_clean$art_current_3cat)
## overall datasets
subgroup_data <- list(
 overall   = sw_combined_clean,
 idu       = sw_combined_clean %>% filter(idu_ever_3cat == 1),
 no_idu    = sw_combined_clean %>% filter(idu_ever_3cat == 0),
 street    = sw_combined_clean %>% filter(street_sw_bin == 1),
 no_street = sw_combined_clean %>% filter(street_sw_bin == 0),
 ngo       = sw_combined_clean %>% filter(ngo_condom_rec_bin == 1),
 no_ngo    = sw_combined_clean %>% filter(ngo_condom_rec_bin == 0)
)

skip_pairs <- list(
  idu = c("idu_ever_3cat", "idu_12m_3cat"),
  no_idu = c("idu_ever_3cat", "idu_12m_3cat"),
  street = c("street_sw_bin"),
  no_street = c("street_sw_bin"),
  ngo = c("ngo_condom_rec_bin"),
  no_ngo = c("ngo_condom_rec_bin")
)

# prevalence ratio modelling
final_tables <- list()

for (outcome in outcomes) {

  model_results <- list()

  for (s_name in names(subgroup_data)) {
    for (e in exposures) {

      if (!is.null(skip_pairs[[s_name]]) && e %in% skip_pairs[[s_name]]) {
        next
      }

      cat("Running:", outcome, "-", s_name, "-", e, "\n")

      model <- as.formula(
        paste0(outcome, " ~ ", e, " + (1 | year) + (1 | city)")
      )

      pr_model <- tryCatch({
        glmer(
          model,
          data = subgroup_data[[s_name]],
          family = poisson(link = "log"),
          control = glmerControl(optimizer = "bobyqa")
        )
      }, error = function(err) {
        cat("FAILED:", outcome, "-", s_name, "-", e, "\n")
        return(NULL)
      })

      if (is.null(pr_model)) next

      coefs <- summary(pr_model)$coefficients

      if (!(e %in% rownames(coefs))) next

      estimate <- coefs[e, "Estimate"]
      se <- coefs[e, "Std. Error"]

      pr <- exp(estimate)
      lb <- exp(estimate - 1.96 * se)
      ub <- exp(estimate + 1.96 * se)

      model_results[[paste(s_name, e, sep = "_")]] <- data.frame(
        outcome = outcome,
        subgroup = s_name,
        exposure = e,
        pr = pr,
        lb = lb,
        ub = ub
      )
    }
  }

model_results_df <- bind_rows(model_results)

  cat("Building table for:", outcome, "\n")

  summary_table_list <- list()

  for (s_name in names(subgroup_data)) {

    df_sub <- subgroup_data[[s_name]]

    tmp <- lapply(exposures, function(v) {

      df_sub %>%
        mutate(level = as.character(.data[[v]])) %>%
          filter(
          !is.na(level), level != "",
          !is.na(.data[[outcome]])
        ) %>%
        group_by(exposure = v, level) %>%
        summarise(
          n = n(),
          cases = sum(.data[[outcome]] == 1, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          percent = (cases / n) * 100,
          cases_pct = paste0(cases, " (", sprintf("%.1f", percent), "%)"),
          subgroup = s_name
        ) %>%
        arrange(level)
    })

    summary_table_list[[s_name]] <- bind_rows(tmp)
  }

  summary_table <- bind_rows(summary_table_list) %>%
    mutate(linkage = paste0(subgroup, exposure))

  model_lookup <- model_results_df %>%
    mutate(
      pr = paste0(
        sprintf("%.2f", pr),
        " (",
        sprintf("%.2f", lb),
        "-",
        sprintf("%.2f", ub),
        ")"
      ),
      linkage = paste0(subgroup, exposure)
    ) %>%
    select(linkage, pr) %>%
    distinct()

  final_table <- summary_table %>%
    left_join(model_lookup, by = "linkage") %>%
    group_by(linkage) %>%
    mutate(pr = if_else(row_number() == 1, "ref.", pr)) %>%
    ungroup()

  final_tables[[outcome]] <- final_table
}

# subgroups to save
subgroups <- c("overall", "idu", "no_idu", "street", "no_street", "ngo", "no_ngo")

for (sg in subgroups) {

  wb <- createWorkbook()

  for (out in names(final_tables)) {

    df <- final_tables[[out]] %>%
      filter(subgroup == sg)

    addWorksheet(wb, out)

    writeData(
      wb,
      sheet = out,
      x = df
    )
  }

  saveWorkbook(
    wb,
    file = paste0("hiv_pr_results_", sg, ".xlsx"),
    overwrite = TRUE
  )
}


