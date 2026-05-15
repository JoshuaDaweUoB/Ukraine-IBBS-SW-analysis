## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# load clean cross sectional data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# check levels of each variable
levels_list <- lapply(sw_combined_clean, unique)

# convert to df
max_length <- max(sapply(levels_list, length))
levels_df <- as.data.frame(
  lapply(levels_list, function(x) {
    length(x) <- max_length
    x
  }),
  stringsAsFactors = FALSE
)

levels(sw_combined_clean$ngo_access_lifetime)

# save
write_xlsx(levels_df, "sw_combined_clean_levels.xlsx")

## prepare longitudinal data 

# load appended clean data
sw_data_linkage <- read_excel("SW IBBS linkage.xlsx")
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# indicator for skipped years
sw_data_linkage <- sw_data_linkage %>%
  mutate(
    w2013 = !is.na(`2013_id`),
    w2015 = !is.na(`2015_id`),
    w2017 = !is.na(`2017_id`),
    w2021 = !is.na(`2021_id`)
  )

skip_flags <- sw_data_linkage %>%
  group_by(UIC) %>%
  summarise(
    p2013 = any(w2013),
    p2015 = any(w2015),
    p2017 = any(w2017),
    p2021 = any(w2021),
    .groups = "drop"
  ) %>%
  mutate(
    skip_2015 = p2013 & !p2015 & (p2017 | p2021),
    skip_2017 = (p2013 | p2015) & !p2017 & p2021,
    skip_any = skip_2015 | skip_2017
  ) %>%
  select(UIC, skip_any)

# attach skip flag
sw_data_linkage <- sw_data_linkage %>%
  left_join(skip_flags, by = "UIC")

# check date formatting 
sw_combined_clean %>%
  filter(year %in% c(2013, 2015, 2017, 2021)) %>%
  select(year, interview_dte) %>%
  group_by(year) %>%
  slice_head(n = 20) %>%
  ungroup() %>%
  print(n = Inf)

# convert to date from character
sw_combined_clean <- sw_combined_clean %>%
  filter(year %in% c(2013, 2015, 2017, 2021)) %>%
  mutate(interview_dte = as.Date(interview_dte))  

# convert IDs to character
sw_data_linkage <- sw_data_linkage %>%
  mutate(across(ends_with("_id"), as.character),
         id = as.character(id))

# pivot linkage to long
linkage_long <- sw_data_linkage %>%
  pivot_longer(
    cols = ends_with("_id"),
    names_to = "year_column",
    values_to = "year_id"
  ) %>%
  mutate(
    year = case_when(
      year_column == "2013_id" ~ 2013,
      year_column == "2015_id" ~ 2015,
      year_column == "2017_id" ~ 2017,
      year_column == "2021_id" ~ 2021
    )
  ) %>%
  filter(!is.na(year_id))

# combine linkage keys and cleaned data
sw_data_long <- linkage_long %>%
  left_join(
    sw_combined_clean,
    by = c("year" = "year", "year_id" = "id")
  ) %>%
  arrange(id, year)

# delete those with missing survey years
sw_data_long <- sw_data_long %>%
  filter(skip_any == "FALSE")

# check number of unique participants
length(unique(sw_data_long$id))
nrow(sw_data_long)

# id sequence variable
sw_data_long <- sw_data_long %>%
  group_by(id) %>%
  mutate(id_seq = row_number()) %>%
  ungroup()

saveRDS(sw_data_long, "sw_data_long.rds")

## code to calculate rates for subgroups of subgroups

# load data
sw_data_long <- readRDS("sw_data_long.rds")

sw_data_long_idu <- sw_data_long %>% filter(idu_ever_bin == "Yes")
sw_data_long_noidu <- sw_data_long %>% filter(idu_ever_bin == "No")
sw_data_long_street <- sw_data_long %>% filter(street_sw_bin == "Yes")
sw_data_long_indoor <- sw_data_long %>% filter(street_sw_bin == "No")
sw_data_long_ngo <- sw_data_long %>% filter(ngo_client_lifetime_bin == "Yes")
sw_data_long_nongo <- sw_data_long %>% filter(ngo_client_lifetime_bin == "No")

dataframes <- c(
  "sw_data_long",
  "sw_data_long_idu",
  "sw_data_long_noidu",
  "sw_data_long_street",
  "sw_data_long_indoor",
  "sw_data_long_ngo",
  "sw_data_long_nongo"
)

## function to calculate incidence rates
calc_transition_rate <- function(data,
                                 id_var = "id",
                                 date_var = "interview_dte",
                                 state_var,
                                 baseline_state,
                                 start_state,
                                 end_state,
                                 scale = 100,
                                 save_data = FALSE,
                                 rds_name = NULL,
                                 xlsx_name = NULL) {

  id <- sym(id_var)
  date <- sym(date_var)
  state <- sym(state_var)
  
  # baseline
  first <- data %>%
    arrange(!!id, !!date) %>%
    group_by(!!id) %>%
    slice_min(!!date, n = 1) %>%
    ungroup()
  
  baseline_ids <- first %>%
    filter(!!state == baseline_state) %>%
    pull(!!id)
  
  # cohort
  cohort <- data %>%
    filter((!!id) %in% baseline_ids) %>%
    arrange(!!id, !!date) %>%
    group_by(!!id) %>%
    mutate(
      visit_number = row_number(),
      state_start = lag(!!state),
      state_end = !!state,
      t_start = lag(!!date),
      t_end = !!date
    ) %>%
    ungroup() %>%
    filter(visit_number != 1) %>%
    filter(state_start == start_state) %>%
    filter(!is.na(state_end)) %>%
    mutate(
      event = ifelse(state_start == start_state & state_end == end_state, 1, 0),
      days_risk = ifelse(
        event == 1,
        as.numeric(t_end - t_start) / 2,
        as.numeric(t_end - t_start)
      ),
      py = days_risk / 365.25
    )
  
  # results
  events <- sum(cohort$event, na.rm = TRUE)
  person_years <- sum(cohort$py, na.rm = TRUE)
  rate <- (events / person_years) * scale
  
  # 95% CI
  lower_ci <- (qchisq(0.025, 2 * events) / 2) / person_years * scale
  upper_ci <- (qchisq(0.975, 2 * (events + 1)) / 2) / person_years * scale

  # print results
  cat("\n--- Transition summary ---\n")
  cat("From:", start_state, "→ To:", end_state, "\n")
  cat("Baseline:", baseline_state, "\n")
  cat("Events:", events, "\n")
  cat("Person-years:", round(person_years, 2), "\n")
  cat("Rate per", scale, "PY:", round(rate, 3), "\n")
  cat("95% CI:", 
      paste0("(", round(lower_ci, 3), ", ", round(upper_ci, 3), ")"), 
      "\n\n")
  
  # save
  if (save_data) {
    if (!is.null(rds_name)) saveRDS(cohort, rds_name)
    if (!is.null(xlsx_name)) write_xlsx(cohort, xlsx_name)
  }
  
  return(list(
    events = events,
    person_years = person_years,
    rate = rate,
    lower_ci = lower_ci,
    upper_ci = upper_ci,
    cohort = cohort
  ))
}

datasets_list <- list(
  overall = "sw_data_long",
  hiv = dataframes,
  idu = dataframes,
  noidu = dataframes,
  street = dataframes,
  indoor = dataframes,
  ngo = dataframes,
  nongo = dataframes
)

outcome_vars <- list(
  overall = "hiv_test_rslt_bin",
  hiv = "hiv_test_rslt_bin",
  idu = "idu_12m_bin",
  noidu = "idu_12m_bin",
  street = "street_sw_bin",
  indoor = "street_sw_bin",
  ngo = "ngo_client_lifetime_bin",
  nongo = "ngo_client_lifetime_bin"
)

transition_states <- list(
  overall = list(start = "Negative", end = "Positive"),
  hiv = list(start = "Negative", end = "Positive"),
  idu = list(start = "No", end = "Yes"),
  noidu = list(start = "Yes", end = "No"),
  street = list(start = "No", end = "Yes"),
  indoor = list(start = "Yes", end = "No"),
  ngo = list(start = "No", end = "Yes"),
  nongo = list(start = "Yes", end = "No")
)

baseline_states <- list(
  overall = "Negative",
  hiv = "Negative",
  idu = "No",
  noidu = "Yes",
  street = "No",
  indoor = "Yes",
  ngo = "No",
  nongo = "Yes"
)

run_sheet <- function(datasets, state_var, baseline_state, start_state, end_state) {
  results_table <- data.frame()

  for (d in datasets) {
    base_data <- get(d)

    res_overall <- calc_transition_rate(
      data = base_data,
      state_var = state_var,
      baseline_state = baseline_state,
      start_state = start_state,
      end_state = end_state,
      save_data = FALSE
    )

    results_table <- bind_rows(
      results_table,
      data.frame(
        dataset = d,
        subgroup = "overall",
        events = res_overall$events,
        person_years = round(res_overall$person_years, 2),
        incidence_rate = round(res_overall$rate, 3),
        lower_ci = round(res_overall$lower_ci, 3),
        upper_ci = round(res_overall$upper_ci, 3),
        IR_95CI = sprintf(
          "%.1f (%.1f-%.1f)",
          res_overall$rate,
          res_overall$lower_ci,
          res_overall$upper_ci
        )
      )
    )

    for (sg in setdiff(names(subgroups), "overall")) {
      dat <- dplyr::filter(base_data, !!subgroups[[sg]])

      if (nrow(dat) == 0) next

      res <- calc_transition_rate(
        data = dat,
        state_var = state_var,
        baseline_state = baseline_state,
        start_state = start_state,
        end_state = end_state,
        save_data = FALSE
      )

      results_table <- bind_rows(
        results_table,
        data.frame(
          dataset = d,
          subgroup = sg,
          events = res$events,
          person_years = round(res$person_years, 2),
          incidence_rate = round(res$rate, 3),
          lower_ci = round(res$lower_ci, 3),
          upper_ci = round(res$upper_ci, 3),
          IR_95CI = sprintf(
            "%.1f (%.1f-%.1f)",
            res$rate,
            res$lower_ci,
            res$upper_ci
          )
        )
      )
    }
  }

  results_table
}

all_sheets <- list()

for (name in names(datasets_list)) {
  states <- transition_states[[name]]
  all_sheets[[name]] <- run_sheet(
    datasets = datasets_list[[name]],
    state_var = outcome_vars[[name]],
    baseline_state = baseline_states[[name]],
    start_state = states$start,
    end_state = states$end
  )
}

write_xlsx(
  all_sheets,
  "incidence_all_sheets.xlsx"
)



































