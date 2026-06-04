## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate, broom, survival, ggplot2, scales)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# exposures
exposure_vars <- c(
  "condom_access_12m_bin",
  "client_condom_lastsex_bin",
  "ngo_client_lifetime_bin",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_bin",
  "violence_any_ever_bin",
  "violence_rape_ever_bin",
  "violence_beaten_ever_bin",
  "violence_physical_abuse_ever_bin",
  "violence_police_bin",
  "used_syringe_last_bin",
  "idu_ever_bin",
  "idu_12m_bin",
  "underage_first_sw_bin",
  "violence_support_ngo_bin",
  "sw_partners_clients_30d_3cat"
)

# binary variables
binary_vars <- c(
  "condom_access_12m_bin",
  "client_condom_lastsex_bin",
  "ngo_client_lifetime_bin",
  "street_sw_bin",
  "alcohol_30d_bin",
  "city_travel_12m_bin",
  "violence_any_ever_bin",
  "violence_rape_ever_bin",
  "violence_beaten_ever_bin",
  "violence_physical_abuse_ever_bin",
  "violence_police_bin",
  "used_syringe_last_bin",
  "idu_ever_bin",
  "idu_12m_bin",
  "underage_first_sw_bin",
  "violence_support_ngo_bin"
)

# function to calculate IR 
calc_ir_ci <- function(cases, person_years, scale = 100) {
    if (cases == 0) {
    ir <- 0
    lower <- 0
    upper <- -log(0.05) / person_years * scale
    return(list(IR = ir, IR_lower = lower, IR_upper = upper))
  }
    lower <- qchisq(0.025, 2 * cases) / (2 * person_years) * scale
  upper <- qchisq(0.975, 2 * (cases + 1)) / (2 * person_years) * scale
  ir <- (cases / person_years) * scale
    list(IR = ir, IR_lower = lower, IR_upper = upper)
}

# function to calculate hazard ratios and save formatted results
run_cox_ir_analysis <- function(data,
                                outcome_var,
                                time_var = "py",
                                exposure_vars,
                                adjust_vars = NULL,
                                binary_vars = NULL,
                                outcome_positive = 1,
                                scale_ir = 100,
                                save_file = NULL) {
  
  df <- data
  
  # outcome
df[[outcome_var]] <- ifelse(
  df[[outcome_var]] %in% c("Yes", "Positive", 1, "1"),
  1, 0)

df[[outcome_var]] <- as.numeric(df[[outcome_var]])
  
  # convert covariates
  if (!is.null(binary_vars)) {
    df <- df %>%
      mutate(across(all_of(binary_vars),
                    ~ factor(ifelse(. == "Yes", "Yes", "No"),
                             levels = c("No", "Yes"))))
  }
  
  if (!is.null(adjust_vars)) {
    df <- df %>%
      mutate(across(all_of(adjust_vars), as.factor))
  }
  
  results_list <- list()
  
  for (var in exposure_vars) {
    
    df[[var]] <- as.factor(df[[var]])
    
    # build formula
    rhs <- paste(c(var, adjust_vars), collapse = " + ")
    fml <- as.formula(paste("Surv(", time_var, ",", outcome_var, ") ~", rhs))
    
    model <- coxph(fml, data = df)
    tidy_mod <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    
    tidy_mod <- tidy_mod %>%
      filter(grepl(paste0("^", var), term))
    
    levels_var <- levels(df[[var]])
    
    for (lev in levels_var) {
      
      subset_data <- df %>%
        filter(!is.na(.data[[var]]),
               .data[[var]] == lev)
      
      cases <- sum(subset_data[[outcome_var]] == 1, na.rm = TRUE)
      person_years <- sum(subset_data[[time_var]], na.rm = TRUE)
      
      ir_res <- calc_ir_ci(cases, person_years, scale_ir)

      ir <- ir_res$IR
      ir_lower <- ir_res$IR_lower
      ir_upper <- ir_res$IR_upper
      
      if (lev == levels_var[1]) {
        results_list[[length(results_list) + 1]] <- data.frame(
          Variable = var,
          Level = lev,
          HR = 1,
          CI_lower = NA,
          CI_upper = NA,
          Cases = cases,
          Person_Years = person_years,
          IR = ir,
          IR_lower = ir_lower,
          IR_upper = ir_upper
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
          Person_Years = person_years,
          IR = ir,
          IR_lower = ir_lower,
          IR_upper = ir_upper
        )
      }
    }
  }
  
  results_df <- bind_rows(results_list)
  
  if (!is.null(save_file)) {
    write_xlsx(results_df, save_file)
  }
  
  return(results_df)
}

run_cox_mediation_analysis <- function(data,
                                       outcome_var,
                                       mediator_var = NULL,
                                       time_var = "py",
                                       exposure_vars,
                                       adjust_vars = NULL,
                                       binary_vars = NULL,
                                       scale_ir = 100,
                                       save_file = NULL) {
  
  df <- data
  
  # Outcome recoding
  df[[outcome_var]] <- ifelse(
    df[[outcome_var]] %in% c("Yes", "Positive", 1, "1"),
    1, 0
  )
  
  df[[outcome_var]] <- as.numeric(df[[outcome_var]])
  
  # Binary variables
  if (!is.null(binary_vars)) {
    df <- df %>%
      mutate(across(all_of(binary_vars),
                    ~ factor(ifelse(. == "Yes", "Yes", "No"),
                             levels = c("No", "Yes"))))
  }
  
  # Covariates
  if (!is.null(adjust_vars)) {
    df <- df %>%
      mutate(across(all_of(adjust_vars), as.factor))
  }
  
  # Mediator
  if (!is.null(mediator_var)) {
    if (is.character(df[[mediator_var]]) ||
        is.factor(df[[mediator_var]])) {
      df[[mediator_var]] <- as.factor(df[[mediator_var]])
    }
  }
  
  results_list <- list()
  
  for (var in exposure_vars) {
    
    df[[var]] <- as.factor(df[[var]])
    
    # Total effect model
    total_rhs <- paste(c(var, adjust_vars), collapse = " + ")
    
    total_formula <- as.formula(
      paste0("Surv(", time_var, ", ", outcome_var, ") ~ ",
             total_rhs)
    )
    
    total_model <- coxph(total_formula, data = df)
    
    total_tidy <- tidy(
      total_model,
      exponentiate = TRUE,
      conf.int = TRUE
    ) %>%
      filter(grepl(paste0("^", var), term))
    
    # Direct effect model (adds mediator)
    direct_tidy <- NULL
    
    if (!is.null(mediator_var)) {
      
      direct_rhs <- paste(
        c(var, mediator_var, adjust_vars),
        collapse = " + "
      )
      
      direct_formula <- as.formula(
        paste0("Surv(", time_var, ", ", outcome_var, ") ~ ",
               direct_rhs)
      )
      
      direct_model <- coxph(direct_formula, data = df)
      
      direct_tidy <- tidy(
        direct_model,
        exponentiate = TRUE,
        conf.int = TRUE
      ) %>%
        filter(grepl(paste0("^", var), term))
    }
    
    levels_var <- levels(df[[var]])
    
    for (lev in levels_var) {
      
      subset_data <- df %>%
        filter(!is.na(.data[[var]]),
               .data[[var]] == lev)
      
      cases <- sum(subset_data[[outcome_var]] == 1,
                   na.rm = TRUE)
      
      person_years <- sum(subset_data[[time_var]],
                          na.rm = TRUE)
      
      ir_res <- calc_ir_ci(
        cases,
        person_years,
        scale = scale_ir
      )
      
      if (lev == levels_var[1]) {
        
        results_list[[length(results_list) + 1]] <- data.frame(
          Variable = var,
          Level = lev,
          HR_Total = 1,
          LCL_Total = NA,
          UCL_Total = NA,
          HR_Direct = 1,
          LCL_Direct = NA,
          UCL_Direct = NA,
          Cases = cases,
          Person_Years = person_years,
          IR = ir_res$IR,
          IR_lower = ir_res$IR_lower,
          IR_upper = ir_res$IR_upper
        )
        
      } else {
        
        total_row <- total_tidy %>%
          filter(grepl(lev, term))
        
        direct_row <- NULL
        
        if (!is.null(direct_tidy)) {
          direct_row <- direct_tidy %>%
            filter(grepl(lev, term))
        }
        
        results_list[[length(results_list) + 1]] <- data.frame(
          Variable = var,
          Level = lev,
          
          HR_Total = total_row$estimate,
          LCL_Total = total_row$conf.low,
          UCL_Total = total_row$conf.high,
          
          HR_Direct = ifelse(
            is.null(direct_row),
            NA,
            direct_row$estimate
          ),
          
          LCL_Direct = ifelse(
            is.null(direct_row),
            NA,
            direct_row$conf.low
          ),
          
          UCL_Direct = ifelse(
            is.null(direct_row),
            NA,
            direct_row$conf.high
          ),
          
          Cases = cases,
          Person_Years = person_years,
          IR = ir_res$IR,
          IR_lower = ir_res$IR_lower,
          IR_upper = ir_res$IR_upper
        )
      }
    }
  }
  
  results_df <- bind_rows(results_list)
  
  # Approximate proportion mediated
  if (!is.null(mediator_var)) {
    results_df <- results_df %>%
      mutate(
        Prop_Mediated =
          (log(HR_Total) - log(HR_Direct)) /
          log(HR_Total)
      )
  }
  
  if (!is.null(save_file)) {
    write_xlsx(results_df, save_file)
  }
  
  return(results_df)
}

# load data
sw_negative_cohort_hiv <- readRDS("sw_incidence_hiv_dataset.rds")
sw_rape_cohort <- readRDS("sw_incident_rape_dataset.rds")
sw_idu_cohort <- readRDS("sw_incident_idu_dataset.rds")
sw_noidu_cohort <- readRDS("sw_incident_noidu_dataset.rds")
sw_sb_cohort <- readRDS("sw_incident_sb_dataset.rds")
sw_nosb_cohort <- readRDS("sw_incident_nosb_dataset.rds")

exposures <- c(
  "condom_access_12m_3cat_bin",
  "street_sw_bin",
  "violence_any_ever_3cat_bin",
  "violence_rape_ever_bin",
  "violence_beaten_ever_bin",
  "violence_forced_any_12m_bin",
  "violence_forced_any_ever_bin",  
  "used_syringe_last_3cat_bin",
  "idu_ever_3cat_bin"
)

mediators <- c(
  "condom_access_12m_3cat_bin",
  "street_sw_bin",
  "violence_any_ever_3cat_bin",
  "violence_rape_ever_bin",
  "violence_beaten_ever_bin",
  "violence_forced_any_12m_bin",
  "violence_forced_any_ever_bin",
  "used_syringe_last_3cat_bin",
  "idu_ever_3cat_bin"
)

names(sw_negative_cohort_hiv)

results_hiv_mediation <- run_cox_mediation_analysis(
  data = sw_negative_cohort_hiv,
  outcome_var = "hiv_test_rslt_bin",
  mediator_var = mediators,
  time_var = "py",
  exposure_vars = exposures,
  adjust_vars = c(
    "ukraine_region",
    "year",
    "years_in_sw_3cat"
  ),
  binary_vars = binary_vars,
  save_file = "cox_mediation_hiv.xlsx"
)

# hiv incidence
results_hiv <- run_cox_ir_analysis(
  data = sw_negative_cohort_hiv,
  outcome_var = "hiv_test_rslt_bin",
  time_var = "py",
  exposure_vars = exposure_vars,
  adjust_vars = c("ukraine_region", "year", "years_in_sw_3cat"),
  binary_vars = binary_vars,
  save_file = "cox_model_results_hiv.xlsx"
)

# rape incidence
results_rape <- run_cox_ir_analysis(
  data = sw_rape_cohort,
  outcome_var = "event",
  time_var = "py",
  exposure_vars = exposure_vars,
  adjust_vars = c("ukraine_region", "year", "years_in_sw_3cat"),
  binary_vars = binary_vars,
  save_file = "cox_model_results_rape.xlsx"
)

# street-based incidence
results_sb <- run_cox_ir_analysis(
  data = sw_sb_cohort,
  outcome_var = "event",
  time_var = "py",
  exposure_vars = exposure_vars,
  adjust_vars = c("ukraine_region", "year", "years_in_sw_3cat"),
  binary_vars = binary_vars,
  save_file = "cox_model_results_sb.xlsx"
)

# indoor-based incidence
results_nosb <- run_cox_ir_analysis(
  data = sw_nosb_cohort,
  outcome_var = "event",
  time_var = "py",
  exposure_vars = exposure_vars,
  adjust_vars = c("ukraine_region", "year", "years_in_sw_3cat"),
  binary_vars = binary_vars,
  save_file = "cox_model_results_nosb.xlsx"
)

# idu incidence
results_idu <- run_cox_ir_analysis(
  data = sw_idu_cohort,
  outcome_var = "event",
  time_var = "py",
  exposure_vars = exposure_vars,
  adjust_vars = c("ukraine_region", "year", "years_in_sw_3cat"),
  binary_vars = binary_vars,
  save_file = "cox_model_results_idu.xlsx"
)

# street-based incidence
results_noidu <- run_cox_ir_analysis(
  data = sw_noidu_cohort,
  outcome_var = "event",
  time_var = "py",
  exposure_vars = exposure_vars,
  adjust_vars = c("ukraine_region", "year", "years_in_sw_3cat"),
  binary_vars = binary_vars,
  save_file = "cox_model_results_noidu.xlsx"
)
