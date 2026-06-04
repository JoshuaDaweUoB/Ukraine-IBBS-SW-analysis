# load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, 
               lubridate, broom, survival, ggplot2, scales, sandwich, lmtest, lme4, 
               broom.mixed, openxlsx) 

# load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# filter some dfs
sw_combined_clean_idu <- sw_combined_clean %>% filter(idu_ever_bin == "Yes")
sw_combined_clean_noidu <- sw_combined_clean %>% filter(idu_ever_bin == "No")

sw_combined_clean_street <- sw_combined_clean %>% filter(street_sw_bin == "Yes")
sw_combined_clean_indoor <- sw_combined_clean %>% filter(street_sw_bin == "No")

sw_combined_clean_rape <- sw_combined_clean %>% filter(violence_rape_ever_bin == "Yes")
sw_combined_clean_norape <- sw_combined_clean %>% filter(violence_rape_ever_bin == "No")

exposures <- c(
  "condom_access_12m_bin",
  "client_condom_lastsex_bin",
  "street_sw_bin",
  "violence_any_ever_bin",
  "violence_rape_ever_bin",
  "violence_beaten_ever_bin",
  "violence_forced_any_12m_bin",
  "violence_forced_any_ever_bin",  
  "used_syringe_last_bin",
  "idu_ever_bin"
)

mediators <- c(
  "condom_access_12m_bin",
  "client_condom_lastsex_bin",
  "street_sw_bin",
  "violence_any_ever_bin",
  "violence_rape_ever_bin",
  "violence_beaten_ever_bin",
  "violence_forced_any_12m_bin",
  "violence_forced_any_ever_bin",  
  "used_syringe_last_bin",
  "idu_ever_bin"
)

# cross-sectional mediation
run_cross_sectional_mediation_screen <- function(data,
                                                 outcome_var,
                                                 exposure_vars,
                                                 mediator_vars,
                                                 adjust_vars = NULL,
                                                 n_boot,
                                                 save_file = NULL) {
  
  df <- data
  results <- list()
  
  for (exp in exposure_vars) {
    if (!exp %in% names(df)) next
    
    df[[exp]] <- as.factor(df[[exp]])
    
    f0 <- as.formula(
      paste0(
        outcome_var, " ~ ", exp,
        if (!is.null(adjust_vars)) paste0(" + ", paste(adjust_vars, collapse = " + ")) else "",
        " + (1 | year) + (1 | city)"
      )
    )
    
    m0 <- glmer(
      f0,
      data = df,
      family = binomial,
      control = glmerControl(optimizer = "bobyqa")
    )
    
    coef0_name <- grep(paste0("^", exp), names(fixef(m0)), value = TRUE)[1]
    
    b0 <- fixef(m0)[coef0_name]
    se0 <- sqrt(vcov(m0)[coef0_name, coef0_name])
    
    hr0 <- exp(b0)
    ci0 <- exp(b0 + c(-1.96, 1.96) * se0)
    
    for (med in mediator_vars) {
      if (!med %in% names(df)) next
      
      df[[med]] <- as.factor(df[[med]])
      
      f1 <- as.formula(
        paste0(
          outcome_var, " ~ ", exp, " + ", med,
          if (!is.null(adjust_vars)) paste0(" + ", paste(adjust_vars, collapse = " + ")) else "",
          " + (1 | year) + (1 | city)"
        )
      )
      
      m1 <- glmer(
        f1,
        data = df,
        family = binomial,
        control = glmerControl(optimizer = "bobyqa")
      )
      
      coef1_name <- grep(paste0("^", exp), names(fixef(m1)), value = TRUE)[1]
      
      b1 <- fixef(m1)[coef1_name]
      se1 <- sqrt(vcov(m1)[coef1_name, coef1_name])
      
      hr1 <- exp(b1)
      ci1 <- exp(b1 + c(-1.96, 1.96) * se1)
      
      boot_att <- numeric(n_boot)
      
      for (i in 1:n_boot) {
        b0_i <- rnorm(1, b0, se0)
        b1_i <- rnorm(1, b1, se1)
        
        hr0_i <- exp(b0_i)
        hr1_i <- exp(b1_i)
        
        boot_att[i] <- (log(hr0_i) - log(hr1_i)) / log(hr0_i)
      }
      
      prop_change <- mean(boot_att, na.rm = TRUE)
      att_lcl <- quantile(boot_att, 0.025, na.rm = TRUE)
      att_ucl <- quantile(boot_att, 0.975, na.rm = TRUE)
      
      results[[paste(exp, med, sep = "__")]] <- data.frame(
        Exposure = exp,
        Mediator = med,
        
        OR_Total = hr0,
        LCL_Total = ci0[1],
        UCL_Total = ci0[2],
        
        OR_Direct = hr1,
        LCL_Direct = ci1[1],
        UCL_Direct = ci1[2],
        
        Prop_Attenuation = prop_change,
        Att_LCL = att_lcl,
        Att_UCL = att_ucl
      )
    }
  }
  
  results_df <- dplyr::bind_rows(results)
  
  if (!is.null(save_file)) {
    writexl::write_xlsx(results_df, save_file)
  }
  
  return(results_df)
}


# idu on hiv
cross_results <- run_cross_sectional_mediation_screen(
  data = sw_combined_clean_street,
  outcome_var = "hiv_test_rslt_bin",
  exposure_vars = "idu_ever_bin",
  mediator_vars = "used_syringe_last_bin",
  adjust_vars = "years_in_sw_3cat",
  n_boot = 200,  
  save_file = "cross_sectional_mediation_sb.xlsx"
)

# overall
mediators <- c("condom_access_12m_bin", "client_condom_lastsex_bin", "used_syringe_last_bin", "idu_ever_bin", "street_sw_bin", "violence_forced_any_12m_bin", "violence_forced_any_ever_bin")
exposures <- c("condom_access_12m_bin", "client_condom_lastsex_bin", "used_syringe_last_bin", "idu_ever_bin", "street_sw_bin", "violence_forced_any_12m_bin", "violence_forced_any_ever_bin")

cross_results <- run_cross_sectional_mediation_screen(
  data = sw_combined_clean,
  outcome_var = "hiv_test_rslt_bin",
  exposure_vars = exposures,
  mediator_vars = mediators,
  adjust_vars = "years_in_sw_3cat",
  n_boot = 200,  
  save_file = "cross_sectional_mediation_overall.xlsx"
)

# IDU
mediators <- c("condom_access_12m_bin", "client_condom_lastsex_bin", "used_syringe_last_bin", "street_sw_bin", "violence_forced_any_12m_bin", "violence_forced_any_ever_bin")
exposures <- c("condom_access_12m_bin", "client_condom_lastsex_bin", "used_syringe_last_bin", "street_sw_bin", "violence_forced_any_12m_bin", "violence_forced_any_ever_bin")

cross_results <- run_cross_sectional_mediation_screen(
  data = sw_combined_clean_idu,
  outcome_var = "hiv_test_rslt_bin",
  exposure_vars = exposures,
  mediator_vars = mediators,
  adjust_vars = "years_in_sw_3cat",
  n_boot = 200,  
  save_file = "cross_sectional_mediation_idu.xlsx"
)

# street-based
mediators <- c("condom_access_12m_bin", "client_condom_lastsex_bin", "used_syringe_last_bin", "idu_ever_bin", "violence_forced_any_12m_bin", "violence_forced_any_ever_bin")
exposures <- c("condom_access_12m_bin", "client_condom_lastsex_bin", "used_syringe_last_bin", "idu_ever_bin", "violence_forced_any_12m_bin", "violence_forced_any_ever_bin")

cross_results <- run_cross_sectional_mediation_screen(
  data = sw_combined_clean_street,
  outcome_var = "hiv_test_rslt_bin",
  exposure_vars = exposures,
  mediator_vars = mediators,
  adjust_vars = "years_in_sw_3cat",
  n_boot = 200,  
  save_file = "cross_sectional_mediation_sb.xlsx"
)







# longitudinal mediation

run_bootstrap_cox_mediation <- function(data,
                                        outcome_var,
                                        time_var = "py",
                                        exposure_vars,
                                        mediator_vars,
                                        adjust_vars = NULL,
                                        n_boot = 200,
                                        seed = 1) {
  
  library(dplyr)
  library(survival)
  
  set.seed(seed)
  df <- data
  
  # outcome
  df[[outcome_var]] <- ifelse(
    df[[outcome_var]] %in% c("Yes", "Positive", 1, "1"),
    1, 0
  )
  df[[outcome_var]] <- as.numeric(df[[outcome_var]])
  
  results <- list()
  
  for (exp in exposure_vars) {
    
    df[[exp]] <- as.factor(df[[exp]])
    
    for (med in mediator_vars) {
      
      df[[med]] <- as.factor(df[[med]])
      
      message("Running: ", exp, " → ", med)
      
      # store bootstrap estimates
      boot_prop <- numeric(n_boot)
      
      for (b in 1:n_boot) {
        
        idx <- sample(1:nrow(df), replace = TRUE)
        d <- df[idx, ]
        
        # total model
        f1 <- as.formula(
          paste0("Surv(", time_var, ", ", outcome_var, ") ~ ",
                 exp,
                 if (!is.null(adjust_vars)) paste0(" + ", paste(adjust_vars, collapse = " + ")) else "")
        )
        
        m1 <- coxph(f1, data = d)
        hr_total <- exp(coef(m1)[1])
        
        # direct model
        f2 <- as.formula(
          paste0("Surv(", time_var, ", ", outcome_var, ") ~ ",
                 exp, " + ", med,
                 if (!is.null(adjust_vars)) paste0(" + ", paste(adjust_vars, collapse = " + ")) else "")
        )
        
        m2 <- coxph(f2, data = d)
        hr_direct <- exp(coef(m2)[1])
        
        # mediation proportion
        boot_prop[b] <- (log(hr_total) - log(hr_direct)) / log(hr_total)
      }
      
      results[[paste(exp, med, sep="__")]] <- data.frame(
        Exposure = exp,
        Mediator = med,
        
        Prop_Mediated = mean(boot_prop, na.rm = TRUE),
        Prop_LCL = quantile(boot_prop, 0.025, na.rm = TRUE),
        Prop_UCL = quantile(boot_prop, 0.975, na.rm = TRUE)
      )
    }
  }
  
  bind_rows(results)
}


results_mediation <- run_bootstrap_cox_mediation(
  data = sw_negative_cohort_hiv,
  outcome_var = "hiv_test_rslt_bin",
  time_var = "py",
  exposure_vars = "street_sw_bin",
  mediator_vars = idu_mediators,
  adjust_vars = c(
    "ukraine_region",
    "year",
    "years_in_sw_3cat"
  ),
  n_boot = 1000
)

write_xlsx(results_mediation, "cox_mediation_results.xlsx")