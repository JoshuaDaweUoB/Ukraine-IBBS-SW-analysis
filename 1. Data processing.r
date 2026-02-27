## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled, lubridate)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# month multiplier
days_in_month <- 30/7

# load 2008 data
sw_data_2008_raw <- read_excel("2008_IBBS_FSW_TLS AND RDS_Data.xlsx")

# derive volume variables
sw_data_2008_raw <- sw_data_2008_raw %>%
  mutate(
    sw_partners_clients_7d_raw = `B3.1. During the last (working) week, how many of your sexual partners [clients] were partners [clients] from whom you received compensation?`,
    sw_partners_nonclients_7d_raw = `B3.2. During the last (working) week, how many of your sexual partners [clients] were partners from whom you did not receive compensation?`,
    sw_partners_total_7d_raw = `B3.3. How many total sexual partners did you have during the last (working) week (including spouse and others)?`
  )

sw_data_2008_raw <- sw_data_2008_raw %>%
  mutate(
  sw_partners_clients_7d = as.numeric(ifelse(sw_partners_clients_7d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_clients_7d_raw)),
  sw_partners_clients_30d = sw_partners_clients_7d*days_in_month,

  sw_partners_nonclients_7d = as.numeric(ifelse(sw_partners_nonclients_7d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_nonclients_7d_raw)),
  sw_partners_nonclients_30d = sw_partners_nonclients_7d*days_in_month,

  sw_partners_total_7d = as.numeric(ifelse(sw_partners_total_7d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_total_7d_raw)),
  sw_partners_total_30d = sw_partners_total_7d*days_in_month,
  )

# variables to rename
rename_map_2008 <- c(
  education = "A3. What is your education?",
  marital_status = "A2. Which of the following best describes your current marital status?",
  age = "A1. What is your age?",
  age_first_sex = "B1. At what age did you first engage in sexual activity?",
  age_first_sw = "B2. How old were you when you first provided sexual services for compensation (money or other)?",
  city = "City",
  city_travel_12m = "A6.1. Over the past 12 months, have you left this city for more than one month [30 days] to provide sexual services in other cities or regions of Ukraine?",
  country_travel_12m = "A6.2. Over the past 12 months, have you left this city for more than one month [30 days] to provide sexual services in other countries?",
  sw_partners_clients_7d_raw = "sw_partners_clients_7d_raw",
  sw_partners_nonclients_7d_raw = "sw_partners_nonclients_7d_raw",
  sw_partners_total_7d_raw = "sw_partners_total_7d_raw",
  sw_partners_clients_7d = "sw_partners_clients_7d",
  sw_partners_clients_30d = "sw_partners_clients_30d",
  sw_partners_nonclients_7d = "sw_partners_nonclients_7d",
  sw_partners_nonclients_30d = "sw_partners_nonclients_30d",
  sw_partners_total_7d = "sw_partners_total_7d",
  sw_partners_total_30d = "sw_partners_total_30d",
  sw_partners_total_24h = "B4. How many different clients did you provide sexual services to for compensation on your last working day (24 hours)?",
  client_condom_lastsex = "B5. During your last sex with a client, did you use a condom?",
  client_condom_freq_30d = "B8. Recall all your sex with clients in the LAST MONTH (30 days). How often did you use a condom?",
  perm_partner_condom_lastsex = "B11. During your last sexual contact with a PERMANENT partner from whom you DID NOT RECEIVE PAYMENT, did you use a condom?",
  cas_partner_condom_lastsex = "B13. During your last sexual contact with a CASUAL partner from whom you DID NOT RECEIVE PAYMENT, did you use a condom?",
  condom_access_12m = "В15.1.  In the last 12 months, have you received free condoms (from a representative of a public organization, medical worker, volunteer, at needle exchange points, or through the \"peer-to-peer\" program)?",
  alcohol_30d_bin = "C1. Have you consumed any alcoholic beverages in the past 30 days?",
  alcohol_30d_num = "C1.1. How often?",
  drugs_30d_bin = "C2. Some people try using various drugs. Do you use drugs?",
  idu_12m_bin = "C3. Have you used drugs through injection in the past 12 months?",
  idu_30d_num = "C4. How often have you used drugs through injection in the past 30 days?",
  used_syringe_last = "C5. During your most recent injection, did you use shared injecting equipment (syringe or needle) that someone else had already used?",
  used_syringe_30d_bin = "C6. In the past 30 days, how often did you use shared injecting equipment?",
  ngo_access_lifetime = "D2.1. Over your lifetime, have you ever sought assistance from NGOs that work with women engaged in commercial sex?",
  ngo_access_12m = "D2.2. In the past 12 months, have you sought assistance from NGOs that work with women engaged in commercial sex?",
  ngo_access_30d = "D2.3. In the past 30 days, have you sought assistance from NGOs that work with women engaged in commercial sex?",
  hiv_tested_lifetime = "G5. I don’t ask you about the result but I want to ask if you underwent HIV testing?",
  hiv_tested_12m = "G7. Let’s specify if that was during the RECENT 12 MONTHS?",
  hiv_tested_result = "G8. I don’t ask you about the result, but did you receive your last test result?",
  hiv_status_selfreport = "G10.1 HIV status",
  blood_screen_bin = "N1. Was blood screening conducted or not?",
  hiv_test_rslt = "N3. HIV testing result:",
  syphilis_test_rslt = "S3. Syphilis testing result"
)

# map to vars
sw_data_2008_clean <- sw_data_2008_raw %>%
  rename(!!!setNames(rename_map_2008, names(rename_map_2008))) %>%
  select(all_of(names(rename_map_2008)))

# check variables are all present
renamed_vars <- names(rename_map_2008)
missing_renamed <- setdiff(renamed_vars, names(sw_data_2008_clean))
if (length(missing_renamed) == 0) {
  cat("All renamed variables are present in sw_data_2008_clean.\n")
} else {
  cat("Missing renamed variables:\n")
  print(missing_renamed)
}

# load 2009 data
sw_data_2009_raw <- read_excel("2009_IBBS_FSW_TLS AND RDS_Data.xlsx")

# derive volume variables
sw_data_2009_raw <- sw_data_2009_raw %>%
  mutate(
    sw_partners_clients_7d_raw = `В4.1 During THE RECENT (WORKING) WEEK, how many persons listed below were among your sexual partners [clients]? Partners [clients] who you RECEIVED A FEE [money or other] from`,
    sw_partners_perm_7d_raw = `В4.2 During THE RECENT (WORKING) WEEK, how many persons listed below were among your sexual partners [clients]? Permanent partners who you RECEIVED NO FEE [money or other] from`,
    sw_partners_nonperm_7d_raw = `В4.3 During THE RECENT (WORKING) WEEK, how many persons listed below were among your sexual partners [clients]? Casual partners who you RECEIVED NO FEE [money or other] from`,
    sw_partners_total_7d_raw = `В4.4 How MANY DIFFERENT sexual partners in total did you have during the recent (working) week, including your husband or your permanent sexual partner with whom you live?`
  )

sw_data_2009_raw <- sw_data_2009_raw %>%
  mutate(
  sw_partners_clients_7d = as.numeric(ifelse(sw_partners_clients_7d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_clients_7d_raw)),
  sw_partners_clients_30d = sw_partners_clients_7d*days_in_month,

  sw_partners_perm_7d = as.numeric(ifelse(sw_partners_perm_7d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_perm_7d_raw)),
  sw_partners_perm_30d = sw_partners_perm_7d*days_in_month,

  sw_partners_nonperm_7d = as.numeric(ifelse(sw_partners_nonperm_7d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_nonperm_7d_raw)),
  sw_partners_nonperm_30d = sw_partners_nonperm_7d*days_in_month,

  sw_partners_nonclients_7d = sw_partners_perm_7d+sw_partners_nonperm_7d,
  sw_partners_nonclients_30d = sw_partners_nonclients_7d*days_in_month,

  sw_partners_total_7d = as.numeric(ifelse(sw_partners_total_7d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_total_7d_raw)),
  sw_partners_total_30d = sw_partners_total_7d*days_in_month,
  )

# variables to rename
rename_map_2009 <- c(
  sw_freq_6m = "А11. How often have your provided sexual services for a fee in THE LAST 6 MONTHS?",
  sw_days_total_7d = "А12. How many days DURING THE LAST WEEK [7 days] have you provided sexual services for a fee?",
  education = "А1. Your education",
  residence = "А3. Where do you live in this city?",
  marital_status = "А8. Choose from the suggested options one corresponding to your marital status at the moment:",
  age = "S2. Your age",
  age_first_sex = "В1. At what age did you have sexual relations for the first time",
  age_first_sw = "В2. How old were you when you provided sexual services for a fee (money or other) for the first time?",
  city = "І7.1 City",
  city_travel_12m = "А5. Have you ever left this city [CITY OF SURVEY] for more than 1 month [30 days] in THE LAST 12 MONTHS to provide sexual services?",
  sw_partners_clients_7d_raw = "sw_partners_clients_7d_raw",
  sw_partners_perm_7d_raw = "sw_partners_perm_7d_raw",
  sw_partners_nonperm_7d_raw = "sw_partners_nonperm_7d_raw",
  sw_partners_total_7d_raw = "sw_partners_total_7d_raw",
  sw_partners_clients_7d = "sw_partners_clients_7d",
  sw_partners_clients_30d = "sw_partners_clients_30d",
  sw_partners_perm_7d = "sw_partners_perm_7d",
  sw_partners_perm_30d = "sw_partners_perm_30d",
  sw_partners_nonperm_7d = "sw_partners_nonperm_7d",
  sw_partners_nonperm_30d = "sw_partners_nonperm_30d",
  sw_partners_nonclients_7d = "sw_partners_nonclients_7d",
  sw_partners_nonclients_30d = "sw_partners_nonclients_30d",
  sw_partners_total_7d = "sw_partners_total_7d",
  sw_partners_total_30d = "sw_partners_total_30d",
  partners_sw_24h = "В5. How many different CLIENTS whom you provided sexual services for a fee did you have FOR THE LAST WORKING DAY (24 HOURS)?",
  client_condom_lastsex = "В6. Remember your sexual contact with your MOST RECENT CLIENT. Did you use a condom?",
  client_condom_bin_30d = "В16.2.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with a  CLIENT during vaginal sex?",
  client_condom_freq_30d = "В16.2.1 How often did you use a condom during vaginal sex?",
  perm_partner_condom_lastsex = "В11. Remember your last sexual contact with a PERMANENT partner from whom you RECEIVED NO FEE. Did you use a condom?",
  cas_partner_condom_lastsex = "В13. Remember your last sexual contact with a CASUAL partner from whom you RECEIVED NO FEE. Did you use a condom?",
  perm_partner_condom_30d = "В18.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with a PERMANENT PARTNER?",
  cas_partner_condom_30d = "В20.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with CASUAL PARTNERS?",
  ngo_access_lifetime = "D3.1 Are you a client of any public organization (have a card or an individual code) that works with female sex workers or injecting drug users?",
  typology_primary_6m = "А14.2 Please, tell me, which of the ways to find clients you indicated is the primary one for you?",
  aids_center_bin = "F14. Please tell if you are registered with an AIDS center?",
  alcohol_30d_num = "С1. How often did you use alcohol in the LAST MONTH [30 days]?",
  drugs_30d_bin = "С2. Some people try to use various drugs. Do you use any drugs?",
  used_syringe_last = "С4. Did you use common injection equipment (syringe or needle already used by someone) during your MOST RECENT INJECTION?",
  hiv_tested_lifetime = "F6. I don’t ask you about the result but I want to ask if you underwent HIV testing?",
  hiv_tested_12m = "F8. Let’s specify if that was during the RECENT 12 MONTHS?",
  hiv_tested_result = "F11. I don’t ask you about the result, but did you receive your last test result?",
  hiv_status_selfreport = "F13.1 HIV status",
  blood_screen_bin = "І1. Was blood screening conducted or not?",
  hiv_test_rslt = "І3. HIV testing result",
  syphilis_test_rslt = "S3. Syphilis testing result"
)

# map to vars
sw_data_2009_clean <- sw_data_2009_raw %>%
  rename(!!!setNames(rename_map_2009, names(rename_map_2009)))
  
# check variables are all present
renamed_vars <- names(rename_map_2009)
missing_renamed <- setdiff(renamed_vars, names(sw_data_2009_clean))
if (length(missing_renamed) == 0) {
  cat("All renamed variables are present in sw_data_2009_clean.\n")
} else {
  cat("Missing renamed variables:\n")
  print(missing_renamed)
}

# load 2011 data
sw_data_2011_raw <- read_excel("2011_IBBS_FSW_TLS AND RDS_Data.xlsx")

# derive volume variables
sw_data_2011_raw <- sw_data_2011_raw %>%
  mutate(
    sw_partners_clients_7d_raw = `В4.1 During THE RECENT (WORKING) WEEK, how many persons listed below were among your sexual partners [clients]? Partners [clients] who you RECEIVED A FEE [money or other] from`,
    sw_partners_perm_7d_raw = `В4.2 During THE RECENT (WORKING) WEEK, how many persons listed below were among your sexual partners [clients]? Permanent partners who you RECEIVED NO FEE [money or other] from`,
    sw_partners_nonperm_7d_raw = `В4.3 During THE RECENT (WORKING) WEEK, how many persons listed below were among your sexual partners [clients]? Casual partners who you RECEIVED NO FEE [money or other] fromе]`,
    sw_partners_total_7d_raw = `В4.4 How MANY DIFFERENT sexual partners in total did you have during the recent (working) week, including your husband or your permanent sexual partner with whom you live?`
  )

sw_data_2011_raw <- sw_data_2011_raw %>%
  mutate(
  sw_partners_clients_7d = as.numeric(ifelse(sw_partners_clients_7d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_clients_7d_raw)),
  sw_partners_clients_30d = sw_partners_clients_7d*days_in_month,

  sw_partners_perm_7d = as.numeric(ifelse(sw_partners_perm_7d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_perm_7d_raw)),
  sw_partners_perm_30d = sw_partners_perm_7d*days_in_month,

  sw_partners_nonperm_7d = as.numeric(ifelse(sw_partners_nonperm_7d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_nonperm_7d_raw)),
  sw_partners_nonperm_30d = sw_partners_nonperm_7d*days_in_month,

  sw_partners_nonclients_7d = sw_partners_perm_7d+sw_partners_nonperm_7d,
  sw_partners_nonclients_30d = sw_partners_nonclients_7d*days_in_month,

  sw_partners_total_7d = as.numeric(ifelse(sw_partners_total_7d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_total_7d_raw)),
  sw_partners_total_30d = sw_partners_total_7d*days_in_month,
  )

# variables to rename
rename_map_2011 <- c(
  sw_freq_6m = "А11. How often have your provided sexual services for a fee in THE LAST 6 MONTHS?",
  sw_days_total_7d = "А12. How many days DURING THE LAST WEEK [7 days] have you provided sexual services for a fee?",
  education = "А1. Your education",
  residence = "А3. Where do you live in this city?",
  marital_status = "А8. Choose from the suggested options one corresponding to your marital status at the moment",
  age = "S2. Your age:",
  age_first_sex = "В1. At what age did you have sexual relations for the first time?",
  age_first_sw = "В2. How old were you when you provided sexual services for a fee (money or other) for the first time?",
  city = "City",
  city_travel_12m = "А5. Have you ever left this city [CITY OF SURVEY] for more than 1 month [30 days] in THE LAST 12 MONTHS to provide sexual services?",
  sw_partners_clients_7d_raw = "sw_partners_clients_7d_raw",
  sw_partners_perm_7d_raw = "sw_partners_perm_7d_raw",
  sw_partners_nonperm_7d_raw = "sw_partners_nonperm_7d_raw",
  sw_partners_total_7d_raw = "sw_partners_total_7d_raw",  
  sw_partners_clients_7d = "sw_partners_clients_7d",
  sw_partners_clients_30d = "sw_partners_clients_30d",
  sw_partners_perm_7d = "sw_partners_perm_7d",
  sw_partners_perm_30d = "sw_partners_perm_30d",
  sw_partners_nonperm_7d = "sw_partners_nonperm_7d",
  sw_partners_nonperm_30d = "sw_partners_nonperm_30d",
  sw_partners_nonclients_7d = "sw_partners_nonclients_7d",
  sw_partners_nonclients_30d = "sw_partners_nonclients_30d",
  sw_partners_total_7d = "sw_partners_total_7d",
  sw_partners_total_30d = "sw_partners_total_30d",
  partners_sw_24h = "В5. How many different CLIENTS whom you provided sexual services for a fee did you have FOR THE LAST WORKING DAY (24 HOURS)?",
  partners_age_6m = "В3.Б2. Among the age groups you indicated, representatives of which one have you met most often?",
  client_condom_lastsex = "В6.1 Remember your sexual contact with your MOST RECENT CLIENT. Did you use a condom?",
  perm_partner_condom_lastsex = "В13. Remember your last sexual contact with a PERMANENT partner from whom you RECEIVED NO FEE. Did you use a condom?",
  cas_partner_condom_lastsex = "В17. Remember your last sexual contact with CASUAL partner from whom you RECEIVED NO FEE. Did you use a condom?",
  condom_access_12m = "D2. Did you receive condoms free of charge in the LAST 12 MONTHS (e.g. via educational and awareness-raising programs or projects, syringe exchange sites, counseling centers, centers of social services for family, children and youth, during actions, etc.)",
  ngo_access_lifetime = "D3.1 Are you a client of any non-governmental organization (have a card or an individual code) that works with female sex workers or injecting drug users?",
  typology_primary_6m = "А14.2 Please, tell me, which of the ways to find clients you indicated is the primary one for you?",
  aids_center_bin = "F14. Please tell if you are registered with an AIDS center?",
  alcohol_30d_num = "С1. How often did you use alcohol in the LAST MONTH [30 days]?",
  idu_12m_bin = "C3.2.2 Have you used drugs by injection in the last 12 months?",
  drugs_30d_bin = "С2. Some people try to use various drugs. Do you use any drugs?",
  used_syringe_last = "С4.1 Did you use common injection equipment (syringe or needle already used by someone) during your MOST RECENT INJECTION?",
  oat_current = "С4.3 Are you participating in substitution therapy program?",
  primary_drug_3m = "С3.4 What drug listed in the previous question do you consider the primary one for you?",
  violence_any_ever = "V1. Have you ever been subjected to violence while providing sexual services? For example, you were verbally or physically humiliated / beaten / forced to provide sexual services for free?",
  hiv_tested_lifetime = "F6. I am not asking you about the result but I want to ask if you underwent HIV testing",
  hiv_tested_12m = "F8. Let’s specify if that was during the RECENT 12 MONTHS",
  hiv_tested_result = "F11. I am not asking you about the result, but did you receive your last test result?",
  hiv_status_selfreport = "F13.1 HIV status",
  art_current = "F15. Are you participating in ART program?",
  blood_screen_bin = "І1. Was blood screening conducted or not?",
  hiv_test_rslt = "І3. HIV testing result",
  syphilis_test_rslt = "S3. Syphilis testing result"
)

# map to vars
sw_data_2011_clean <- sw_data_2011_raw %>%
  rename(!!!setNames(rename_map_2011, names(rename_map_2011)))

# check variables are all present
renamed_vars <- names(rename_map_2011)
missing_renamed <- setdiff(renamed_vars, names(sw_data_2011_clean))
if (length(missing_renamed) == 0) {
  cat("All renamed variables are present in sw_data_2011_clean.\n")
} else {
  cat("Missing renamed variables:\n")
  print(missing_renamed)
}

# load 2013 data
sw_data_2013_raw <- read_excel("2013_IBBS_FSW_TLS AND RDS_Data.xlsx")

# derive volume variables
sw_data_2013_raw <- sw_data_2013_raw %>%
  mutate(
    sw_partners_clients_30d_raw = `B.4.1_2 Number of sexual partners in the PAST MONTH (30 days) Clients from which you RECEIVED A FEE [money or other] for sexual services`,
    sw_partners_perm_30d_raw = `B.4.2_2 Number of sexual partners in the PAST MONTH (30 days) Permanent from which you RECEIVED NO FEE [money or other]`,
    sw_partners_nonperm_30d_raw = `B.4.3_2 Number of sexual partners in the PAST MONTH (30 days) Casual partners from which you RECEIVED NO FEE [money or other])`,
    sw_partners_total_30d_raw = `В4.4 How MANY DIFFERENT sexual partners in total did you have during the past month  (30 days), including your husband or your permanent sexual partner with whom you live?`
  )

sw_data_2013_raw <- sw_data_2013_raw %>%
  mutate(
    sw_partners_clients_30d = as.numeric(case_when(
      sw_partners_clients_30d_raw %in% c("No answer", "Do not remember", "Refusal to answer", 998, 999) ~ NA_real_,
      sw_partners_clients_30d_raw == "No question asked" ~ 0,
      TRUE ~ as.numeric(sw_partners_clients_30d_raw)
    )),
    sw_partners_clients_7d = sw_partners_clients_30d / days_in_month,

    sw_partners_perm_30d = as.numeric(case_when(
      sw_partners_perm_30d_raw %in% c("No answer", "Do not remember", "Refusal to answer", 997, 998, 999) ~ NA_real_,
      sw_partners_perm_30d_raw == "No question asked" ~ 0,
      TRUE ~ as.numeric(sw_partners_perm_30d_raw)
    )),
    sw_partners_perm_7d = sw_partners_perm_30d / days_in_month,

    sw_partners_nonperm_30d = as.numeric(case_when(
      sw_partners_nonperm_30d_raw %in% c("No answer", "Do not remember", "Refusal to answer", 997, 998, 999) ~ NA_real_,
      sw_partners_nonperm_30d_raw == "No question asked" ~ 0,
      TRUE ~ as.numeric(sw_partners_nonperm_30d_raw)
    )),
    sw_partners_nonperm_7d = sw_partners_nonperm_30d / days_in_month,

    sw_partners_nonclients_30d = sw_partners_perm_30d + sw_partners_nonperm_30d,
    sw_partners_nonclients_7d = sw_partners_nonclients_30d / days_in_month,

    sw_partners_total_30d = as.numeric(case_when(
      sw_partners_total_30d_raw %in% c("No answer", "Do not remember", "Refusal to answer", 998, 999) ~ NA_real_,
      sw_partners_total_30d_raw == "No question asked" ~ 0,
      TRUE ~ as.numeric(sw_partners_total_30d_raw)
    )),
    sw_partners_total_7d = sw_partners_total_30d / days_in_month
  )

# derive date variable
sw_data_2013_raw <- sw_data_2013_raw %>%
  mutate(
    interview_dte = as.Date(
      paste(`Interview Date - Day`,
            `Interview Date - Month`,
            2013,
            sep = "/"),
      format = "%d/%m/%Y"
    )
  )

# variables to rename
rename_map_2013 <- c(
  id = "ID",
  interview_dte = "interview_dte",
  sw_days_total_7d = "А19. How many days in the LAST WEEK [7 days] did you provide sexual services?",
  education = "А6. What is your educational level?",
  residence_30d = "А8. What has been your permanent place of residence in the last month (30 days)?",
  marital_status = "А12. Choose from the suggested alternatives the one corresponding to your marital status at the moment",
  income_30d_cat = "А17. Tell me please, what has been your PERSONAL income in the last 30 days?",
  age = "I2. Your age",
  age_first_sex = "В1.At what age did you start sexual relations for the first time?",
  age_first_sw = "В2. How old were you when you provided sexual services for a fee (money or other) for the first time?",
  city = "City",
  city_travel_12m = "А11. Have you ever left this city for more than 1 month [30 days] during THE LAST 12 MONTHS to provide sexual services?",
  sw_partners_clients_30d_raw = "sw_partners_clients_30d_raw",
  sw_partners_perm_30d_raw = "sw_partners_perm_30d_raw",
  sw_partners_nonperm_30d_raw = "sw_partners_nonperm_30d_raw",
  sw_partners_total_30d_raw = "sw_partners_total_30d_raw",
  sw_partners_clients_7d = "sw_partners_clients_7d",
  sw_partners_clients_30d = "sw_partners_clients_30d",
  sw_partners_perm_7d = "sw_partners_perm_7d",
  sw_partners_perm_30d = "sw_partners_perm_30d",
  sw_partners_nonperm_7d = "sw_partners_nonperm_7d",
  sw_partners_nonperm_30d = "sw_partners_nonperm_30d",
  sw_partners_nonclients_7d = "sw_partners_nonclients_7d",
  sw_partners_nonclients_30d = "sw_partners_nonclients_30d",
  sw_partners_total_7d = "sw_partners_total_7d",
  sw_partners_total_30d = "sw_partners_total_30d",
  partners_sw_24h = "В5. How many different CLIENTS whom you provided sexual services for a fee you had FOR THE LAST WORKING DAY (24 HOURS)?",
  partners_age_30d = "В3.4. Among the age groups you indicated, representatives of which one have you met most often in the LAST month (30 days)?",
  client_condom_lastsex = "В6.1 Remember your sexual contact with your LAST CLIENT. Did you use a condom?",
  client_condom_bin_30d = "В8.1.Please think of the last 30 DAYS once again. Was there a case when you DID NOT use a condom with your client during oral sex?",
  client_condom_freq_30d = "B8. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during oral sex?",
  perm_partner_condom_lastsex = "В15. Remember your last sexual contact with a PERMANENT partner from whom you received no remuneration. Did you used a condom?",
  perm_client_condom_lastsex = "В12. Remember your last sexual contact with a PERMANENT CLIENT. Did you or your partner use a condom?",
  cas_partner_condom_lastsex = "В21.Remember your last sexual contact with a CASUAL partner from whom you received no remuneration. Did you used a condom?",
  group_sex_30d = "В30. Have you practiced GROUP SEX in the last 30 days?",
  condom_access_12m = "D2. Did you receive condoms free of charge in the LAST 12 MONTHS (e.g. in NGOs, counseling centers, centers of social services for family, children and youth, during actions, etc.)?",
  typology_primary_30d = "B3.7 . Please tell me, among the following ways to find your clients what is the PRIMARY for you?",
  aids_center_bin = "G14. Please say if you are registered in the AIDS centre?",
  alcohol_30d_num = "С1. How often did you use alcohol or Alco pops in the LAST MONTH [30 days]?",
  drugs_30d_bin = "С2. Some people try to use various drugs. Do you use / Did you use any drugs?",
  idu_12m_bin = "С3. Have you injected drugs (withthesyringe) in the last 12 months?",
  idu_30d_bin = "С4. Have you injected drugs in the last 30 days (last month)?",
  used_syringe_last = "С4.2. Did you inject a drug with the injecting equipment (syringe, needle) previously used by another person or drugs that were prepared in the common pot or you don’t know how the syringe has been filled?",
  sex_with_drugs_30d = "С6_2 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration Narcotic substances",
  sex_with_alcohol_30d = "С6_1. How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration Alcohol, alco pops",
  sex_with_drugs_and_alcohol_30d = "С6_3 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration Alcohol + drugs",
  primary_drug_inj_30d = "С4.1. Which of the injecting drugs do you consider a primary one for you?",
  ngo_access_lifetime = "D3.Are you a client of any non-governmental organization (have a card or an individual code) that works with female sex workers?",
  violence_any_ever = "F1. Did you suffer violence at time of sexual services?",
  violence_rape_ever = "F1_2_1 If “yes”, how? Raped",
  violence_beaten_ever = "F1_2_3 If “yes”, how? Beaten",
  violence_humiliated_ever = "F1_2_2 If “yes”, how? Humiliated morally ( verbally )",
  violence_physical_abuse_ever = "F1_2_4 If “yes”, how? Physically abused",
  violence_client = "F2_1 Who inflicted violence? Clients",
  violence_perm_partner = "F2_2 Who inflicted violence? Permanent sexual partner",
  violence_casual_partner = "F2_3 Who inflicted violence? Casual sexual partner",
  violence_police = "F2_4 Who inflicted violence? Law enforcement officer",
  violence_pimp = "F2_5 Who inflicted violence? Pimp/manager of apartment",
  violence_fsw = "F2_8 Who inflicted violence? Girls from among FSW",
  violence_support_ngo = "F3_1. Have you addressed anywhere or to anyone for help? To NGO/ crisis centre",
  hiv_tested_lifetime = "G5. I am not asking now about the test result, but have you ever had an HIV- test?",
  hiv_tested_12m = "G8. Let's be more precise. Was it within the last 12 months?",
  hiv_tested_result = "G11. I am not asking you about the result, but did you get your result of the last test?",
  hiv_status_selfreport = "G13.1.If «yes», it was:",
  art_current = "С5. Are you participating in the antiretroviral therapy program (ART)?",
  hiv_test_rslt = "Т2. Indicate the result of respondent's HIV-test."
)

# map to vars
sw_data_2013_clean <- sw_data_2013_raw %>%
  rename(!!!setNames(rename_map_2013, names(rename_map_2013)))

# check variables are all present
renamed_vars <- names(rename_map_2013)
missing_renamed <- setdiff(renamed_vars, names(rename_map_2013))
if (length(missing_renamed) == 0) {
  cat("All renamed variables are present in sw_data_2013_clean.\n")
} else {
  cat("Missing renamed variables:\n")
  print(missing_renamed)
}

# load 2015 data
sw_data_2015_raw <- read_excel("2015_IBBS_SW_TLS AND RDS_Data.xlsx")

# derive volume variables
sw_data_2015_raw <- sw_data_2015_raw %>%
  mutate(
    sw_partners_reg_clients_30d_raw = `Number of regular clients from whom you RECEIVED COMPENSATION [money or other] for providing sexual services in the LAST 30 DAYS.`,
    sw_partners_irreg_clients_30d_raw = `Number of irregular clients from whom you RECEIVED COMPENSATION [money or other] for providing sexual services in the LAST 30 DAYS.`,    
    sw_partners_perm_30d_raw = `Number of regular sexual partners from whom you did NOT RECEIVE COMPENSATION [money or other] in the LAST 30 DAYS.`,
    sw_partners_nonperm_30d_raw = `Number of casual sexual partners from whom you did NOT RECEIVE COMPENSATION [money or other] in the LAST 30 DAYS.`,
    sw_partners_total_30d_raw = `Total number of different sexual partners in the LAST 30 DAYS.`
  )

sw_data_2015_raw <- sw_data_2015_raw %>%
  mutate(
    sw_partners_reg_clients_30d = as.numeric(case_when(
      sw_partners_reg_clients_30d_raw %in% c("No answer", "Do not remember", "Refusal to answer", 998, 999) ~ NA_real_,
      sw_partners_reg_clients_30d_raw == "No question asked" ~ 0,
      TRUE ~ as.numeric(sw_partners_reg_clients_30d_raw)
    )),
    sw_partners_reg_clients_7d = sw_partners_reg_clients_30d / days_in_month,

    sw_partners_irreg_clients_30d = as.numeric(case_when(
      sw_partners_irreg_clients_30d_raw %in% c("No answer", "Do not remember", "Refusal to answer", 998, 999) ~ NA_real_,
      sw_partners_irreg_clients_30d_raw == "No question asked" ~ 0,
      TRUE ~ as.numeric(sw_partners_irreg_clients_30d_raw)
    )),
    sw_partners_irreg_clients_7d = sw_partners_irreg_clients_30d / days_in_month,

    sw_partners_clients_30d = sw_partners_irreg_clients_30d + sw_partners_reg_clients_30d,
    sw_partners_clients_7d = sw_partners_clients_30d / days_in_month,

    sw_partners_perm_30d = as.numeric(case_when(
      sw_partners_perm_30d_raw %in% c("No answer", "Do not remember", "Refusal to answer", 997, 998, 999) ~ NA_real_,
      sw_partners_perm_30d_raw == "No question asked" ~ 0,
      TRUE ~ as.numeric(sw_partners_perm_30d_raw)
    )),
    sw_partners_perm_7d = sw_partners_perm_30d / days_in_month,

    sw_partners_nonperm_30d = as.numeric(case_when(
      sw_partners_nonperm_30d_raw %in% c("No answer", "Do not remember", "Refusal to answer", 997, 998, 999) ~ NA_real_,
      sw_partners_nonperm_30d_raw == "No question asked" ~ 0,
      TRUE ~ as.numeric(sw_partners_nonperm_30d_raw)
    )),
    sw_partners_nonperm_7d = sw_partners_nonperm_30d / days_in_month,

    sw_partners_nonclients_30d = sw_partners_perm_30d + sw_partners_nonperm_30d,
    sw_partners_nonclients_7d = sw_partners_nonclients_30d / days_in_month,

    sw_partners_total_30d = as.numeric(case_when(
      sw_partners_total_30d_raw %in% c("No answer", "Do not remember", "Refusal to answer", 998, 999) ~ NA_real_,
      sw_partners_total_30d_raw == "No question asked" ~ 0,
      TRUE ~ as.numeric(sw_partners_total_30d_raw)
    )),
    sw_partners_total_7d = sw_partners_total_30d / days_in_month
  )

sw_data_2015_raw <- sw_data_2015_raw %>%
  mutate(
    interview_dte = as.Date(`Date of interview`, format = "%d.%m.%Y")
  )

# variables to rename
rename_map_2015 <- c(
  id = "Respondent’s ID",
  interview_dte = "interview_dte",
  gender = "Gender",
  education = "А7. What is your educational level?",
  marital_status = "А13. Choose from the suggested alternatives the one corresponding to your marital status at the moment:",
  income_30d_cat = "А18. Tell me please, what has been your PERSONAL income in the last 30 days?",
  age = "F5.1. Specify your age",
  age_first_sex = "В1.At what age did you start sexual relations for the first time?",
  age_first_sw = "В2. How old were you when you provided sexual services for a fee (money or other) for the first time?",
  city = "City",
  city_travel_12m = "А12. Have you ever left this city for more than 1 month [30 days] during THE LAST 12 MONTHS to provide sexual services?",
  country_travel_12m = "A12_2 Where have you been? In another country",
  sw_days_total_7d = "А20. How many days in the LAST WEEK [7 days] did you provide sexual services?",
  sw_partners_reg_clients_30d_raw = "sw_partners_reg_clients_30d_raw",
  sw_partners_irreg_clients_30d_raw = "sw_partners_irreg_clients_30d_raw",
  sw_partners_perm_30d_raw = "sw_partners_perm_30d_raw",
  sw_partners_nonperm_30d_raw = "sw_partners_nonperm_30d_raw",
  sw_partners_total_30d_raw = "sw_partners_total_30d_raw",
  sw_partners_reg_clients_7d = "sw_partners_reg_clients_7d",
  sw_partners_reg_clients_30d = "sw_partners_reg_clients_30d",
  sw_partners_irreg_clients_7d = "sw_partners_irreg_clients_7d",
  sw_partners_irreg_clients_30d = "sw_partners_irreg_clients_30d",
  sw_partners_perm_7d = "sw_partners_perm_7d",
  sw_partners_perm_30d = "sw_partners_perm_30d",
  sw_partners_nonperm_7d = "sw_partners_nonperm_7d",
  sw_partners_nonperm_30d = "sw_partners_nonperm_30d",
  sw_partners_nonclients_7d = "sw_partners_nonclients_7d",
  sw_partners_nonclients_30d = "sw_partners_nonclients_30d",
  sw_partners_total_7d = "sw_partners_total_7d",
  sw_partners_total_30d = "sw_partners_total_30d",
  partners_sw_24h = "В5. How many different CLIENTS whom you provided sexual services for a fee you had FOR THE LAST WORKING DAY (24 HOURS)?",
  partners_age_30d = "В3.4. Among the age groups you indicated, representatives of which one have you met most often in the LAST month (30 days)",
  client_condom_lastsex = "В6.1. Remember your sexual contact with your LAST CLIENT. Did you use a condom?",
  perm_partner_condom_lastsex = "В15. Remember your last sexual contact with a PERMANENT partner from whom you received no remuneration. Did you used a condom?",
  cas_partner_condom_lastsex = "В21. Remember your last sexual contact with a CASUAL partner from whom you received no remuneration. Did you used a condom?",
  group_sex_30d = "В30.Have you practiced GROUP SEX in the last 30 days?",
  condom_access_12m = "D2. Did you receive condoms free of charge in the LAST 12 MONTHS (e.g. in NGOs, counseling centers, centers of social services for family, children and youth, during actions, etc.)?",
  typology_primary_30d = "B3.7 . Please tell me , among the following ways to find your clients what is the PRIMARY for you?",
  aids_center_bin = "G14. Please say if you are registered in the AIDS center?",
  alcohol_30d_num = "С1. How often did you use alcohol or Alco pops in the LAST MONTH [30 days]?",
  idu_30d_bin = "С4. Have you injected drugs in the last 30 days (last month)?",
  idu_30d_num = "С4_1. Have you injected drugs in the last 30 days (last month)? - If “yes”, how often?",
  used_syringe_last = "С4.2. Did you used sterile syringe and needle when you last injected drug?",
  ngo_access_lifetime = "D3. Are you a client of any non-governmental organization (have a card or an individual code) that works with sex workers",
  ngo_access_6m = "D4_2 Have you received female condoms from a representative of this NGO in the past 6 months?",
  harm_reduction_12m = "F7. Your access to prevention materials (e.g. syringes, condoms) and counseling in the past 12 months:",
  violence_any_ever = "F1. Did you suffer violence at time of sexual services?",
  violence_beaten_ever = "F2.1_4 If “yes”, how? - Beaten",
  violence_rape_ever = "F2.1_7 If “yes”, how? - Raped",
  violence_humiliated_ever = "F2.1_1 If “yes”, how? - Humiliated morally (verbally)",
  violence_physical_abuse_ever = "F2.1_5 If “yes”, how? - Physically abused",
  violence_client = "F2_1 Who inflicted violence? - Clients",
  violence_perm_partner = "F2_2 Who inflicted violence? - Permanent sexual partner",
  violence_casual_partner = "F2_3 Who inflicted violence? - Casual sexual partner",
  violence_police = "F2_4 Who inflicted violence? - Law enforcement officer",
  violence_pimp = "F2_5 Who inflicted violence? - Pimp/manager of apartment",
  violence_fsw = "F2_8 Who inflicted violence? - Girls from among FSW",
  violence_rape_12m = "F4. In the last 12 months have you ever been forced to provide sexual services for clients without any remuneration?",
  violence_support_ngo = "F3_1 Have you addressed any where or to anyone for help? - To NGO/ crisis center",
  hiv_tested_lifetime = "G5. I am not asking now about the test result, but have you ever had an HIV- test?",
  hiv_tested_12m = "G8. Let's be more precise. Was it within the last 12 months?",
  hiv_tested_result = "G11. I am not asking you about the result, but did you get your result of the last test?",
  hiv_status_selfreport = "G13.1.If «yes», it was",
  art_current = "G15. Are you participating in the antiretroviral therapy program (ART)?",
  blood_screen_bin = "Т1. Did a respondent have pre-test counseling?",
  hiv_test_rslt = "Т2_1. Indicate the results of respondent's tests: HIV",
  hiv_vl = "VL_result",
  syphilis_test_rslt = "Т2_4. Indicate the results of respondent's tests: yphilis"
)

# map to vars
sw_data_2015_clean <- sw_data_2015_raw %>%
  rename(!!!setNames(rename_map_2015, names(rename_map_2015)))

# check variables are all present
renamed_vars <- names(rename_map_2015)
missing_renamed <- setdiff(renamed_vars, names(rename_map_2015))
if (length(missing_renamed) == 0) {
  cat("All renamed variables are present in sw_data_2015_clean.\n")
} else {
  cat("Missing renamed variables:\n")
  print(missing_renamed)
}

# load 2017 data
sw_data_2017_raw <- read_excel("2017-2018_IBBS_SW_TLS AND RDS_Data.xlsx")

# derive volume variables
sw_data_2017_raw <- sw_data_2017_raw %>%
  mutate(
    sw_partners_reg_clients_30d_raw = `B4_1_1 Number in the PAST MONTH (30 days) Permanent clients from which you RECEIVED A FEE [money or other] for sexual services`,
    sw_partners_irreg_clients_30d_raw = `B4_2_1 Number in the PAST MONTH (30 days) Casual clients from which you RECEIVED FEE [money or other]`,    
    sw_partners_perm_30d_raw = `B4_3_1 Number in the PAST MONTH (30 days) Permanent partners from which you RECEIVED NO FEE [money or other])`,
    sw_partners_nonperm_30d_raw = `B4_4_1 Number in the PAST MONTH (30 days) Casual partners from which you RECEIVED NO FEE [money or other]`,
    sw_partners_total_30d_raw = `В4.6 How MANY DIFFERENT sexual partners in total did you have during the past 30 days, including your permanent sexual partner with whom you live?`
  )

sw_data_2017_raw <- sw_data_2017_raw %>%
  mutate(
  sw_partners_reg_clients_30d = as.numeric(ifelse(sw_partners_reg_clients_30d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_reg_clients_30d_raw)),
  sw_partners_reg_clients_7d = sw_partners_reg_clients_30d/days_in_month,

  sw_partners_irreg_clients_30d = as.numeric(ifelse(sw_partners_irreg_clients_30d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_irreg_clients_30d_raw)),
  sw_partners_irreg_clients_7d = sw_partners_irreg_clients_30d/days_in_month,

  sw_partners_clients_30d = sw_partners_irreg_clients_30d+sw_partners_reg_clients_30d,
  sw_partners_clients_7d = sw_partners_irreg_clients_7d+sw_partners_reg_clients_7d,

  sw_partners_perm_30d = as.numeric(ifelse(sw_partners_perm_30d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_perm_30d_raw)),
  sw_partners_perm_7d = sw_partners_perm_30d/days_in_month,

  sw_partners_nonperm_30d = as.numeric(ifelse(sw_partners_nonperm_30d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_nonperm_30d_raw)),
  sw_partners_nonperm_7d = sw_partners_nonperm_30d/days_in_month,

  sw_partners_nonclients_30d = sw_partners_perm_30d+sw_partners_nonperm_30d,
  sw_partners_nonclients_7d = sw_partners_nonclients_30d/days_in_month,

  sw_partners_total_30d = as.numeric(ifelse(sw_partners_total_30d_raw %in% c("No answer","Do not remember","Refusal to answer",998,999), NA, sw_partners_total_30d_raw)),
  sw_partners_total_7d = sw_partners_total_30d/days_in_month,
  )

# derive age variable
sw_data_2017_raw <- sw_data_2017_raw %>%
  mutate(
    age = year(`Interview date`) - as.numeric(`А6. Specify your year of birth`)
  )

# interview date
sw_data_2017_raw <- sw_data_2017_raw %>%
  mutate(
    interview_dte = as.Date(`Interview date`, format = "%d.%m.%Y")
  )

# variables to rename
rename_map_2017 <- c(
  id = "Respondent’s ID",
  interview_dte = "interview_dte",
  gender = "A1. Respondent’ gender",
  sw_days_total_7d = "А17. How many days in the LAST WEEK [7 days] did you provide sexual services?",
  sw_num_30d = "B6. Please remember sexual intercourses with all clients during last month (30 days)/ How many sexual intercourses did you have during last month (including vaginal, anal and oral intercourses)?",
  education = "А9. What is your educational level?",
  marital_status = "А13. Choose from the suggested alternatives the one corresponding to your marital status at the moment:",
  income_30d = "А15. Tell me please, what has been your PERSONAL income in the last 30 days? UAN",
  age = "age",
  age_first_sex = "В1.At what age did you start sexual relations for the first time?",
  age_first_sw = "В2. How old were you when you provided sexual services for a fee (money or other) for the first time?",
  city = "City",
  city_travel_12m = "А12. Have you ever left this city for more than 1 month [30 days] during THE LAST 12 MONTHS to provide sexual services?",
  partners_sw_24h = "В5. How many different CLIENTS whom you provided sexual services for a fee you had FOR THE LAST WORKING DAY (24 HOURS)?",
  sw_partners_reg_clients_7d = "sw_partners_reg_clients_7d",
  sw_partners_reg_clients_30d_raw = "sw_partners_reg_clients_30d_raw",
  sw_partners_irreg_clients_30d_raw = "sw_partners_irreg_clients_30d_raw",
  sw_partners_perm_30d_raw = "sw_partners_perm_30d_raw",
  sw_partners_nonperm_30d_raw = "sw_partners_nonperm_30d_raw",
  sw_partners_total_30d_raw = "sw_partners_total_30d_raw",
  sw_partners_reg_clients_30d = "sw_partners_reg_clients_30d",
  sw_partners_irreg_clients_7d = "sw_partners_irreg_clients_7d",
  sw_partners_irreg_clients_30d = "sw_partners_irreg_clients_30d",
  sw_partners_perm_7d = "sw_partners_perm_7d",
  sw_partners_perm_30d = "sw_partners_perm_30d",
  sw_partners_nonperm_7d = "sw_partners_nonperm_7d",
  sw_partners_nonperm_30d = "sw_partners_nonperm_30d",
  sw_partners_nonclients_7d = "sw_partners_nonclients_7d",
  sw_partners_nonclients_30d = "sw_partners_nonclients_30d",
  sw_partners_total_7d = "sw_partners_total_7d",
  sw_partners_total_30d = "sw_partners_total_30d",
  partners_age_30d = "В3.4. Among the age groups you indicated, representatives of which one have you met most often in the LAST month (30 days)?",
  client_condom_lastsex = "В8. Remember your sexual contact with your LAST CLIENT. Did you use a condom?",
  client_condom_bin_7d = "B14. Think about your LAST WORKING WEEK (7 days), when you provided sexual services for remuneration. Were there cases of not using a condom?",
  perm_partner_condom_lastsex = "В19. Remember your last sexual contact with a PERMANENT partner from whom you received no remuneration. Did you used a condom?",
  cas_partner_condom_lastsex = "B22 Remember your last sexual contact with a CASUAL partner from whom you received no remuneration. Did you used a condom?",
  group_sex_30d = "В25. Have you practiced GROUP SEX in the last 30 days?",
  condom_access_12m = "F1. Did you receive condoms for free during last 12 months? (from NGO, medical staff, at the parties etc.)",
  typology_primary_30d = "B3.7 . Please tell me, among the following ways to find your clients what is the PRIMARY for you?",
  aids_center_bin = "G12. Please say if you are registered in the AIDS center?",
  alcohol_30d_num = "С1. How many times in the last 30 days have you used alcohol drinks?",
  drugs_12m_bin = "С2. Some people try to use various drugs. Do you use / Did you use any non-injection drugs?",
  idu_12m_bin = "C4. Have you injected drugs (with the syringe)?",
  used_syringe_last = "С6. Did you used sterile syringe and needle when you last injected drug?",
  ngo_access_lifetime = "F2. Are you a client of any non-governmental organization (have a card or individual code), that provides prevention services for SW",
  prep_12m = "F10. Have you used PrEP in the last 12 months?",
  harm_reduction_12m = "F6. Your access to prevention materials (e.g. syringes, condoms) and counseling in the past 12 months:",
  violence_any_ever = "H1. Did you suffer violence at time of sexual services?",
  violence_beaten_ever = "H1_3 If “yes”, how?  - Beaten",
  violence_humiliated_ever = "H1_1 If “yes”, how? - Humiliated morally (verbally)",
  violence_physical_abuse_ever = "H1_4 If “yes”, how?  - Physically abused",
  violence_rape_ever = "H1_6 If “yes”, how?  - Raped",
  violence_client = "H2_1 Who inflicted violence? - Clients",
  violence_perm_partner = "H2_2 Who inflicted violence? - Permanent sexual partner",
  violence_casual_partner = "H2_3 Who inflicted violence? - Casual sexual partner",
  violence_police = "H2_4 Who inflicted violence? - Law enforcement officer",
  violence_pimp = "H2_5 Who inflicted violence? - Pimp/manager of apartment",
  violence_fsw = "H2_8 Who inflicted violence? - Girls from among FSW",
  violence_support_ngo = "H3_1 Have you addressed anywhere or to anyone for help?  - To NGO/ crisis center",
  violence_rape_12m = "H4.Were any situations when you was forced to provide sexual services without remuneration during the last 12 months?",
  hiv_tested_lifetime = "G3.  I don’t ask about the result, but have ever been tested for HIV?",
  hiv_tested_12m = "G7. Let's be more precise. Was it within the last 12 months?",
  hiv_tested_result = "G9. Did you get your result of the last test?",
  hiv_status_selfreport = "G11.1. If «yes», it was",
  art_current = "G14. Are you using the antiretroviral therapy (ART)?",
  blood_screen_bin = "Т1. Did a respondent have pre-test counselling?",
  hiv_test_rslt = "T5_1 Yes, the result was",
  hiv_vl = "VL result",
  hiv_recent_infection = "Recent infection test in lab"
)

# map to vars
sw_data_2017_clean <- sw_data_2017_raw %>%
  rename(!!!setNames(rename_map_2017, names(rename_map_2017)))

# check variables are all present
renamed_vars <- names(rename_map_2017)
missing_renamed <- setdiff(renamed_vars, names(rename_map_2017))
if (length(missing_renamed) == 0) {
  cat("All renamed variables are present in sw_data_2017_clean.\n")
} else {
  cat("Missing renamed variables:\n")
  print(missing_renamed)
}

# load 2021 data
sw_data_2021_raw <- read_excel("2021_IBBS_SW_TLS_Data.xlsx")

sw_data_2021_raw <- sw_data_2021_raw %>%
  mutate(
    sw_partners_reg_clients_30d_bin = `b2_1 Having sexual partners within the last 30 day Regular client`,
    sw_partners_reg_clients_30d_raw = `b2_1_1 How many partners with whom you had sexual contact within the last 30 days belonged to Regular client`,
    sw_partners_irreg_clients_30d_bin = `b2_2 Having sexual partners within the last 30 day Non-regular client`,
    sw_partners_irreg_clients_30d_raw = `b2_2_1 How many partners with whom you had sexual contact within the last 30 days belonged to Non-regular client`,
    sw_partners_perm_30d_bin = `b2_3 Having sexual partners within the last 30 day Permanent sexual partner`,
    sw_partners_perm_30d_raw = `b2_3_1 How many partners with whom you had sexual contact within the last 30 days belonged to Permanent sexual partner`,
    sw_partners_nonperm_30d_bin = `b2_4 Having sexual partners within the last 30 day Casual sexual partner`,
    sw_partners_nonperm_30d_raw = `b2_4_1 How many partners with whom you had sexual contact within the last 30 days belonged to Casual sexual partner`,
    sw_partners_total_30d_raw = `b2_5 Total number of partners within the last month (30 days)`
  )

sw_data_2021_raw <- sw_data_2021_raw %>%
  mutate(
  sw_partners_reg_clients_30d = as.numeric(ifelse(sw_partners_reg_clients_30d_raw %in% c("No question asked",998,999), NA, sw_partners_reg_clients_30d_raw)),
  sw_partners_reg_clients_30d = ifelse(sw_partners_reg_clients_30d_bin == "No", 0, sw_partners_reg_clients_30d),
  sw_partners_reg_clients_7d = sw_partners_reg_clients_30d/days_in_month,

  sw_partners_irreg_clients_30d = as.numeric(ifelse(sw_partners_irreg_clients_30d_raw %in% c("No question asked",998,999), NA, sw_partners_irreg_clients_30d_raw)),
  sw_partners_irreg_clients_30d = ifelse(sw_partners_irreg_clients_30d_bin == "No", 0, sw_partners_irreg_clients_30d),
  sw_partners_irreg_clients_7d = sw_partners_irreg_clients_30d/days_in_month,

  sw_partners_clients_30d =
    ifelse(
      is.na(sw_partners_irreg_clients_30d) &
      is.na(sw_partners_reg_clients_30d),
      NA,
      rowSums(
        cbind(sw_partners_irreg_clients_30d,
              sw_partners_reg_clients_30d),
        na.rm = TRUE)),

  sw_partners_clients_7d = sw_partners_clients_30d / days_in_month,

  sw_partners_perm_30d = as.numeric(ifelse(sw_partners_perm_30d_raw %in% c("No question asked",998,999), NA, sw_partners_perm_30d_raw)),
  sw_partners_perm_30d = ifelse(sw_partners_perm_30d_bin == "No", 0, sw_partners_perm_30d),
  sw_partners_perm_7d = sw_partners_perm_30d/days_in_month,

  sw_partners_nonperm_30d = as.numeric(ifelse(sw_partners_nonperm_30d_raw %in% c("No question asked",998,999), NA, sw_partners_nonperm_30d_raw)),
  sw_partners_nonperm_30d = ifelse(sw_partners_nonperm_30d_bin == "No", 0, sw_partners_nonperm_30d),
  sw_partners_nonperm_7d = sw_partners_nonperm_30d/days_in_month,

  sw_partners_nonclients_30d =
    ifelse(
      is.na(sw_partners_perm_30d) &
      is.na(sw_partners_nonperm_30d),
      NA,
      rowSums(
        cbind(sw_partners_perm_30d,
              sw_partners_nonperm_30d),
        na.rm = TRUE
      )
    ),
  sw_partners_nonclients_7d = sw_partners_nonclients_30d / days_in_month,

  sw_partners_total_30d = as.numeric(ifelse(sw_partners_total_30d_raw %in% c("No question asked",998,999), NA, sw_partners_total_30d_raw)),
  sw_partners_total_30d = ifelse(sw_partners_total_30d_raw == "No question asked", 0, sw_partners_total_30d),
  sw_partners_total_7d = sw_partners_total_30d/days_in_month
  )

# interview date
sw_data_2021_raw <- sw_data_2021_raw %>%
  mutate(
    interview_dte = as.Date(`Interview date`, format = "%d.%m.%Y")
  )

# variables to rename
rename_map_2021 <- c(
  id = "Respondent’s ID",
  interview_dte = "interview_dte",
  gender = "a7 Respondent’s gender",
  sw_days_total_7d = "b18_1 How many days during the last week (days) you Provided commercial sex services",
  education = "a9 What is your education level",
  residence_12m = "a11 What place of residence have you had for the last 12 months",
  marital_status = "a12 From the proposed options, choose the one that reflects your marital status at the moment",
  income_30d = "a14 What was your personal income within the last 30 days UAN",
  age = "a6 Specify your age (full years)",
  age_first_sex = "b1_1 How old were you, when you for the first time Had sexual relations",
  age_first_sw = "b1_2 How old were you, when you for the first time Provided commercial sex services",
  city = "City",
  city_travel_12m = "a16 Have you left this city for more than one month (30 days) in the last 12 months to provide commercial sex services or not",
  travel_soldiers_2014 = "b45 Have you traveled since 2014 to the territory of hostilities in eastern Ukraine (to checkpoints, uncontrolled cities) to provide commercial sex services for Ukrainian soldiers?",
  sw_partners_reg_clients_30d_raw = "sw_partners_reg_clients_30d_raw",
  sw_partners_irreg_clients_30d_raw = "sw_partners_irreg_clients_30d_raw",
  sw_partners_perm_30d_raw = "sw_partners_perm_30d_raw",
  sw_partners_nonperm_30d_raw = "sw_partners_nonperm_30d_raw",
  sw_partners_total_30d_raw = "sw_partners_total_30d_raw",
  sw_partners_reg_clients_7d = "sw_partners_reg_clients_7d",
  sw_partners_reg_clients_30d = "sw_partners_reg_clients_30d",
  sw_partners_irreg_clients_7d = "sw_partners_irreg_clients_7d",
  sw_partners_irreg_clients_30d = "sw_partners_irreg_clients_30d",
  sw_partners_perm_7d = "sw_partners_perm_7d",
  sw_partners_perm_30d = "sw_partners_perm_30d",
  sw_partners_nonperm_7d = "sw_partners_nonperm_7d",
  sw_partners_nonperm_30d = "sw_partners_nonperm_30d",
  sw_partners_nonclients_7d = "sw_partners_nonclients_7d",
  sw_partners_nonclients_30d = "sw_partners_nonclients_30d",
  sw_partners_total_7d = "sw_partners_total_7d",
  sw_partners_total_30d = "sw_partners_total_30d",  
  partners_sw_24h = "b22 How many different clients have you had for the past working day (24 hours)?",
  partners_age_30d = "b7 Among the representatives of age groups indicated by you, whom you come across most often in the last month (30 days)?",
  client_condom_lastsex = "b24 Did you use condom during the last sex or not?",
  group_sex_30d = "b42 Have you had sex with several partners at once (group sex) in the last month (30 days)?",
  condom_access_12m = "f1_1 Have you received condoms for free (from a representative of NGO, a medical worker, in nightclubs, at parties, etc.) In 12 months",
  condom_access_30d = "f1_2 Have you received condoms for free (from a representative of NGO, a medical worker, in nightclubs, at parties, etc.) In 30 days",
  typology_primary_30d = "b10 What method of search do you consider the main one for yourself",
  aids_center_bin = "k2 Were you registered as person living with HIV in a medical facility (e.g.  AIDS Center?)",
  alcohol_30d_num = "c4 How many times during the last month (30 days) did you use alcohol drinks?",
  used_syringe_last = "c9 Did you use a sterile needle and syringe during your last injecting drug use or not?",
  accessed_syringe_12m = "f6_10 Have you received following free items, from NGO or social worker within the last 12 months? - Sterile needles/syringes",
  age_inject = "c10_2 At what age did you try injectable drugs  for the first time",
  mental_health = "g0 PHQ9 level",
  anxiety = "g1 GAD7 level",
  ngo_access_lifetime = "f2 Are you a client of a non-governmental organization that works on HIV prevention among sex workers, or not?",
  prep_12m = "f15 Have you taken pre-exposure prophylaxis (PrEP) drugs within the last 12 months?",
  violence_any_ever = "h1 Have you ever experienced violence (for example, beatings, rapes, verbal humiliation, extortion, etc.) during your involvement in commercial sexual services?",
  violence_rape_ever = "h2_6 If yes, in what way? Raped",
  violence_beaten_ever = "h2_2 If yes, in what way? Beaten",
  violence_humiliated_ever = "h2_1 If yes, in what way? Verbally humiliated",
  violence_physical_abuse_ever = "h2_4 If yes, in what way? Harassed physically",
  violence_perm_partner = "h3_3 Who was the perpetrator? Permanent sexual partner",
  violence_casual_partner = "h3_4 Who was the perpetrator? Casual sexual partner",
  violence_police = "h3_5 Who was the perpetrator? Law enforcement officer",
  violence_pimp = "h3_6 Who was the perpetrator? Pimp/apartment administrator",
  violence_fsw = "h3_9 Who was the perpetrator? Other sex workers",
  violence_support_ngo = "h4_1 Have you contacted any person or facility after the violence incident? - To NGO/crisis center",
  avoided_healthcare_12m_stigma = "i11_1 Have you ever avoided seeking HEALTH-CARE in the last 12 months due to - Fear of or concern about stigma (insult, rejection, gossip, and condemnation of behavior)",
  avoided_healthcare_12m_disclosure = "i11_2 Have you ever avoided seeking HEALTH-CARE in the last 12 months due to - Fear or concern someone may learn you provide sexual services for a fee",
  avoided_healthcare_12m_violence = "i11_3 Have you ever avoided seeking HEALTH-CARE in the last 12 months due to - Fear of or concern about or experienced violence?",
  avoided_healthcare_12m_police = "i11_4 Have you ever avoided seeking HEALTH-CARE in the last 12 months due to - Fear of or concern about or experienced police harassment or arrest",
  avoided_hiv_test_12m_stigma = "i12_1 Have you ever avoided seeking HIV TESTING in the last 12 months due to - Fear of or concern about stigma (insult, rejection, gossip, and condemnation of behavior)",
  avoided_hiv_test_12m_disclosure = "i12_2 Have you ever avoided seeking HIV TESTING in the last 12 months due to - Fear or concern someone may learn you provide sexual services for a fee",
  avoided_hiv_test_12m_violence = "i12_3 Have you ever avoided seeking HIV TESTING in the last 12 months due to - Fear of or concern about or experienced violence",
  avoided_hiv_test_12m_police = "i12_4 Have you ever avoided seeking HIV TESTING in the last 12 months due to - Fear of or concern about or experienced police harassment or arrest",
  avoided_hiv_care_12m_stigma = "k10_1 Have you ever avoided seeking HIV MEDICAL CARE in the last 12 months due to Fear of or concern about stigma (insult, rejection, gossip, and condemnation of behavior)",
  avoided_hiv_care_12m_disclosure = "k10_2 Have you ever avoided seeking HIV MEDICAL CARE in the last 12 months due to Fear or concern someone may learn you provide sexual services for a fee",
  avoided_hiv_care_12_violence = "k10_3 Have you ever avoided seeking HIV MEDICAL CARE in the last 12 months due to Fear of or concern about or experienced violence",
  avoided_hiv_care_12m_police = "k10_4 Have you ever avoided seeking HIV MEDICAL CARE in the last 12 months due to Fear of or concern about or experienced police harassment or arrest",
  avoided_hiv_treat_12m_stigma = "k11_1 Have you ever avoided seeking HIV TREATMENT  in the last 12 months due to Fear of or concern about stigma (insult, rejection, gossip, and condemnation of behavior)",
  avoided_hiv_treat_12m_disclosure = "k11_2 Have you ever avoided seeking HIV TREATMENT  in the last 12 months due to Fear or concern someone may learn you provide sexual services for a fee",
  avoided_hiv_treat_12m_violence = "k11_3 Have you ever avoided seeking HIV TREATMENT  in the last 12 months due to Fear of or concern about or experienced violence",
  avoided_hiv_treat_12m_police = "k11_4 Have you ever avoided seeking HIV TREATMENT  in the last 12 months due to Fear of or concern about or experienced police harassment or arrest",
  hiv_tested_lifetime = "i3 I am not asking you about the test result, but have you ever been tested for HIV?",
  hiv_tested_result = "i9 I am not asking you about the test result, but did you receive it or not?",
  hiv_status_selfreport = "k1 Could you tell me your HIV status?",
  art_current = "FINAL ON ART (self + medical records + AIDSCenter)",
  hiv_test_rslt = "Rapid test: HIV test result",
  hiv_vl = "FINAL LAB Viral Load Result (copies/ml)",
  hiv_recent_infection = "LAB: RECENTLY INFECTION TEST",
  syphilis_test_rslt = "Rapid test: SYPHILIS test result",
  prep_start = "PREP Start of PREP"
)

# map to vars
sw_data_2021_clean <- sw_data_2021_raw %>%
  rename(!!!setNames(rename_map_2021, names(rename_map_2021)))

# check variables are all present
renamed_vars <- names(rename_map_2021)
missing_renamed <- setdiff(renamed_vars, names(rename_map_2021))
if (length(missing_renamed) == 0) {
  cat("All renamed variables are present in sw_data_2021_clean.\n")
} else {
  cat("Missing renamed variables:\n")
  print(missing_renamed)
}

## append data

# List of all datasets
datasets <- list(
  sw_data_2008_clean,
  sw_data_2009_clean,
  sw_data_2011_clean,
  sw_data_2013_clean,
  sw_data_2015_clean,
  sw_data_2017_clean,
  sw_data_2021_clean
)

# Only keep columns from your rename maps
all_vars <- unique(c(
  names(rename_map_2008), names(rename_map_2009), names(rename_map_2011),
  names(rename_map_2013), names(rename_map_2015), names(rename_map_2017),
  names(rename_map_2021)
))

# Align datasets: add missing columns and reorder
datasets_aligned <- lapply(datasets, function(df) {
  missing_cols <- setdiff(all_vars, names(df))
  if(length(missing_cols) > 0) df[missing_cols] <- NA
  df <- df[, all_vars, drop = FALSE]
  # Convert all columns to character to avoid type conflicts
  df[] <- lapply(df, as.character)
  df
})

# Combine datasets
sw_combined_raw <- bind_rows(datasets_aligned, .id = "source_year")

# check and save
dim(sw_combined_raw)
names(sw_combined_raw)
table(sw_combined_raw$year)
saveRDS(sw_combined_raw, "sw_combined_raw.rds")
write_xlsx(sw_combined_raw, "sw_combined_raw.xlsx")

## relevel variables to be consistent

# load appended raw data
sw_combined_raw <- readRDS("sw_combined_raw.rds")

# Add meaningful year variable
year_map <- c("1"=2008,"2"=2009,"3"=2011,"4"=2013,"5"=2015,"6"=2017,"7"=2021)
sw_combined_raw$year <- year_map[sw_combined_raw$source_year]

# check levels of each variable
levels_list <- lapply(sw_combined_raw, unique)

# Convert to a data frame suitable for Excel
max_length <- max(sapply(levels_list, length))
levels_df <- as.data.frame(
  lapply(levels_list, function(x) {
    length(x) <- max_length
    x
  }),
  stringsAsFactors = FALSE
)

# Write to Excel
write_xlsx(levels_df, "sw_combined_raw_levels.xlsx")

# relevel education
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    education_3cat = factor(case_when(
      grepl("Primary", education, ignore.case = TRUE) ~ 0,
      grepl("Secondary|basic.*secondary|Vocational|complete general secondary", education, ignore.case = TRUE) ~ 1,
      grepl("Higher|Incomplete higher|бакалавр|магістр|technical school", education, ignore.case = TRUE) ~ 2,
      grepl("Other|Refusal|No answer|Difficult to answer|Don't know", education, ignore.case = TRUE) ~ NA_real_,
      TRUE ~ NA_real_
    ), 
    levels = c(0, 1, 2), 
    labels = c("Primary", "Secondary", "Higher"))
  )

table(sw_combined_raw$education3cat, useNA = "ifany")
table(sw_combined_raw$education, useNA = "ifany")

# relevel marital status
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    marital_status_3cat = factor(case_when(
      grepl("unmarried|single|don't have a sexual partner", marital_status, ignore.case = TRUE) & !grepl("permanent sexual partner", marital_status, ignore.case = TRUE) ~ 0,
      grepl("Officially unmarried|Single but have regular sexual partner", marital_status, ignore.case = TRUE) & grepl("permanent|regular", marital_status, ignore.case = TRUE) ~ 1,
      grepl("married|common-law|living with husband|living with permanent sexual partner|have other sexual partner", marital_status, ignore.case = TRUE) ~ 2,
      grepl("No answer", marital_status, ignore.case = TRUE) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    levels = c(0, 1, 2),
    labels = c("Never married", "Partnered", "Married"))
  )

table(sw_combined_raw$marital_status_3cat, useNA = "ifany")
table(sw_combined_raw$marital_status, useNA = "ifany")

# age variables
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    age_numeric = suppressWarnings(as.numeric(age)),
    age_first_sex_numeric = suppressWarnings(as.numeric(age_first_sex)),
    age_first_sw_numeric = suppressWarnings(as.numeric(age_first_sw)),
    age_cat = factor(case_when(
      age_numeric < 18 ~ 0,
      age_numeric >= 18 & age_numeric <= 24 ~ 1,
      age_numeric >= 25 & age_numeric <= 34 ~ 2,
      age_numeric >= 35 ~ 3,
      TRUE ~ NA_real_
    ), levels = c(0, 1, 2, 3), labels = c("Underage", "18-24", "25-34", "35+")),
    age_bin = factor(case_when(
      age_numeric <25 ~ 0,
      age_numeric >= 25 & age_numeric < 96 ~ 1,
      TRUE ~ NA_real_
    ), levels = c(0, 1), labels = c("No", "Yes")),
    age_first_sex_cat = factor(case_when(
      age_first_sex_numeric < 18 ~ 0,
      age_first_sex_numeric >= 18 & age_first_sex_numeric <= 24 ~ 1,
      age_first_sex_numeric >= 25 ~ 2,
      TRUE ~ NA_real_
    ), levels = c(0, 1, 2), labels = c("Underage", "18-24", "25+")),
    age_first_sw_cat = factor(case_when(
      age_first_sw_numeric < 18 ~ 0,
      age_first_sw_numeric >= 18 & age_first_sw_numeric <= 24 ~ 1,
      age_first_sw_numeric >= 25 ~ 2,
      TRUE ~ NA_real_
    ), levels = c(0, 1, 2), labels = c("Underage", "18-24", "25+")),
    underage_first_sw_bin = factor(case_when(
      age_first_sw_numeric < 18 ~ 1,
      age_first_sw_numeric >= 18 ~ 0,
      TRUE ~ NA_real_
    ), levels = c(0, 1), labels = c("No", "Yes"))
  )

table(sw_combined_raw$age_cat, useNA = "ifany")
table(sw_combined_raw$age_first_sex_cat, useNA = "ifany")
table(sw_combined_raw$age_bin, useNA = "ifany")
table(sw_combined_raw$age_first_sw_cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$underage_first_sw_bin, useNA = "ifany")

# clean city variable
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    city = case_when(
      city %in% c("Kyiv", "Kyiv City") ~ "Kyiv",
      TRUE ~ city
    )
  )
city_year_binary <- sw_combined_raw %>%
  group_by(city, year) %>%
  summarise(present = ifelse(any(!is.na(city)), 1, 0), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = present, values_fill = 0) %>%
  arrange(city)

city_year_binary <- as.data.frame(city_year_binary)
city_year_binary

# travelled for sex work
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    city_travel_12m_cat = factor(case_when(
      city_travel_12m %in% c("No", "No, I haven't") ~ 0,
      grepl("^Yes", city_travel_12m) ~ 1,
      city_travel_12m %in% c(
        "Do not know / Do not remember",
        "Do not remember",
        "Difficult to answer / Don’t remember"
      ) ~ 2,
      city_travel_12m == "Refusal to answer" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    levels = c(0, 1, 2),
    labels = c("No", "Yes", "Don't know / Do not remember"))
  )
table(sw_combined_raw$city_travel_12m_cat, useNA = "ifany")

# volumne of clients variables past 7 days
sw_combined_raw <- sw_combined_raw %>%
  mutate(across(c(
    sw_partners_clients_30d, sw_partners_perm_30d, sw_partners_nonperm_30d, sw_partners_total_30d, partners_sw_24h,
    sw_partners_clients_7d, sw_partners_perm_7d, sw_partners_nonperm_7d, sw_partners_nonclients_7d, sw_partners_total_7d
  ), as.numeric))

sw_combined_raw <- sw_combined_raw %>%
  mutate(
    sw_partners_clients_7d_5cat = factor(
      case_when(
        sw_partners_clients_7d == 0 ~ 0,
        sw_partners_clients_7d %in% 1:3 ~ 1,
        sw_partners_clients_7d %in% 4:9 ~ 2,
        sw_partners_clients_7d %in% 10:19 ~ 3,
        sw_partners_clients_7d >= 20 ~ 4,
        TRUE ~ NA_real_
      ),
      levels = 0:4,
      labels = c("No client partners", "1-3", "4-9", "10-19", "20+")
    ),
    sw_partners_clients_30d_4cat = factor(
      case_when(
        sw_partners_clients_30d >= 0  & sw_partners_clients_30d <= 19  ~ 0,
        sw_partners_clients_30d >= 20 & sw_partners_clients_30d <= 49  ~ 1,
        sw_partners_clients_30d >= 50 & sw_partners_clients_30d <= 74  ~ 2,
        sw_partners_clients_30d >= 75 ~ 3,
        TRUE ~ NA_real_
      ), 
      levels = 0:3, 
      labels = c("0-19","20-49","50-74","75+")
    ),     
    sw_partners_nonclients_7d_5cat = factor(case_when(
      sw_partners_nonclients_7d == 0 ~ 0,
      sw_partners_nonclients_7d %in% 1:3 ~ 1,
      sw_partners_nonclients_7d %in% 4:9 ~ 2,
      sw_partners_nonclients_7d %in% 10:19 ~ 3,
      sw_partners_nonclients_7d >= 20 ~ 4,
      TRUE ~ NA_real_
      ), levels = 0:4, labels = c("No non-client partners","1-3","4-9","10-19","20+")),
    sw_partners_nonclients_30d_5cat = factor(case_when(
        sw_partners_nonclients_30d >= 0 & sw_partners_nonclients_30d < 5 ~ 0,
        sw_partners_nonclients_30d >= 5 & sw_partners_nonclients_30d < 10 ~ 1,
        sw_partners_nonclients_30d >= 10 & sw_partners_nonclients_30d < 20 ~ 2,
        sw_partners_nonclients_30d >= 20 & sw_partners_nonclients_30d < 30 ~ 3,
        sw_partners_nonclients_30d >= 30 ~ 4,
        TRUE ~ NA_real_
      ), levels = 0:4, labels = c("0-4","5-9","10-19","20-29","30+")),
    sw_partners_total_7d_4cat = factor(case_when(
        sw_partners_total_7d %in% 0:3 ~ 1,
        sw_partners_total_7d %in% 4:9 ~ 2,
        sw_partners_total_7d %in% 10:19 ~ 3,
        sw_partners_total_7d >= 20 ~ 4,
        TRUE ~ NA_real_
      ), levels = 1:4, labels = c("0-3","4-9","10-19","20+")),
    sw_partners_total_30d_5cat = factor(case_when(
      sw_partners_total_30d >= 0  & sw_partners_total_30d <= 19  ~ 0,
      sw_partners_total_30d >= 20 & sw_partners_total_30d <= 49  ~ 1,
      sw_partners_total_30d >= 50 & sw_partners_total_30d <= 74  ~ 2,
      sw_partners_total_30d >= 75 & sw_partners_total_30d <= 99  ~ 3,
      sw_partners_total_30d >= 100 ~ 4,
      TRUE ~ NA_real_
      ), levels = 0:4, labels = c("0-19","20-49","50-74","74-99","100+")), 
    sw_partners_total_30d_4cat = factor(case_when(
      sw_partners_total_30d >= 0  & sw_partners_total_30d <= 19  ~ 0,
      sw_partners_total_30d >= 20 & sw_partners_total_30d <= 49  ~ 1,
      sw_partners_total_30d >= 50 & sw_partners_total_30d <= 74  ~ 2,
      sw_partners_total_30d >= 75 ~ 3,
      TRUE ~ NA_real_
      ), levels = 0:3, labels = c("0-19","20-49","50-74","75+")),     
    sw_partners_total_24h_5cat = factor(case_when(
      partners_sw_24h == 0 ~ 0,
      partners_sw_24h >= 1 & partners_sw_24h <3 ~ 1,
      partners_sw_24h >= 3 & partners_sw_24h <6 ~ 2,
      partners_sw_24h >= 6 & partners_sw_24h <11 ~ 3,
      partners_sw_24h >= 11 & partners_sw_24h <98 ~ 4,
      TRUE ~ NA_real_
      ), levels = 0:4, labels = c("0","1-2","3-5","6-10","11+"))
    )

# tab categorical variables
table(sw_combined_raw$year, sw_combined_raw$sw_partners_clients_30d_4cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$sw_partners_clients_7d_4cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$sw_partners_nonclients_30d_5cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$sw_partners_nonclients_7d_5cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$sw_partners_total_7d_4cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$partners_sw_24h, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$sw_partners_total_24h_5cat, useNA = "ifany")

# Condom use and access variables
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    # client condom at last sex
    client_condom_lastsex_3cat = factor(
      case_when(
        grepl("^No$", client_condom_lastsex, ignore.case = TRUE) ~ 0,
        grepl("^Yes$", client_condom_lastsex, ignore.case = TRUE) ~ 1,
        grepl("No question asked|Difficult to answer|No answer|Don’t know|Don't know/don't remember|Refusal to answer", client_condom_lastsex, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    ),

    # client condom frequency past 30 days
    client_condom_freq_30d_5cat = factor(
      case_when(
        grepl("^Never$", client_condom_freq_30d, ignore.case = TRUE) ~ 0,
        grepl("Less than in a half of cases|Less often than half of the cases|Rarely|Sometimes", client_condom_freq_30d, ignore.case = TRUE) ~ 1,
        grepl("In a half of cases|Half of the cases|In half of cases", client_condom_freq_30d, ignore.case = TRUE) ~ 2,
        grepl("Not always, but more than in a half of cases|More often than half of the cases|In the majority of cases", client_condom_freq_30d, ignore.case = TRUE) ~ 3,
        grepl("Always", client_condom_freq_30d, ignore.case = TRUE) ~ 4,
        grepl("Do not know|No answer|Did not provide|I didn’t have such sex|No question asked", client_condom_freq_30d, ignore.case = TRUE) ~ 5,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2, 3, 4, 5),
      labels = c("Never", "Less than half", "Half", "More than half", "Always", "Missing / Unknown")
    ),

    # condom with permanent partner last sex
    perm_partner_condom_lastsex_3cat = factor(
      case_when(
        grepl("^No$", perm_partner_condom_lastsex, ignore.case = TRUE) ~ 0,
        grepl("^Yes$", perm_partner_condom_lastsex, ignore.case = TRUE) ~ 1,
        grepl("No such partner|No such partners|No permanent partners in the last 30 days|No answer|No question asked|Don’t know|Difficult to answer", perm_partner_condom_lastsex, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / No partner")
    ),

    # condom with casual partner last sex
    cas_partner_condom_lastsex_3cat = factor(
      case_when(
        grepl("^No$", cas_partner_condom_lastsex, ignore.case = TRUE) ~ 0,
        grepl("^Yes$", cas_partner_condom_lastsex, ignore.case = TRUE) ~ 1,
        grepl("No such partner|No such partners|No such partners in the last 30 days|No answer|No question asked|Difficult to answer|Don’t know", cas_partner_condom_lastsex, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / No partner")
    ),

    # condom access past 12 months
    condom_access_12m_3cat = factor(
      case_when(
        grepl("^No$", condom_access_12m, ignore.case = TRUE) ~ 0,
        grepl("^Yes$", condom_access_12m, ignore.case = TRUE) ~ 1,
        grepl("Difficult to answer|Do not remember|No answer|Don't know|Refusal to answer", condom_access_12m, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    ),

    # client condom binary past 30 days
    client_condom_bin_30d_3cat = factor(
      case_when(
        grepl("^I used a condom every time$|^Always used$|^Yes, there was such a case$", client_condom_bin_30d, ignore.case = TRUE) ~ 1,
        grepl("^There was a case of not using$|^No question asked$", client_condom_bin_30d, ignore.case = TRUE) ~ 0,
        grepl("^No answer$|Difficult to answer|Don't know|Refusal to answer", client_condom_bin_30d, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    ),

    # permanent partner condom binary past 30 days
    perm_partner_condom_30d_3cat = factor(
      case_when(
        grepl("^I used a condom every time$|^Always used$|^Yes, there was such a case$", perm_partner_condom_30d, ignore.case = TRUE) ~ 1,
        grepl("^There was a case of not using$|^No question asked$", perm_partner_condom_30d, ignore.case = TRUE) ~ 0,
        grepl("^No answer$|Difficult to answer|Don't know|Refusal to answer", perm_partner_condom_30d, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    ),

    # casual partner condom binary past 30 days
    cas_partner_condom_30d_3cat = factor(
      case_when(
        grepl("^I used a condom every time$|^Always used$|^Yes, there was such a case$", cas_partner_condom_30d, ignore.case = TRUE) ~ 1,
        grepl("^There was a case of not using$|^No question asked$", cas_partner_condom_30d, ignore.case = TRUE) ~ 0,
        grepl("^No answer$|Difficult to answer|Don't know|Refusal to answer", cas_partner_condom_30d, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    )
  )

condom_vars <- c(
  "client_condom_lastsex_3cat",
  "client_condom_freq_30d_5cat",
  "client_condom_bin_30d_3cat",
  "perm_partner_condom_lastsex_3cat",
  "perm_partner_condom_30d_3cat",
  "cas_partner_condom_lastsex_3cat",
  "cas_partner_condom_30d_3cat",
  "condom_access_12m_3cat"
)

# Crosstab each variable with year
for (var in condom_vars) {
  cat(var)
  print(table(sw_combined_raw$year, sw_combined_raw[[var]], useNA = "ifany"))
}

# alcohol 30d
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    alcohol_30d_bin = factor(
      case_when(
        grepl("^Never$|^0$|Not used in the last 30 days", alcohol_30d_num, ignore.case = TRUE) ~ 0,  # No use
        grepl("Every day|Almost every day|1-2 times a month|1-2 times a week|Less than once a week|No less than once a week|\\d+", alcohol_30d_num, ignore.case = TRUE) ~ 1,  # Any use
        grepl("No answer|Don’t remember|Used, don't remember the amount|Refusal to answer|Don't know/don't remember", alcohol_30d_num, ignore.case = TRUE) ~ 2, # Missing / Unknown
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    )
  )

table(sw_combined_raw$year, sw_combined_raw$alcohol_30d_bin, useNA = "ifany")

# approximate number of times weekly
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    alcohol_30d_num_numeric = case_when(
      grepl("^0$|Never|Not used in the last 30 days", alcohol_30d_num, ignore.case = TRUE) ~ 0,
      grepl("1-2 times a month", alcohol_30d_num, ignore.case = TRUE) ~ 1,
      grepl("Less than once a week", alcohol_30d_num, ignore.case = TRUE) ~ 1,
      grepl("1-2 times a week", alcohol_30d_num, ignore.case = TRUE) ~ 6,
      grepl("No less than once a week", alcohol_30d_num, ignore.case = TRUE) ~ 6,
      grepl("Every day|Almost every day|Always", alcohol_30d_num, ignore.case = TRUE) ~ 30,
      grepl("\\d+", alcohol_30d_num) ~ as.numeric(alcohol_30d_num),
      TRUE ~ NA_real_
    )
  )

# create alcohol use categorical variable
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    alcohol_30d_use_5cat = factor(
      case_when(
        alcohol_30d_num_numeric == 0 ~ 0,
        alcohol_30d_num_numeric >= 1 & alcohol_30d_num_numeric <= 4 ~ 1,
        alcohol_30d_num_numeric >= 5 & alcohol_30d_num_numeric <= 14 ~ 2,
        alcohol_30d_num_numeric >= 15 ~ 3,
        is.na(alcohol_30d_num_numeric) ~ 4
      ),
      levels = c(0, 1, 2, 3, 4),
      labels = c("None [0]", "Low use [1–4 days]", "Moderate use [5–14 days]", "High use [15+ days]", "Missing / Unknown [NA]")
    )
  )

table(sw_combined_raw$alcohol_30d_bin, useNA = "ifany")
table(sw_combined_raw$alcohol_30d_use_3cat, useNA = "ifany")

# drug use variables
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    drugs_30d_bin_3cat = factor(
      case_when(
        grepl("^No$", drugs_30d_bin, ignore.case = TRUE) ~ 0,
        grepl("^Yes$", drugs_30d_bin, ignore.case = TRUE) ~ 1,
        grepl("I used before, now I don’t|Never used|No answer|Difficult to answer", 
              drugs_30d_bin, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    ),
    
    idu_12m_3cat = factor(
      case_when(
        grepl("^No$|Never used|Used in the past, but not in the last 12 months|Yes, I have used more than 12 months ago", 
              idu_12m_bin, ignore.case = TRUE) ~ 0,
        grepl("Yes, I have used during last 12 months \\(not last 30 days\\)|Yes, I have used during last 30 days", 
              idu_12m_bin, ignore.case = TRUE) ~ 1,
        grepl("No answer|Difficult to answer|Difficult to answer/don’t remember|Refuse to answer", 
              idu_12m_bin, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    ),
    
    idu_ever_3cat = factor(
      case_when(
        grepl("^No$|Never used", idu_12m_bin, ignore.case = TRUE) ~ 0,
        grepl("Yes, I have used during last 12 months \\(not last 30 days\\)|Yes, I have used during last 30 days|Used in the past, but not in the last 12 months|Yes, I have used more than 12 months ago",
              idu_12m_bin, ignore.case = TRUE) ~ 1,
        grepl("No answer|Difficult to answer|Difficult to answer/don’t remember|Refuse to answer", 
              idu_12m_bin, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("Never", "Ever", "Missing / Unknown")
    ),
    
    idu_30d_num_numeric = case_when(
      grepl("^Never$", idu_30d_num, ignore.case = TRUE) ~ 0,
      grepl("^No answer$", idu_30d_num, ignore.case = TRUE) ~ NA_real_,
      grepl("^No question asked$", idu_30d_num, ignore.case = TRUE) ~ 0,
      grepl("^Only once a month$", idu_30d_num, ignore.case = TRUE) ~ 1,
      grepl("^2-3 times a month$", idu_30d_num, ignore.case = TRUE) ~ 2.5,
      grepl("^On average once a week$", idu_30d_num, ignore.case = TRUE) ~ 4,
      grepl("^2-3 times a week$", idu_30d_num, ignore.case = TRUE) ~ 10,
      grepl("^4-6 times a week$", idu_30d_num, ignore.case = TRUE) ~ 20,
      grepl("^On average once a day$", idu_30d_num, ignore.case = TRUE) ~ 30,
      grepl("^2-3 times a day$", idu_30d_num, ignore.case = TRUE) ~ 60,
      grepl("^At least four times a day$", idu_30d_num, ignore.case = TRUE) ~ 120,
      grepl("\\d+", idu_30d_num) ~ as.numeric(idu_30d_num),
      TRUE ~ NA_real_
    ),
    
    idu_30d_use_3cat = factor(
      case_when(
        idu_30d_num_numeric == 0 ~ 0,
        idu_30d_num_numeric >= 1 & idu_30d_num_numeric <= 4 ~ 1,
        idu_30d_num_numeric >= 5 & idu_30d_num_numeric <= 14 ~ 2,
        idu_30d_num_numeric >= 15 ~ 3,
        is.na(idu_30d_num_numeric) ~ 4
      ),
      levels = c(0, 1, 2, 3, 4),
      labels = c("None [0]", "Low use [1–4]", "Moderate use [5–14]", "High use [15+]", "Missing / Unknown [NA]")
    ),
    
    used_syringe_last_3cat = factor(
      case_when(
        grepl("^No$|No, I did not|Never$|No question asked$", used_syringe_last, ignore.case = TRUE) ~ 0,
        grepl("^Yes$|Yes, I did$", used_syringe_last, ignore.case = TRUE) ~ 1,
        grepl("No answer|I don't know how the syringe was filled|Don’t remember|Refusal to answer|Don't know/don't remember", used_syringe_last, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    ),
    
    used_syringe_30d_bin_3cat = factor(
      case_when(
        grepl("^No$|Never$|No question asked$", used_syringe_30d_bin, ignore.case = TRUE) ~ 0,
        grepl("Always|Not always, but more than in a half of cases \\(>50% cases\\)|In a half of cases \\(50% cases\\)|Less than in a half of cases \\(<50% cases\\)", used_syringe_30d_bin, ignore.case = TRUE) ~ 1,
        grepl("No answer|I don't know how the syringe was filled|Don’t remember|Refusal to answer|Don't know/don't remember", used_syringe_30d_bin, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    )
  )

table(sw_combined_raw$year, sw_combined_raw$drugs_30d_bin_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$idu_12m_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$idu_ever_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$idu_30d_use_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$used_syringe_last_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$used_syringe_30d_bin_3cat, useNA = "ifany")

sw_combined_raw <- sw_combined_raw %>%
  mutate(
    # NGO lifetime
    ngo_access_lifetime_3cat = factor(
      case_when(
        grepl("^No$", ngo_access_lifetime, ignore.case = TRUE) ~ 0,
        grepl("^Yes$", ngo_access_lifetime, ignore.case = TRUE) ~ 1,
        grepl("No answer|Don’t remember|Don't know/don't remember|Refusal to answer",
              ngo_access_lifetime, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    ),

    # NGO past 12 months
    ngo_access_12m_3cat = factor(
      case_when(
        grepl("^No$", ngo_access_12m, ignore.case = TRUE) ~ 0,
        grepl("^Yes$", ngo_access_12m, ignore.case = TRUE) ~ 1,
        grepl("No answer|Don’t remember|Don't know/don't remember|Refusal to answer",
              ngo_access_12m, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    ),

    # NGO past 30 days
    ngo_access_30d_3cat = factor(
      case_when(
        grepl("^No$", ngo_access_30d, ignore.case = TRUE) ~ 0,
        grepl("^Yes$", ngo_access_30d, ignore.case = TRUE) ~ 1,
        grepl("No answer|Don’t remember|Don't know/don't remember|Refusal to answer",
              ngo_access_30d, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    ),

    # AIDS center access
    aids_center_bin_3cat = factor(
      case_when(
        grepl("^No$", aids_center_bin, ignore.case = TRUE) ~ 0,
        grepl("^Yes$", aids_center_bin, ignore.case = TRUE) ~ 1,
        grepl("No answer|No question asked|Refusal to answer|Don't know|don't remember",
              aids_center_bin, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    )
  )

table(sw_combined_raw$year, sw_combined_raw$aids_center_bin_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$ngo_access_lifetime_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$ngo_access_12m_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$ngo_access_30d_3cat, useNA = "ifany")

# HIV variables
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    hiv_tested_lifetime_3cat = factor(
      case_when(
        grepl("^Yes$", hiv_tested_lifetime, ignore.case = TRUE) ~ 1,
        grepl("^No$|I have not been tested for HIV|No, it was more than 12 months ago", hiv_tested_lifetime, ignore.case = TRUE) ~ 0,
        grepl("No answer|Difficult to answer|Don't know/don't remember|Refusal to answer|No question asked", hiv_tested_lifetime, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    ),
    
    hiv_tested_12m_3cat = factor(
      case_when(
        grepl("Yes, it was during the recent 12 months|Yes, it was within the last 12 months", hiv_tested_12m, ignore.case = TRUE) ~ 1,
        grepl("No, it was earlier than 12 months ago|No|I have not been tested for HIV", hiv_tested_12m, ignore.case = TRUE) ~ 0,
        grepl("No answer|Difficult to answer|Don't know/don't remember|Refusal to answer|No question asked", hiv_tested_12m, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    ),
    
    hiv_tested_result_3cat = factor(
      case_when(
        grepl("^Yes$|HIV-negative|Yes, HIV-negative", hiv_tested_result, ignore.case = TRUE) ~ 1,
        grepl("^No$|HIV-positive|Yes, HIV-positive|Ні", hiv_tested_result, ignore.case = TRUE) ~ 0,
        grepl("No answer|Difficult to answer|Don't know/don't remember|Refusal to answer|Waiting for a result|No question asked", hiv_tested_result, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    ),
    
    hiv_status_selfreport_3cat = factor(
      case_when(
        grepl("HIV negative|HIV-negative|Yes, HIV-negative", hiv_status_selfreport, ignore.case = TRUE) ~ 1,
        grepl("HIV positive|HIV-positive|Yes, HIV-positive|Ні|No", hiv_status_selfreport, ignore.case = TRUE) ~ 0,
        grepl("No answer|Difficult to answer|Don't know/don't remember|Refusal to answer|No question asked", hiv_status_selfreport, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    )
  )

table(sw_combined_raw$year, sw_combined_raw$hiv_tested_lifetime_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$hiv_tested_12m_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$hiv_tested_result_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$hiv_status_selfreport_3cat, useNA = "ifany")

# sexual activity past week
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    sw_days_total_7d_3cat = factor(
      case_when(
        sw_days_total_7d == 0 ~ 0,
        sw_days_total_7d >= 1 & sw_days_total_7d <= 4 ~ 1,
        sw_days_total_7d >= 5 & sw_days_total_7d <= 7 ~ 2,
        grepl("No answer|Difficult to answer|Refusal to answer|Don't know/don't remember|Don’t remember", sw_days_total_7d, ignore.case = TRUE) ~ 3,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2, 3),
      labels = c("0 days", "1–4 days", "5–7 days", "Missing / Unknown")
    )
  )

table(sw_combined_raw$year, sw_combined_raw$sw_days_total_7d_3cat, useNA = "ifany")

# housing (need to fix because its stored so fucking strangely)
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    residence_3cat = factor(
      case_when(
        grepl("Separate apartment|Shared apartment|Shelter|boarding house|children’s home", residence, ignore.case = TRUE) ~ 0,
        grepl("Hostel|Communal apartment|Basement|attic|Street|nowhere to live|Other", residence, ignore.case = TRUE) ~ 1,
        grepl("No answer|Difficult to answer|Refusal to answer", residence, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("Stable housing", "Unstable housing", "Missing / Unknown")
    )
  )

table(sw_combined_raw$year, sw_combined_raw$residence_3cat, useNA = "ifany")

# sex work typology
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    # 6-month typology
    typology_primary_6m_3cat = factor(
      case_when(
        grepl("On a highway|In a sauna|In the street|At stations|At bus stops",
              typology_primary_6m, ignore.case = TRUE) ~ 0,
        grepl("In a casino|club|bar|discotheque|At a hotel|Provide escort services|Phone calls|Internet|Through friends|acquaintances|pimps|other clients|I have regular clients",
              typology_primary_6m, ignore.case = TRUE) ~ 1,
        grepl("No answer|Difficult to answer",
              typology_primary_6m, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("Street/Outdoor", "Indoor/Clients", "Missing / Unknown")
    ),

    # 30-day typology
    typology_primary_30d_3cat = factor(
      case_when(
        grepl("highways|roads|street|park|square|stations|bus stops|public transport stops|beach|markets|public events",
              typology_primary_30d, ignore.case = TRUE) ~ 0,
        grepl("disco|nightclub|club|strip|sauna|bath|massage|spa|beauty|bar|restaurant|cafe|hotel|motel|casino|fitness|modeling|educational|school|Internet|Tinder|social media|phone|TV|intermediaries|pimp|madam|clients|FSWs|friends|acquaintances",
              typology_primary_30d, ignore.case = TRUE) ~ 1,
        grepl("No answer|Difficult to answer",
              typology_primary_30d, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("Street/Outdoor", "Indoor/Clients", "Missing / Unknown")
    ),

    # typology across years
    typology_primary_3cat = factor(
      case_when(
        year %in% c(2008, 2009, 2011) ~ as.character(typology_primary_6m_3cat),
        year %in% c(2013, 2015, 2017, 2021) ~ as.character(typology_primary_30d_3cat),
        TRUE ~ NA_character_
      ),
      levels = c("Street/Outdoor", "Indoor/Clients", "Missing / Unknown")
    )
  )

# create street based
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    street_sw_bin = case_when(
      typology_primary_3cat == "Street/Outdoor" ~ "Yes",
      typology_primary_3cat == "Indoor/Clients" ~ "No",
      TRUE ~ NA_character_
    ),
    street_sw_bin = factor(street_sw_bin, levels = c("No", "Yes"))
  )
table(sw_combined_raw$street_sw_bin, useNA = "ifany")

table(sw_combined_raw$year, sw_combined_raw$typology_primary_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$typology_primary_30d_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$typology_primary_6m_3cat, useNA = "ifany")

sw_combined_raw <- sw_combined_raw %>%
  mutate(
    partners_age_6m_h = case_when(
      grepl("Youth|Young men", partners_age_6m, ignore.case = TRUE) ~ 0,
      grepl("middle age", partners_age_6m, ignore.case = TRUE) ~ 1,
      grepl("above 50", partners_age_6m, ignore.case = TRUE) ~ 2,
      TRUE ~ NA_real_
    ),
    partners_age_30d_h = case_when(
      grepl("18-25|19-24|25-34|26-35|Young", partners_age_30d, ignore.case = TRUE) ~ 0,
      grepl("35-49|36-50|middle age|Middle-aged", partners_age_30d, ignore.case = TRUE) ~ 1,
      grepl("above 50|over 50", partners_age_30d, ignore.case = TRUE) ~ 2,
      TRUE ~ NA_real_
    ),
    partners_age_3cat = case_when(
      year %in% c(2008, 2009, 2011) ~ partners_age_6m_h,
      year %in% c(2013, 2015, 2017, 2021) ~ partners_age_30d_h,
      TRUE ~ NA_real_
    ),

    partners_age_3cat = factor(
      partners_age_3cat,
      levels = c(0, 1, 2),
      labels = c("18-35", "36-59", "50+")
    )
  )

table(sw_combined_raw$year, sw_combined_raw$partners_age_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$partners_age_30d_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$partners_age_6m_3cat, useNA = "ifany")

# art use
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    art_current_3cat = factor(
      case_when(
        grepl("^Yes|Yes, currently|I’m currently participating", art_current, ignore.case = TRUE) ~ 1,  # On ART
        grepl("^No$|Participated, but now don’t receive|I used to earlier|No, but|NO, but", art_current, ignore.case = TRUE) ~ 0,  # Not on ART
        grepl("No question asked|No answer|I have used but stopped", art_current, ignore.case = TRUE) ~ 2,  # Missing / Unknown
        TRUE ~ 2  # Catch any unexpected responses
      ),
      levels = c(0, 1, 2),
      labels = c("No / Not on ART", "Yes / Currently on ART", "Missing / Unknown")
    )
  )

table(sw_combined_raw$year, sw_combined_raw$art_current_3cat, useNA = "ifany")

# violence variables
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    violence_any_ever_3cat = factor(
      case_when(
        grepl("^Yes|Так$", violence_any_ever, ignore.case = TRUE) ~ 1,
        grepl("^No|Ні$", violence_any_ever, ignore.case = TRUE) ~ 0,
        grepl("НЕМАЄ ВІДПОВІДІ|ВАЖКО ВІДПОВІСТИ|Refusal to answer|Don't know|Don't remember", 
              violence_any_ever, ignore.case = TRUE) ~ 2,
        TRUE ~ 2  # Catch any unexpected responses
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    )
  )

violence_vars <- c(
  "violence_rape_ever",
  "violence_beaten_ever",
  "violence_humiliated_ever",
  "violence_physical_abuse_ever",
  "violence_client",
  "violence_perm_partner",
  "violence_casual_partner",
  "violence_police",
  "violence_pimp",
  "violence_fsw",
  "violence_support_ngo"
)

sw_combined_raw <- sw_combined_raw %>%
  mutate(across(
    all_of(violence_vars),
    ~ factor(
        case_when(
          grepl("^Yes$", ., ignore.case = TRUE) ~ 1,
          grepl("^No$|No question asked", ., ignore.case = TRUE) ~ 0,
          grepl("Don't know|Don't remember|Refusal to answer", ., ignore.case = TRUE) ~ 2,
          TRUE ~ 2  # any unexpected responses
        ),
        levels = c(0, 1, 2),
        labels = c("No", "Yes", "Missing / Unknown")
      )
  ))

sw_combined_raw <- sw_combined_raw %>%
  mutate(
    violence_rape_12m_3cat = factor(
      case_when(
        grepl("^Yes$", violence_rape_12m, ignore.case = TRUE) ~ 1,
        grepl("^No$", violence_rape_12m, ignore.case = TRUE) ~ 0,
        grepl("Refuse to answer|Refused to answer", violence_rape_12m, ignore.case = TRUE) ~ 2,
        TRUE ~ 2  # catch unexpected responses
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    )
  )

table(sw_combined_raw$year, sw_combined_raw$violence_rape_12m_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$violence_rape_ever, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$violence_beaten_ever, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$violence_humiliated_ever, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$violence_physical_abuse_ever, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$violence_client, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$violence_any_ever_3cat, useNA = "ifany")

sw_combined_raw <- sw_combined_raw %>%
  mutate(
    prep_12m_3cat = factor(
      case_when(
        grepl("^Yes$|Yes, I have taken PrEP drugs and take it now|Yes, I have taken PrEP drugs but do not take it now", prep_12m, ignore.case = TRUE) ~ 1,
        grepl("^No$|No, I have not", prep_12m, ignore.case = TRUE) ~ 0,
        grepl("No question asked|Don’t remember|Refusal to answer|Don't know/don't remember", prep_12m, ignore.case = TRUE) ~ 2,
        TRUE ~ 2  # any unexpected responses
      ),
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Missing / Unknown")
    )
  )

# Check distribution by year
table(sw_combined_raw$year, sw_combined_raw$prep_12m_3cat, useNA = "ifany")

# depression and anxiety
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    anxiety_3cat = factor(
      case_when(
        grepl("Absent|Minimal|Mild", anxiety, ignore.case = TRUE) ~ 0,
        grepl("Moderate|Medium", anxiety, ignore.case = TRUE) ~ 1,
        grepl("Severe|High|Moderately severe", anxiety, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("Low / Absent", "Moderate", "High / Severe")
    ),
    
    depression_3cat = factor(
      case_when(
        grepl("Absent|Minimal|Mild", mental_health, ignore.case = TRUE) ~ 0,
        grepl("Moderate|Medium", mental_health, ignore.case = TRUE) ~ 1,
        grepl("Severe|High|Moderately severe", mental_health, ignore.case = TRUE) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1, 2),
      labels = c("Low / Absent", "Moderate", "High / Severe")
    )
  )

table(sw_combined_raw$year, sw_combined_raw$anxiety_3cat, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$depression_3cat, useNA = "ifany")

# stigma questions
avoided_vars <- c(
  "avoided_healthcare_12m_stigma", "avoided_healthcare_12m_disclosure",
  "avoided_healthcare_12m_violence", "avoided_healthcare_12m_police",
  "avoided_hiv_test_12m_stigma", "avoided_hiv_test_12m_disclosure",
  "avoided_hiv_test_12m_violence", "avoided_hiv_test_12m_police",
  "avoided_hiv_care_12m_stigma", "avoided_hiv_care_12m_disclosure",
  "avoided_hiv_care_12_violence", "avoided_hiv_care_12m_police",
  "avoided_hiv_treat_12m_stigma", "avoided_hiv_treat_12m_disclosure",
  "avoided_hiv_treat_12m_violence", "avoided_hiv_treat_12m_police"
)

sw_combined_raw <- sw_combined_raw %>%
  mutate(across(all_of(avoided_vars),
    ~ factor(case_when(
        grepl("^Yes$", ., ignore.case = TRUE) ~ 1,
        grepl("^No$", ., ignore.case = TRUE) ~ 0,
        TRUE ~ NA_real_
      ),
      levels = c(0,1),
      labels = c("No","Yes")
    )
  ))

table(sw_combined_raw$avoided_healthcare_12m_stigma)

# hiv and syphilis test results
sw_combined_raw <- sw_combined_raw %>%
  mutate(
    hiv_test_rslt_bin = case_when(
      year == 2017 & (grepl("No answer|No question asked", hiv_test_rslt, ignore.case = TRUE) | is.na(hiv_test_rslt)) ~ "Negative",
      grepl("^Negative$", hiv_test_rslt, ignore.case = TRUE) ~ "Negative",
      grepl("^Positive$", hiv_test_rslt, ignore.case = TRUE) ~ "Positive",
      grepl("No answer|No question asked", hiv_test_rslt, ignore.case = TRUE) | is.na(hiv_test_rslt) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    hiv_test_rslt_bin = factor(
      hiv_test_rslt_bin,
      levels = c("Negative", "Positive")
    ),
    syphilis_test_rslt_3cat = case_when(
      grepl("^Negative$", syphilis_test_rslt, ignore.case = TRUE) ~ "Negative",
      grepl("^Positive$", syphilis_test_rslt, ignore.case = TRUE) ~ "Positive",
      grepl("No answer|No question asked", syphilis_test_rslt, ignore.case = TRUE) ~ "Missing / Unknown",
      TRUE ~ NA_character_
    ),
    syphilis_test_rslt_3cat = factor(
      syphilis_test_rslt_3cat,
      levels = c("Negative", "Positive", "Missing / Unknown")
    )
  )

table(sw_combined_raw$year, sw_combined_raw$hiv_test_rslt_bin, useNA = "ifany")
table(sw_combined_raw$year, sw_combined_raw$syphilis_test_rslt_3cat, useNA = "ifany")

# create ukraine regions
sw_combined_raw <- sw_combined_raw %>%
  mutate(ukraine_region = case_when(
    city %in% c("Chernivtsi", "Ivano-Frankivsk", "Lutsk", "Lviv", "Ternopil", "Uzhhorod") ~ "West",
    city %in% c("Cherkasy", "Khmelnytskyi", "Kropyvnytskyi", "Kyiv", "Poltava", "Vinnytsya", "Zhytomyr") ~ "Central",
    city %in% c("Dnipro", "Donetsk", "Kharkiv", "Mariupol") ~ "East",
    city %in% c("Kherson", "Mykolayiv", "Odesa") ~ "South",
    city %in% c("Zaporizhzhya") ~ "South-East",
    city %in% c("Sumy") ~ "North-East",
    city %in% c("Sevastopol", "Simferopol") ~ "Crimea",
    TRUE ~ "Other"
  ))

# check and save appended cleaned datasets
dim(sw_combined_raw)
names(sw_combined_raw)
table(sw_combined_raw$year)
saveRDS(sw_combined_raw, "sw_combined_clean.rds")

