## load packages
pacman::p_load(dplyr, tidyr, stringr, tibble, writexl, readxl, forcats, labelled)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Papers/Paper 3a - Ukraine Sex Work HIV/data/SW data")

# load 2008 data
sw_data_2008_raw <- read_excel("2008_IBBS_FSW_TLS AND RDS_Data.xlsx")

# variables to rename
rename_map <- c(
  education = "A3. What is your education?",
  marital_status = "A2. Which of the following best describes your current marital status?",
  age = "A1. What is your age?",
  age_first_sex = "B1. At what age did you first engage in sexual activity?",
  age_first_sw = "B2. How old were you when you first provided sexual services for compensation (money or other)?",
  city = "City",
  city_travel_12m = "A6.1. Over the past 12 months, have you left this city for more than one month [30 days] to provide sexual services in other cities or regions of Ukraine?",
  country_travel_12m = "A6.2. Over the past 12 months, have you left this city for more than one month [30 days] to provide sexual services in other countries?",
  partners_sw_7d = "B3.1. During the last (working) week, how many of your sexual partners [clients] were partners [clients] from whom you received compensation?",
  partners_nonsw_7d = "B3.2. During the last (working) week, how many of your sexual partners [clients] were partners from whom you did not receive compensation?",
  partners_total_7d = "B3.3. How many total sexual partners did you have during the last (working) week (including spouse and others)?",
  partners_sw_24h = "B4. How many different clients did you provide sexual services to for compensation on your last working day (24 hours)?",
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
  rename(!!!setNames(rename_map, names(rename_map))) %>%
  select(all_of(names(rename_map)))

# variables to derive
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    alcohol_30d_daily_bin = case_when(
      alcohol_30d_num == "Every day / Almost every day" ~ "Yes",
      alcohol_30d_num %in% c("1-2 times a week", "1-2 times a month") ~ "No",
      alcohol_30d_bin == "No" ~ "No",
      TRUE ~ NA_character_
    ))

# load 2009 data
sw_data_2009_raw <- read_excel("2009_IBBS_FSW_TLS AND RDS_Data.xlsx")

# variables to rename
rename_map_2009 <- c(
  sw_freq_6m = "А11. How often have your provided sexual services for a fee in THE LAST 6 MONTHS?",
  sw_freq_7d = "А12. How many days DURING THE LAST WEEK [7 days] have you provided sexual services for a fee?",
  education = "А1. Your education",
  residence = "А3. Where do you live in this city?",
  marital_status = "А8. Choose from the suggested options one corresponding to your marital status at the moment:",
  age = "S2. Your age",
  age_first_sex = "В1. At what age did you have sexual relations for the first time",
  age_first_sw = "В2. How old were you when you provided sexual services for a fee (money or other) for the first time?",
  city = "І7.1 City",
  city_travel_12m = "А5. Have you ever left this city [CITY OF SURVEY] for more than 1 month [30 days] in THE LAST 12 MONTHS to provide sexual services?",
  partners_sw_7d = "В4.1 During THE RECENT (WORKING) WEEK, how many persons listed below were among your sexual partners [clients]? Partners [clients] who you RECEIVED A FEE [money or other] from",
  partners_nonsw_perm_7d = "В4.2 During THE RECENT (WORKING) WEEK, how many persons listed below were among your sexual partners [clients]? Permanent partners who you RECEIVED NO FEE [money or other] from",
  partners_nonsw_cas_7d = "В4.3 During THE RECENT (WORKING) WEEK, how many persons listed below were among your sexual partners [clients]? Casual partners who you RECEIVED NO FEE [money or other] from",
  partners_total_7d = "В4.4 How MANY DIFFERENT sexual partners in total did you have during the recent (working) week, including your husband or your permanent sexual partner with whom you live?",
  partners_sw_24h = "В5. How many different CLIENTS whom you provided sexual services for a fee did you have FOR THE LAST WORKING DAY (24 HOURS)?",
  client_condom_lastsex = "В6. Remember your sexual contact with your MOST RECENT CLIENT. Did you use a condom?",
  client_condom_bin_30d = "В16.2.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with a  CLIENT during vaginal sex?",
  client_condom_freq_30d = "В16.2.1 How often did you use a condom during vaginal sex?",
  perm_partner_condom_lastsex = "В11. Remember your last sexual contact with a PERMANENT partner from whom you RECEIVED NO FEE. Did you use a condom?",
  cas_partner_condom_lastsex = "В13. Remember your last sexual contact with a CASUAL partner from whom you RECEIVED NO FEE. Did you use a condom?",
  perm_partner_condom_30d = "В18.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with a PERMANENT PARTNER?",
  cas_partner_condom_30d = "В20.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with CASUAL PARTNERS?",
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
  
# variables to derive
sw_data_2009_clean <- sw_data_2009_clean %>%
  mutate(
    alcohol_30d_bin = case_when(
      alcohol_30d_num %in% c("Every day", "Less than once a week", "No less than once a week") ~ "Yes",
      alcohol_30d_num == "Never" ~ "No",
      TRUE ~ NA_character_
    ),
    alcohol_30d_daily_bin = case_when(
      alcohol_30d_num == "Every day" ~ "Yes",
      alcohol_30d_num %in% c("Never", "Less than once a week", "No less than once a week") ~ "No",
      TRUE ~ NA_character_
    ),
    ngo_access_12m = ifelse(
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Syringe exchange` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Obtaining disinfecting solutions` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Obtaining hygienic means` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Obtaining condoms` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Obtaining information booklets or brochures` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Helpline service` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Attending mutual support groups` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Attending consultations on safe injecting drug use` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Attending consultations on HIV/AIDS, STI, and ways of their prevention` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Lawyer’s consultations` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Psychologist’s consultations` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Peer-to-peer consultations` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Free HIV/AIDS testing` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Free testing for venereal diseases (testing and counseling)` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Free medical treatment of venereal diseases` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Other` == "Yes",
      "Yes",
      "No"
    ),
    idu_30d_bin = case_when(
      if_any(c(
        `С3.2.1 Drugs taken via injection in the past 30 days? Tramadol/tramal`,
        `С3.2.2 Drugs taken via injection in the past 30 days? Heroin`,
        `С3.2.3 Drugs taken via injection in the past 30 days? Liquid opium extract`,
        `С3.2.4 Drugs taken via injection in the past 30 days? Other Opiates`,
        `С3.2.5 Drugs taken via injection in the past 30 days? Cocaine`,
        `С3.2.6 Drugs taken via injection in the past 30 days? Amphetamine`,
        `С3.2.7 Drugs taken via injection in the past 30 days? Methamphetamine powder (crystallized)`,
        `С3.2.8 Drugs taken via injection in the past 30 days? Methamphetamine liquid («tina», «pervitin»)`,
        `С3.2.9 Drugs taken via injection in the past 30 days? Methcathinone («tweaker»)`,
        `С3.2.10 Drugs taken via injection in the past 30 days? Cathinone («magma»)`,
        `С3.2.11 Drugs taken via injection in the past 30 days? “Extasy”, MDMA`,
        `С3.2.12 Drugs taken via injection in the past 30 days? Other Stimulants`,
        `С3.2.13 Drugs taken via injection in the past 30 days? LCD, mushrooms`,
        `С3.2.14 Drugs taken via injection in the past 30 days? Other`
      ), ~ . == "Yes") ~ "Yes",
      
      if_any(c(
        `С3.2.1 Drugs taken via injection in the past 30 days? Tramadol/tramal`,
        `С3.2.2 Drugs taken via injection in the past 30 days? Heroin`,
        `С3.2.3 Drugs taken via injection in the past 30 days? Liquid opium extract`,
        `С3.2.4 Drugs taken via injection in the past 30 days? Other Opiates`,
        `С3.2.5 Drugs taken via injection in the past 30 days? Cocaine`,
        `С3.2.6 Drugs taken via injection in the past 30 days? Amphetamine`,
        `С3.2.7 Drugs taken via injection in the past 30 days? Methamphetamine powder (crystallized)`,
        `С3.2.8 Drugs taken via injection in the past 30 days? Methamphetamine liquid («tina», «pervitin»)`,
        `С3.2.9 Drugs taken via injection in the past 30 days? Methcathinone («tweaker»)`,
        `С3.2.10 Drugs taken via injection in the past 30 days? Cathinone («magma»)`,
        `С3.2.11 Drugs taken via injection in the past 30 days? “Extasy”, MDMA`,
        `С3.2.12 Drugs taken via injection in the past 30 days? Other Stimulants`,
        `С3.2.13 Drugs taken via injection in the past 30 days? LCD, mushrooms`,
        `С3.2.14 Drugs taken via injection in the past 30 days? Other`
      ), ~ . == "No") ~ "No",
      
      TRUE ~ NA_character_
    )
  )

# load 2011 data
sw_data_2011_raw <- read_excel("2011_IBBS_FSW_TLS AND RDS_Data.xlsx")

# variables to rename
rename_map_2011 <- c(
  sw_freq_6m = "А11. How often have your provided sexual services for a fee in THE LAST 6 MONTHS?",
  sw_freq_7d = "А12. How many days DURING THE LAST WEEK [7 days] have you provided sexual services for a fee?",
  education = "А1. Your education",
  residence = "А3. Where do you live in this city?",
  marital_status = "А8. Choose from the suggested options one corresponding to your marital status at the moment",
  age = "S2. Your age:",
  age_first_sex = "В1. At what age did you have sexual relations for the first time?",
  age_first_sw = "В2. How old were you when you provided sexual services for a fee (money or other) for the first time?",
  city = "City",
  partners_sw_7d = "В4.1 During THE RECENT (WORKING) WEEK, how many persons listed below were among your sexual partners [clients]? Partners [clients] who you RECEIVED A FEE [money or other] from",
  partners_nonsw_perm_7d = "В4.2 During THE RECENT (WORKING) WEEK, how many persons listed below were among your sexual partners [clients]? Permanent partners who you RECEIVED NO FEE [money or other] from",
  partners_nonsw_cas_7d = "В4.3 During THE RECENT (WORKING) WEEK, how many persons listed below were among your sexual partners [clients]? Casual partners who you RECEIVED NO FEE [money or other] fromе]",
  partners_total_7d = "В4.4 How MANY DIFFERENT sexual partners in total did you have during the recent (working) week, including your husband or your permanent sexual partner with whom you live?",
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

# derived variables
sw_data_2011_clean <- sw_data_2011_clean %>%
  mutate(
    client_condom_bin_30d = case_when(
      `B12.2.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with a CLIENT while providing such service as vaginal sex?` == "There was a case of not using" |
      `В12.3.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with a CLIENT while providing such service as anal sex?` == "There was a case of not using" ~ "No",
      
      `B12.2.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with a CLIENT while providing such service as vaginal sex?` == "There was no such case" &
      `В12.3.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with a CLIENT while providing such service as anal sex?` == "There was no such case" ~ "Yes",
      
      TRUE ~ NA_character_
    ),
    alcohol_30d_bin = case_when(
      alcohol_30d_num %in% c("Every day", "Less than once a week", "No less than once a week") ~ "Yes",
      alcohol_30d_num == "Never" ~ "No",
      TRUE ~ NA_character_
    ),
    alcohol_30d_daily_bin = case_when(
      alcohol_30d_num == "Every day" ~ "Yes",
      alcohol_30d_num %in% c("Never", "Less than once a week", "No less than once a week") ~ "No",
      TRUE ~ NA_character_
    ),
    ngo_access_12m = ifelse(
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Syringe exchange` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Obtaining disinfecting solutions` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Obtaining hygienic means` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Obtaining condoms` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Obtaining information booklets or brochures` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Helpline service` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Attending mutual support groups` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Attending consultations on safe injecting drug use` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Attending consultations on HIV/AIDS, STI, and ways of their prevention` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Lawyer’s consultations` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Psychologist’s consultations` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Peer-to-peer consultations` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Free HIV/AIDS testing` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Free testing for venereal diseases (testing and counseling)` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Free medical treatment of venereal diseases` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Other` == "Yes",
      "Yes",
      "No"
    ),
    sex_with_alcohol_30d = case_when(
      `С5.1 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s)? Alcohol` == "Never" ~ "No",
      `С5.1 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s)? Alcohol` %in% c("50%", ">50%", "<50%", "Always") ~ "Yes",
      TRUE ~ NA_character_
    ),
    sex_with_drugs_30d = case_when(
      `С5.2 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s)? Narcotic substances` == "Never" ~ "No",
      `С5.2 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s)? Narcotic substances` %in% c("50%", ">50%", "<50%", "Always") ~ "Yes",
      TRUE ~ NA_character_
    )
  )

# load 2013 data
sw_data_2013_raw <- read_excel("2013_IBBS_FSW_TLS AND RDS_Data.xlsx")

# variables to rename
rename_map_2013 <- c(
  sw_freq_7d = "А19. How many days in the LAST WEEK [7 days] did you provide sexual services?",
  education = "А6. What is your educational level?",
  residence_30d = "А8. What has been your permanent place of residence in the last month (30 days)?",
  marital_status = "А12. Choose from the suggested alternatives the one corresponding to your marital status at the moment",
  income_30d_cat = "А17. Tell me please, what has been your PERSONAL income in the last 30 days?",
  age = "I2. Your age",
  age_first_sex = "В1.At what age did you start sexual relations for the first time?",
  age_first_sw = "В2. How old were you when you provided sexual services for a fee (money or other) for the first time?",
  city = "City",
  city_travel_12m = "А11. Have you ever left this city for more than 1 month [30 days] during THE LAST 12 MONTHS to provide sexual services?",
  partners_sw_30d = "B.4.1_2 Number of sexual partners in the PAST MONTH (30 days) Clients from which you RECEIVED A FEE [money or other] for sexual services",
  partners_nonsw_perm_30d = "B.4.2_2 Number of sexual partners in the PAST MONTH (30 days) Permanent from which you RECEIVED NO FEE [money or other]",
  partners_nonsw_cas_30d = "B.4.3_2 Number of sexual partners in the PAST MONTH (30 days) Casual partners from which you RECEIVED NO FEE [money or other])",
  partners_total_30d = "В4.4 How MANY DIFFERENT sexual partners in total did you have during the past month  (30 days), including your husband or your permanent sexual partner with whom you live?",
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
  used_syringe_30d_bin = "С4.2. Did you inject a drug with the injecting equipment (syringe, needle) previously used by another person or drugs that were prepared in the common pot or you don’t know how the syringe has been filled?",
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

# derived variables
sw_data_2013_clean <- sw_data_2013_clean %>%
  mutate(
    alcohol_30d_daily_bin = case_when(
      alcohol_30d_num == c("20-39 times","40 times and more") ~ "Yes",
      alcohol_30d_num %in% c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times") ~ "No",
      TRUE ~ NA_character_
    ),
    alcohol_30d_bin = case_when(
      alcohol_30d_num %in% c("1-2 times", "3-5 times", "6-9 times", "10-19 times", "20-39 times", "40 times and more") ~ "Yes",
      alcohol_30d_num == "Never" ~ "No",
      TRUE ~ NA_character_
    ),
    violence_support_any = case_when(
      violence_support_ngo == "Yes" |
      `F3_2. Have you addressed anywhere or to anyone for help? To relatives (parents, husband/cohabitant, friend)` == "Yes" |
      `F3_3. Have you addressed anywhere or to anyone for help? To other client, whom I provide sexual services` == "Yes" |
      `F3_4. Have you addressed anywhere or to anyone for help? To police` == "Yes" |
      `F3_5. Have you addressed anywhere or to anyone for help? To other girl/woman who provide sexual services` == "Yes" |
      `F3_6. Have you addressed anywhere or to anyone for help? To pimp/”mom”` == "Yes" |
      `F3_7. Have you addressed anywhere or to anyone for help? Other` == "Yes" ~ "Yes",
      
      `F3_8. Have you addressed anywhere or to anyone for help? Did not address for help` == "Yes" ~ "No",
      
      `F3_9. Have you addressed anywhere or to anyone for help? Difficult to answer/refusal to answer` == "Yes" ~ "Refuse to answer",
      
      TRUE ~ NA_character_
    ),
    ngo_access_12m = case_when(
      `D6_1 What help or services did you receive from NGO in the LAST 12 MONTHS? Free treatment of venereal diseases` == "Yes" |
      `D6_2 What help or services did you receive from NGO in the LAST 12 MONTHS? Free testing for venereal diseases` == "Yes" |
      `D6_3 What help or services did you receive from NGO in the LAST 12 MONTHS? Free HIV/AIDS testing` == "Yes" |
      `D6_4 What help or services did you receive from NGO in the LAST 12 MONTHS? Free testing for Hepatitis C` == "Yes" |
      `D6_5 What help or services did you receive from NGO in the LAST 12 MONTHS? Attending groups of mutual support` == "Yes" |
      `D6_6 What help or services did you receive from NGO in the LAST 12 MONTHS? Attending consultations on HIV/AIDS, sexually transmitted diseases, and ways of their prevention` == "Yes" |
      `D6_7 What help or services did you receive from NGO in the LAST 12 MONTHS? Attending consultations on safe injecting drug use` == "Yes" |
      `D6_8 What help or services did you receive from NGO in the LAST 12 MONTHS?  Peer-to-peer consultations` == "Yes" |
      `D6_9 What help or services did you receive from NGO in the LAST 12 MONTHS? Psychologist’s consultations` == "Yes" |
      `D6_10 What help or services did you receive from NGO in the LAST 12 MONTHS?  Lawyer’s consultations` == "Yes" |
      `D6_11 What help or services did you receive from NGO in the LAST 12 MONTHS?  Helpline service` == "Yes" |
      `D6_12 What help or services did you receive from NGO in the LAST 12 MONTHS?  Syringe exchange` == "Yes" |
      `D6_13 What help or services did you receive from NGO in the LAST 12 MONTHS?  Obtaining disinfecting solutions` == "Yes" |
      `D6_14 What help or services did you receive from NGO in the LAST 12 MONTHS?  Obtaining information booklets or brochures` == "Yes" |
      `D6_15 What help or services did you receive from NGO in the LAST 12 MONTHS? Obtaining hygienic means` == "Yes" |
      `D6_16 What help or services did you receive from NGO in the LAST 12 MONTHS?  Obtaining condoms` == "Yes" |
      `D6_17 What help or services did you receive from NGO in the LAST 12 MONTHS? Other` == "Yes" ~ "Yes",
      
      `D6_18 What help or services did you receive from NGO in the LAST 12 MONTHS?  Did not receive help or services` == "Yes" ~ "No",
      
      TRUE ~ NA_character_
    )
  )

# load 2015 data
sw_data_2015_raw <- read_excel("2015_IBBS_SW_TLS AND RDS_Data.xlsx")

# variables to rename
rename_map_2015 <- c(
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
  sw_freq_7d = "А20. How many days in the LAST WEEK [7 days] did you provide sexual services?",
  partners_total_30d = "Total number of different sexual partners in the LAST 30 DAYS.",
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

# derived variables
sw_data_2015_clean <- sw_data_2015_clean %>%
  mutate(
    residence_30d = case_when(
      `А9_1 What has been your permanent place of residence in the last month (30 days) - My own apartment` == "Yes" ~ "Own apartment",
      `А9_2 What has been your permanent place of residence in the last month (30 days) - The apartment of my relatives / friends (do not pay rent)` == "Yes" ~ "Relatives/friends apartment",
      `А9_3 What has been your permanent place of residence in the last month (30 days) - Rented apartment (rent alone or together with someone)` == "Yes" ~ "Rented apartment",
      `А9_4 What has been your permanent place of residence in the last month (30 days) - Hostel` == "Yes" ~ "Hostel",
      `А9_5 What has been your permanent place of residence in the last month (30 days) - Shelter, children’s home, boarding house` == "Yes" ~ "Shelter/boarding house",
      `А9_6 What has been your permanent place of residence in the last month (30 days) - Nowhere to live (frequent change of the place of residence)` == "Yes" ~ "Unstable housing",
      `А9_7 What has been your permanent place of residence in the last month (30 days) - Street, abandoned apartments,basement or attic, railway stations (homeless)` == "Yes" ~ "Homeless",
      `А9_8 What has been your permanent place of residence in the last month (30 days) - Other` == "Yes" ~ "Other",
      TRUE ~ NA_character_
    ),
    partners_sw_30d = as.numeric(`Number of irregular clients from whom you RECEIVED COMPENSATION [money or other] for providing sexual services in the LAST 30 DAYS.`) + 
                      as.numeric(`Number of regular clients from whom you RECEIVED COMPENSATION [money or other] for providing sexual services in the LAST 30 DAYS.`),
    partners_nonsw_30d = as.numeric(`Number of regular sexual partners from whom you did NOT RECEIVE COMPENSATION [money or other] in the LAST 30 DAYS.`) + 
                        as.numeric(`Number of casual sexual partners from whom you did NOT RECEIVE COMPENSATION [money or other] in the LAST 30 DAYS.`),                      
    client_condom_bin_30d = case_when(
    `В9.1. Please think of the last 30 DAYS once again. Was there a case when you DID NOT use a condom with your client during vaginal sex?` == "Yes, there was such a case" |
    `В8.1. Please think of the last 30 DAYS once again. Was there a case when you DID NOT use a condom with your client during anal sex?` == "Yes, there was such a case" ~ "No",
    
    `В9.1. Please think of the last 30 DAYS once again. Was there a case when you DID NOT use a condom with your client during vaginal sex?` == "Always used" &
    `В8.1. Please think of the last 30 DAYS once again. Was there a case when you DID NOT use a condom with your client during anal sex?` == "Always used" ~ "Yes",
    
    TRUE ~ NA_character_
    ),
    client_condom_freq_30d = case_when(
    `B9. How often have you used a condom during vaginal sex?` == "Always (100%)" &
    `B10. How often have you used a condom during anal sex?` == "Always (100%)" ~ "Always (100%)",
    
    `B9. How often have you used a condom during vaginal sex?` == "Always (100%)" &
    `B10. How often have you used a condom during anal sex?` == "I didn't have such sex" ~ "Always (100%)",
    
    `B9. How often have you used a condom during vaginal sex?` == "I didn't have such sex" &
    `B10. How often have you used a condom during anal sex?` == "Always (100%)" ~ "Always (100%)",
    
    `B9. How often have you used a condom during vaginal sex?` %in% c("In the majority of cases (75%)", "Always (100%)") &
    `B10. How often have you used a condom during anal sex?` %in% c("In the majority of cases (75%)", "Always (100%)", "I didn't have such sex") ~ "In the majority of cases (75%)",
    
    `B9. How often have you used a condom during vaginal sex?` %in% c("In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)") &
    `B10. How often have you used a condom during anal sex?` %in% c("In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)", "I didn't have such sex") ~ "In half of cases (50%)",
    
    `B9. How often have you used a condom during vaginal sex?` %in% c("Sometimes (25%)", "In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)") &
    `B10. How often have you used a condom during anal sex?` %in% c("Sometimes (25%)", "In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)", "I didn't have such sex") ~ "Sometimes (25%)",
    
    `B9. How often have you used a condom during vaginal sex?` != "Never" &
    `B10. How often have you used a condom during anal sex?` != "Never" &
    (`B9. How often have you used a condom during vaginal sex?` == "Rarely (less than 10%)" |
    `B10. How often have you used a condom during anal sex?` == "Rarely (less than 10%)") ~ "Rarely (less than 10%)",
    
    `B9. How often have you used a condom during vaginal sex?` == "Never" |
    `B10. How often have you used a condom during anal sex?` == "Never" ~ "Never",
    
    TRUE ~ NA_character_
  ),
    alcohol_30d_daily_bin = case_when(
      alcohol_30d_num >= 30 ~ "Yes",
      alcohol_30d_num < 30 ~ "No",
      TRUE ~ NA_character_
    ),
    alcohol_30d_bin = case_when(
      alcohol_30d_num >= 1 ~ "Yes",
      alcohol_30d_num == 0 ~ "No",
      TRUE ~ NA_character_
    ),
    violence_rape_ever = case_when(
      `F2.1_7 If “yes”, how? - Raped` == "Yes" |
      `F2.1_8 If “yes”, how? - Forced to provide sexual services in the form of perversion` == "Yes" ~ "Yes",
      
      `F2.1_7 If “yes”, how? - Raped` == "No" &
      `F2.1_8 If “yes”, how? - Forced to provide sexual services in the form of perversion` == "No" ~ "No",
      
      `F2.1_7 If “yes”, how? - Raped` == "No question asked" &
      `F2.1_8 If “yes”, how? - Forced to provide sexual services in the form of perversion` == "No question asked" ~ "No question asked",
      
      TRUE ~ NA_character_
    ),
    drugs_30d_bin = case_when(
      `C2_1 Have you used drugs non-injectably (smoked, sniffed, swallowed, etc.) in the past 30 days (last month)?` == "Yes" |
      `idu_30d_bin` == "Yes" ~ "Yes",
      
      `C2_1 Have you used drugs non-injectably (smoked, sniffed, swallowed, etc.) in the past 30 days (last month)?` == "No" &
      `idu_30d_bin` == "No" ~ "No",
      
      `C2_1 Have you used drugs non-injectably (smoked, sniffed, swallowed, etc.) in the past 30 days (last month)?` == "No question asked" &
      `idu_30d_bin` == "No question asked" ~ "No question asked",
      
      TRUE ~ NA_character_
    ),
    drugs_30d_num = as.numeric(`C2_1_1 Have you used drugs non-injectably in the past 30 days (last month)? If yes, how many times?`) + 
                as.numeric(idu_30d_num),
   sex_with_drugs_30d = case_when(
      `С5_2 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Narcotic substances` == "Never" ~ "No",
      `С5_2 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Narcotic substances` %in% c("Always (100%)", "In the majority of cases (75%)", "In half of cases (50%)", "Sometimes (25%)", "Rarely (less than 10%)") ~ "Yes",
      `С5_2 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Narcotic substances` == "Did not use in the last 30 days" ~ "Did not use drugs",
      TRUE ~ NA_character_
    ),
    sex_with_alcohol_30d = case_when(
      `С5_1 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Alcohol` == "Never" ~ "No",
      `С5_1 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Alcohol` %in% c("Always (100%)", "In the majority of cases (75%)", "In half of cases (50%)", "Sometimes (25%)", "Rarely (less than 10%)") ~ "Yes",
      `С5_1 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Alcohol` == "Did not use in the last 30 days" ~ "Did not use alcohol",
      TRUE ~ NA_character_
    ),
    sex_with_drugs_and_alcohol_30d = case_when(
      `С5_3 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Alcohol + drugs` == "Never" ~ "No",
      `С5_3 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Alcohol + drugs` %in% c("Always (100%)", "In the majority of cases (75%)", "In half of cases (50%)", "Sometimes (25%)", "Rarely (less than 10%)") ~ "Yes",
      `С5_3 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Alcohol + drugs` == "Did not use in the last 30 days" ~ "Did not use substances",
      TRUE ~ NA_character_
    ),
     violence_support_any = case_when(
      violence_support_ngo == "Yes" |
      `F3_2 Have you addressed any where or to anyone for help? - To relatives (parents, husband/cohabitant, friend)` == "Yes" |
      `F3_3 Have you addressed any where or to anyone for help? - To other client, whom I provide sexual services` == "Yes" |
      `F3_4 Have you addressed any where or to anyone for help? - To police` == "Yes" |
      `F3_5 Have you addressed any where or to anyone for help? - To other girl/woman who provide sexual services` == "Yes" |
      `F3_6 Have you addressed any where or to anyone for help? - To pimp/”mom”` == "Yes" |
      `F3_7 Have you addressed any where or to anyone for help? - Other` == "Yes" ~ "Yes",
      
      `F3_8 Have you addressed any where or to anyone for help? - Did not address for help` == "Yes" ~ "No",
      
      `F3_9 Have you addressed any where or to anyone for help? - Difficult to answer/refusal to answer` == "Yes" ~ "Refuse to answer",
      
      TRUE ~ NA_character_
    ),             
    primary_drug_30m = case_when(
      `С4.1_1 Which of the injecting drugs do you consider a primary one for you? - Hanka` == "Yes" ~ "Hanka",
      `С4.1_2 Which of the injecting drugs do you consider a primary one for you? - Pervitin` == "Yes" ~ "Pervitin",
      `С4.1_3 Which of the injecting drugs do you consider a primary one for you? - Stimulants` == "Yes" ~ "Stimulants",
      `С4.1_4 Which of the injecting drugs do you consider a primary one for you? - Methadone` == "Yes" ~ "Methadone",
      `С4.1_5 Which of the injecting drugs do you consider a primary one for you? - Nalbuphine` == "Yes" ~ "Nalbuphine",
      `С4.1_6 Which of the injecting drugs do you consider a primary one for you? - Salt` == "Yes" ~ "Salt",
      `С4.1_7 Which of the injecting drugs do you consider a primary one for you? - Opium` == "Yes" ~ "Opium",
      `С4.1_8 Which of the injecting drugs do you consider a primary one for you? - Diphenhydramine` == "Yes" ~ "Diphenhydramine",
      `С4.1_9 Which of the injecting drugs do you consider a primary one for you? - Amphetamine` == "Yes" ~ "Amphetamine",
      `С4.1_10 Which of the injecting drugs do you consider a primary one for you? - Subutex` == "Yes" ~ "Subutex",
      `С4.1_11 Which of the injecting drugs do you consider a primary one for you? - Methamphetamine (crystal)` == "Yes" ~ "Methamphetamine (crystal)",
      
      # If all are "No", then no primary drug
      if_all(c(
        `С4.1_1 Which of the injecting drugs do you consider a primary one for you? - Hanka`,
        `С4.1_2 Which of the injecting drugs do you consider a primary one for you? - Pervitin`,
        `С4.1_3 Which of the injecting drugs do you consider a primary one for you? - Stimulants`,
        `С4.1_4 Which of the injecting drugs do you consider a primary one for you? - Methadone`,
        `С4.1_5 Which of the injecting drugs do you consider a primary one for you? - Nalbuphine`,
        `С4.1_6 Which of the injecting drugs do you consider a primary one for you? - Salt`,
        `С4.1_7 Which of the injecting drugs do you consider a primary one for you? - Opium`,
        `С4.1_8 Which of the injecting drugs do you consider a primary one for you? - Diphenhydramine`,
        `С4.1_9 Which of the injecting drugs do you consider a primary one for you? - Amphetamine`,
        `С4.1_10 Which of the injecting drugs do you consider a primary one for you? - Subutex`,
        `С4.1_11 Which of the injecting drugs do you consider a primary one for you? - Methamphetamine (crystal)`
      ), ~ . == "No") ~ "No drug use",
      
      # If all are "No question asked"
      if_all(c(
        `С4.1_1 Which of the injecting drugs do you consider a primary one for you? - Hanka`,
        `С4.1_2 Which of the injecting drugs do you consider a primary one for you? - Pervitin`,
        `С4.1_3 Which of the injecting drugs do you consider a primary one for you? - Stimulants`,
        `С4.1_4 Which of the injecting drugs do you consider a primary one for you? - Methadone`,
        `С4.1_5 Which of the injecting drugs do you consider a primary one for you? - Nalbuphine`,
        `С4.1_6 Which of the injecting drugs do you consider a primary one for you? - Salt`,
        `С4.1_7 Which of the injecting drugs do you consider a primary one for you? - Opium`,
        `С4.1_8 Which of the injecting drugs do you consider a primary one for you? - Diphenhydramine`,
        `С4.1_9 Which of the injecting drugs do you consider a primary one for you? - Amphetamine`,
        `С4.1_10 Which of the injecting drugs do you consider a primary one for you? - Subutex`,
        `С4.1_11 Which of the injecting drugs do you consider a primary one for you? - Methamphetamine (crystal)`
      ), ~ . == "No question asked") ~ "No question asked",
      
      TRUE ~ NA_character_
    )
)



# recategorise












sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    education = fct_recode(education,
      "Primary education" = "Primary (incomplete 7 years)",
      "Basic secondary education" = "Basic (incomplete) secondary (complete 9 years)",
      "Secondary education" = "Complete general secondary (or vocational) (11 years etc.), incomplete higher",
      "Basic higher education" = "Basic higher (technical school, higher educational institutions of accreditation levels I and II)",
      "Completed higher education" = "Complete higher education (higher educational institutions of accreditation levels III and IV)",
      "No answer" = "No answer",
      "Other" = "Other"
    ),
    marital_status = fct_recode(marital_status,
      "Single and have no regular sexual partner" = "Unmarried and not living with a sexual partner",
      "Living with permanent sexual partner" = "Officially unmarried but living with a permanent sexual partner",
      "Living with permanent sexual partner" = "Married and living with my husband",
      "Married but have other regular sexual partner/partners" = "Married but living with some other sexual partner",
      "Married but do not live with neither a wife/husband" = "Married but living neither with my husband nor with some other sexual partner",
      "NA" = NA
    ),
    country_travel_12m = ifelse(!is.na(country_travel_12m), "Yes", "No"),
    country_travel_12m = factor(country_travel_12m, levels = c("No", "Yes"))
    )

View(sw_data_2008_clean)







## append data

## prepare longitudinal data 

# load data
sw_data_linkage <- read_excel("SW IBBS linkage.xlsx")
sw_data_2013_clean <- read_excel("sw_data_2013_clean.xlsx")
sw_data_2015_clean <- read_excel("sw_data_2015_clean.xlsx")
sw_data_2017_clean <- read_excel("sw_data_2017_clean.xlsx")
sw_data_2021_clean <- read_excel("sw_data_2021_clean.xlsx")

# Create a list of data frames and their corresponding IDs
data_frames <- list(
  "2013_id" = sw_data_2013_clean,
  "2015_id" = sw_data_2015_clean,
  "2017_id" = sw_data_2017_clean,
  "2021_id" = sw_data_2021_clean
)

# Initialize an empty list to store the linked data frames
linked_data <- list()

# Loop through the data frames
for (id_col in names(data_frames)) {
  linked_data[[id_col]] <- sw_data_linkage %>%
    select(id, !!sym(id_col)) %>%
    left_join(data_frames[[id_col]], by = id_col)
}

# Only keep rows where the ID is not NA
linked_data <- lapply(linked_data, function(df) {
  df %>% filter(!is.na(id))
})

# Function to show levels for each variable
variable_levels <- lapply(sw_data_2008_clean, function(x) {
  if (is.factor(x)) {
    levels(x)   # show levels if factor
  } else if (is.character(x)) {
    unique(x)   # show unique values if character
  } else {
    "continuous/numeric"  # mark numeric/continuous variables
  }
})

# Print results
variable_levels
