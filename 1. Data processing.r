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

# load 2017 data
sw_data_2017_raw <- read_excel("2017-2018_IBBS_SW_TLS AND RDS_Data.xlsx")

# variables to rename
rename_map_2017 <- c(
  gender = "A1. Respondent’ gender",
  sw_freq_7d = "А17. How many days in the LAST WEEK [7 days] did you provide sexual services?",
  sw_num_30d = "B6. Please remember sexual intercourses with all clients during last month (30 days)/ How many sexual intercourses did you have during last month (including vaginal, anal and oral intercourses)?",
  education = "А9. What is your educational level?",
  marital_status = "А13. Choose from the suggested alternatives the one corresponding to your marital status at the moment:",
  income_30d = "А15. Tell me please, what has been your PERSONAL income in the last 30 days? UAN",
  age_first_sex = "В1.At what age did you start sexual relations for the first time?",
  age_first_sw = "В2. How old were you when you provided sexual services for a fee (money or other) for the first time?",
  city = "City",
  city_travel_12m = "А12. Have you ever left this city for more than 1 month [30 days] during THE LAST 12 MONTHS to provide sexual services?",
  partners_total_30d = "В4.6 How MANY DIFFERENT sexual partners in total did you have during the past 30 days, including your permanent sexual partner with whom you live?",
  partners_sw_24h = "В5. How many different CLIENTS whom you provided sexual services for a fee you had FOR THE LAST WORKING DAY (24 HOURS)?",
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

# derived variables
sw_data_2017_clean <- sw_data_2017_clean %>%
  mutate(
  age = dplyr::if_else(
    is.na(`Interview date`) | is.na(`А6. Specify your year of birth`),
    NA_integer_,
    as.integer(format(as.Date(`Interview date`, format = "%d/%m/%Y"), "%Y")) -
      as.integer(`А6. Specify your year of birth`)
  ),
  partners_sw_30d = case_when(
  is.na(`B4_1_1 Number in the PAST MONTH (30 days) Permanent clients from which you RECEIVED A FEE [money or other] for sexual services`) & 
  is.na(`B4_2_1 Number in the PAST MONTH (30 days) Casual clients from which you RECEIVED FEE [money or other]`) ~ NA_real_,
  
  TRUE ~ (`B4_1_1 Number in the PAST MONTH (30 days) Permanent clients from which you RECEIVED A FEE [money or other] for sexual services` %||% 0) + 
         (`B4_2_1 Number in the PAST MONTH (30 days) Casual clients from which you RECEIVED FEE [money or other]` %||% 0)
  ),
  partners_nonsw_30d = case_when(
    is.na(`B4_3_1 Number in the PAST MONTH (30 days) Permanent partners from which you RECEIVED NO FEE [money or other])`) & 
    is.na(`B4_4_1 Number in the PAST MONTH (30 days) Casual partners from which you RECEIVED NO FEE [money or other]`) ~ NA_real_,
    
    TRUE ~ (`B4_3_1 Number in the PAST MONTH (30 days) Permanent partners from which you RECEIVED NO FEE [money or other])` %||% 0) + 
          (`B4_4_1 Number in the PAST MONTH (30 days) Casual partners from which you RECEIVED NO FEE [money or other]` %||% 0)
  ),
  residence_90d = case_when(
    `А11_1 What has been your permanent place of residence in the last 3 months (90 days) - My own apartment` == "Yes" ~ "Own apartment",
    `А11_2 What has been your permanent place of residence in the last 3 months (90 days)  - The apartment of my relatives / friends (do not pay rent)` == "Yes" ~ "Relatives/friends apartment",
    `А11_3 What has been your permanent place of residence in the last 3 months (90 days)  - Rented apartment (rent alone or together with someone)` == "Yes" ~ "Rented apartment",
    `А11_4 What has been your permanent place of residence in the last 3 months (90 days)  - Hostel` == "Yes" ~ "Hostel",
    `А11_5 What has been your permanent place of residence in the last 3 months (90 days)  - Shelter, children’s home, boarding house` == "Yes" ~ "Shelter/boarding house",
    `А11_6 What has been your permanent place of residence in the last 3 months (90 days)  - Nowhere to live (frequent change of the place of residence)` == "Yes" ~ "Unstable housing",
    `А11_7 What has been your permanent place of residence in the last 3 months (90 days)  - Street, abandoned apartments,basement or attic, railway stations (homeless)` == "Yes" ~ "Homeless",
    `А11_8 What has been your permanent place of residence in the last 3 months (90 days)  - Other without information` == "Yes" ~ "Other",
    `А11_9 What has been your permanent place of residence in the last 3 months (90 days)  - Just released from incarceration (Other)` == "Yes" ~ "Released from incarceration",
    TRUE ~ NA_character_
  ),
  client_condom_bin_30d = dplyr::case_when(
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "Always (100%)" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "Always (100%)" ~ "Yes",

    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "Always (100%)" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "No such contact" ~ "Yes",

    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "No such contact" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "Always (100%)" ~ "Yes",

    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "No such contact" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "No such contact" ~ "No such contact",

    TRUE ~ "No"
  ),
  client_condom_freq_30d = case_when(
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "Always (100%)" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "Always (100%)" ~ "Always (100%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "Always (100%)" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "No such contact" ~ "Always (100%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "No such contact" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "Always (100%)" ~ "Always (100%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` %in% c("In the majority of cases (75%)", "Always (100%)") &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` %in% c("In the majority of cases (75%)", "Always (100%)", "No such contact") ~ "In the majority of cases (75%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` %in% c("In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)") &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` %in% c("In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)", "No such contact") ~ "In half of cases (50%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` %in% c("Sometimes (25%)", "In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)") &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` %in% c("Sometimes (25%)", "In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)", "No such contact") ~ "Sometimes (25%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` != "Never" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` != "Never" &
    (`B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "Rarely (less than 10%)" |
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "Rarely (less than 10%)") ~ "Rarely (less than 10%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "Never" |
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "Never" ~ "Never",
    
    TRUE ~ NA_character_
  ),
  alcohol_30d_bin = case_when(
    alcohol_30d_num >= 1 ~ "Yes",
    alcohol_30d_num == 0 ~ "No",
    TRUE ~ NA_character_
  ),
  alcohol_30d_daily_bin = case_when(
    alcohol_30d_num >= 28 ~ "Yes",
    alcohol_30d_num < 28 ~ "No",
    TRUE ~ NA_character_
  ),
  primary_drug_30d = case_when(
    `С5_1 Which of the injecting drugs have you used most often during last 12 months? - Amphetamine in powder form (“fen”)` == "Yes" ~ "Amphetamine in powder form (“fen”)",
    `С5_2 Which of the injecting drugs have you used most often during last 12 months? - Liquid opium extract (“shyrka”, “chorna”)` == "Yes" ~ "Liquid opium extract (“shyrka”, “chorna”)",
    `С5_3 Which of the injecting drugs have you used most often during last 12 months? - Heroin` == "Yes" ~ "Heroin",
    `С5_4 Which of the injecting drugs have you used most often during last 12 months? - Methadone` == "Yes" ~ "Methadone",
    `С5_5 Which of the injecting drugs have you used most often during last 12 months? - Subitex` == "Yes" ~ "Subitex",
    `С5_6 Which of the injecting drugs have you used most often during last 12 months? - Buprenorphine` == "Yes" ~ "Buprenorphine",
    `С5_7 Which of the injecting drugs have you used most often during last 12 months? - Methamphetamine solution (“vint”, “pervintin”)` == "Yes" ~ "Methamphetamine solution (“vint”, “pervintin”)",
    `С5_8 Which of the injecting drugs have you used most often during last 12 months? - Desomorphine` == "Yes" ~ "Desomorphine",
    `С5_9 Which of the injecting drugs have you used most often during last 12 months? - Poppy (poppy straw, seeds)` == "Yes" ~ "Poppy (poppy straw, seeds)",
    `С5_10 Which of the injecting drugs have you used most often during last 12 months? - Opiates` == "Yes" ~ "Opiates",
    `С5_11 Which of the injecting drugs have you used most often during last 12 months? - Bath salt` == "Yes" ~ "Bath salt",
    `С5_12 Which of the injecting drugs have you used most often during last 12 months? - Methylenedioxymethamphetamine (ecstasy, MDMA)` == "Yes" ~ "Methylenedioxymethamphetamine (ecstasy, MDMA)",
    `С5_13 Which of the injecting drugs have you used most often during last 12 months? - Marijuana (hashish, weed)` == "Yes" ~ "Marijuana (hashish, weed)",
    `С5_14 Which of the injecting drugs have you used most often during last 12 months? - Stimulants` == "Yes" ~ "Stimulants",
    `С5_15 Which of the injecting drugs have you used most often during last 12 months? - Nalbuphine` == "Yes" ~ "Nalbuphine",
    `С5_16 Which of the injecting drugs have you used most often during last 12 months? - Diphenhydramine` == "Yes" ~ "Diphenhydramine",
    `С5_17 Which of the injecting drugs have you used most often during last 12 months? -  Difficult to answer / don’t remember` == "Yes" ~ "Difficult to answer / don’t remember",
    `С5_18 Which of the injecting drugs have you used most often during last 12 months? - Pharmaceutical drugs` == "Yes" ~ "Pharmaceutical drugs",

    if_all(c(
      `С5_1 Which of the injecting drugs have you used most often during last 12 months? - Amphetamine in powder form (“fen”)`,
      `С5_2 Which of the injecting drugs have you used most often during last 12 months? - Liquid opium extract (“shyrka”, “chorna”)`,
      `С5_3 Which of the injecting drugs have you used most often during last 12 months? - Heroin`,
      `С5_4 Which of the injecting drugs have you used most often during last 12 months? - Methadone`,
      `С5_5 Which of the injecting drugs have you used most often during last 12 months? - Subitex`,
      `С5_6 Which of the injecting drugs have you used most often during last 12 months? - Buprenorphine`,
      `С5_7 Which of the injecting drugs have you used most often during last 12 months? - Methamphetamine solution (“vint”, “pervintin”)`,
      `С5_8 Which of the injecting drugs have you used most often during last 12 months? - Desomorphine`,
      `С5_9 Which of the injecting drugs have you used most often during last 12 months? - Poppy (poppy straw, seeds)`,
      `С5_10 Which of the injecting drugs have you used most often during last 12 months? - Opiates`,
      `С5_11 Which of the injecting drugs have you used most often during last 12 months? - Bath salt`,
      `С5_12 Which of the injecting drugs have you used most often during last 12 months? - Methylenedioxymethamphetamine (ecstasy, MDMA)`,
      `С5_13 Which of the injecting drugs have you used most often during last 12 months? - Marijuana (hashish, weed)`,
      `С5_14 Which of the injecting drugs have you used most often during last 12 months? - Stimulants`,
      `С5_15 Which of the injecting drugs have you used most often during last 12 months? - Nalbuphine`,
      `С5_16 Which of the injecting drugs have you used most often during last 12 months? - Diphenhydramine`,
      `С5_17 Which of the injecting drugs have you used most often during last 12 months? -  Difficult to answer / don’t remember`,
      `С5_18 Which of the injecting drugs have you used most often during last 12 months? - Pharmaceutical drugs`
    ), ~ . == "No") ~ "No drug use",

    if_all(c(
      `С5_1 Which of the injecting drugs have you used most often during last 12 months? - Amphetamine in powder form (“fen”)`,
      `С5_2 Which of the injecting drugs have you used most often during last 12 months? - Liquid opium extract (“shyrka”, “chorna”)`,
      `С5_3 Which of the injecting drugs have you used most often during last 12 months? - Heroin`,
      `С5_4 Which of the injecting drugs have you used most often during last 12 months? - Methadone`,
      `С5_5 Which of the injecting drugs have you used most often during last 12 months? - Subitex`,
      `С5_6 Which of the injecting drugs have you used most often during last 12 months? - Buprenorphine`,
      `С5_7 Which of the injecting drugs have you used most often during last 12 months? - Methamphetamine solution (“vint”, “pervintin”)`,
      `С5_8 Which of the injecting drugs have you used most often during last 12 months? - Desomorphine`,
      `С5_9 Which of the injecting drugs have you used most often during last 12 months? - Poppy (poppy straw, seeds)`,
      `С5_10 Which of the injecting drugs have you used most often during last 12 months? - Opiates`,
      `С5_11 Which of the injecting drugs have you used most often during last 12 months? - Bath salt`,
      `С5_12 Which of the injecting drugs have you used most often during last 12 months? - Methylenedioxymethamphetamine (ecstasy, MDMA)`,
      `С5_13 Which of the injecting drugs have you used most often during last 12 months? - Marijuana (hashish, weed)`,
      `С5_14 Which of the injecting drugs have you used most often during last 12 months? - Stimulants`,
      `С5_15 Which of the injecting drugs have you used most often during last 12 months? - Nalbuphine`,
      `С5_16 Which of the injecting drugs have you used most often during last 12 months? - Diphenhydramine`,
      `С5_17 Which of the injecting drugs have you used most often during last 12 months? -  Difficult to answer / don’t remember`,
      `С5_18 Which of the injecting drugs have you used most often during last 12 months? - Pharmaceutical drugs`
    ), ~ . == "No question asked") ~ "No question asked",
    TRUE ~ NA_character_
  ),
  sex_with_drugs_30d = case_when(
    `С7_2 How often did you use the Narcotic substances during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` == "Never" ~ "No",
    `С7_2 How often did you use the Narcotic substances during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` %in% c("Always (100%)", "In the majority of cases (75%)", "In half of cases (50%)", "Sometimes (25%)", "Rarely (less than 10%)") ~ "Yes",
    `С7_2 How often did you use the Narcotic substances during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` == "Didn’t use during last 30 days" ~ "Did not use drugs",
    TRUE ~ NA_character_
  ),

  sex_with_alcohol_30d = case_when(
    `С7_1 How often did you use the Alcohol during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` == "Never" ~ "No",
    `С7_1 How often did you use the Alcohol during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` %in% c("Always (100%)", "In the majority of cases (75%)", "In half of cases (50%)", "Sometimes (25%)", "Rarely (less than 10%)") ~ "Yes",
    `С7_1 How often did you use the Alcohol during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` == "Didn’t use during last 30 days" ~ "Did not use alcohol",
    TRUE ~ NA_character_
  ),

  sex_with_drugs_and_alcohol_30d = case_when(
    `С7_2 How often did you use the Alcohol + drugs during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` == "Never" ~ "No",
    `С7_2 How often did you use the Alcohol + drugs during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` %in% c("Always (100%)", "In the majority of cases (75%)", "In half of cases (50%)", "Sometimes (25%)", "Rarely (less than 10%)") ~ "Yes",
    `С7_2 How often did you use the Alcohol + drugs during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` == "Didn’t use during last 30 days" ~ "Did not use substances",
    TRUE ~ NA_character_
  ),
  ngo_access_6m = case_when(
    `F5_1 Have you received condoms from representatives of this NGO during the last 6 months? - Male condoms` == "Yes" |
    `F5_2 Have you received condoms from representatives of this NGO during the last 6 months? - Female condoms` == "Yes" ~ "Yes",

    `F5_1 Have you received condoms from representatives of this NGO during the last 6 months? - Male condoms` == "No" &
    `F5_2 Have you received condoms from representatives of this NGO during the last 6 months? - Female condoms` == "No" ~ "No",

    `F5_1 Have you received condoms from representatives of this NGO during the last 6 months? - Male condoms` == "No question asked" &
    `F5_2 Have you received condoms from representatives of this NGO during the last 6 months? - Female condoms` == "No question asked" ~ "No question asked",

    TRUE ~ NA_character_
  ),
  violence_rape_ever = case_when(
    `H1_6 If “yes”, how?  - Raped` == "Yes" |
    `H1_7 If “yes”, how?  - Forced to provide sexual services in the form of perversion` == "Yes" ~ "Yes",

    `H1_6 If “yes”, how?  - Raped` == "No" &
    `H1_7 If “yes”, how?  - Forced to provide sexual services in the form of perversion` == "No" ~ "No",

    `H1_6 If “yes”, how?  - Raped` == "No question asked" &
    `H1_7 If “yes”, how?  - Forced to provide sexual services in the form of perversion` == "No question asked" ~ "No question asked",

    TRUE ~ NA_character_
  ),
  violence_support = case_when(

    if_any(c(
      violence_support_ngo,
      `H3_2 Have you addressed anywhere or to anyone for help? - To relatives (parents, husband/cohabitant, friend)`,
      `H3_3 Have you addressed anywhere or to anyone for help? - To other client, whom I provide sexual services`,
      `H3_4 Have you addressed anywhere or to anyone for help? - To police`,
      `H3_5 Have you addressed anywhere or to anyone for help? - To other girl/woman who provide sexual services`,
      `H3_6 Have you addressed anywhere or to anyone for help? - To pimp/”mom”`,
      `H3_7 Have you addressed anywhere or to anyone for help? - Other`
    ), ~ . == "Yes") ~ "Yes",

    `H3_8 Have you addressed anywhere or to anyone for help? - Did not address for help` == "Yes" ~ "No",
    if_any(c(
      `H3_9 Have you addressed anywhere or to anyone for help? - Difficult to answer`,
      `H3_10 Have you addressed anywhere or to anyone for help? - Refusal to answer`
    ), ~ . == "Yes") ~ NA_character_,
   
    if_all(c(
      violence_support_ngo,
      `H3_2 Have you addressed anywhere or to anyone for help? - To relatives (parents, husband/cohabitant, friend)`,
      `H3_3 Have you addressed anywhere or to anyone for help? - To other client, whom I provide sexual services`,
      `H3_4 Have you addressed anywhere or to anyone for help? - To police`,
      `H3_5 Have you addressed anywhere or to anyone for help? - To other girl/woman who provide sexual services`,
      `H3_6 Have you addressed anywhere or to anyone for help? - To pimp/”mom”`,
      `H3_7 Have you addressed anywhere or to anyone for help? - Other`,
      `H3_8 Have you addressed anywhere or to anyone for help? - Did not address for help`
    ), ~ . == "No") ~ "No",

    TRUE ~ NA_character_
  )
)

# load 2021 data
sw_data_2021_raw <- read_excel("2021_IBBS_SW_TLS_Data.xlsx")

# variables to rename
rename_map_2021 <- c(
  gender = "a7 Respondent’s gender",
  sw_freq_7d = "b18_1 How many days during the last week (days) you Provided commercial sex services",
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
  partners_sw_7d = "b20 How many sexual contacts with clients you had during these days?",
  partners_total_30d = "b2_5 Total number of partners within the last month (30 days)",
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

# derived variables
sw_data_2021_clean <- sw_data_2021_clean %>%
  mutate(
  partners_sw_30d = case_when(
    is.na(`b2_1_1 How many partners with whom you had sexual contact within the last 30 days belonged to Regular client`) &
    is.na(`b2_2_1 How many partners with whom you had sexual contact within the last 30 days belonged to Non-regular client`) ~ NA_real_,
    TRUE ~
      coalesce(as.numeric(`b2_1_1 How many partners with whom you had sexual contact within the last 30 days belonged to Regular client`), 0) +
      coalesce(as.numeric(`b2_2_1 How many partners with whom you had sexual contact within the last 30 days belonged to Non-regular client`), 0)
  ),

  partners_nonsw_30d = case_when(
    is.na(`b2_3_1 How many partners with whom you had sexual contact within the last 30 days belonged to Permanent sexual partner`) &
    is.na(`b2_4_1 How many partners with whom you had sexual contact within the last 30 days belonged to Casual sexual partner`) ~ NA_real_,
    TRUE ~
      coalesce(as.numeric(`b2_3_1 How many partners with whom you had sexual contact within the last 30 days belonged to Permanent sexual partner`), 0) +
      coalesce(as.numeric(`b2_4_1 How many partners with whom you had sexual contact within the last 30 days belonged to Casual sexual partner`), 0)
  ),
  client_condom_bin_30d = case_when(
    `b12 During VAGINAL sex during the last 30 days?` == "Always (100%)" &
    `b13 And during ANAL sex during the last 30 days?` == "Always (100%)" ~ "Yes",

    `b12 During VAGINAL sex during the last 30 days?` == "Always (100%)" &
    `b13 And during ANAL sex during the last 30 days?` == "No such contacts" ~ "Yes",

    `b12 During VAGINAL sex during the last 30 days?` == "No such contacts" &
    `b13 And during ANAL sex during the last 30 days?` == "Always (100%)" ~ "Yes",

    `b12 During VAGINAL sex during the last 30 days?` == "No such contacts" &
    `b13 And during ANAL sex during the last 30 days?` == "No such contacts" ~ "No such contacts",

    TRUE ~ "No"
  ),
  client_condom_freq_30d = case_when(
    `b12 During VAGINAL sex during the last 30 days?` == "Always (100%)" &
    `b13 And during ANAL sex during the last 30 days?` == "Always (100%)" ~ "Always (100%)",
    
    `b12 During VAGINAL sex during the last 30 days?` == "Always (100%)" &
    `b13 And during ANAL sex during the last 30 days?` == "No such contacts" ~ "Always (100%)",
    
    `b12 During VAGINAL sex during the last 30 days?` == "No such contacts" &
    `b13 And during ANAL sex during the last 30 days?` == "Always (100%)" ~ "Always (100%)",
    
    `b12 During VAGINAL sex during the last 30 days?` %in% c("In most cases (75%)", "Always (100%)") &
    `b13 And during ANAL sex during the last 30 days?` %in% c("In most cases (75%)", "Always (100%)", "No such contacts") ~ "In most cases (75%)",
    
    `b12 During VAGINAL sex during the last 30 days?` %in% c("In half of the cases (50%)", "In most cases (75%)", "Always (100%)") &
    `b13 And during ANAL sex during the last 30 days?` %in% c("In half of the cases (50%)", "In most cases (75%)", "Always (100%)", "No such contacts") ~ "In half of the cases (50%)",
    
    `b12 During VAGINAL sex during the last 30 days?` %in% c("Sometimes (25%)", "In half of the cases (50%)", "In most cases (75%)", "Always (100%)") &
    `b13 And during ANAL sex during the last 30 days?` %in% c("Sometimes (25%)", "In half of the cases (50%)", "In most cases (75%)", "Always (100%)", "No such contacts") ~ "Sometimes (25%)",
    
    `b12 During VAGINAL sex during the last 30 days?` != "Never" &
    `b13 And during ANAL sex during the last 30 days?` != "Never" &
    (`b12 During VAGINAL sex during the last 30 days?` == "Rare (10%)" |
    `b13 And during ANAL sex during the last 30 days?` == "Rare (10%)") ~ "Rare (10%)",
    
    `b12 During VAGINAL sex during the last 30 days?` == "Never" |
    `b13 And during ANAL sex during the last 30 days?` == "Never" ~ "Never",
    
    TRUE ~ NA_character_
  ),
   perm_partner_condom_lastsex = case_when(
    `b33 During VAGINAL sex during the last 30 days?` %in% c("No question asked", "Refusal to answer") |
      `b34 And during ANAL sex during the last 30 days?` %in% c("No question asked", "Refusal to answer") ~ "No question asked",
    `b33 During VAGINAL sex during the last 30 days?` == "No such contacts" &
      `b34 And during ANAL sex during the last 30 days?` == "No such contacts" ~ "No such contacts",
    `b33 During VAGINAL sex during the last 30 days?` %in% c("Never", "Rare (10%)", "Sometimes (25%)", "In half of the cases (50%)", "In most cases (75%)") |
      `b34 And during ANAL sex during the last 30 days?` %in% c("Never", "Rare (10%)", "Sometimes (25%)", "In half of the cases (50%)", "In most cases (75%)") ~ "No condom",
    `b33 During VAGINAL sex during the last 30 days?` %in% c("Always (100%)", "No such contacts") &
      `b34 And during ANAL sex during the last 30 days?` %in% c("Always (100%)", "No such contacts") ~ "Condom",
    TRUE ~ NA_character_
  ),
  cas_partner_condom_lastsex = case_when(
    `b39 During VAGINAL sex during the last 30 days?` %in% c("No question asked", "Refusal to answer") |
      `b40 And during ANAL sex during the last 30 days?` %in% c("No question asked", "Refusal to answer") ~ "No question asked",
    `b39 During VAGINAL sex during the last 30 days?` == "No such contacts" &
      `b40 And during ANAL sex during the last 30 days?` == "No such contacts" ~ "No such contacts",
    `b39 During VAGINAL sex during the last 30 days?` %in% c("Never", "Rare (10%)", "Sometimes (25%)", "In half of the cases (50%)", "In most cases (75%)") |
      `b40 And during ANAL sex during the last 30 days?` %in% c("Never", "Rare (10%)", "Sometimes (25%)", "In half of the cases (50%)", "In most cases (75%)") ~ "No condom",
    `b39 During VAGINAL sex during the last 30 days?` %in% c("Always (100%)", "No such contacts") &
      `b40 And during ANAL sex during the last 30 days?` %in% c("Always (100%)", "No such contacts") ~ "Condom",
    TRUE ~ NA_character_
  ),
  alcohol_30d_bin = case_when(
    `c1 How often do you have a drink containing alcohol?` %in% c("Never") ~ "No",
    `c1 How often do you have a drink containing alcohol?` %in% c("2-4 times a month", "2-3 times a week", "4 or more times a week", "Monthly or less") ~ "Yes",
    `c1 How often do you have a drink containing alcohol?` %in% c("Don't know/don't remember", "Refusal to answer") ~ "No answer",
    TRUE ~ NA_character_
  ),
  alcohol_30d_daily_bin = case_when(
    alcohol_30d_num %in% c("Used, don't remember the amount") ~ "don't remember",
    alcohol_30d_num %in% c("Refusal to answer") ~ "Refuse to answer",
    alcohol_30d_num %in% c("Not used in the last 30 days") ~ "No",
    suppressWarnings(as.numeric(alcohol_30d_num)) >= 28 ~ "Yes",
    suppressWarnings(as.numeric(alcohol_30d_num)) < 28 ~ "No",
    TRUE ~ NA_character_
  ),
  drugs_12m_bin = case_when(
    `c5 Some people try different drugs. Have you used any non-injectable drugs (smoked, sniffed, swallowed, etc.) or not?` %in% c(
      "Yes, I have been using them for the last 30 days",
      "Yes, I've been using them for the last 12 months (but not for 30 days)",
      "Так, я вживав/ла протягом останніх 12 місяців (але не протягом 30 днів)"
    ) |
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c(
      "Yes, I have been using them for the last 30 days",
      "Yes, I've been using them for the last 12 months (but not for 30 days)",
      "Так, я вживав/ла протягом останніх 12 місяців (але не протягом 30 днів)"
    ) ~ "Yes",
    `c5 Some people try different drugs. Have you used any non-injectable drugs (smoked, sniffed, swallowed, etc.) or not?` %in% c("Don't know/don't remember", "Refusal to answer") &
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c("Don't know/don't remember", "Refusal to answer") ~ "No answer",
    `c5 Some people try different drugs. Have you used any non-injectable drugs (smoked, sniffed, swallowed, etc.) or not?` %in% c("Never used or never even tried", "Yes, I used it, but more than 12 months ago") &
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c("Never used or never even tried", "Yes, I used it, but more than 12 months ago", "Don't know/don't remember", "Refusal to answer") ~ "No",
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c("Never used or never even tried", "Yes, I used it, but more than 12 months ago") &
    `c5 Some people try different drugs. Have you used any non-injectable drugs (smoked, sniffed, swallowed, etc.) or not?` %in% c("Never used or never even tried", "Yes, I used it, but more than 12 months ago", "Don't know/don't remember", "Refusal to answer") ~ "No",
    TRUE ~ NA_character_
  ),
  idu_12m_bin = case_when(
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c(
      "Yes, I have been using them for the last 30 days",
      "Yes, I've been using them for the last 12 months (but not for 30 days)",
      "Так, я вживав/ла протягом останніх 12 місяців (але не протягом 30 днів)"
    ) ~ "Yes",
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c(
      "Never used or never even tried",
      "Yes, I used it, but more than 12 months ago"
    ) ~ "No",
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c(
      "Don't know/don't remember",
      "Refusal to answer"
    ) ~ "No answer",
    TRUE ~ NA_character_
  ),
  sex_with_alcohol_30d = case_when(
    `c12_1 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL` == "Never" ~ "No",
    `c12_1 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL` %in% c(
      "Always (100%)", "In most cases (75%)", "In half of the cases (50%)", "Sometimes (25%)", "Rarely (10%)"
    ) ~ "Yes",
    `c12_1 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL` == "Has not used for the last 30 days" ~ "Did not use alcohol",
    `c12_1 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL` == "Don't know/don't remember" ~ NA_character_,
    TRUE ~ NA_character_
  ),
  sex_with_drugs_30d = case_when(
    `c12_2 How often in the last month (30 days) before sexual intercourse with the client, you used DRUGS` == "Never" ~ "No",
    `c12_2 How often in the last month (30 days) before sexual intercourse with the client, you used DRUGS` %in% c(
      "Always (100%)", "In most cases (75%)", "In half of the cases (50%)", "Sometimes (25%)", "Rarely (10%)"
    ) ~ "Yes",
    `c12_2 How often in the last month (30 days) before sexual intercourse with the client, you used DRUGS` == "Has not used for the last 30 days" ~ "Did not use drugs",
    `c12_2 How often in the last month (30 days) before sexual intercourse with the client, you used DRUGS` == "Don't know/don't remember" ~ NA_character_,
    TRUE ~ NA_character_
  ),
  sex_with_drugs_and_alcohol_30d = case_when(
    `c12_3 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL+DRUGS` == "Never" ~ "No",
    `c12_3 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL+DRUGS` %in% c(
      "Always (100%)", "In most cases (75%)", "In half of the cases (50%)", "Sometimes (25%)", "Rarely (10%)"
    ) ~ "Yes",
    `c12_3 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL+DRUGS` == "Has not used for the last 30 days" ~ "Did not use substances",
    `c12_3 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL+DRUGS` == "Don't know/don't remember" ~ NA_character_,
    TRUE ~ NA_character_
  ),
  ngo_access_12m = case_when(
    if_any(
      c(
        `f6_1 Have you received following free items, from NGO or social worker within the last 12 months? - Male condoms`,
        `f6_2 Have you received following free items, from NGO or social worker within the last 12 months? - Female condoms`,
        `f6_3 Have you received following free items, from NGO or social worker within the last 12 months? - Lubricants`,
        `f6_4 Have you received following free items, from NGO or social worker within the last 12 months? - Social worker consultation`,
        `f6_5 Have you received following free items, from NGO or social worker within the last 12 months? - HIV testing`,
        `f6_6 Have you received following free items, from NGO or social worker within the last 12 months? - Hepatitis B testing`,
        `f6_7 Have you received following free items, from NGO or social worker within the last 12 months? - Hepatitis C testing`,
        `f6_8 Have you received following free items, from NGO or social worker within the last 12 months? - Syphilis testing`,
        `f6_9 Have you received following free items, from NGO or social worker within the last 12 months? - TB screening`,
        accessed_syringe_12m,
        `f6_11 Have you received following free items, from NGO or social worker within the last 12 months? - Information materials`,
        `f6_12 Have you received following free items, from NGO or social worker within the last 12 months? - Case management by a social worker`,
      ),
      ~ . == "Yes"
    ) ~ "Yes",
    if_all(
      c(
        `f6_1 Have you received following free items, from NGO or social worker within the last 12 months? - Male condoms`,
        `f6_2 Have you received following free items, from NGO or social worker within the last 12 months? - Female condoms`,
        `f6_3 Have you received following free items, from NGO or social worker within the last 12 months? - Lubricants`,
        `f6_4 Have you received following free items, from NGO or social worker within the last 12 months? - Social worker consultation`,
        `f6_5 Have you received following free items, from NGO or social worker within the last 12 months? - HIV testing`,
        `f6_6 Have you received following free items, from NGO or social worker within the last 12 months? - Hepatitis B testing`,
        `f6_7 Have you received following free items, from NGO or social worker within the last 12 months? - Hepatitis C testing`,
        `f6_8 Have you received following free items, from NGO or social worker within the last 12 months? - Syphilis testing`,
        `f6_9 Have you received following free items, from NGO or social worker within the last 12 months? - TB screening`,
        accessed_syringe_12m,
        `f6_11 Have you received following free items, from NGO or social worker within the last 12 months? - Information materials`,
        `f6_12 Have you received following free items, from NGO or social worker within the last 12 months? - Case management by a social worker`,
      ),
      ~ . %in% c("No", "Never")
    ) ~ "No",
    if_any(
      c(
        `f6_1 Have you received following free items, from NGO or social worker within the last 12 months? - Male condoms`,
        `f6_2 Have you received following free items, from NGO or social worker within the last 12 months? - Female condoms`,
        `f6_3 Have you received following free items, from NGO or social worker within the last 12 months? - Lubricants`,
        `f6_4 Have you received following free items, from NGO or social worker within the last 12 months? - Social worker consultation`,
        `f6_5 Have you received following free items, from NGO or social worker within the last 12 months? - HIV testing`,
        `f6_6 Have you received following free items, from NGO or social worker within the last 12 months? - Hepatitis B testing`,
        `f6_7 Have you received following free items, from NGO or social worker within the last 12 months? - Hepatitis C testing`,
        `f6_8 Have you received following free items, from NGO or social worker within the last 12 months? - Syphilis testing`,
        `f6_9 Have you received following free items, from NGO or social worker within the last 12 months? - TB screening`,
        accessed_syringe_12m,
        `f6_11 Have you received following free items, from NGO or social worker within the last 12 months? - Information materials`,
        `f6_12 Have you received following free items, from NGO or social worker within the last 12 months? - Case management by a social worker`,
      ),
      ~ . == "Don't know/don’t remember"
    ) ~ "Don’t know/don’t remember",
    TRUE ~ NA_character_
  ),
  ngo_sti_12m = case_when(
    `f6_8 Have you received following free items, from NGO or social worker within the last 12 months? - Syphilis testing` == "Yes" ~ "Yes",
    `f6_8 Have you received following free items, from NGO or social worker within the last 12 months? - Syphilis testing` %in% c("No", "Never") ~ "No",
    `f6_8 Have you received following free items, from NGO or social worker within the last 12 months? - Syphilis testing` == "Don't know/don’t remember" ~ "Don’t know/don’t remember",
    TRUE ~ NA_character_
  ),
  harm_reduction_12m = case_when(
    accessed_syringe_12m == "Yes" |
      `f6_1 Have you received following free items, from NGO or social worker within the last 12 months? - Male condoms` == "Yes" |
      `f6_2 Have you received following free items, from NGO or social worker within the last 12 months? - Female condoms` == "Yes" ~ "Yes",
    accessed_syringe_12m == "No" &
      `f6_1 Have you received following free items, from NGO or social worker within the last 12 months? - Male condoms` == "No" &
      `f6_2 Have you received following free items, from NGO or social worker within the last 12 months? - Female condoms` == "No" ~ "No",
    accessed_syringe_12m == "Don't know/don’t remember" |
      `f6_1 Have you received following free items, from NGO or social worker within the last 12 months? - Male condoms` == "Don't know/don’t remember" |
      `f6_2 Have you received following free items, from NGO or social worker within the last 12 months? - Female condoms` == "Don't know/don’t remember" ~ "Don’t know/don’t remember",
    TRUE ~ NA_character_
  ),
  violence_rape_ever = case_when(
      `h2_5 If yes, in what way? Forced to provide free sexual services` == "Yes" | `h2_6 If yes, in what way? Raped` == "Yes" ~ "Yes",
      `h2_5 If yes, in what way? Forced to provide free sexual services` == "No" & `h2_6 If yes, in what way? Raped` == "No" ~ "No",
      `h2_5 If yes, in what way? Forced to provide free sexual services` == "No question asked" | `h2_6 If yes, in what way? Raped` == "No question asked" ~ "No question asked",
      TRUE ~ NA_character_
  ),
  violence_client = case_when(
      `h3_1 Who was the perpetrator? Regular client` == "Yes" | `h3_2 Who was the perpetrator? Non-regular client` == "Yes" ~ "Yes",
      `h3_1 Who was the perpetrator? Regular client` == "No" & `h3_2 Who was the perpetrator? Non-regular client` == "No" ~ "No",
      `h3_1 Who was the perpetrator? Regular client` == "No question asked" | `h3_2 Who was the perpetrator? Non-regular client` == "No question asked" ~ "No question asked",
      TRUE ~ NA_character_
  ),
  violence_support_any = case_when(
      violence_support_ngo == "Yes" |
      `h4_2 Have you contacted any person or facility after the violence incident? - To a close person (parents, husband/roommate, girlfriend)` == "Yes" |
      `h4_3 Have you contacted any person or facility after the violence incident? - To another client to whom I provide sexual services` == "Yes" |
      `h4_4 Have you contacted any person or facility after the violence incident? - To the police` == "Yes" |
      `h4_5 Have you contacted any person or facility after the violence incident? - To another sex worker` == "Yes" |
      `h4_6 Have you contacted any person or facility after the violence incident? - To the pimp/administrator of the apartment` == "Yes" |
      `h4_7 Have you contacted any person or facility after the violence incident? - Other` == "Yes" ~ "Yes",
      
      `h4_8 Have you contacted any person or facility after the violence incident? - Did not ask for help` == "Yes" ~ "No",
      
      `h4_9 Have you contacted any person or facility after the violence incident? - Don't know/don't remember` == "Yes" |
      `h4_10 Have you contacted any person or facility after the violence incident? - Refusal to answer` == "Yes" ~ "Refuse to answer",
      
      TRUE ~ NA_character_
  ),
  hiv_tested_12m = case_when(
      hiv_tested_lifetime == "No" ~ "No",
      `i8 Please, try to recall when was the last time you were tested?` == "More than 12 months ago" ~ "No",
      `i8 Please, try to recall when was the last time you were tested?` %in% c("Within the last 6 months", "Within the last 6-12 months") ~ "Yes",
      `i8 Please, try to recall when was the last time you were tested?` == "Don't know/don't remember" ~ "Don't know/don't remember",
      TRUE ~ NA_character_
  ),
  avoided_healthcare_12m_bin = case_when(
      avoided_healthcare_12m_stigma == "Yes" |
      avoided_healthcare_12m_disclosure == "Yes" |
      avoided_healthcare_12m_violence == "Yes" |
      avoided_healthcare_12m_police == "Yes" ~ "Yes",
      avoided_healthcare_12m_stigma == "No question asked" |
      avoided_healthcare_12m_disclosure == "No question asked" |
      avoided_healthcare_12m_violence == "No question asked" |
      avoided_healthcare_12m_police == "No question asked" ~ "No question asked",
      TRUE ~ "No"
  ),
  avoided_hiv_test_12m_bin = case_when(
      avoided_hiv_test_12m_stigma == "Yes" |
      avoided_hiv_test_12m_disclosure == "Yes" |
      avoided_hiv_test_12m_violence == "Yes" |
      avoided_hiv_test_12m_police == "Yes" ~ "Yes",
      avoided_hiv_test_12m_stigma == "No question asked" |
      avoided_hiv_test_12m_disclosure == "No question asked" |
      avoided_hiv_test_12m_violence == "No question asked" |
      avoided_hiv_test_12m_police == "No question asked" ~ "No question asked",
      TRUE ~ "No"
  ),
  avoided_hiv_care_12m_bin = case_when(
      avoided_hiv_care_12m_stigma == "Yes" |
      avoided_hiv_care_12m_disclosure == "Yes" |
      avoided_hiv_care_12_violence == "Yes" |
      avoided_hiv_care_12m_police == "Yes" ~ "Yes",
      avoided_hiv_care_12m_stigma == "No question asked" |
      avoided_hiv_care_12m_disclosure == "No question asked" |
      avoided_hiv_care_12_violence == "No question asked" |
      avoided_hiv_care_12m_police == "No question asked" ~ "No question asked",
      TRUE ~ "No"
  ),
  avoided_hiv_treat_12m_bin = case_when(
      avoided_hiv_treat_12m_stigma == "Yes" |
      avoided_hiv_treat_12m_disclosure == "Yes" |
      avoided_hiv_treat_12m_violence == "Yes" |
      avoided_hiv_treat_12m_police == "Yes" ~ "Yes",
      avoided_hiv_treat_12m_stigma == "No question asked" |
      avoided_hiv_treat_12m_disclosure == "No question asked" |
      avoided_hiv_treat_12m_violence == "No question asked" |
      avoided_hiv_treat_12m_police == "No question asked" ~ "No question asked",
      TRUE ~ "No"
  )
)

# recategorise



vars <- c("partners_sw_30d", "partners_nonsw_30d", "client_condom_bin_30d", "client_condom_freq_30d", "perm_partner_condom_lastsex", "alcohol_30d_daily_bin", "drugs_12m_bin", "idu_12m_bin", "sex_with_drugs_and_alcohol_30d", "sex_with_alcohol_30d", "sex_with_drugs_30d", "ngo_access_12m", "violence_rape_ever", "violence_client", "violence_support_any", "hiv_tested_12m", "avoided_healthcare_12m_bin", "avoided_hiv_test_12m_bin", "avoided_hiv_care_12m_bin", "avoided_hiv_treat_12m_bin")

present_vars <- intersect(vars, names(sw_data_2021_clean))

cat_tabs_2021 <- setNames(lapply(present_vars, function(v) {
  tbl <- table(sw_data_2021_clean[[v]], sw_data_2021_clean$hiv_test_rslt, useNA = "ifany")
  list(
    counts  = addmargins(tbl),
    row_pct = round(100 * prop.table(tbl, 1), 1)   # within category level
  )
}), present_vars)

cat_tabs_2021






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
