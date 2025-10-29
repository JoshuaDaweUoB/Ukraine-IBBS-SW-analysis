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
  partners_sw_7d = "А12. В4.1 During THE RECENT (WORKING) WEEK, how many persons listed below were among your sexual partners [clients]? Partners [clients] who you RECEIVED A FEE [money or other] from",
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
  hiv_tested_12m = "F8. Let's specify if that was during the RECENT 12 MONTHS",
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
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Lawyer's consultations` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Psychologist's consultations` == "Yes" |
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

# recategorise

# Assume your data is called df
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(

    ## ---- EDUCATION ----
    education_rec = case_when(
      education == "Primary (incomplete 7 years)" ~ 0,
      education == "Basic (incomplete) secondary (complete 9 years)" ~ 1,
      education == "Complete general secondary (or vocational) (11 years etc.)" ~ 2,
      education == "Basic higher (technical school, higher educational institutions of accreditation levels I and II)" ~ 3,
      education == "incomplete higher" ~ 4,
      education == "Complete higher education (higher educational institutions of accreditation levels III and IV)" ~ 5,
      TRUE ~ 6 # Other or No answer
    ),
    education_rec = factor(
      education_rec,
      levels = 0:6,
      labels = c(
        "Primary education",
        "Basic secondary education",
        "Secondary education",
        "Basic higher education",
        "Incomplete higher education",
        "Completed higher education",
        "Don't know"
      )
    ),

    ## ---- MARITAL STATUS ----
    marital_status_rec = case_when(
      marital_status == "Unmarried and not living with a sexual partner" ~ 0,
      marital_status == "Officially unmarried but living with a permanent sexual partner" ~ 1,
      marital_status == "Married but living with some other sexual partner" ~ 2,
      marital_status == "Married but living neither with my husband nor with some other sexual partner" ~ 3,
      TRUE ~ NA_real_
    ),
    marital_status_rec = factor(
      marital_status_rec,
      levels = 0:3,
      labels = c(
        "Single and have no regular sexual partner",
        "Single but have regular sexual partner(s)",
        "Married but have other regular sexual partner(s)",
        "Married but do not live with wife/husband"
      )
    ),

    ## ---- CITY ----
    city_rec = case_when(
      city == "Dnipro" ~ 4,
      city == "Donetsk" ~ 5,
      city == "Kyiv" ~ 11,
      city == "Kropyvnytskyi" ~ 10,
      city == "Luhansk" ~ 12,
      city == "Lutsk" ~ 14,
      city == "Lviv" ~ 13,
      city == "Mykolayiv" ~ 16,
      city == "Odesa" ~ 17,
      city == "Poltava" ~ 18,
      city == "Simferopol" ~ 21,
      city == "Sumy" ~ 22,
      city == "Kharkiv" ~ 7,
      city == "Kherson" ~ 8,
      city == "Khmelnytskyi" ~ 9,
      city == "Cherkasy" ~ 1,
      TRUE ~ NA_real_
    ),
    city_rec = factor(
      city_rec,
      levels = c(1,4,5,7,8,9,10,11,12,13,14,16,17,18,21,22),
      labels = c("Cherkasy", "Dnipro", "Donetsk", "Kharkiv", "Kherson", "Khmelnytskyi",
                 "Kropyvnytskyi", "Kyiv", "Luhansk", "Lviv", "Lutsk", "Mykolayiv",
                 "Odesa", "Poltava", "Simferopol", "Sumy")
    ),

    ## ---- CITY TRAVEL 12M ----
    city_travel_12m_rec = case_when(
      city_travel_12m == "No" ~ 0,
      city_travel_12m == "Yes" ~ 1,
      TRUE ~ NA_real_
    ),
    city_travel_12m_rec = factor(
      city_travel_12m_rec,
      levels = c(0,1),
      labels = c("No, I haven't", "Yes, to another city or country")
    ),

    ## ---- CLIENT CONDOM LAST SEX ----
    client_condom_lastsex_rec = case_when(
      client_condom_lastsex == "No" ~ 0,
      client_condom_lastsex == "Yes" ~ 1,
      client_condom_lastsex == "Difficult to answer" ~ 3,
      TRUE ~ 2  # No answer/refusal
    ),
    client_condom_lastsex_rec = factor(
      client_condom_lastsex_rec,
      levels = 0:3,
      labels = c("No", "Yes", "Refusal to answer", "Don't know/don't remember")
    ),

    ## ---- CONDOM ACCESS 12M ----
    condom_access_12m_rec = case_when(
      condom_access_12m == "No" ~ 0,
      condom_access_12m == "Yes" ~ 1,
      condom_access_12m == "Difficult to answer/ Do not remember" ~ 3,
      TRUE ~ 2
    ),
    condom_access_12m_rec = factor(
      condom_access_12m_rec,
      levels = 0:3,
      labels = c("No", "Yes", "Refusal to answer", "Don't know/don't remember")
    ),

    ## ---- ALCOHOL 30D (binary) ----
    alcohol_30d_bin_rec = case_when(
      alcohol_30d_bin == "No" ~ 0,
      alcohol_30d_bin == "Yes" ~ 1,
      TRUE ~ 2
    ),
    alcohol_30d_bin_rec = factor(
      alcohol_30d_bin_rec,
      levels = 0:2,
      labels = c("No", "Yes", "No question asked")
    ),

    ## ---- DRUGS 30D (binary) ----
    drugs_30d_bin_rec = case_when(
      drugs_30d_bin == "No" ~ 0,
      drugs_30d_bin == "Yes" ~ 1,
      drugs_30d_bin == "I used before, now I don’t" ~ 1,
      TRUE ~ 2
    ),
    drugs_30d_bin_rec = factor(
      drugs_30d_bin_rec,
      levels = 0:2,
      labels = c("No", "Yes", "No question asked")
    ),

    ## ---- IDU 12M ----
    idu_12m_bin_rec = case_when(
      idu_12m_bin == "No" ~ 0,
      idu_12m_bin == "Yes" ~ 1,
      idu_12m_bin == "Used in the past, but not in the last 12 months" ~ 1,
      TRUE ~ 2
    ),
    idu_12m_bin_rec = factor(
      idu_12m_bin_rec,
      levels = 0:2,
      labels = c("No", "Yes", "No question asked")
    ),

    ## ---- NGO ACCESS 12M ----
    ngo_access_12m_rec = case_when(
      ngo_access_12m == "No" ~ 0,
      ngo_access_12m == "Yes" ~ 1,
      TRUE ~ 2
    ),
    ngo_access_12m_rec = factor(
      ngo_access_12m_rec,
      levels = 0:2,
      labels = c("No", "Yes", "No question asked")
    ),

    ## ---- HIV TESTED 12M ----
    hiv_tested_12m_rec = case_when(
      hiv_tested_12m == "No, it was earlier than 12 months ago" ~ 0,
      hiv_tested_12m == "Yes, it was during the recent 12 months" ~ 1,
      hiv_tested_12m == "No answer" ~ 2,
      hiv_tested_12m == "Difficult to answer" ~ 4,
      TRUE ~ 3
    ),
    hiv_tested_12m_rec = factor(
      hiv_tested_12m_rec,
      levels = 0:4,
      labels = c("No", "Yes", "No question asked", "Refusal to answer", "Don't know/don't remember")
    ),

    ## ---- HIV STATUS SELF-REPORT ----
    hiv_status_selfreport_rec = case_when(
      hiv_status_selfreport == "HIV negative" ~ 0,
      hiv_status_selfreport == "HIV positive" ~ 1,
      TRUE ~ 2
    ),
    hiv_status_selfreport_rec = factor(
      hiv_status_selfreport_rec,
      levels = 0:2,
      labels = c("Yes, HIV-negative", "Yes, HIV-positive", "No answer")
    ),

    ## ---- HIV TEST RESULT ----
    hiv_test_rslt_rec = case_when(
      hiv_test_rslt == "Negative" ~ 0,
      hiv_test_rslt == "Positive" ~ 1,
      TRUE ~ 2
    ),
    hiv_test_rslt_rec = factor(
      hiv_test_rslt_rec,
      levels = 0:2,
      labels = c("Negative", "Positive", "No answer")
    ),

    ## ---- SYPHILIS TEST RESULT ----
    syphilis_test_rslt_rec = case_when(
      syphilis_test_rslt == "Negative" ~ 0,
      syphilis_test_rslt == "Positive" ~ 1,
      TRUE ~ 2
    ),
    syphilis_test_rslt_rec = factor(
      syphilis_test_rslt_rec,
      levels = 0:2,
      labels = c("Negative", "Positive", "No answer")
    )
  )
















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
